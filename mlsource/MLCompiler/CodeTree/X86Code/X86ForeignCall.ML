(*
    Copyright (c) 2016-22, 2026 David C.J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor X86ForeignCall(

    structure X86Code: X86CODE

    structure X86Optimise:
    sig
        type operation
        type code
        type operations = operation list
        type closureRef

        (* Optimise and code-generate. *)
        val generateCode: {code: code, ops: operations, labelCount: int, resultClosure: closureRef} -> unit

        structure Sharing:
        sig
            type operation = operation
            type code = code
            type closureRef = closureRef
        end
    end

    structure CodeArray: CODEARRAY

    sharing X86Code.Sharing = X86Optimise.Sharing = CodeArray.Sharing
): FOREIGNCALL
=
struct
    open X86Code
    open Address
    open CodeArray
        
    (* Unix X64.  The first six arguments are in rdi, rsi, rdx, rcx, r8, r9.
                  The rest are on the stack.
       Windows X64. The first four arguments are in rcx, rdx, r8 and r9.  The rest are
                   on the stack.  The caller must ensure the stack is aligned on 16-byte boundary
                   and must allocate 32-byte save area for the register args.
                   rbx, rbp, rdi, rsi, rsp, r12-r15 are saved by the called function.
       X86/32.  Arguments are pushed to the stack.
               ebx, edi, esi, ebp and esp are saved by the called function.
               We use esi to hold the argument data pointer and edi to save the ML stack pointer
       Our ML conventions use eax, ebx for the first two arguments in X86/32,
               rax, ebx, r8, r9, r10 for the first five arguments in X86/64 and
               rax, rsi, r8, r9 and r10 for the first five arguments in X86/64-32 bit.
    *)
    
    val memRegSize = 0

    val (polyWordOpSize, nativeWordOpSize) =
        case targetArch of
            Native32Bit         => (OpSize32, OpSize32)
        |   Native64Bit         => (OpSize64, OpSize64)
        |   ObjectId32Bit2Word  => (OpSize32, OpSize64)
        |   ObjectId32Bit4Word  => (OpSize32, OpSize64)
    
    (* Ebx/Rbx is used for the second argument on the native architectures but
       is replaced by esi on the object ID arch because ebx is used as the
       global base register. *)
    val mlArg2Reg = case targetArch of ObjectId32Bit2Word => esi | ObjectId32Bit4Word => esi | _ => ebx
    
    exception InternalError = Misc.InternalError
  
    fun opSizeToMove OpSize32 = Move32 | opSizeToMove OpSize64 = Move64

    val pushR = PushToStack o RegisterArg

    fun moveRR{source, output, opSize} =
        Move{source=RegisterArg source, destination=RegisterArg output, moveSize=opSizeToMove opSize}

    fun loadMemory(reg, base, offset, opSize) =
        Move{source=MemoryArg{base=base, offset=offset, index=NoIndex}, destination=RegisterArg reg, moveSize=opSizeToMove opSize}
    and storeMemory(reg, base, offset, opSize) =
        Move{source=RegisterArg reg, destination=MemoryArg {base=base, offset=offset, index=NoIndex}, moveSize=opSizeToMove opSize}
    
    val loadHeapMemory =
        case targetArch of
            ObjectId32Bit2Word =>
                (
                    fn (reg, base, offset, opSize) => 
                        Move{source=MemoryArg{base=ebx, offset=offset, index=Index4 base},
                             destination=RegisterArg reg, moveSize=opSizeToMove opSize}
                )
        |   ObjectId32Bit4Word =>
                (
                    fn (reg, base, offset, opSize) => 
                        Move{source=MemoryArg{base=ebx, offset=offset, index=Index8 base},
                             destination=RegisterArg reg, moveSize=opSizeToMove opSize}
                )
        |   _ => loadMemory


    fun loadAddress{source=(srcReg, 0), destination} =
            Move{source=RegisterArg srcReg, destination=RegisterArg destination, moveSize=opSizeToMove nativeWordOpSize}
    |   loadAddress{source=(srcReg, srcOffset), destination} =
            LoadAddress{offset=srcOffset, base=SOME srcReg, index=NoIndex, output=destination, opSize=nativeWordOpSize }

    (* Sequence of operations to move memory. *)
    fun moveMemory{source, destination, length} =
    [
        loadAddress{source=source, destination=rsi},
        loadAddress{source=destination, destination=rdi},
        (* N.B. When moving a struct in a Win64 callback the source could be rcx so only move this
           after copying the source to rsi. *)
        Move{source=NonAddressConstArg(LargeInt.fromInt length), destination=RegisterArg rcx,
             moveSize=opSizeToMove nativeWordOpSize},
        RepeatOperation MOVS8
    ]

    val makeEntryPoint: string -> machineWord = RunCall.rtsCallFull1 "PolyCreateEntryPointObject"

    datatype abi = X86_32 | X64Win | X64Unix
    
    local
        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val getABICall: unit -> int = RunCall.rtsCallFast0 "PolyGetABI"
    in
        fun getABI() =
            case getABICall() of
                0 => X86_32
            |   1 => X64Unix
            |   2 => X64Win
            |   n => raise InternalError ("Unknown ABI type " ^ Int.toString n)
    end

    (* This is now the standard entry call code. *)
    datatype fastArgs = FastArgFixed | FastArgDouble | FastArgFloat


    fun rtsCallFastGeneral (functionName, argFormats, (*resultFormat*) _, debugSwitches) =
    let
        val entryPointAddr = makeEntryPoint functionName

        (* Get the ABI.  On 64-bit Windows and Unix use different calling conventions. *)
        val abi = getABI()

        val entryPtrReg = if targetArch <> Native32Bit then r11 else ecx
        
        val nArgs = List.length argFormats
        
        local
            (* Compute stack space.  The actual number of args passed is nArgs. *)
            val argSpace =
                case abi of
                    X64Unix => Int.max(0, nArgs-6)*8
                |   X64Win => Int.max(0, nArgs-4)*8
                |   X86_32 => List.foldl(fn (FastArgDouble, n) => n+8 | (_, n) => n+4) 0 argFormats
            val align = argSpace mod 16
        in
            (* Add sufficient space so that esp will be 16-byte aligned after we
               have pushed any arguments we need to push. *)
            val stackSpace =
                if align = 0
                then memRegSize
                else memRegSize + 16 - align
        end

        (* The number of ML arguments passed on the stack. *)
        val mlArgsOnStack = Int.max(case abi of X86_32 => nArgs - 2 | _ => nArgs - 5, 0)

        val rtsExceptionLab = Label {labelNo=0}
        val labelCount = 1

        val code =
            [
                (* Clear the RTS exception - this may not be necessary but just to be safe. The RTS clears this for
                   "full" calls but it could be left over from a previous call. *)
                Move{source=NonAddressConstArg 1, destination=MemoryArg{base=ebp, offset=memRegExceptionPacket, index=NoIndex},
                    moveSize=opSizeToMove polyWordOpSize},
                Move{source=AddressConstArg entryPointAddr, destination=RegisterArg entryPtrReg, moveSize=opSizeToMove polyWordOpSize}, (* Load the entry point ref. *)
                loadHeapMemory(entryPtrReg, entryPtrReg, 0, nativeWordOpSize)(* Load its value. *)
            ] @
            (
                (* Save heap ptr.  This is in r15 in X86/64 *)
                if targetArch <> Native32Bit then [storeMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize)] (* Save heap ptr *)
                else []
            ) @
            (
                if (case abi of X86_32 => nArgs >= 3 | _ => nArgs >= 6)
                then [moveRR{source=esp, output=edi, opSize=nativeWordOpSize}] (* Needed if we have to load from the stack. *)
                else []
            ) @
            [
                storeMemory(esp, ebp, memRegStackPtr, nativeWordOpSize), (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize),
                (* Set the stack pointer past the data on the stack.  For Windows/64 add in a 32 byte save area *)
                ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackSpace), opSize=nativeWordOpSize}
            ] @
            (
                case abi of  (* Set the argument registers. *)
                    X86_32 =>
                    let
                        fun pushReg(reg, FastArgFixed) = [pushR reg]
                        |   pushReg(reg, FastArgDouble) = 
                            (* reg contains the address of the value.  This must be unboxed onto the stack. *)
                                [
                                    FPLoadFromMemory{address={base=reg, offset=0, index=NoIndex}, precision=DoublePrecision},
                                    ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 8, opSize=nativeWordOpSize},
                                    FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=DoublePrecision, andPop=true }
                                ]
                        |   pushReg(reg, FastArgFloat) =
                            (* reg contains the address of the value.  This must be unboxed onto the stack. *)
                            [
                                FPLoadFromMemory{address={base=reg, offset=0, index=NoIndex}, precision=SinglePrecision},
                                ArithToGenReg{ opc=SUB, output=esp, source=NonAddressConstArg 4, opSize=nativeWordOpSize},
                                FPStoreToMemory{ address={base=esp, offset=0, index=NoIndex}, precision=SinglePrecision, andPop=true }
                            ]

                        (* The stack arguments have to be copied first followed by the ebx and finally eax. *)
                        fun pushArgs (_, []) = []
                        |   pushArgs (_, [argType]) = pushReg(eax, argType)
                        |   pushArgs (_, [arg2Type, arg1Type]) = pushReg(ebx, arg2Type) @ pushReg(eax, arg1Type)
                        |   pushArgs (n, FastArgFixed :: argTypes) =
                                PushToStack(MemoryArg{base=edi, offset=(nArgs-n+1)* 4, index=NoIndex}) :: pushArgs(n-1, argTypes)
                        |   pushArgs (n, argType :: argTypes) =
                                (* Use esi as a temporary register. *)
                                loadMemory(esi, edi, (nArgs-n+1)* 4, polyWordOpSize) :: pushReg(esi, argType) @ pushArgs(n-1, argTypes)
                    in
                        pushArgs(nArgs, List.rev argFormats)
                    end
                
                |   X64Unix =>
                    (
                        if List.all (fn FastArgFixed => true | _ => false) argFormats
                        then
                        let
                            fun pushArgs 0 = []
                            |   pushArgs 1 = [moveRR{source=eax, output=edi, opSize=polyWordOpSize}]
                            |   pushArgs 2 = moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize} :: pushArgs 1
                            |   pushArgs 3 = moveRR{source=r8, output=edx, opSize=polyWordOpSize} :: pushArgs 2
                            |   pushArgs 4 = moveRR{source=r9, output=ecx, opSize=polyWordOpSize} :: pushArgs 3
                            |   pushArgs 5 =
                                    (* We have to move r8 into edx before we can move r10 into r8 *)
                                    moveRR{source=r8, output=edx, opSize=polyWordOpSize} ::
                                    moveRR{source=r9, output=ecx, opSize=polyWordOpSize} ::
                                    moveRR{source=r10, output=r8, opSize=polyWordOpSize} :: pushArgs 2
                            |   pushArgs 6 =
                                    (* We have to move r9 into edi before we can load r9 from the stack. *)
                                    moveRR{source=r8, output=edx, opSize=polyWordOpSize} ::
                                    moveRR{source=r9, output=ecx, opSize=polyWordOpSize} ::
                                    loadMemory(r9, edi, 8, polyWordOpSize) ::
                                    moveRR{source=r10, output=r8, opSize=polyWordOpSize} :: pushArgs 2
                            |   pushArgs _ = raise InternalError "rtsCall: Abi/argument count not implemented"
                        in
                            pushArgs nArgs
                        end
                        else case argFormats of
                            [] => []
                        |   [FastArgFixed] => [ moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed] =>
                                (* Since mlArgs2Reg is esi on 32-in-64 this is redundant. *)
                                [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed, FastArgFixed] => 
                                [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize},
                                  moveRR{source=r8, output=edx, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed, FastArgFixed, FastArgFixed] => 
                                [ moveRR{source=mlArg2Reg, output=esi, opSize=polyWordOpSize}, moveRR{source=eax, output=edi, opSize=polyWordOpSize},
                                  moveRR{source=r8, output=edx, opSize=polyWordOpSize}, moveRR{source=r9, output=ecx, opSize=polyWordOpSize} ]
                            (* One "double" argument.  The value needs to be unboxed. *)
                        |   [FastArgDouble] => [] (* Already in xmm0 *)
                            (* X64 on both Windows and Unix take the first arg in xmm0 and the second in xmm1. They are already there. *)
                        |   [FastArgDouble, FastArgDouble] => []
                        |   [FastArgDouble, FastArgFixed] => [ moveRR{source=eax, output=edi, opSize=nativeWordOpSize} ]
                        |   [FastArgFloat] => [] (* Already in xmm0 *)
                        |   [FastArgFloat, FastArgFloat] => [] (* Already in xmm0 and xmm1 *)
                                (* One float argument and one fixed. *)
                        |   [FastArgFloat, FastArgFixed] => [moveRR{source=mlArg2Reg, output=edi, opSize=polyWordOpSize} ]
                        
                        |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
                            
                    )
                
                |   X64Win =>
                    (
                        if List.all (fn FastArgFixed => true | _ => false) argFormats
                        then
                        let
                            fun pushArgs 0 = []
                            |   pushArgs 1 = [moveRR{source=eax, output=ecx, opSize=polyWordOpSize}]
                            |   pushArgs 2 = moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} :: pushArgs 1
                            |   pushArgs 3 = (* Already in r8 *) pushArgs 2
                            |   pushArgs 4 = (* Already in r9, and r8 *) pushArgs 2
                            |   pushArgs 5 = pushR r10 :: pushArgs 2
                            |   pushArgs 6 = PushToStack(MemoryArg{base=edi, offset=8, index=NoIndex}) :: pushArgs 5
                            |   pushArgs _ = raise InternalError "rtsCall: Abi/argument count not implemented"
                        in
                            pushArgs nArgs
                        end

                        else case argFormats of
                            [FastArgFixed] => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed] => [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} ]
                        |   [FastArgFixed, FastArgFixed, FastArgFixed] =>
                                [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} (* Arg3 is already in r8. *) ]
                        |   [FastArgFixed, FastArgFixed, FastArgFixed, FastArgFixed] =>
                                [ moveRR{source=eax, output=ecx, opSize=polyWordOpSize}, moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize} (* Arg3 is already in r8 and arg4 in r9. *) ]
                        |   [FastArgDouble] => [ (* Already in xmm0 *) ]
                            (* X64 on both Windows and Unix take the first arg in xmm0 and the second in xmm1. They are already there. *)
                        |   [FastArgDouble, FastArgDouble] => [ ]
                            (* X64 on both Windows and Unix take the first arg in xmm0.  On Unix the integer argument is treated
                               as the first argument and goes into edi.  On Windows it's treated as the second and goes into edx.
                               N.B.  It's also the first argument in ML so is in rax. *)
                        |   [FastArgDouble, FastArgFixed] => [ moveRR{source=eax, output=edx, opSize=nativeWordOpSize} ]
                        |   [FastArgFloat] => []
                        |   [FastArgFloat, FastArgFloat] => [] (* Already in xmm0 and xmm1 *)
                        |   [FastArgFloat, FastArgFixed] => [moveRR{source=mlArg2Reg, output=edx, opSize=polyWordOpSize}]

                        |   _ => raise InternalError "rtsCall: Abi/argument count not implemented"
                    )
            ) @
            (*  For Windows/64 add in a 32 byte save area ater we've pushed any arguments. *)
            (case abi of X64Win => [ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg 32, opSize=nativeWordOpSize}] | _ => []) @
            [
                CallAddress(RegisterArg entryPtrReg), (* Call the function *)
                loadMemory(esp, ebp, memRegStackPtr, nativeWordOpSize) (* Restore the ML stack pointer. *)
            ] @
            (
            if targetArch <> Native32Bit then [loadMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize) ] (* Copy back the heap ptr *)
            else []
            ) @
            [
                ArithMemConst{opc=CMP, address={base=ebp, offset=memRegExceptionPacket, index=NoIndex}, source=1, opSize=polyWordOpSize},
                ConditionalBranch{test=JNE, label=rtsExceptionLab},
                (* Since this is an ML function we need to remove any ML stack arguments. *)
                ReturnFromFunction mlArgsOnStack,
                JumpLabel rtsExceptionLab,
                loadMemory(eax, ebp, memRegExceptionPacket, polyWordOpSize),
                RaiseException{workReg=ecx}
            ]
 
        val profileObject = createProfileObject ()
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86Optimise.generateCode{code=newCode, labelCount=labelCount, ops=code, resultClosure=closure}
    in
        closureAsAddress closure
    end
    
    
    fun rtsCallFast (functionName, nArgs, debugSwitches) =
        rtsCallFastGeneral (functionName, List.tabulate(nArgs, fn _ => FastArgFixed), FastArgFixed, debugSwitches)
    
    (* RTS call with one double-precision floating point argument and a floating point result. *)
    fun rtsCallFastRealtoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble], FastArgDouble, debugSwitches)
    
    (* RTS call with two double-precision floating point arguments and a floating point result. *)
    fun rtsCallFastRealRealtoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble, FastArgDouble], FastArgDouble, debugSwitches)

    (* RTS call with one double-precision floating point argument, one fixed point argument and a
       floating point result. *)
    fun rtsCallFastRealGeneraltoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgDouble, FastArgFixed], FastArgDouble, debugSwitches)

    (* RTS call with one general (i.e. ML word) argument and a floating point result.
       This is used only to convert arbitrary precision values to floats. *)
    fun rtsCallFastGeneraltoReal (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFixed], FastArgDouble, debugSwitches)

    (* Operations on Real32.real values. *)

    fun rtsCallFastFloattoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat], FastArgFloat, debugSwitches)
    
    fun rtsCallFastFloatFloattoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat, FastArgFloat], FastArgFloat, debugSwitches)

    (* RTS call with one double-precision floating point argument, one fixed point argument and a
       floating point result. *)
    fun rtsCallFastFloatGeneraltoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFloat, FastArgFixed], FastArgFloat, debugSwitches)

    (* RTS call with one general (i.e. ML word) argument and a floating point result.
       This is used only to convert arbitrary precision values to floats. *)
    fun rtsCallFastGeneraltoFloat (functionName, debugSwitches) =
        rtsCallFastGeneral (functionName, [FastArgFixed], FastArgFloat, debugSwitches)

    datatype ffiABI =
        FFI_SYSV        (* Unix 32 bit and Windows GCC 32-bit *)
    |   FFI_STDCALL     (* Windows 32-bit system ABI.  Callee clears the stack. *)
    |   FFI_MS_CDECL    (* VS 32-bit.  Same as SYSV except when returning a struct.  Default on Windows including GCC in Mingw. *)
    |   FFI_WIN64       (* Windows 64 bit *)
    |   FFI_UNIX64      (* Unix 64 bit. libffi also implements this on X86/32. *)
    (* We don't include various other 32-bit Windows ABIs. *)

    local
        val getOSType: unit -> int = RunCall.rtsCallFast0 "PolyGetOSType"
    in
        (* This actually a constant since each exported saved state has
           a distinct ABI.  However for compatibility with the interpreted
           version we make this a function. *)
        fun abiList () =
            case getABI() of
                X86_32 =>
                    [("sysv", FFI_SYSV), ("stdcall", FFI_STDCALL), ("ms_cdecl", FFI_MS_CDECL),
                        (* Default to MS_CDECL on Windows otherwise SYSV. *)
                     ("default", if getOSType() = 1 then FFI_MS_CDECL else FFI_SYSV)]
            |   X64Win => [("win64", FFI_WIN64), ("default", FFI_WIN64)]
            |   X64Unix => [("unix64", FFI_UNIX64), ("default", FFI_UNIX64)]

        type abi = ffiABI
    end

    fun alignUp(s, align) = Word.andb(s + align-0w1, ~ align)
    fun intAlignUp(s, align) = Word.toInt(alignUp(Word.fromInt s, align))
    
    val getThreadDataCall = makeEntryPoint "PolyX86GetThreadData"

    local
        val sysWordSize = Word.toInt(nativeWordSize div wordSize)
    in
        (* Code to box an address as a SysWord.word value *)
        fun boxRegAsSysWord(boxReg, outputReg, saveRegs) =
            AllocStore{ size=sysWordSize, output=outputReg, saveRegs=saveRegs } ::
            (
                if targetArch = Native64Bit
                then 
                [
                    Move{source=NonAddressConstArg(LargeInt.fromInt sysWordSize),
                          destination=MemoryArg {offset= ~ (Word.toInt wordSize), base=outputReg, index=NoIndex},
                          moveSize=opSizeToMove polyWordOpSize},
                    Move{moveSize=Move8, source=NonAddressConstArg 1 (* byte *), destination=MemoryArg {offset= ~1, base=outputReg, index=NoIndex}}
                ]
                else
                let
                    val lengthWord = IntInf.orb(IntInf.fromInt sysWordSize, IntInf.<<(1, 0w24))
                in
                    [Move{source=NonAddressConstArg lengthWord, destination=MemoryArg {offset= ~ (Word.toInt wordSize), base=outputReg, index=NoIndex},
                          moveSize=opSizeToMove polyWordOpSize}]
                end
            ) @
            Move{source=RegisterArg boxReg, destination=MemoryArg {offset=0, base=outputReg, index=NoIndex}, moveSize=opSizeToMove nativeWordOpSize} ::
            (
                case targetArch of
                    ObjectId32Bit2Word =>
                    [
                        ArithToGenReg{ opc=SUB, output=outputReg, source=RegisterArg rbx, opSize=nativeWordOpSize },
                        ShiftConstant{ shiftType=SHR, output=outputReg, shift=0w2, opSize=OpSize64 }
                    ]
                |   ObjectId32Bit4Word =>
                    [
                        ArithToGenReg{ opc=SUB, output=outputReg, source=RegisterArg rbx, opSize=nativeWordOpSize },
                        ShiftConstant{ shiftType=SHR, output=outputReg, shift=0w3, opSize=OpSize64 }
                    ]
                |   _ => []
            ) @ [StoreInitialised]
    end

    (* Build a foreign call function.  The arguments are the abi, the list of argument types and the result type.
       The result is the code of the ML function that takes three arguments: the C function to call, the arguments
       as a vector of C values and the address of the memory for the result. *)

    (* This must match the type in Foreign.LowLevel.  Once this is bootstrapped we could use that
       type but note that this is the type we use within the compiler and we build Foreign.LowLevel
       AFTER compiling this. *)
    datatype cTypeForm =
        CTypeFloatingPt | CTypePointer | CTypeSignedInt | CTypeUnsignedInt
    |   CTypeStruct of cType list | CTypeVoid
    withtype cType = { typeForm: cTypeForm, align: word, size: word }

    fun call32Bits(abi, args, result) =
    let
        (* 32-bit arguments.  These all go to the stack so we can simply push them.  The arguments go on the
           stack in reverse order. *)
        fun loadArgs32([], stackOffset, argOffset, code, continue) = continue(stackOffset, argOffset, code)

        |   loadArgs32(arg::args, stackOffset, argOffset, code, continue) =
            let
                val {size, align, typeForm} = arg
                val newArgOffset = alignUp(argOffset, align)
                val baseAddr = {base=mlArg2Reg, offset=Word.toInt newArgOffset, index=NoIndex}
            in
                case (typeForm, size) of
                    (CTypeStruct elements, _) => (* structs passed as values are recursively unpacked. *)
                        loadArgs32(elements, stackOffset, newArgOffset (* Struct is aligned. *), code,
                            fn (so, ao, code) => loadArgs32(args, so, ao, code, continue))
                |   (CTypeVoid, _) => raise Foreign.Foreign "Void cannot be used for a function argument"
                |   (CTypeUnsignedInt, 0w1) => (* Unsigned char. *)
                        loadArgs32(args, stackOffset+4, newArgOffset+size,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg edx, moveSize=Move8 }
                                :: PushToStack(RegisterArg edx) :: code, continue)
                |   (CTypeSignedInt, 0w1) => (* Signed char. *)
                        loadArgs32(args, stackOffset+4, newArgOffset+size,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg edx, moveSize=Move8X32 }
                                :: PushToStack(RegisterArg edx) :: code, continue)
                |   (CTypeUnsignedInt, 0w2) => (* Unsigned 16-bits. *)
                        loadArgs32(args, stackOffset+4, newArgOffset+size,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg edx, moveSize=Move16 }
                                :: PushToStack(RegisterArg edx) :: code, continue)
                |   (CTypeSignedInt, 0w2) => (* Signed 16-bits. *)
                        loadArgs32(args, stackOffset+4, newArgOffset+size,
                            Move{source=MemoryArg baseAddr, destination=RegisterArg edx, moveSize=Move16X32 }
                                :: PushToStack(RegisterArg edx) :: code, continue)
                |   (_, 0w4) => (* 32-bits. *)
                        loadArgs32(args, stackOffset+4, newArgOffset+size, PushToStack(MemoryArg baseAddr) :: code, continue)
                |   (CTypeFloatingPt, 0w8) =>(* Double: push the two words.  High-order word first, then low-order. *)
                        loadArgs32(args, stackOffset+8, newArgOffset+size,
                            PushToStack(MemoryArg{base=mlArg2Reg, offset=Word.toInt newArgOffset+4, index=NoIndex}) ::
                                PushToStack(MemoryArg{base=mlArg2Reg, offset=Word.toInt newArgOffset, index=NoIndex}) :: code, continue)
                |   _ => raise Foreign.Foreign "argument type not supported"
            end

        val {typeForm, size, ...} =  result

        val resultMemory = {base=ecx, offset=0, index=NoIndex}
        (* Structures are passed by reference by storing the address of the result as
           the first argument except that in MS_CDECL (and STDCALL?) structures of
           size 1, 2, 4 and 8 are returned in EAX, and for 8, EDX.  *)
        val (getResult, needResultAddress) =
            if (case typeForm of CTypeStruct _ => true | _ => false) andalso
                (abi = FFI_SYSV orelse (size <> 0w1 andalso size <> 0w2 andalso size <> 0w4 andalso size <> 0w8))
            (* TODO: We have to get the address of the destination area. *)
            then ([], true)
            else if typeForm = CTypeVoid
            then ([], false)
            else
                (loadMemory(ecx, esp, 4, nativeWordOpSize)  ::
                loadHeapMemory(ecx, ecx, 0, nativeWordOpSize) ::
                (if size = 0w1
                then (* Single byte *) [Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=Move8}]
                else if size = 0w2
                then (* 16-bits *) [Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=Move16}]
                else if typeForm = CTypeFloatingPt andalso size = 0w4
                then [FPStoreToMemory{address=resultMemory, precision=SinglePrecision, andPop=true }]
                else if size = 0w4
                then [Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=Move32}]
                else if typeForm = CTypeFloatingPt andalso size = 0w8
                then [FPStoreToMemory{address=resultMemory, precision=DoublePrecision, andPop=true }]
                else if size = 0w8
                then
                [
                    Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=Move32},
                    Move{source=RegisterArg edx, destination=MemoryArg {base=ecx, offset=4, index=NoIndex}, moveSize=Move32}
                ]
                else raise Foreign.Foreign "Unrecognised result type"), false)

        local
            (* Load the arguments.  If we need to pass the return address for a struct that is the first arg. *)
            val (startStack, startCode) =
                if needResultAddress
                then (4, [PushToStack(MemoryArg{base=ecx, offset=0, index=NoIndex})])
                else (0, [])
        in
            val (argCode, argStack) =
                loadArgs32(args, startStack, 0w0, startCode, fn (stackOffset, _, code) => (code, stackOffset))
        end

        local
            val align = argStack mod 16
        in
            (* Always align the stack.  It's not always necessary on 32-bits but GCC prefers it. *)
            val preArgAlign = if align = 0 then 0 else 16-align
            (* Adjustment to be made when the function returns.  Stdcall resets the stack in the callee. *)
            val postCallStackReset =
                preArgAlign + (if abi = FFI_STDCALL then 0 else argStack)
        end

        in
            (
                (* If we're returning a struct we need the result address before we call. *)
                if needResultAddress then [loadMemory(ecx, esp, 4, nativeWordOpSize)] else []
            ) @
            [
                (* Save the stack pointer. *)
                storeMemory(esp, ebp, memRegStackPtr, nativeWordOpSize), (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize)  (* Load the saved C stack pointer. *)
            ] @
            (
                if preArgAlign = 0
                then []
                else [ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt preArgAlign), opSize=nativeWordOpSize}]
            ) @
            (
                (* The second argument is a SysWord containing the address of a malloced area of memory
                   with the actual arguments in it. *)
                if null args
                then []
                else [loadHeapMemory(mlArg2Reg, mlArg2Reg, 0, nativeWordOpSize)]
            ) @ argCode @
            CallAddress(MemoryArg{base=eax, offset=0, index=NoIndex}) ::
            (* Restore the C stack.  This is really only necessary if we've called a callback
               since that will store its esp value.  *)
            (
                if postCallStackReset = 0
                then []
                else [ArithToGenReg{opc=ADD, output=esp, source=NonAddressConstArg(LargeInt.fromInt postCallStackReset), opSize=nativeWordOpSize}]
            ) @
            [
                storeMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize),
                loadMemory(esp, ebp, memRegStackPtr, nativeWordOpSize) (* Restore the ML stack pointer. *)
            ] @ getResult @ (* Store the result in the destination. *) [ ReturnFromFunction 1 ]
        end
    
    fun closure32Bits(abi, args, result) =
    let
        (* Arguments are copied from the stack into a struct that is then passed to the
           ML function. *)
        fun copyArgs([], nArgs, argOffset, code, continue) = continue(nArgs, argOffset, code)

        |   copyArgs(arg::args, nArgs, argOffset, code, continue) =
            let
                val {size, align, typeForm} =  arg
                val newArgOffset = alignUp(argOffset, align)
                val sourceAddr = {base=ebx, offset=nArgs*4, index=NoIndex}
                val destAddr = {base=esp, offset=Word.toInt newArgOffset, index=NoIndex}
            in
                case (typeForm, size) of
                    (CTypeStruct elements, _) =>
                        (* structs passed as values are recursively unpacked. *)
                        copyArgs(elements, nArgs, newArgOffset (* Struct is aligned. *), code,
                            fn (na, ao, c) => copyArgs(args, na, ao, c, continue))
                |   (CTypeVoid, _) =>
                        raise Foreign.Foreign "Void cannot be used for a function argument"
                |   (CTypeFloatingPt, 0w8) =>
                    (* Double: copy the two words.  High-order word first, then low-order. *)
                    copyArgs(args, nArgs+2, argOffset+size,
                        Move{source=MemoryArg sourceAddr, destination=RegisterArg eax, moveSize=Move32} ::
                        Move{source=RegisterArg eax, destination=MemoryArg destAddr, moveSize=Move32} ::
                        Move{source=MemoryArg {base=ebx, offset=nArgs*4+4, index=NoIndex}, destination=RegisterArg eax, moveSize=Move32} ::
                        Move{source=RegisterArg eax, destination=MemoryArg{base=esp, offset=Word.toInt newArgOffset + 4, index=NoIndex}, moveSize=Move32} ::
                            code, continue)
                |   _ => (* Everything else is a single word on the stack. *)
                    let
                        val moveOp =
                            case size of
                                0w1 => Move8
                            |   0w2 => Move16
                            |   0w4 => Move32
                            |   _ => raise Foreign.Foreign "copyArgs: Invalid size"
                    in
                        copyArgs(args, nArgs+1, argOffset+size,
                            Move{source=MemoryArg sourceAddr, destination=RegisterArg eax, moveSize=Move32} ::
                                Move{source=RegisterArg eax, destination=MemoryArg destAddr, moveSize=moveOp} :: code, continue)
                    end
            end

        val {typeForm, size, align, ...} =  result

        (* Struct results are normally passed by reference. *)
        val resultStructByRef =
            (case typeForm of CTypeStruct _ => true | _ => false) andalso
                (abi = FFI_SYSV orelse (size <> 0w1 andalso size <> 0w2 andalso size <> 0w4 andalso size <> 0w8))

        val (argCount, argumentSpace, copyArgsFromStack) =
            copyArgs(args, if resultStructByRef then 1 else 0, 0w0, [], fn result => result)

        val resultOffset = alignUp(argumentSpace, align) (* Offset of result area *)

        val (loadResults, resultSize) =
            if typeForm = CTypeVoid orelse resultStructByRef
            then ([], 0w0)
            else
            let
                val resultMem = {base=esp, offset=Word.toInt resultOffset, index=NoIndex}
                val resultCode =
                    case (typeForm, size) of
                        (CTypeSignedInt, 0w1) => [Move{source=MemoryArg resultMem, destination=RegisterArg eax, moveSize=Move8X32 }]
                    |   (_, 0w1) => [Move{source=MemoryArg resultMem, destination=RegisterArg eax, moveSize=Move8 }]
                    |   (CTypeSignedInt, 0w2) => [Move{source=MemoryArg resultMem, destination=RegisterArg eax, moveSize=Move16X32 }]
                    |   (_, 0w2) => [Move{source=MemoryArg resultMem, destination=RegisterArg eax, moveSize=Move16 }]
                    |   (CTypeFloatingPt, 0w4) => [FPLoadFromMemory{ address=resultMem, precision=SinglePrecision }]
                    |   (_, 0w4) => [Move{source=MemoryArg resultMem, destination=RegisterArg eax, moveSize=Move32 }]
                    |   (CTypeFloatingPt, 0w8) => [FPLoadFromMemory{ address=resultMem, precision=DoublePrecision }]
                    |   (_, 0w8) => (* MSC only.  Struct returned in eax/edx. *)
                        [
                            Move{source=MemoryArg resultMem, destination=RegisterArg eax, moveSize=Move32 },
                            Move{source=MemoryArg {base=esp, offset=Word.toInt resultOffset + 4, index=NoIndex},
                                destination=RegisterArg edx, moveSize=Move32 }
                        ]
                    |   _ => raise Foreign.Foreign "Unrecognised result type"
            in
                (resultCode, size)
            end

        val stackSpace = Word.toInt(resultOffset + resultSize)
        
        local
            val align = stackSpace mod 16
        in
            (* Stack space.  In order to align the stack correctly for GCC we need the value in memRegCStackPtr
               to be a multiple of 16 bytes + 8.  esp would have been on a 16 byte boundary before the return address
               was pushed so after pushing the return address and four registers we need a further 4 bytes
               to get the alignment back again.  The effect of this is that the argument and result area is
               on an 8-byte boundary. *)
            val stackToAllocate = stackSpace + (if align = 0 then 0 else 16-align) + 4
        end
    in
        [
            (* Push callee-save registers. *)
            PushToStack(RegisterArg ebp), PushToStack(RegisterArg ebx), PushToStack(RegisterArg edi), PushToStack(RegisterArg esi),
            (* Set ebx to point to the original args. *)
            LoadAddress{ output=ebx, offset=20, base=SOME esp, index=NoIndex, opSize=OpSize32},
            (* Allocate stack space. *)
            ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackToAllocate), opSize=OpSize32},
            (* Move the function address in eax into esi since that's a callee-save register. *)
            Move{source=RegisterArg eax, destination=RegisterArg esi, moveSize=Move32}
        ] @ copyArgsFromStack @
        [
            (* Get the value for ebp. *)
            Move{source=AddressConstArg getThreadDataCall, destination=RegisterArg ecx, moveSize=Move32},
            CallAddress(MemoryArg{base=ecx, offset=0, index=NoIndex}), (* Get the address - N.B. Heap addr in 32-in-64. *)
            moveRR{source=eax, output=ebp, opSize=OpSize32},
            (* Save the address of the argument and result area. *)
            moveRR{source=esp, output=ecx, opSize=OpSize32},
            (* Switch to the ML stack. *)
            storeMemory(esp, ebp, memRegCStackPtr, OpSize32),
            loadMemory(esp, ebp, memRegStackPtr, OpSize32),
            (* Move esi into the closure register edx *)
            Move{source=RegisterArg esi, destination=RegisterArg edx, moveSize=Move32}
        ] @ boxRegAsSysWord(ecx, eax, []) @
        (
            (* If we're returning a struct the address for the result will have been passed in the
               first argument.  We use that as the result area.  Otherwise point to the result
               area on the stack.  *)
            if resultStructByRef
            then Move{source=MemoryArg {offset=0, base=ebx, index=NoIndex}, destination=RegisterArg ecx, moveSize=Move32}
            else ArithToGenReg{opc=ADD, output=ecx, source=NonAddressConstArg(Word.toLargeInt resultOffset), opSize=OpSize32}
        ) :: boxRegAsSysWord(ecx, ebx, [eax]) @
        [
            (* Call the ML function using the full closure call. *)
            CallAddress(MemoryArg{offset=0, base=edx, index=NoIndex}),
            (* Save the ML stack pointer because we may have grown the stack.  Switch to the C stack. *)
            storeMemory(esp, ebp, memRegStackPtr, OpSize32),
            loadMemory(esp, ebp, memRegCStackPtr, OpSize32)
        ] @ loadResults @
        [
            (* Remove the stack space. *)
            ArithToGenReg{opc=ADD, output=esp, source=NonAddressConstArg(LargeInt.fromInt stackToAllocate), opSize=OpSize32},
            PopR esi, PopR edi, PopR ebx, PopR ebp (* Restore callee-save registers. *)
        ] @
        (
            (* If we've passed in the address of the area for the result structure
               we're supposed to pass that back in eax. *)
            if resultStructByRef
            then [loadMemory(eax, esp, 4, OpSize32)]
            else []
        ) @
        [
            (* Callee removes arguments in StdCall. *)
            ReturnFromFunction (if abi = FFI_STDCALL then argCount else 0)
        ]
    end

    local (* Windows on X64. *)
        val win64ArgRegs = [ (rcx, xmm0), (rdx, xmm1), (r8, xmm2), (r9, xmm3) ]
    in
        fun callWindows64Bits(args, result) =
        let
            val extraStackReg = r10 (* Not used for any arguments. *)

            fun loadWin64Args([], stackOffset, _, _, code, extraStack, preCode) = (code, stackOffset, preCode, extraStack)

            |   loadWin64Args(arg::args, stackOffset, argOffset, regs, code, extraStack, preCode) =
                let
                    val {size, align, typeForm, ...} =  arg
                    val newArgOffset = alignUp(argOffset, align)
                    val baseAddr = {base=mlArg2Reg, offset=Word.toInt newArgOffset, index=NoIndex}
                    val workReg = rcx (* rcx: the last to be loaded. *)
                
                    (* Integer arguments. *)
                    fun loadIntArg moveOp =
                        case regs of
                            (areg, _) :: regs' => 
                                loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                                    Move{source=MemoryArg baseAddr, destination=RegisterArg areg, moveSize=moveOp } :: code,
                                    extraStack, preCode)
                        |   [] =>
                                loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                                    if size = 0w8
                                    then PushToStack(MemoryArg baseAddr) :: code
                                    else (* Need to load it into a register first. *)
                                        Move{source=MemoryArg baseAddr,
                                               destination=RegisterArg workReg, moveSize=moveOp } :: PushToStack(RegisterArg workReg) :: code,
                                    extraStack, preCode)
                in
                    (* Structs of 1, 2, 4 and 8 bytes are passed as the corresponding int.  It may not
                       be necessary to sign-extend 1, 2 or 4-byte values.
                       2, 4 or 8-byte structs may not be aligned onto the appropriate boundary but
                       it should still work. *)
                    case (size, typeForm) of
                        (0w1, CTypeSignedInt) => (* Signed char. *) loadIntArg Move8X64
                    |   (0w1, _) => (* Unsigned char or single byte struct *) loadIntArg Move8

                    |   (0w2, CTypeSignedInt) =>(* Signed 16-bits. *) loadIntArg Move16X64
                    |   (0w2, _) => (* Unsigned 16-bits. *) loadIntArg Move16

                    |   (0w4, CTypeFloatingPt) =>
                        (
                            case regs of
                                (_, fpReg) :: regs' => 
                                loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                                    XMMArith{opc=SSE2MoveFloat, source=MemoryArg baseAddr, output=fpReg } :: code, extraStack, preCode)
                            |   [] =>
                                loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                                    Move{source=MemoryArg baseAddr,
                                           destination=RegisterArg workReg, moveSize=Move32 } :: PushToStack(RegisterArg workReg) :: code,
                                    extraStack, preCode)
                        )
                    |   (0w4, CTypeSignedInt) => (* Signed 32-bits. *) loadIntArg Move32X64
                    |   (0w4, _) => (* Unsigned 32-bits. *) loadIntArg Move32

                    |   (0w8, CTypeFloatingPt) =>
                        (
                            case regs of
                                (_, fpReg) :: regs' => 
                                loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                                    XMMArith{opc=SSE2MoveDouble, source=MemoryArg baseAddr, output=fpReg } :: code, extraStack, preCode)
                            |   [] =>
                                loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                                    Move{source=MemoryArg baseAddr,
                                           destination=RegisterArg workReg, moveSize=Move64 } :: PushToStack(RegisterArg workReg) :: code,
                                    extraStack, preCode)
                        )
                    |   (0w8, _) => (* 64-bits. *) loadIntArg Move64

                    |   (_, CTypeStruct _) =>
                        let
                            (* Structures of other sizes are passed by reference.  They are first
                               copied into new areas on the stack.  This ensures that the called function
                               can update the structure without changing the original values. *)
                            val newExtra = intAlignUp(extraStack + Word.toInt size, 0w16)
                            val newPreCode =
                                moveMemory{source=(mlArg2Reg, Word.toInt newArgOffset), destination=(extraStackReg, extraStack),
                                           length=Word.toInt size} @ preCode
                        in
                            case regs of
                                (areg, _) :: regs' => 
                                loadWin64Args(args, stackOffset, newArgOffset+size, regs',
                                    loadAddress{source=(extraStackReg, extraStack), destination=areg} :: code,
                                    newExtra, newPreCode)
                            |   [] =>
                                loadWin64Args(args, stackOffset+8, newArgOffset+size, [],
                                    loadAddress{source=(extraStackReg, extraStack), destination=workReg} ::
                                    PushToStack(RegisterArg workReg) :: code, newExtra, newPreCode)
                        end
                   
                    |   _ => raise Foreign.Foreign "Unrecognised type for function argument"
                end

            val {typeForm, size, ...} =  result
        
            val resultAreaPtr = r12 (* Saved value of r8 - This is callee save. *)
            val resultMemory = {base=resultAreaPtr, offset=0, index=NoIndex}
            fun storeIntValue moveOp =
                ([Move{source=RegisterArg eax, destination=MemoryArg resultMemory, moveSize=moveOp}], false)
            and storeFloatValue precision =
                ([XMMStoreToMemory{toStore=xmm0, address=resultMemory, precision=precision}], false)

            val (getResult, passStructAddress) =
                case (typeForm, size) of
                    (CTypeVoid, _) => ([], false)
                |   (_, 0w1) (* Includes structs *) => (* Single byte *) storeIntValue Move8
                |   (_, 0w2) => (* 16-bits *) storeIntValue Move16
                |   (CTypeFloatingPt, 0w4) => storeFloatValue SinglePrecision
                |   (_, 0w4) => storeIntValue Move32
                |   (CTypeFloatingPt, 0w8) => storeFloatValue DoublePrecision
                |   (_, 0w8) => storeIntValue Move64
                |   (CTypeStruct _, _) => ([], true)
                |   _ => raise Foreign.Foreign "Unrecognised result type"

            (* argCode is the code to load and push the arguments.  argStack is the amount of stack space
               the arguments will take.  It's only used to ensure that the stack is aligned onto a 16-byte
               boundary.  preArgCode is any code that is needed to copy the arguments before they are
               actually loaded.  Because it is done before the argument registers are loaded it can
               use rcx, rdi and rsi.  extraStack is local stack space needed.  It is usually zero but
               if it is non-zero it must be a multiple of 16 bytes.  The address of this area is loaded
               into r10 before preArgCode is called. *)
            val (argCode, argStack, preArgCode, extraStack) =
                if passStructAddress
                then (* The address of the result structure goes in the first argument register: rcx *)
                    loadWin64Args(args, 0, 0w0, tl win64ArgRegs,
                        [moveRR{source=resultAreaPtr, output=rcx, opSize=nativeWordOpSize}], 0, [])
                else loadWin64Args(args, 0, 0w0, win64ArgRegs, [], 0, [])

            local
                val align = argStack mod 16
            in
                (* Always align the stack. *)
                val preArgAlign = if align = 0 then 0 else 16-align
                (* The total space on the stack that needs to be removed at the end. *)
                val postCallStackReset = argStack + preArgAlign + extraStack + 32
            end

            in
                (* Save heap ptr.  Needed in case we have a callback. *)
                [storeMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize)] @
                (
                    (* Put the destination address into a callee save resgister.
                       We have to put the C address in there now because an ML address wouldn't be updated
                       by a possible GC in a callback. *)
                    if #typeForm( result) <> CTypeVoid
                    then [loadHeapMemory(resultAreaPtr, r8, 0, nativeWordOpSize)]
                    else []
                ) @
                [
                    (* Save the stack pointer. *)
                    storeMemory(esp, ebp, memRegStackPtr, nativeWordOpSize), (* Save ML stack and switch to C stack. *)
                    loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize)  (* Load the saved C stack pointer. *)
                ] @
                (
                    if extraStack = 0
                    then []
                    else
                    [
                        ArithToGenReg{opc=SUB, output=rsp, source=NonAddressConstArg(LargeInt.fromInt extraStack), opSize=nativeWordOpSize},
                        Move{source=RegisterArg rsp, destination=RegisterArg extraStackReg, moveSize=Move64}
                    ]
                ) @
                (
                    if preArgAlign = 0
                    then []
                    else [ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt preArgAlign), opSize=nativeWordOpSize}]
                ) @
                (
                    (* The second argument is a SysWord containing the address of a malloced area of memory
                       with the actual arguments in it. *)
                    if null args
                    then []
                    else [loadHeapMemory(mlArg2Reg, mlArg2Reg, 0, nativeWordOpSize)]
                ) @ preArgCode @ argCode @
                [ (* Reserve a 32-byte area after the arguments.  This is specific to the Windows ABI. *)
                    ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt 32), opSize=nativeWordOpSize},
                    let
                        (* The entry point is in a SysWord.word value in RAX. *)
                        val entryPoint =
                            case targetArch of
                                ObjectId32Bit2Word => MemoryArg{base=ebx, offset=0, index=Index4 eax}
                            |   ObjectId32Bit4Word => MemoryArg{base=ebx, offset=0, index=Index8 eax}
                            |   _ => MemoryArg{base=eax, offset=0, index=NoIndex}
                    in
                        (* Call the function.  We're discarding the value in rsp so no need to remove args. *)
                        CallAddress entryPoint
                    end,
                    (* Restore the C stack value in case it's been changed by a callback. *)
                    ArithToGenReg{opc=ADD, output=rsp, source=NonAddressConstArg(LargeInt.fromInt postCallStackReset), opSize=nativeWordOpSize},
                    storeMemory(rsp, rbp, memRegCStackPtr, nativeWordOpSize),
                    loadMemory(rsp, rbp, memRegStackPtr, nativeWordOpSize), (* Restore the ML stack pointer. *)
                    (* Reload the heap pointer.  If we've called back to ML this could well have changed. *)
                    loadMemory(r15, rbp, memRegLocalMPointer, nativeWordOpSize)
                ] @ (* Store the result in the destination. *) getResult @ [ReturnFromFunction 0 ]
            end (* callWindows64Bits *)

        fun closureWindows64Bits(args, result) =
        let
            val {typeForm, size, align, ...} =  result

            (* Struct results are normally passed by reference. *)
            val resultStructByRef = (* If true we've copied rcx (the first arg) into r9 *)
                (case typeForm of CTypeStruct _ => true | _ => false) andalso
                    size <> 0w1 andalso size <> 0w2 andalso size <> 0w4 andalso size <> 0w8

            (* Store the register arguments and copy everything else into the argument structure on the stack.
               The code is ordered so that the early arguments are stored first. *)
            fun copyWin64Args([], _, _, _) = []

            |   copyWin64Args(arg::args, nStackArgs, argOffset, regs) =
                let
                    val {size, align, typeForm, ...} =  arg
                    val newArgOffset = alignUp(argOffset, align)
                    val destAddr = {base=rsp, offset=Word.toInt newArgOffset, index=NoIndex}
                
                    (* Integer arguments. *)
                    fun moveIntArg moveOp =
                        case regs of
                            (areg, _) :: regs' =>
                                Move{source=RegisterArg areg, destination=MemoryArg destAddr, moveSize=moveOp } ::
                                    copyWin64Args(args, nStackArgs, newArgOffset+size, regs')
                        |   [] =>
                                Move{source=MemoryArg {base=r10, offset=nStackArgs*8, index=NoIndex}, destination=RegisterArg rax, moveSize=Move64} ::
                                    Move{source=RegisterArg rax, destination=MemoryArg destAddr, moveSize=moveOp} :: 
                                    copyWin64Args(args, nStackArgs+1, newArgOffset+size, [])
                in
                    (* Structs of 1, 2, 4 and 8 bytes are passed as the corresponding int. *)
                    case (typeForm, size) of
                        (_, 0w1) => moveIntArg Move8
                    |   (_, 0w2) => moveIntArg Move16

                    |   (CTypeFloatingPt, 0w4) =>
                        (
                            case regs of
                                (_, fpReg) :: regs' =>
                                    XMMStoreToMemory{ toStore=fpReg, address=destAddr, precision=SinglePrecision} :: 
                                        copyWin64Args(args, nStackArgs, newArgOffset+size, regs')
                            |   [] => moveIntArg Move32
                        )
                    |   (_, 0w4) => (* 32-bits *) moveIntArg Move32

                    |   (CTypeFloatingPt, 0w8) =>
                        (
                            case regs of
                                (_, fpReg) :: regs' =>
                                    XMMStoreToMemory{ toStore=fpReg, address=destAddr, precision=DoublePrecision} :: 
                                        copyWin64Args(args, nStackArgs, newArgOffset+size, regs')
                            |   [] => moveIntArg Move64
                        )
                    |   (_, 0w8) => (* 64-bits. *) moveIntArg Move64

                    |   (CTypeStruct _, _) =>
                        (* Structures of other size are passed by reference.  We need to copy the source
                               structure into our stack area.  Since rsi and rdi aren't used as args and
                               rcx is only used for the first argument we can copy the argument now. *)
                        (
                            case regs of
                                (arg, _) :: regs' =>
                                    moveMemory{source=(arg, 0), destination=(rsp, Word.toInt newArgOffset), length=Word.toInt size} @
                                    copyWin64Args(args, nStackArgs, newArgOffset+size, regs')
                            |   [] =>
                                    moveMemory{source=(r10, nStackArgs*8), destination=(rsp, Word.toInt newArgOffset), length=Word.toInt size} @
                                    copyWin64Args(args, nStackArgs+1, newArgOffset+size, [])
                        )
                            
                    |   _ => raise Foreign.Foreign "Unrecognised type for function argument"
                end

            val copyArgsFromRegsAndStack =
                if resultStructByRef
                then (* If we're returning a struct by reference we copy the address into r9 and pass that
                        as the result address. *)
                    Move{source=RegisterArg rcx, destination=RegisterArg r9, moveSize=Move64} ::
                        copyWin64Args(args, 0, 0w0, tl win64ArgRegs)
                else copyWin64Args(args, 0, 0w0, win64ArgRegs)

            local
                fun getNextSize (arg, argOffset) =
                let val {size, align, ...} =  arg in alignUp(argOffset, align) + size end
            in
                val argumentSpace = List.foldl getNextSize 0w0 args
            end

            val resultOffset = alignUp(argumentSpace, align) (* Offset of result area *)
        
            val (loadResults, resultSize) =
                if typeForm = CTypeVoid orelse resultStructByRef
                then ([], 0w0)
                else
                let
                    val resultMem = {base=rsp, offset=Word.toInt resultOffset, index=NoIndex}
                    val resultCode =
                        case (typeForm, size) of
                            (CTypeSignedInt, 0w1) => [Move{source=MemoryArg resultMem, destination=RegisterArg rax, moveSize=Move8X64}]
                        |   (_, 0w1) => [Move{source=MemoryArg resultMem, destination=RegisterArg rax, moveSize=Move8}]
                        |   (CTypeSignedInt, 0w2) => [Move{source=MemoryArg resultMem, destination=RegisterArg rax, moveSize=Move16X64}]
                        |   (_, 0w2) => [Move{source=MemoryArg resultMem, destination=RegisterArg rax, moveSize=Move16}]
                        |   (CTypeFloatingPt, 0w4) => [XMMArith{opc=SSE2MoveFloat, source=MemoryArg resultMem, output=xmm0}]
                        |   (CTypeSignedInt, 0w4) => [Move{source=MemoryArg resultMem, destination=RegisterArg rax, moveSize=Move32X64}]
                        |   (_, 0w4) => [Move{source=MemoryArg resultMem, destination=RegisterArg rax, moveSize=Move32}]
                        |   (CTypeFloatingPt, 0w8) => [XMMArith{opc=SSE2MoveDouble, source=MemoryArg resultMem, output=xmm0}]
                        |   (_, 0w8) => [Move{source=MemoryArg resultMem, destination=RegisterArg rax, moveSize=Move64}]
                        |   _ => raise Foreign.Foreign "Unrecognised result type"
                in
                    (resultCode, size)
                end

            (* Stack space.  The stack must be 16 byte aligned.  We've pushed 8 regs and a return address
               so add a further 8 bytes to bring it back into alignment.  If we're returning a struct
               by reference, though, we've pushed 9 regs so don't add 8. *)
            val stackToAllocate =
                Word.toInt(alignUp(resultOffset + resultSize, 0w16)) + (if resultStructByRef then 0 else 8)
        in
        [
            (* Push callee-save registers. *)
            PushToStack(RegisterArg rbp), PushToStack(RegisterArg rbx), PushToStack(RegisterArg r12), PushToStack(RegisterArg r13),
            PushToStack(RegisterArg r14), PushToStack(RegisterArg r15), PushToStack(RegisterArg rdi), PushToStack(RegisterArg rsi)
        ] @
        (
            (* If we're returning a struct by reference we have to return the address in rax even though
               it's been set by the caller.  Save this address. *)
            if resultStructByRef
            then [PushToStack(RegisterArg rcx)]
            else []
        ) @
        [
            (* Set r10 to point to the original stack args if any.  This is beyond the pushed regs and also the 32-byte area. *)
            LoadAddress{ output=r10, offset=if resultStructByRef then 112 else 104, base=SOME rsp, index=NoIndex, opSize=nativeWordOpSize},
            (* Allocate stack space. *)
            ArithToGenReg{opc=SUB, output=rsp, source=NonAddressConstArg(LargeInt.fromInt stackToAllocate), opSize=nativeWordOpSize},
            (* Move the function we're calling, in rax, into r13, a callee-save register *)
            moveRR{source=rax, output=r13, opSize=polyWordOpSize}
        ]
          @ copyArgsFromRegsAndStack @
        [
            (* Get the value for rbp. *)
            (* This is a problem for 32-in-64.  The value of getThreadDataCall is an object ID but rbx may well no
               longer hold the heap base address.  We use a special inline constant to hold the full 64-bit address. *)
            LoadAbsolute{value=getThreadDataCall, destination=rcx},
            CallAddress(MemoryArg{base=rcx, offset=0, index=NoIndex}),
            moveRR{source=rax, output=rbp, opSize=nativeWordOpSize},
            (* Save the address of the argument and result area. *)
            moveRR{source=rsp, output=rcx, opSize=nativeWordOpSize},
            (* Switch to the ML stack. *)
            storeMemory(rsp, rbp, memRegCStackPtr, nativeWordOpSize),
            loadMemory(rsp, rbp, memRegStackPtr, nativeWordOpSize),
            (* Load the ML heap pointer. *)
            loadMemory(r15, rbp, memRegLocalMPointer, nativeWordOpSize),
            (* Now move the function closure into the closure register ready for the call. *)
            moveRR{source=r13, output=rdx, opSize=polyWordOpSize}
        ] @
        (* Reload the heap base address in 32-in-64. *)
        ( if targetArch = ObjectId32Bit2Word orelse targetArch = ObjectId32Bit4Word then [loadMemory(rbx, rbp, memRegSavedRbx, nativeWordOpSize)] else [] )
         @ boxRegAsSysWord(rcx, rax, []) @
        (
            (* If we're returning a struct by reference the address for the result will have been passed in the
               first argument.  We use that as the result area.  Otherwise point to the result area on the stack.  *)
            if resultStructByRef
            then loadMemory(rcx, r10, ~112, nativeWordOpSize)
            else ArithToGenReg{opc=ADD, output=rcx, source=NonAddressConstArg(Word.toLargeInt resultOffset), opSize=nativeWordOpSize}
        ) :: boxRegAsSysWord(rcx, mlArg2Reg, [rax]) @
        [
            (* Call the ML function using the full closure call. *)
            CallAddress(
                case targetArch of
                    ObjectId32Bit2Word => MemoryArg{base=rbx, index=Index4 rdx, offset=0}
                |   ObjectId32Bit4Word => MemoryArg{base=rbx, index=Index8 rdx, offset=0}
                |   _ => MemoryArg{base=rdx, index=NoIndex, offset=0}),
            (* Save the ML stack pointer because we may have grown the stack.  Switch to the C stack. *)
            storeMemory(rsp, rbp, memRegStackPtr, nativeWordOpSize),
            loadMemory(rsp, rbp, memRegCStackPtr, nativeWordOpSize),
            storeMemory(r15, rbp, memRegLocalMPointer, nativeWordOpSize)
        ] @ loadResults @
        [
            (* Remove the stack space. *)
            ArithToGenReg{opc=ADD, output=rsp, source=NonAddressConstArg(LargeInt.fromInt stackToAllocate), opSize=nativeWordOpSize}
        ] @
        ( if resultStructByRef then [PopR rax] else [] ) @
        [
            PopR rsi, PopR rdi, PopR r15, PopR r14, PopR r13, PopR r12, PopR rbx, PopR rbp, (* Restore callee-save registers. *)
            ReturnFromFunction 0 (* Caller removes any stack arguments. *)
        ]
        end
    end

    local
        (* The rules for passing structs in SysV on X86/64 are complicated but most of the special
           cases don't apply.  We don't support floating point larger than 8 bytes, packed structures
           or C++ constructors.  It then reduces to the following:
           Structures of up to 8 bytes are passed in a single register and of 8-16 bytes in two
           registers.  Larger structures are passed on the stack.  The question is whether to use
           general registers or SSE2 registers.  Each 8 byte chunk is considered independently after
           any internal structs have been unwrapped.  Each chunk will consist of either a single
           8-byte value (i.e.. a pointer, int64_t or a double) or one or more smaller values and
           possibly some padding.  An SSE2 register is used if the value is a double, two floats
           or a single float and padding.  Otherwise it must have at least one shorter int-like
           type (e.g. int, char, short etc) in which case a general register is used.  That
           applies even if it also contains a float.  If, having selected the kind of
           registers to be used, there are not enough for the whole struct it is passed
           on the stack.
        
           We don't really need this for simple arguments but it's easier to consider
           them all together. *)
        datatype argClass = ArgInMemory | ArgInRegs of { firstInSSE: bool, secondInSSE: bool }
        
        fun classifyArg arg =
        let
            val {size, ...} =  arg

            (* Unwrap the struct and any internal structs. *)
            fun getFields([], _) = []

            |   getFields(field::fields, offset) =
                let
                    val {size, align, typeForm} =  field
                    val alignedOffset = alignUp(offset, align) (* Align this even if it's a sub-struct *)
                in
                    case typeForm of
                        CTypeVoid => raise Foreign.Foreign "Void cannot be used for a function argument"
                    |   CTypeStruct elements =>
                            getFields(elements, alignedOffset) @ getFields(fields, alignedOffset+size)
                    |   _  => (typeForm, alignedOffset) :: getFields(fields, alignedOffset+size)
                end
       
            val isSSE =
                List.all (fn (CTypeFloatingPt, _) => true | _ => false)
        in
            if size > 0w16
            then ArgInMemory
            else
            let
                val fieldsAndOffsets = getFields([arg], 0w0)
            in
                if size <= 0w8 (* Only the first register will be used. *)
                then ArgInRegs{firstInSSE=isSSE fieldsAndOffsets, secondInSSE=false}
                else
                let
                    (* Partition the structure: first register is offsets < 8, second >= 8. *)
                    val (first8Bytes, second8Bytes) =
                        List.partition (fn (_, off) => off < 0w8) fieldsAndOffsets
                in
                    ArgInRegs{firstInSSE=isSSE first8Bytes, secondInSSE=isSSE second8Bytes}
                end
            end
        end

        val sysVGenRegs = [rdi, rsi, rdx, rcx, r8, r9]
        and sysVFPRegs = [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7]

        (* Store a register into upto 8 bytes.  Most values will involve a single store but odd-sized
           structs can require shifts and multiple stores.  N.B.  May modify the source register. *)
        fun storeUpTo8(reg, base, offset, size) =
        let
            val moveOp =
                if size = 0w8 then Move64 else if size >= 0w4 then Move32 else if size >= 0w2 then Move16 else Move8
        in
            [Move{source=RegisterArg reg, destination=MemoryArg {base=base, offset=offset, index=NoIndex}, moveSize=moveOp}]
        end @
        (
            if size = 0w6 orelse size = 0w7
            then
            [
                ShiftConstant{ shiftType=SHR, output=reg, shift=0w32, opSize=OpSize64 },
                Move{source=RegisterArg reg, destination=MemoryArg {base=base, offset=offset+4, index=NoIndex}, moveSize=Move16}
            ]
            else []
        ) @
        (
            if size = 0w3 orelse size = 0w5 orelse size = 0w7
            then
            [
                ShiftConstant{ shiftType=SHR, output=reg, shift=Word8.fromLargeWord(Word.toLargeWord((size-0w1)*0w8)), opSize=OpSize64 },
                Move{source=RegisterArg reg, destination=MemoryArg {base=base, offset=offset+Word.toInt(size-0w1), index=NoIndex}, moveSize=Move8}
            ]
            else []
        )

    in
        fun callUnix64Bits(args, result) =
        let
            val argWorkReg = r10 (* Not used for any arguments. *)
            val resultAreaPtr = r12 (* Saved value of r8 - This is callee save. *)
            val argPtrReg = r11 (* Pointer to argument area - Can't use mlArg2Reg since that's RSI on 32-in-64. *)

            fun loadSysV64Args([], stackOffset, _, _, _, code, preCode) = (code, stackOffset, preCode)

            |   loadSysV64Args(arg::args, stackOffset, argOffset, gRegs, fpRegs, code, preCode) =
                let
                    val {size, align, typeForm, ...} =  arg

                    (* Load a value into a register.  Normally the size will be 1, 2, 4 or 8 bytes and
                       this will just involve a simple load.  Structs, though, can be of any size up
                       to 8 bytes. *)
                    fun loadRegister(reg, offset, size) =
                    let
                        (* We don't necessarily have to sign-extend.  There's a comment in libffi that
                           suggests that LVM expects it even though the SysV ABI doesn't require it. *)
                        val moveOp =
                            if size = 0w8
                            then Move64
                            else if typeForm = CTypeSignedInt andalso size = 0w4
                            then Move32X64
                            else if size >= 0w4
                            then Move32
                            else if typeForm = CTypeSignedInt andalso size = 0w2
                            then Move16X64
                            else if size >= 0w2
                            then Move16
                            else if typeForm = CTypeSignedInt andalso size = 0w1
                            then Move8X64 else Move8
                    in
                        [Move{source=MemoryArg{base=argPtrReg, offset=Word.toInt offset, index=NoIndex}, destination=RegisterArg reg, moveSize=moveOp}]
                    end @
                    (
                        if size = 0w6 orelse size = 0w7
                        then
                        [
                            Move{source=MemoryArg{base=argPtrReg, offset=Word.toInt offset + 4, index=NoIndex},
                                destination=RegisterArg argWorkReg, moveSize=Move16},
                            ShiftConstant{ shiftType=SHL, output=argWorkReg, shift=0w32, opSize=OpSize64 },
                            ArithToGenReg{ opc=OR, output=reg, source=RegisterArg argWorkReg, opSize=OpSize64 }
                        ]
                        else []
                    ) @
                    (
                        if size = 0w3 orelse size = 0w5 orelse size = 0w7
                        then
                        [
                            Move{source=MemoryArg{base=argPtrReg, offset=Word.toInt offset + Word.toInt(size-0w1), index=NoIndex},
                                destination=RegisterArg argWorkReg, moveSize=Move8},
                            ShiftConstant{ shiftType=SHL, output=argWorkReg, shift=Word8.fromLargeWord(Word.toLargeWord((size-0w1)*0w8)), opSize=OpSize64 },
                            ArithToGenReg{ opc=OR, output=reg, source=RegisterArg argWorkReg, opSize=OpSize64 }
                        ]
                        else []
                    )

                    val newArgOffset = alignUp(argOffset, align)
                    val word1Addr = {base=argPtrReg, offset=Word.toInt newArgOffset, index=NoIndex}
                    val word2Addr = {base=argPtrReg, offset=Word.toInt newArgOffset + 8, index=NoIndex}
                in
                    case (classifyArg arg, size > 0w8, gRegs, fpRegs) of
                        (* 8 bytes or smaller - single general reg.  This is the usual case. *)
                        (ArgInRegs{firstInSSE=false, ...}, false, gReg :: gRegs', fpRegs') =>
                            loadSysV64Args(args, stackOffset, newArgOffset+size, gRegs', fpRegs',
                                loadRegister(gReg, newArgOffset, size) @ code, preCode)

                        (* 8 bytes or smaller - single SSE reg.  Usual case for real arguments. *)
                    |   (ArgInRegs{firstInSSE=true, ...}, false, gRegs', fpReg :: fpRegs') =>
                            loadSysV64Args(args, stackOffset, newArgOffset+size, gRegs', fpRegs',
                                XMMArith{opc=if size = 0w4 then SSE2MoveFloat else SSE2MoveDouble, source=MemoryArg word1Addr, output=fpReg } :: code,
                                preCode)
                    
                        (* 9-16 bytes - both values in general regs. *)
                    |   (ArgInRegs{firstInSSE=false, secondInSSE=false}, true, gReg1 :: gReg2 :: gRegs', fpRegs') =>
                            loadSysV64Args(args, stackOffset, newArgOffset+size, gRegs', fpRegs',
                                Move{source=MemoryArg word1Addr, destination=RegisterArg gReg1, moveSize=Move64} ::
                                loadRegister(gReg2, newArgOffset+0w8, size-0w8) @ code, preCode)

                        (* 9-16 bytes - first in general, second in SSE. *)
                    |   (ArgInRegs{firstInSSE=false, secondInSSE=true}, true, gReg :: gRegs', fpReg :: fpRegs') =>
                            loadSysV64Args(args, stackOffset, newArgOffset+size, gRegs', fpRegs',
                                Move{source=MemoryArg word1Addr, destination=RegisterArg gReg, moveSize=Move64} ::
                                    XMMArith{opc=if size = 0w12 then SSE2MoveFloat else SSE2MoveDouble, source=MemoryArg word2Addr, output=fpReg } :: code,
                                preCode)

                        (* 9-16 bytes - first in SSE, second in general. *)
                    |   (ArgInRegs{firstInSSE=true, secondInSSE=false}, true, gReg :: gRegs', fpReg :: fpRegs') =>
                            loadSysV64Args(args, stackOffset, newArgOffset+size, gRegs', fpRegs',
                                XMMArith{opc=SSE2MoveDouble, source=MemoryArg word1Addr, output=fpReg } ::
                                    loadRegister(gReg, newArgOffset+0w8, size-0w8) @ code,
                                preCode)

                    |   (* 9-16 bytes - both values in SSE regs. *)
                        (ArgInRegs{firstInSSE=true, secondInSSE=true}, true, gRegs', fpReg1 :: fpReg2 :: fpRegs') =>
                            loadSysV64Args(args, stackOffset, newArgOffset+size, gRegs', fpRegs',
                                XMMArith{opc=SSE2MoveDouble, source=MemoryArg word1Addr, output=fpReg1 } ::
                                XMMArith{opc=if size = 0w12 then SSE2MoveFloat else SSE2MoveDouble,
                                        source=MemoryArg word2Addr, output=fpReg2 } :: code,
                                preCode)

                    |   (_, _, gRegs', fpRegs') => (* Either larger than 16 bytes or we've run out of the right kind of registers. *)
                            (* Move the argument in the preCode.  It's possible a large struct could be the first argument
                               and if we left it until the end RDI and RSI would already have been loaded.
                               Structs are passed by value on the stack not, as in Win64, by reference. *)
                        let
                            val space = intAlignUp(Word.toInt size, 0w8)
                        in
                            loadSysV64Args(args, stackOffset+space, newArgOffset+size, gRegs', fpRegs', code,
                                ArithToGenReg{opc=SUB, output=rsp, source=NonAddressConstArg(LargeInt.fromInt space), opSize=nativeWordOpSize} ::
                                    moveMemory{source=(argPtrReg, Word.toInt newArgOffset), destination=(rsp, 0), length=Word.toInt size} @ preCode)
                        end
                end

            (* The rules for returning structs are similar to those for parameters. *)
            local
                (* Store a result register into the result area.  In almost all cases
                   this is very simple: the only complication is with structs of odd sizes. *)
                fun storeResult(reg, offset, size) = storeUpTo8(reg, resultAreaPtr, offset, size)
                    
                val {size, typeForm, ...} =  result
            in
                val (getResult, passArgAddress) =
                    if typeForm = CTypeVoid
                    then ([], false)
                    else case (classifyArg result, size > 0w8) of
                        (* 8 bytes or smaller - returned in RAX - Normal case for int-like results. *)
                        (ArgInRegs{firstInSSE=false, ...}, false) => (storeResult(rax, 0, size), false)

                        (* 8 bytes or smaller - returned in XMM0 - Normal case for real results. *)
                    |   (ArgInRegs{firstInSSE=true, ...}, false) =>
                            ([XMMStoreToMemory{toStore=xmm0, address={base=resultAreaPtr, offset=0, index=NoIndex},
                                  precision=if size = 0w4 then SinglePrecision else DoublePrecision}], false)

                        (* 9-16 bytes - returned in RAX/RDX. *)
                    |   (ArgInRegs{firstInSSE=false, secondInSSE=false}, true) =>
                            (storeResult(rax, 0, 0w8) @ storeResult(rdx, 0, size-0w8), false)

                        (* 9-16 bytes - first in RAX, second in XMM0. *)
                    |   (ArgInRegs{firstInSSE=false, secondInSSE=true}, true) =>
                            (XMMStoreToMemory{toStore=xmm0, address={base=resultAreaPtr, offset=8, index=NoIndex},
                                    precision=if size = 0w12 then SinglePrecision else DoublePrecision} ::
                                storeResult(rax, 0, 0w8), false)

                        (* 9-16 bytes - first in XMM0, second in RAX. *)
                    |   (ArgInRegs{firstInSSE=true, secondInSSE=false}, true) =>
                            (XMMStoreToMemory{toStore=xmm0, address={base=resultAreaPtr, offset=0, index=NoIndex}, precision=DoublePrecision} ::
                                storeResult(rax, 8, size-0w8), false)

                        (* 9-16 bytes - both values in SSE regs.*)
                    |   (ArgInRegs{firstInSSE=true, secondInSSE=true}, true) =>
                            ([XMMStoreToMemory{toStore=xmm0, address={base=resultAreaPtr, offset=0, index=NoIndex}, precision=DoublePrecision},
                              XMMStoreToMemory{toStore=xmm1, address={base=resultAreaPtr, offset=8, index=NoIndex},
                                    precision=if size = 0w12 then SinglePrecision else DoublePrecision}], false)

                    |   _ => ([], true) (* Have to pass the address of the area in memory *)
            end

            val (argCode, argStack, preArgCode) =
                if passArgAddress (* If we have to pass the address of the result struct it goes in rdi. *)
                then loadSysV64Args(args, 0, 0w0, tl sysVGenRegs, sysVFPRegs,
                        [moveRR{source=resultAreaPtr, output=rdi, opSize=nativeWordOpSize}], [])
                else loadSysV64Args(args, 0, 0w0, sysVGenRegs, sysVFPRegs, [], [])

            local
                val align = argStack mod 16
            in
                (* Always align the stack. *)
                val preArgAlign = if align = 0 then 0 else 16-align
                (* The total space on the stack that needs to be removed at the end. *)
                val postCallStackReset = argStack + preArgAlign
            end

        in
            (* Save heap ptr.  Needed in case we have a callback. *)
            [storeMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize)] @
            (
                (* Put the destination address into a callee save resgister.
                   We have to put the C address in there now because an ML address wouldn't be updated
                   by a possible GC in a callback. *)
                if #typeForm( result) <> CTypeVoid
                then [loadHeapMemory(resultAreaPtr, r8, 0, nativeWordOpSize)]
                else []
            ) @
            [
                (* Save the stack pointer. *)
                storeMemory(esp, ebp, memRegStackPtr, nativeWordOpSize), (* Save ML stack and switch to C stack. *)
                loadMemory(esp, ebp, memRegCStackPtr, nativeWordOpSize)  (* Load the saved C stack pointer. *)
            ] @
            (
                if preArgAlign = 0
                then []
                else [ArithToGenReg{opc=SUB, output=esp, source=NonAddressConstArg(LargeInt.fromInt preArgAlign), opSize=nativeWordOpSize}]
            ) @
            (
                (* The second argument is a SysWord containing the address of a malloced area of memory
                   with the actual arguments in it. *)
                if null args
                then []
                else [loadHeapMemory(argPtrReg, mlArg2Reg, 0, nativeWordOpSize)]
            ) @ preArgCode @ argCode @
            [
                let
                    (* The entry point is in a SysWord.word value in RAX. *)
                    val entryPoint =
                        case targetArch of
                            ObjectId32Bit2Word => MemoryArg{base=ebx, offset=0, index=Index4 eax}
                        |   ObjectId32Bit4Word => MemoryArg{base=ebx, offset=0, index=Index8 eax}
                        |   _ => MemoryArg{base=eax, offset=0, index=NoIndex}
                in
                    (* Call the function.  We're discarding the value in rsp so no need to remove args. *)
                    CallAddress entryPoint
                end
            ] @
            (* Restore the C stack value in case it's been changed by a callback. *)
            (
                if postCallStackReset = 0
                then []
                else [ArithToGenReg{opc=ADD, output=rsp, source=NonAddressConstArg(LargeInt.fromInt postCallStackReset), opSize=nativeWordOpSize}]
            ) @
            [
                storeMemory(rsp, rbp, memRegCStackPtr, nativeWordOpSize),
                loadMemory(esp, ebp, memRegStackPtr, nativeWordOpSize), (* Restore the ML stack pointer. *)
                (* Reload the heap pointer.  If we've called back to ML this could well have changed. *)
                loadMemory(r15, ebp, memRegLocalMPointer, nativeWordOpSize)
            ] @ (* Store the result in the destination. *) getResult @ [ ReturnFromFunction 0 ]
        end (* callUnix64Bits *)
        
        fun closureUnix64Bits(args, result) =
        let
            fun moveSysV64Args([], _, _, _, _, moveFromStack) = moveFromStack

            |   moveSysV64Args(arg::args, stackSpace, argOffset, gRegs, fpRegs, moveFromStack) =
                let
                    val {size, align, ...} =  arg
                    fun storeRegister(reg, offset, size) = storeUpTo8(reg, rsp, offset, size)
                    val newArgOffset = alignUp(argOffset, align)
                    val word1Addr = {base=rsp, offset=Word.toInt newArgOffset, index=NoIndex}
                    val word2Addr = {base=rsp, offset=Word.toInt newArgOffset + 8, index=NoIndex}
                in
                    case (classifyArg arg, size > 0w8, gRegs, fpRegs) of
                        (* 8 bytes or smaller - single general reg.  This is the usual case. *)
                        (ArgInRegs{firstInSSE=false, ...}, false, gReg :: gRegs', fpRegs') =>
                            storeRegister(gReg, Word.toInt newArgOffset, size) @
                                moveSysV64Args(args, stackSpace, newArgOffset+size, gRegs', fpRegs', moveFromStack)

                        (* 8 bytes or smaller - single SSE reg.  Usual case for real arguments. *)
                    |   (ArgInRegs{firstInSSE=true, ...}, false, gRegs', fpReg :: fpRegs') =>
                            XMMStoreToMemory{precision=if size = 0w4 then SinglePrecision else DoublePrecision, address=word1Addr, toStore=fpReg } :: 
                                moveSysV64Args(args, stackSpace, newArgOffset+size, gRegs', fpRegs', moveFromStack)
                    
                        (* 9-16 bytes - both values in general regs. *)
                    |   (ArgInRegs{firstInSSE=false, secondInSSE=false}, true, gReg1 :: gReg2 :: gRegs', fpRegs') =>
                            Move{source=MemoryArg word1Addr, destination=RegisterArg gReg1, moveSize=Move64} ::
                                storeRegister(gReg2, Word.toInt newArgOffset+8, size-0w8) @ 
                                moveSysV64Args(args, stackSpace, newArgOffset+size, gRegs', fpRegs', moveFromStack)

                        (* 9-16 bytes - first in general, second in SSE. *)
                    |   (ArgInRegs{firstInSSE=false, secondInSSE=true}, true, gReg :: gRegs', fpReg :: fpRegs') =>
                            Move{source=MemoryArg word1Addr, destination=RegisterArg gReg, moveSize=Move64} ::
                                XMMStoreToMemory{precision=if size = 0w12 then SinglePrecision else DoublePrecision, address=word2Addr, toStore=fpReg } :: 
                                moveSysV64Args(args, stackSpace, newArgOffset+size, gRegs', fpRegs', moveFromStack)

                        (* 9-16 bytes - first in SSE, second in general. *)
                    |   (ArgInRegs{firstInSSE=true, secondInSSE=false}, true, gReg :: gRegs', fpReg :: fpRegs') =>
                            XMMStoreToMemory{precision=DoublePrecision, address=word1Addr, toStore=fpReg } ::
                                storeRegister(gReg, Word.toInt newArgOffset+8, size-0w8) @
                                moveSysV64Args(args, stackSpace, newArgOffset+size, gRegs', fpRegs', moveFromStack)

                    |   (* 9-16 bytes - both values in SSE regs. *)
                        (ArgInRegs{firstInSSE=true, secondInSSE=true}, true, gRegs', fpReg1 :: fpReg2 :: fpRegs') =>
                            XMMStoreToMemory{precision=DoublePrecision, address=word1Addr, toStore=fpReg1 } ::
                                XMMStoreToMemory{precision=if size = 0w12 then SinglePrecision else DoublePrecision,
                                        address=word2Addr, toStore=fpReg2 } :: 
                                moveSysV64Args(args, stackSpace, newArgOffset+size, gRegs', fpRegs', moveFromStack)

                    |   (_, _, gRegs', fpRegs') =>
                        (* Either larger than 16 bytes or we've run out of the right kind of register.
                           Structs larger than 16 bytes are passed by value on the stack.  Move the
                           argument after we've stored all the registers in particular rsi and rdi. *)
                        let
                            val space = intAlignUp(Word.toInt size, 0w8)
                        in
                            moveSysV64Args(args, stackSpace+space, newArgOffset+size, gRegs', fpRegs',
                                moveMemory{source=(r10, stackSpace), destination=(rsp, Word.toInt newArgOffset), length=Word.toInt size} @ 
                                    moveFromStack)
                        end
                end

            (* Result structs larger than 16 bytes are returned by reference.  *)
            val resultStructByRef = #size ( result) > 0w16

            val copyArgsFromRegsAndStack =
                if resultStructByRef (* rdi contains the address for the result. *)
                then moveSysV64Args(args, 0, 0w0, tl sysVGenRegs, sysVFPRegs, [])
                else moveSysV64Args(args, 0, 0w0, sysVGenRegs, sysVFPRegs, [])

            local
                fun getNextSize (arg, argOffset) =
                let val {size, align, ...} =  arg in alignUp(argOffset, align) + size end
            in
                val argumentSpace = List.foldl getNextSize 0w0 args
            end

            (* Allocate a 16-byte area for any results returned in registers.  This will not be used
               if the result is a structure larger than 16-bytes. *)
            val resultOffset = alignUp(argumentSpace, 0w8)
            (* Ensure the stack is 16 bytes aligned.  We've pushed 6 regs and a return address
               so add a further 8 bytes to bring it back into alignment.  If we're returning a struct
               by reference, though, we've pushed 7 regs so don't add 8. *)
            val stackToAllocate =
                Word.toInt(alignUp(resultOffset + 0w16, 0w16)) + (if resultStructByRef then 0 else 8)

            (* The rules for returning structs are similar to those for parameters. *)
            local
                (* The result area is always 16 bytes wide so we can load the result without risking reading outside.
                   At least at the moment we ignore any sign extension. *)                    
                val {size, typeForm, ...} = result
                val resultOffset = Word.toInt resultOffset
            in
                val loadResults =
                    if typeForm = CTypeVoid
                    then []
                    else case (classifyArg result, size > 0w8) of
                        (* 8 bytes or smaller - returned in RAX - Normal case for int-like results. *)
                        (ArgInRegs{firstInSSE=false, ...}, false) =>
                            [Move{source=MemoryArg {base=rsp, offset=resultOffset, index=NoIndex}, destination=RegisterArg rax, moveSize=Move64}]

                        (* 8 bytes or smaller - returned in XMM0 - Normal case for real results. *)
                    |   (ArgInRegs{firstInSSE=true, ...}, false) =>
                            [XMMStoreToMemory{toStore=xmm0, address={base=rsp, offset=resultOffset, index=NoIndex},
                                  precision=if size = 0w4 then SinglePrecision else DoublePrecision}]

                        (* 9-16 bytes - returned in RAX/RDX. *)
                    |   (ArgInRegs{firstInSSE=false, secondInSSE=false}, true) =>
                            [Move{source=MemoryArg {base=rsp, offset=resultOffset, index=NoIndex}, destination=RegisterArg rax, moveSize=Move64},
                              Move{source=MemoryArg {base=rsp, offset=resultOffset+8, index=NoIndex}, destination=RegisterArg rdx, moveSize=Move64}]

                        (* 9-16 bytes - first in RAX, second in XMM0. *)
                    |   (ArgInRegs{firstInSSE=false, secondInSSE=true}, true) =>
                            [Move{source=MemoryArg {base=rsp, offset=resultOffset, index=NoIndex}, destination=RegisterArg rax, moveSize=Move64},
                              XMMStoreToMemory{toStore=xmm0, address={base=rsp, offset=resultOffset+8, index=NoIndex},
                                    precision=if size = 0w12 then SinglePrecision else DoublePrecision}]

                        (* 9-16 bytes - first in XMM0, second in RAX. *)
                    |   (ArgInRegs{firstInSSE=true, secondInSSE=false}, true) =>
                            [XMMStoreToMemory{toStore=xmm0, address={base=rsp, offset=resultOffset, index=NoIndex}, precision=DoublePrecision},
                             Move{source=MemoryArg {base=rsp, offset=resultOffset+8, index=NoIndex}, destination=RegisterArg rax, moveSize=Move64}]

                        (* 9-16 bytes - both values in SSE regs.*)
                    |   (ArgInRegs{firstInSSE=true, secondInSSE=true}, true) =>
                            [XMMStoreToMemory{toStore=xmm0, address={base=rsp, offset=resultOffset, index=NoIndex}, precision=DoublePrecision},
                              XMMStoreToMemory{toStore=xmm1, address={base=rsp, offset=resultOffset+8, index=NoIndex},
                                    precision=if size = 0w12 then SinglePrecision else DoublePrecision}]

                    |   _ => [] (* Have to pass the address of the area in memory *)
            end
        in
        [
            (* Push callee-save registers. *)
            PushToStack(RegisterArg rbp), PushToStack(RegisterArg rbx), PushToStack(RegisterArg r12),
            PushToStack(RegisterArg r13), PushToStack(RegisterArg r14), PushToStack(RegisterArg r15)
        ] @
        (
            (* If we're returning a struct by reference we have to return the address in rax even though
               it's been set by the caller.  Save this address. *)
            if resultStructByRef
            then [PushToStack(RegisterArg rdi)]
            else []
        ) @
        [
            (* Set r10 to point to the original stack args if any. *)
            LoadAddress{ output=r10, offset=if resultStructByRef then 64 else 56, base=SOME rsp, index=NoIndex, opSize=nativeWordOpSize},
            (* Allocate stack space. *)
            ArithToGenReg{opc=SUB, output=rsp, source=NonAddressConstArg(LargeInt.fromInt stackToAllocate), opSize=nativeWordOpSize},
            (* Move the function we're calling, in rax, into r13, a callee-save register *)
            moveRR{source=rax, output=r13, opSize=polyWordOpSize}
        ]
          @ copyArgsFromRegsAndStack @
        [
            (* Get the value for rbp.  This has to be an absolute address in 32-in-64. *)
            LoadAbsolute{value=getThreadDataCall, destination=rcx},
            CallAddress(MemoryArg{base=rcx, offset=0, index=NoIndex}),
            moveRR{source=rax, output=rbp, opSize=nativeWordOpSize},
            (* Save the address of the argument and result area. *)
            moveRR{source=rsp, output=rcx, opSize=nativeWordOpSize},
            (* Switch to the ML stack. *)
            storeMemory(rsp, rbp, memRegCStackPtr, nativeWordOpSize),
            loadMemory(rsp, rbp, memRegStackPtr, nativeWordOpSize),
            (* Load the ML heap pointer. *)
            loadMemory(r15, rbp, memRegLocalMPointer, nativeWordOpSize),
            (* Now move the function closure into the closure register ready for the call. *)
            moveRR{source=r13, output=rdx, opSize=polyWordOpSize}
        ] @
        (* Reload the heap base address in 32-in-64. *)
        ( if targetArch = ObjectId32Bit2Word orelse targetArch = ObjectId32Bit4Word then [loadMemory(rbx, rbp, memRegSavedRbx, nativeWordOpSize)] else [] )
         @ boxRegAsSysWord(rcx, rax, []) @
        (
            (* If we're returning a struct by reference the address for the result will have been passed in the
               first argument.  We use that as the result area.  Otherwise point to the result area on the stack.  *)
            if resultStructByRef
            then loadMemory(rcx, r10, ~64, nativeWordOpSize)
            else ArithToGenReg{opc=ADD, output=rcx, source=NonAddressConstArg(Word.toLargeInt resultOffset), opSize=nativeWordOpSize}
        ) :: boxRegAsSysWord(rcx, mlArg2Reg, [rax]) @
        [
            (* Call the ML function using the full closure call. *)
            CallAddress(
                case targetArch of
                    ObjectId32Bit2Word => MemoryArg{base=rbx, index=Index4 rdx, offset=0}
                |   ObjectId32Bit4Word => MemoryArg{base=rbx, index=Index8 rdx, offset=0}
                |   _ => MemoryArg{base=rdx, index=NoIndex, offset=0}),
            (* Save the ML stack pointer because we may have grown the stack.  Switch to the C stack. *)
            storeMemory(rsp, rbp, memRegStackPtr, nativeWordOpSize),
            loadMemory(rsp, rbp, memRegCStackPtr, nativeWordOpSize),
            storeMemory(r15, rbp, memRegLocalMPointer, nativeWordOpSize)
        ] @ loadResults @
        [
            (* Remove the stack space. *)
            ArithToGenReg{opc=ADD, output=rsp, source=NonAddressConstArg(LargeInt.fromInt stackToAllocate), opSize=nativeWordOpSize}
        ] @
        ( if resultStructByRef then [PopR rax] else [] ) @
        [
            PopR r15, PopR r14, PopR r13, PopR r12, PopR rbx, PopR rbp, (* Restore callee-save registers. *)
            ReturnFromFunction 0 (* Caller removes any stack arguments. *)
        ]
        end
    end

    fun foreignCall(abi: ffiABI, args: cType list, result: cType): Address.machineWord =
    let
        val code =
            case abi of
                FFI_UNIX64 => callUnix64Bits(args, result)
            |   FFI_WIN64 => callWindows64Bits(args, result)
            |   abi => call32Bits(abi, args, result)

        val functionName = "foreignCall"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject DEBUG.assemblyCodeTag true*)]
        val profileObject = createProfileObject ()
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86Optimise.generateCode{code=newCode, labelCount=0, ops=code, resultClosure=closure}
    in
        closureAsAddress closure
    end

    (* Build a callback function.  The arguments are the abi, the list of argument types and the result type.
       The result is an ML function that takes an ML function, f, as its argument, registers it as a callback and
       returns the C function as its result.  When the C function is called the arguments are copied into
       temporary memory and the vector passed to f along with the address of the memory for the result.
       "f" stores the result in it when it returns and the result is then passed back as the result of the
       callback.
       N.B.  This returns a closure cell which contains the address of the code.  It can be used as a
       SysWord.word value except that while it exists the code will not be GCed.  *)
    fun buildCallBack(abi: ffiABI, args: cType list, result: cType): Address.machineWord =
    let
        val code =
            case abi of
                FFI_UNIX64 => closureUnix64Bits(args, result)
            |   FFI_WIN64 => closureWindows64Bits(args, result)
            |   abi => closure32Bits(abi, args, result)

        val functionName = "foreignCallBack(2)"
        val debugSwitches =
            [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
               Universal.tagInject DEBUG.assemblyCodeTag true*)]
        val profileObject = createProfileObject ()
        val newCode = codeCreate (functionName, profileObject, debugSwitches)
        val closure = makeConstantClosure()
        val () = X86Optimise.generateCode{code=newCode, labelCount=0, ops=code, resultClosure=closure}
        val stage2Code = closureAsAddress closure

        fun resultFunction f =
        let
            (* Generate a small function to load the address of f into rax/eax and then jump to stage2.
               The idea is that it should be possible to generate this eventually in a single RTS call.
               That could be done by using a version of this as a model. *)
            val codeAddress =
                (* In the native code versions we extract the code address from the closure.
                   We don't do that in 32-in-64 and instead the RTS does it. *)
                if targetArch = ObjectId32Bit2Word orelse targetArch = ObjectId32Bit4Word
                then stage2Code
                else Address.loadWord(Address.toAddress stage2Code, 0w0)
            val code =
                [
                    Move{source=AddressConstArg(Address.toMachineWord f), destination=RegisterArg rax, moveSize=opSizeToMove polyWordOpSize},
                    JumpAddress(AddressConstArg codeAddress)
                ]
            val functionName = "foreignCallBack(1)"
            val debugSwitches =
                [(*Universal.tagInject Pretty.compilerOutputTag (Pretty.prettyPrint(print, 70)),
                   Universal.tagInject DEBUG.assemblyCodeTag true*)]
            val profileObject = createProfileObject ()
            val newCode = codeCreate (functionName, profileObject, debugSwitches)
            val closure = makeConstantClosure()
            val () = X86Optimise.generateCode{code=newCode, labelCount=0, ops=code, resultClosure=closure}
            val res = closureAsAddress closure
            (*val _ = print("Address is " ^ (LargeWord.toString(RunCall.unsafeCast res)) ^ "\n")*)
        in
            res
        end
    in
        Address.toMachineWord resultFunction
    end

end;
