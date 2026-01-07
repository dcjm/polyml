(*
    Signature for the high-level ARM64 code

    Copyright David C. J. Matthews 2021-3, 2026

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

signature ARM64ICODE =
sig
    type machineWord = Address.machineWord
    type address = Address.address
    
    type closureRef

    (* Registers. *)
    datatype xReg = XReg of Word8.word | XZero | XSP
    and vReg = VReg of Word8.word

    (* It is simpler to use a single type for all registers. *)
    datatype reg = GenReg of xReg | FPReg of vReg

    val X0:  xReg   and X1:  xReg   and X2:  xReg   and X3: xReg
    and X4:  xReg   and X5:  xReg   and X6:  xReg   and X7: xReg
    and X8:  xReg   and X9:  xReg   and X10: xReg   and X11: xReg
    and X12: xReg   and X13: xReg   and X14: xReg   and X15: xReg
    and X16: xReg   and X17: xReg   and X18: xReg   and X19: xReg
    and X20: xReg   and X21: xReg   and X22: xReg   and X23: xReg
    and X24: xReg   and X25: xReg   and X26: xReg   and X27: xReg
    and X28: xReg   and X29: xReg   and X30: xReg

    val V0:  vReg   and V1:  vReg   and V2:  vReg   and V3: vReg
    and V4:  vReg   and V5:  vReg   and V6:  vReg   and V7: vReg
    
	datatype archType = ArchNative | ArchC32 of word
	val archType: archType

    val isBigEndian: bool
    
    (* Condition for conditional branches etc. *)
    datatype condition =
        CondEqual            (* Z=1 *)
    |   CondNotEqual         (* Z=0 *)
    |   CondCarrySet         (* C=1 *)
    |   CondCarryClear       (* C=0 *)
    |   CondNegative         (* N=1 *)
    |   CondPositive         (* N=0 imcludes zero *)
    |   CondOverflow         (* V=1 *)
    |   CondNoOverflow       (* V=0 *)
    |   CondUnsignedHigher   (* C=1 && Z=0 *)
    |   CondUnsignedLowOrEq  (* ! (C=1 && Z=0) *)
    |   CondSignedGreaterEq  (* N=V *)
    |   CondSignedLess       (* N<>V *)
    |   CondSignedGreater    (* Z==0 && N=V *)
    |   CondSignedLessEq     (* !(Z==0 && N=V) *)

    (* The shift used in arithemtic operations. *)
    and shiftType =
        ShiftLSL of Word8.word
    |   ShiftLSR of Word8.word
    |   ShiftASR of Word8.word
    |   ShiftNone

    datatype preg = PReg of int (* A pseudo-register - an abstract register. *)

    (* If the value is zero we can use X0/W0. *)
    datatype pregOrZero = SomeReg of preg | ZeroReg
    
    (* A location on the stack.  May be more than word if this is a container or a handler entry. *)
    datatype stackLocn = StackLoc of {size: int, rno: int }
    
    (* This combines pregKind and stackLocn.  *)
    datatype regProperty =
        RegPropGeneral      (* A general register. *)
    |   RegPropUntagged     (* An untagged general register. *)
    |   RegPropStack of int (* A stack location or container. *)
    |   RegPropCacheTagged
    |   RegPropCacheUntagged
    |   RegPropMultiple     (* The result of a conditional or case. May be defined at multiple points. *)
    
    (* The reference to a condition code. *)
    datatype ccRef = CcRef of int

    datatype loadType = Load64 | Load32 | Load16 | Load8
    and opSize = OpSize32 | OpSize64
    and logicalOp = LogAnd | LogOr | LogXor
    and callKind = Recursive | ConstantCode of machineWord | FullCall
    and floatSize = Float32 | Double64
    and shiftDirection = ShiftLeft | ShiftRightLogical | ShiftRightArithmetic
    and multKind =
        MultAdd32 | MultSub32 | MultAdd64 | MultSub64 |
        SignedMultAddLong (* 32bit*32bit + 64bit => 64Bit *) |
        SignedMultHigh (* High order part of 64bit*64Bit *)
    and fpUnary =
        NegFloat | NegDouble | AbsFloat | AbsDouble | ConvFloatToDble |
        ConvDbleToFloat | MoveDouble | MoveFloat
    and fpBinary = MultiplyFP | DivideFP | AddFP | SubtractFP
        (* Some of the atomic operations added in 8.1 *)
    and atomicOp = LoadAddAL | LoadUmaxAL | SwapAL | LoadAddAcquire | LoadUMaxAcquire | SwapRelease

    (* Function calls can have an unlimited number of arguments so it isn't always
       going to be possible to load them into registers. *)
    datatype 'genReg fnarg = ArgInReg of 'genReg | ArgOnStack of { wordOffset: int, container: stackLocn, field: int }
    
    (* When allocating memory we may garbage collect. Any registers containing addresses must be updated to contain
       their new location.  All other registers are saved and restored without any update.  The exception is X30
       and we must not use X30 for any value that contains an untagged value that is needed after the call. *)
    type 'genReg saveSet = { gcUpdate: 'genReg list, noUpdate: 'genReg list }

    datatype ('genReg, 'optGenReg, 'fpReg) arm64ICode =
        (* Move the contents of one preg to another.  These are always 64-bits. *)
        MoveRegister of { source: 'genReg, dest: 'genReg }

        (* Numerical constant. *)
    |   LoadNonAddressConstant of { source: Word64.word, dest: 'genReg }

        (* Floating point constant *)
    |   LoadFPConstant of { source: Word64.word, dest: 'fpReg, floatSize: floatSize }

        (* Address constant. *)
    |   LoadAddressConstant of { source: machineWord, dest: 'genReg }

        (* Load a value into a register using a constant, signed, byte offset.  The offset
           is in the range of -256 to (+4095*unit size). *)
    |   LoadWithConstantOffset of { base: 'genReg, dest: 'genReg, byteOffset: int, loadType: loadType }

        (* Similarly for FP registers. *)
    |   LoadFPWithConstantOffset of { base: 'genReg, dest: 'fpReg, byteOffset: int, floatSize: floatSize }

        (* Load a value into a register using an index register. *)
    |   LoadWithIndexedOffset of { base: 'genReg, dest: 'genReg, index: 'genReg, loadType: loadType, signExtendIndex: bool }

        (* Ditto for FP. *)
    |   LoadFPWithIndexedOffset of { base: 'genReg, dest: 'fpReg, index: 'genReg, floatSize: floatSize, signExtendIndex: bool }

        (* Returns the current thread ID.  Always a 64-bit value.. *)
    |   GetThreadId of { dest: 'genReg }

        (* Convert a 32-in-64 object index into an absolute address. *)
    |   ObjectIndexAddressToAbsolute of { source: 'genReg, dest: 'genReg }

        (* Convert an absolute address into an object index. *)
    |   AbsoluteToObjectIndex of { source: 'genReg, dest: 'genReg }

        (* Allocate a fixed sized piece of memory and puts the absolute address into dest.
           bytesRequired is the total number of bytes including the length word and any alignment
           necessary for 32-in-64. saveRegs is the list of registers that need to be saved if we
           need to do a garbage collection. *)
    |   AllocateMemoryFixed of { bytesRequired: Word64.word, dest: 'genReg, saveRegs: 'genReg saveSet }

        (* Allocate a piece of memory.  The size argument is an untagged value containing
           the number of words i.e. the same value used for InitialiseMemory and to store
           in the length word. *)
    |   AllocateMemoryVariable of { size: 'genReg, dest: 'genReg, saveRegs: 'genReg saveSet }

        (* Initialise a piece of memory by writing "size" copies of the value
           in "init".  N.B. The size is an untagged value containing the
           number of words. *)
    |   InitialiseMem of { size: 'genReg, addr: 'genReg, init: 'genReg }

        (* Mark the beginning of a loop.  This is really only to prevent the initialisation code being
           duplicated in ICodeOptimise. *)
    |   BeginLoop

        (* Set up the registers for a jump back to the start of a loop. *)
    |   JumpLoop of
            { regArgs: {src: 'genReg fnarg, dst: 'genReg} list,
              stackArgs: {src: 'genReg fnarg, wordOffset: int, stackloc: stackLocn} list,
              checkInterrupt: 'genReg saveSet option }

        (* Store a register using a constant, signed, byte offset.  The offset
           is in the range of -256 to (+4095*unit size). *)
    |   StoreWithConstantOffset of { source: 'genReg, base: 'genReg, byteOffset: int, loadType: loadType }

        (* Ditto for FP regs. *)
    |   StoreFPWithConstantOffset of { source: 'fpReg, base: 'genReg, byteOffset: int, floatSize: floatSize }

        (* Store a register using an index register. *)
    |   StoreWithIndexedOffset of { source: 'genReg, base: 'genReg, index: 'genReg, loadType: loadType, signExtendIndex: bool }

        (* and for FP regs. *)
    |   StoreFPWithIndexedOffset of { source: 'fpReg, base: 'genReg, index: 'genReg, floatSize: floatSize, signExtendIndex: bool }

        (* Add/Subtract immediate.  The destination is optional in which case XZero is used.
           ccRef is optional.  If it is NONE the version of the instruction that does not generate
           a condition code is used. immed must be < 0wx1000. *)
    |   AddSubImmediate of { source: 'genReg, dest: 'optGenReg, ccRef: ccRef option, immed: word,
                             isAdd: bool, length: opSize }

        (* Add/Subtract register.  As with AddSubImmediate, both the destination and cc are optional. *)
    |   AddSubRegister of { base: 'genReg, shifted: 'genReg, dest: 'optGenReg, ccRef: ccRef option,
                            isAdd: bool, length: opSize, shift: shiftType }

        (* Bitwise logical operations.  The immediate value must be a valid bit pattern.  ccRef can
           only be SOME if logOp is LogAnd. *)
    |   LogicalImmediate of { source: 'genReg, dest: 'optGenReg, ccRef: ccRef option, immed: Word64.word,
                              logOp: logicalOp, length: opSize }

        (* Register logical operations.  ccRef can only be SOME if logOp is LogAnd.*)
    |   LogicalRegister of { base: 'genReg, shifted: 'genReg, dest: 'optGenReg, ccRef: ccRef option,
                             logOp: logicalOp, length: opSize, shift: shiftType }

        (* Shift a word by an amount specified in a register. *)
    |   ShiftRegister of { direction: shiftDirection, dest: 'genReg, source: 'genReg, shift: 'genReg, opSize: opSize }

        (* Count leading zeros.  This could be expand to other single-source instruction but currently that is the
           only one used. *)
    |   CountLeadingZeroBits of { dest: 'genReg, source: 'genReg, opSize: opSize }

        (* The various forms of multiply all take three arguments and the general form is
           dest = M * N +/- A..   *)
    |   Multiplication of { kind: multKind, dest: 'genReg, sourceA: 'optGenReg, sourceM: 'genReg, sourceN: 'genReg }

        (* Signed or unsigned division.  Sets the result to zero if the divisor is zero. *)
    |   Division of { isSigned: bool, dest: 'genReg, dividend: 'genReg, divisor: 'genReg, opSize: opSize }

        (* Start of function.  Set the register arguments.  stackArgs is the list of
           stack arguments.  If the function has a real closure regArgs includes the
           closure register (X8).  The register arguments include the return register
           (X30). *)
    |   BeginFunction of { regArgs: ('genReg * xReg) list, fpRegArgs: ('fpReg * vReg) list, stackArgs: stackLocn list }

        (* Call a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through X8 which has been loaded
           as one of the argument registers.  The results are stored in the result registers,
           usually just X0.
           The "containers" argument is used to ensure that any container whose address is passed
           as one of the other arguments continues to be referenced until the function is called
           since there's a possibility that it isn't actually used after the function. *)
    |   FunctionCall of
            { callKind: callKind, regArgs: ('genReg fnarg * xReg) list,
              stackArgs: 'genReg fnarg list, dests: ('genReg * xReg) list,
              fpRegArgs: ('fpReg * vReg) list, fpDests: ('fpReg * vReg) list,
              saveRegs: 'genReg saveSet, containers: stackLocn list}

        (* Jump to a tail-recursive function.  This is similar to FunctionCall
           but complicated for stack arguments because the stack and the return
           address need to be overwritten.
           stackAdjust is the number of words to remove (positive) or add
           (negative) to the stack before the call.
           currStackSize contains the number of items currently on the stack. *)
    |   TailRecursiveCall of
            { callKind: callKind, regArgs: ('genReg fnarg * xReg) list,
              stackArgs: {src: 'genReg fnarg, stack: int} list,
              fpRegArgs: ('fpReg * vReg) list,
              stackAdjust: int, currStackSize: int }

        (* Return from the function.  resultRegs are the registers containing
           the result,
           returnReg is the preg that contains the return address. *)
    |   ReturnResultFromFunction of
            { results: ('genReg * xReg) list, fpResults: ('fpReg * vReg) list, returnReg: 'genReg, numStackArgs: int }

        (* Raise an exception.  The packet is always loaded into X0. *)
    |   RaiseExceptionPacket of { packetReg: 'genReg }

        (* Push a register to the stack.  This is used both for a normal push, copies=1, and
           also to reserve a container. *)
    |   PushToStack of { source: 'genReg, copies: int, container: stackLocn }

        (* Load a register from the stack.  The container is the stack location identifier,
           the field is an offset in a container. *)
    |   LoadStack of { dest: 'genReg, wordOffset: int, container: stackLocn, field: int }

        (* Store a value into the stack. *)
    |   StoreToStack of { source: 'genReg, container: stackLocn, field: int, stackOffset: int }

        (* Set the register to the address of the container i.e. a specific offset on the stack. *)
    |   ContainerAddress of { dest: 'genReg, container: stackLocn, stackOffset: int }

        (* Remove items from the stack.  Used to remove containers or
           registers pushed to the stack.. *)
    |   ResetStackPtr of { numWords: int }

        (* Tag a value by shifting and setting the tag bit. *)
    |   TagValue of { source: 'genReg, dest: 'genReg, isSigned: bool, opSize: opSize }

        (* Shift a value to remove the tag bit.  The cache is used if this is untagging a
           value that has previously been tagged. *)
    |   UntagValue of { source: 'genReg, dest: 'genReg, isSigned: bool, opSize: opSize }

        (* Box a largeword value.  Stores a value
           into a byte area.  This can be implemented using AllocateMemoryFixed
           but keeping it separate makes optimisation easier.
           The result is always an address and needs to be converted to an
           object index on 32-in-64. *)
    |   BoxLarge of { source: 'genReg, dest: 'genReg, saveRegs: 'genReg saveSet }

        (* Load a value from a box.  This can be implemented using a load but
           is kept separate to simplify optimisation.  The source is always
           an absolute address. *)
    |   UnboxLarge of { source: 'genReg, dest: 'genReg }

        (* Convert a floating point value into a value suitable for storing
           in the heap.  This normally involves boxing except that 32-bit
           floats can be tagged in native 64-bits. *)
    |   BoxTagFloat of { floatSize: floatSize, source: 'fpReg, dest: 'genReg, saveRegs: 'genReg saveSet }

        (* The reverse of BoxTagFloat. *)
    |   UnboxTagFloat of { floatSize: floatSize, source: 'genReg, dest: 'fpReg }

        (* Load a value with acquire semantics.  This means that any other
           load in this thread after this sees the value of the shared
           memory at this point and not earlier.  This is used for
           references and arrays to ensure that if another thread has
           built a data structure on the heap and then assigns the
           address to a shared ref this thread will see the updated heap
           and not any locally cached previous version. *)
    |   LoadAcquire of { base: 'genReg, dest: 'genReg, loadType: loadType }

        (* Store a value with release semantics.  This ensures that any
           other write completes before this operation and works with
           LoadAcquire. *)
    |   StoreRelease of { base: 'genReg, source: 'genReg, loadType: loadType }

        (* This is a generalised constant shift which includes selection of a
           range of bits. *)
    |   BitFieldShift of { source: 'genReg, dest: 'genReg, isSigned: bool, length: opSize, immr: word, imms: word }

        (*  Copy a range of bits and insert it into another register.  This is the
            only case where a register functions both as a source and a destination. *)
    |   BitFieldInsert of { source: 'genReg, destAsSource: 'genReg, dest: 'genReg,
                            length: opSize, immr: word, imms: word }

        (* Indexed case. *)
    |   IndexedCaseOperation of { testReg: 'genReg }

        (* Exception handling.  - Set up an exception handler. *)
    |   PushExceptionHandler

        (* End of a handled section.  Restore the previous handler. *)
    |   PopExceptionHandler

        (* Marks the start of a handler.  This sets the stack pointer and
           restores the old handler.  Sets the exception packet register. *) 
    |   BeginHandler of { packetReg: 'genReg }

        (* Compare two vectors of bytes and set the condition code on the result.
           The registers are modified by the instruction. *)
    |   CompareByteVectors of
            { vec1Addr: 'genReg, vec2Addr: 'genReg, length: 'genReg, ccRef: ccRef }

        (* Move a block of bytes (isByteMove true) or words (isByteMove false).  The length is the
           number of items (bytes or words) to move. The registers are modified by
           the instruction. *)
    |   BlockMove of { srcAddr: 'genReg, destAddr: 'genReg, length: 'genReg, isByteMove: bool }

        (* Add or subtract to the system stack pointer and optionally return the new value.
           This is used to allocate and deallocate C space. *)
    |   AddSubXSP of { source: 'genReg, dest: 'optGenReg, isAdd: bool  }

        (* Ensures the value will actually be referenced although it doesn't generate any code. *)
    |   TouchValue of { source: 'genReg }

        (* Load a value at the address and get exclusive access.  Always loads a
           64-bit value. *)
    |   LoadAcquireExclusive of { base: 'genReg, dest: 'genReg }

        (* Store a value into an address releasing the lock.  Sets the result to
           either 0 or 1 if it succeeds or fails. *)
    |   StoreReleaseExclusive of { base: 'genReg, source: 'optGenReg, result: 'genReg }

        (* Insert a memory barrier. dmb ish. *)
    |   MemoryBarrier

        (* Convert an integer to a floating point value. *)
    |   ConvertIntToFloat of { source: 'genReg, dest: 'fpReg, srcSize: opSize, destSize: floatSize }

        (* Convert a floating point value to an integer using the specified rounding mode.
           We could get an overflow here but fortunately the ARM generates a value
           that will cause an overflow when we tag it, provided we tag it explicitly. *)
    |   ConvertFloatToInt of { source: 'fpReg, dest: 'genReg, srcSize: floatSize, destSize: opSize, rounding: IEEEReal.rounding_mode }

        (* Unary floating point.  This includes moves and conversions between float and double. *)
    |   UnaryFloatingPt of { source: 'fpReg, dest: 'fpReg, fpOp: fpUnary }

        (* Binary floating point: addition, subtraction, multiplication and division. *)
    |   BinaryFloatingPoint of { arg1: 'fpReg, arg2: 'fpReg, dest: 'fpReg, fpOp: fpBinary, opSize: floatSize }

        (* Floating point comparison. *)
    |   CompareFloatingPoint of { arg1: 'fpReg, arg2: 'fpReg, ccRef: ccRef, opSize: floatSize }

        (* Yield control during a spin-lock. *)
    |   CPUYield

        (* Atomic operations added for ARM 8.1 *)
    |   AtomicOperation of { base: 'genReg, source: 'optGenReg, dest: 'optGenReg, atOp: atomicOp }

        (* Destinations at the end of a basic block. *)
    and controlFlow =
        (* Unconditional branch to a label - should be a merge point. *)
        Unconditional of int
        (* Conditional branch. Jumps to trueJump if the condional is false, falseJump if false. *)
    |   Conditional of { ccRef: ccRef, condition: condition, trueJump: int, falseJump: int }
        (* Exit - the last instruction of the block is a return, raise or tailcall. *)
    |   ExitCode
        (* Indexed case - this branches to one of a number of labels *)
    |   IndexedBr of int list
        (* Set up a handler.  This doesn't cause an immediate branch but the state at the
           start of the handler is the state at this point. *)
    |   SetHandler of { handler: int, continue: int }
        (* Unconditional branch to a handler.  If an exception is raised explicitly
           within the scope of a handler. *)
    |   UnconditionalHandle of int
        (* Conditional branch to a handler.  Occurs if there is a call to a
           function within the scope of a handler.  It may jump to the handler. *)
    |   ConditionalHandle of { handler: int, continue: int }

    and ('genReg, 'optGenReg, 'fpReg) basicBlock =
            BasicBlock of { block: ('genReg, 'optGenReg, 'fpReg) arm64ICode list, flow: controlFlow }
    
    (* Return the successor blocks from a control flow. *)
    val successorBlocks: controlFlow -> int list

    type iCodeAbstract = (preg, pregOrZero, preg) arm64ICode and basicBlockAbstract = (preg, pregOrZero, preg) basicBlock
    and  iCodeConcrete = (xReg, xReg, vReg) arm64ICode and basicBlockConcrete = (xReg, xReg, vReg) basicBlock

    val printICodeAbstract: basicBlockAbstract vector * (string -> unit) -> unit
    and printICodeConcrete: basicBlockConcrete vector * (string -> unit) -> unit

    (* Check whether this value is acceptable for LogicalImmediate. *)
    val isEncodableBitPattern: Word64.word * opSize -> bool

    (* This generates a  BitField instruction with the appropriate values for immr and imms. *)
    val shiftConstant:
        { direction: shiftDirection, dest: preg, source: preg, shift: word, opSize: opSize } -> iCodeAbstract
    
    structure Sharing:
    sig
        type xReg           = xReg
        and  vReg           = vReg
        and  reg            = reg
        and  condition      = condition
        and  shiftType      = shiftType
        and  ('genReg, 'optGenReg, 'fpReg) arm64ICode = ('genReg, 'optGenReg, 'fpReg) arm64ICode
        and  preg           = preg
        and  pregOrZero     = pregOrZero
        and  controlFlow    = controlFlow
        and  ('genReg, 'optGenReg, 'fpReg) basicBlock = ('genReg, 'optGenReg, 'fpReg) basicBlock
        and  stackLocn      = stackLocn
        and  regProperty    = regProperty
        and  ccRef          = ccRef
        and  'genReg fnarg  = 'genReg fnarg
        and  closureRef     = closureRef
        and  loadType       = loadType
        and  opSize         = opSize
        and  logicalOp      = logicalOp
        and  callKind       = callKind
        and  floatSize      = floatSize
        and  shiftDirection = shiftDirection
        and  multKind       = multKind
        and  fpUnary        = fpUnary
        and  fpBinary       = fpBinary
        and  atomicOp       = atomicOp
   end
end;
