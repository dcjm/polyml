(*
    Copyright David C. J. Matthews 2021-3

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

functor Arm64ICode(

    structure Arm64Code: ARM64PREASSEMBLY

): ARM64ICODE =

struct
    open Arm64Code

    open Address
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
    datatype reg = GenReg of xReg | FPReg of vReg

    datatype callKind = Recursive | ConstantCode of machineWord | FullCall

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

        (* Unary floating point.  This includes conversions between float and double. *)
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
  
    type iCodeAbstract = (preg, pregOrZero, preg) arm64ICode and basicBlockAbstract = (preg, pregOrZero, preg) basicBlock
    and  iCodeConcrete = (xReg, xReg, vReg) arm64ICode and basicBlockConcrete = (xReg, xReg, vReg) basicBlock

    (* Return the list of blocks that are the immediate successor of this. *)
    fun successorBlocks(Unconditional l) = [l]
    |   successorBlocks(Conditional{trueJump, falseJump, ...}) = [trueJump, falseJump]
    |   successorBlocks ExitCode = []
    |   successorBlocks(IndexedBr cases) = cases
    |   successorBlocks(SetHandler{handler, continue, ...}) = [handler, continue]
        (* We only need "handler" in SetHandler because we may have a handler that is never actually jumped to. *)
    |   successorBlocks(UnconditionalHandle handler) = [handler]
    |   successorBlocks(ConditionalHandle{handler, continue, ...}) = [handler, continue]

    local
        fun printCC(CcRef ccRef, stream) = stream ("CC" ^ Int.toString ccRef)

        fun printStackLoc(StackLoc{size, rno}, stream) =
            (stream "S"; stream(Int.toString rno); stream "("; stream(Int.toString size); stream ")")
        
        fun regRepr(XReg w) = "X" ^ Int.toString(Word8.toInt w)
        |   regRepr XZero = "XZ"
        |   regRepr XSP = "SP"
        
        and vRegRepr(VReg v) = "V" ^ Int.toString(Word8.toInt v)

        fun arithRepr OpSize64 = "64"
        |   arithRepr OpSize32 = "32"

        fun printLoadType(Load64, stream) = stream "64"
        |   printLoadType(Load32, stream) = stream "32"
        |   printLoadType(Load16, stream) = stream "16"
        |   printLoadType(Load8, stream) = stream "8"

        
        fun printSaves({ gcUpdate, noUpdate }, stream, printReg) =
        let
            fun printRegs [] = ()
            |   printRegs [areg] = printReg areg
            |   printRegs(areg::more) =
                    (printReg areg; stream ","; printRegs more)
        in
            stream"{ gcUpdate= "; printRegs gcUpdate; stream ", noUpdate= "; printRegs noUpdate; stream " }"
        end

        fun printArg(ArgInReg reg, _, printReg) = printReg reg
        |   printArg(ArgOnStack{wordOffset, container, field, ...}, stream, _) =
            (
                printStackLoc(container, stream); stream " + ";
                stream(Int.toString field);
                stream " (";  stream(Int.toString wordOffset); stream ")"
            )

        fun printShift(ShiftLSL w, stream) = stream(" LSL " ^ Word8.toString w)
        |   printShift(ShiftLSR w, stream) = stream(" LSR " ^ Word8.toString w)
        |   printShift(ShiftASR w, stream) = stream(" ASR " ^ Word8.toString w)
        |   printShift(ShiftNone, _) = ()

        fun printFloatSize(Float32, stream) = stream "F"
        |   printFloatSize(Double64, stream) = stream "D"

        fun printICode {stream, printGenReg, ...} (MoveRegister{ source, dest }: ('a, 'b, 'c) arm64ICode) =
            (
                stream "\tMove\t";
                printGenReg source;
                stream " => ";
                printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (LoadNonAddressConstant{ source, dest }) =
            (
                stream "\tLoadNonAddress\t"; stream(Word64.toString source);
                stream " => ";
                printGenReg dest
            )

        |   printICode {stream, printFPReg, ...} (LoadFPConstant{ source, dest, floatSize }) =
            (
                stream "\tLoadFPConstant"; printFloatSize(floatSize, stream); stream "\t";
                stream(Word64.toString source); stream " => "; printFPReg dest
            )

        |   printICode {stream, printGenReg, ...} (LoadAddressConstant{ source, dest }) =
            (
                stream "\tLoadAddress\t"; stream(Address.stringOfWord source);
                stream " => ";
                printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (LoadWithConstantOffset{ base, dest, byteOffset, loadType }) =
            (
                stream "\tLoadConstOffset"; printLoadType(loadType, stream); stream "\t[";
                printGenReg base; stream "]+";
                stream(Int.toString byteOffset);
                stream " => ";
                printGenReg dest
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (LoadFPWithConstantOffset{ base, dest, byteOffset, floatSize }) =
            (
                stream "\tLoadConstOffset"; printFloatSize(floatSize, stream); stream "\t[";
                printGenReg base; stream "]+";
                stream(Int.toString byteOffset);
                stream " => ";
                printFPReg dest
            )

        |   printICode {stream, printGenReg, ...} (LoadWithIndexedOffset{ base, dest, index, loadType, signExtendIndex }) =
            (
                stream "\tLoadIndexed"; printLoadType(loadType, stream); stream "\t[";
                printGenReg base; stream "+"; printGenReg index; if signExtendIndex then stream " SX" else ();
                stream "] => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (LoadFPWithIndexedOffset{ base, dest, index, floatSize, signExtendIndex }) =
            (
                stream "\tLoadIndexed"; printFloatSize(floatSize, stream); stream "\t[";
                printGenReg base; stream "+"; printGenReg index; if signExtendIndex then stream " SX" else ();
                stream "] => "; printFPReg dest
            )

        |   printICode {stream, printGenReg, ...} (GetThreadId { dest}) =
                ( stream "\tGetThreadId\t"; stream " => "; printGenReg dest )

        |   printICode {stream, printGenReg, ...} (ObjectIndexAddressToAbsolute{ source, dest }) =
            (
                stream "\tObjectAddrToAbs\t";
                printGenReg source; stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (AbsoluteToObjectIndex{ source, dest }) =
            (
                stream "\tAbsToObjectAddr\t";
                printGenReg source; stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (AllocateMemoryFixed{bytesRequired, dest, saveRegs}) =
            (
                stream "\tAllocateMemory\t";
                stream(Word64.fmt StringCvt.DEC bytesRequired); stream " => ";
                printGenReg dest; stream " save="; printSaves(saveRegs, stream, printGenReg)
            )

        |   printICode {stream, printGenReg, ...} (AllocateMemoryVariable{size, dest, saveRegs}) =
            (
                stream "\tAllocateMemory\t";
                stream "s="; printGenReg(size);
                stream " => "; printGenReg dest;
                stream " save="; printSaves(saveRegs, stream, printGenReg)
            )

        |   printICode {stream, printGenReg, ...} (InitialiseMem{size, addr, init}) =
            (
                stream "\tInitialiseMem\t";
                stream "s="; printGenReg(size);
                stream ",i="; printGenReg(init);
                stream ",a="; printGenReg(addr)
            )

        |   printICode {stream, ...} BeginLoop = stream "\tBeginLoop"

        |   printICode {stream, printGenReg, ...} (JumpLoop{regArgs, stackArgs, checkInterrupt, ... }) =
            (
                stream "\tJumpLoop\t";
                List.app(fn {src, dst} => (printGenReg(dst); stream "="; printArg(src, stream, printGenReg); stream " ")) regArgs;
                List.app(
                    fn {src, wordOffset, stackloc} =>
                        (printStackLoc(stackloc, stream); stream("(sp" ^ Int.toString wordOffset); stream ")="; printArg(src, stream, printGenReg); stream " ")
                    ) stackArgs;
                case checkInterrupt of
                    NONE => ()
                |   SOME saveRegs => (stream " Check:save="; printSaves(saveRegs, stream, printGenReg))
            )

        |   printICode {stream, printGenReg, ...} (StoreWithConstantOffset{ base, source, byteOffset, loadType }) =
            (
                stream "\tStoreConstOffset"; printLoadType(loadType, stream); stream "\t";
                printGenReg source; stream " => [";
                printGenReg base; stream "+";
                stream(Int.toString byteOffset); stream "]"
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (StoreFPWithConstantOffset{ base, source, byteOffset, floatSize }) =
            (
                stream "\tStoreConstOffset"; printFloatSize(floatSize, stream); stream "\t";
                printFPReg source; stream " => [";
                printGenReg base; stream "+";
                stream(Int.toString byteOffset); stream "]"
            )

        |   printICode {stream, printGenReg, ...} (StoreWithIndexedOffset{ base, source, index, loadType, signExtendIndex }) =
            (
                stream "\tStoreIndexed"; printLoadType(loadType, stream); stream "\t";
                printGenReg source; stream " => [";
                printGenReg base; stream "+"; printGenReg index;
                if signExtendIndex then stream " SX" else (); stream "]"
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (StoreFPWithIndexedOffset{ base, source, index, floatSize, signExtendIndex }) =
            (
                stream "\tStoreIndexed"; printFloatSize(floatSize, stream); stream "\t";
                printFPReg source; stream " => [";
                printGenReg base; stream "+"; printGenReg index;
                if signExtendIndex then stream " SX" else (); stream "]"
            )

        |   printICode {stream, printGenReg, printOptGenReg, ...} (AddSubImmediate{ source, dest, ccRef, immed, isAdd, length }) =
            (
                stream (if isAdd then "\tAddImmediate" else "\tSubImmediate"); stream(arithRepr length);
                stream "\t"; printGenReg source; stream ",0x"; stream(Word.toString immed);
                stream " => "; printOptGenReg dest;
                case ccRef of NONE => () | SOME cc => (stream ", "; printCC(cc, stream))
            )

        |   printICode {stream, printGenReg, printOptGenReg, ...} (AddSubRegister{ base, shifted, dest, ccRef, isAdd, length, shift }) =
            (
                stream (if isAdd then "\tAddRegister" else "\tSubRegister"); stream(arithRepr length);
                stream "\t"; printGenReg base; stream ", ";
                printGenReg(shifted); printShift(shift, stream);
                stream " => "; printOptGenReg dest;
                case ccRef of NONE => () | SOME cc => (stream ", "; printCC(cc, stream))
            )

        |   printICode {stream, printGenReg, printOptGenReg, ...} (LogicalImmediate{ source, dest, ccRef, immed, logOp, length }) =
            (
                stream (case logOp of LogAnd => "\tAndImmediate" | LogOr => "\tOrImmediate" | LogXor => "\tXorImmediate");
                stream(arithRepr length);
                stream "\t"; printGenReg source; stream ",0x"; stream(Word64.toString immed);
                stream " => "; printOptGenReg dest;
                case ccRef of NONE => () | SOME cc => (stream ", "; printCC(cc, stream))
            )

        |   printICode {stream, printGenReg, printOptGenReg, ...} (LogicalRegister{ base, shifted, dest, ccRef, logOp, length, shift }) =
            (
                stream (case logOp of LogAnd => "\tAndRegister"  | LogOr => "\tOrRegister" | LogXor => "\tXorRegister");
                stream(arithRepr length);
                stream "\t"; printGenReg base; stream ", ";
                printGenReg(shifted); printShift(shift, stream);
                stream " => "; printOptGenReg dest;
                case ccRef of NONE => () | SOME cc => (stream ", "; printCC(cc, stream))
            )

        |   printICode {stream, printGenReg, ...} (ShiftRegister{ direction, dest, source, shift, opSize }) =
            (
                stream (
                    case direction of
                        ShiftLeft => "\tShiftLeft"
                    |   ShiftRightLogical => "\tShiftRightLog"
                    |   ShiftRightArithmetic => "\tShiftRightArith");
                stream(arithRepr opSize);
                stream "\t"; printGenReg source; stream " by ";
                printGenReg(shift);
                stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (CountLeadingZeroBits{ dest, source, opSize }) =
            (
                stream "\tCountLeadingZeroBits";
                stream(arithRepr opSize);
                stream "\t"; printGenReg source;
                stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, printOptGenReg, ...} (Multiplication{ kind, dest, sourceA, sourceM, sourceN }) =
            (
                stream (
                    case kind of
                        MultAdd32 => "\tMultAdd32\t"
                    |   MultSub32 => "\tMultSub32\t"
                    |   MultAdd64 => "\tMultAdd64\t"
                    |   MultSub64 => "\tMultSub64\t"
                    |   SignedMultAddLong => "\tSignedMultAddLong\t"
                    |   SignedMultHigh => "\tSignedMultHigh\t");
                printGenReg(sourceM); stream " * ";
                printGenReg(sourceN);
                stream " +/- "; printOptGenReg sourceA;
                stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (Division{ isSigned, dest, dividend, divisor, opSize }) =
            (
                stream (if isSigned then "\tSignedDivide" else "\tUnsignedDivide");
                stream(arithRepr opSize);
                stream "\t"; printGenReg(dividend); stream " by ";
                printGenReg(divisor);
                stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (BeginFunction{ regArgs, stackArgs, fpRegArgs }) =
            (
                stream "\tBeginFunction\t";
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printGenReg(arg); stream " ")) regArgs;
                List.app(fn (arg, r) => (stream(vRegRepr r); stream "="; printFPReg(arg); stream " ")) fpRegArgs;
                List.app(fn s => printStackLoc(s, stream)) stackArgs
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (FunctionCall{callKind, regArgs, stackArgs, dests,
                        fpRegArgs, fpDests, saveRegs, containers}) =
            (
                stream "\tFunctionCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ();
                stream "(";
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream, printGenReg); stream " ")) regArgs;
                List.app(fn (arg, r) => (stream(vRegRepr r); stream "="; printFPReg(arg); stream " ")) fpRegArgs;
                List.app(fn arg => (stream "p="; printArg(arg, stream, printGenReg); stream " ")) stackArgs;
                stream ") ";
                List.app(fn (pr, r) => (stream(regRepr r); stream "=>"; printGenReg pr; stream " ")) dests;
                List.app(fn (pr, r) => (stream(vRegRepr r); stream "=>"; printFPReg pr; stream " ")) fpDests;
                stream " save="; printSaves(saveRegs, stream, printGenReg);
                if null containers then ()
                else (stream " containers="; List.app (fn c => (printStackLoc(c, stream); stream " ")) containers)
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, currStackSize, fpRegArgs, ...}) =
            (
                stream "\tTailCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ();
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream, printGenReg); stream " ")) regArgs;
                List.app(fn (arg, r) => (stream(vRegRepr r); stream "="; printFPReg(arg); stream " ")) fpRegArgs;
                List.app(fn {src, stack} => (stream (Int.toString stack); stream "<="; printArg(src, stream, printGenReg); stream " ")) stackArgs;
                stream "adjust="; stream(Int.toString stackAdjust);
                stream " stackSize="; stream(Int.toString currStackSize)
            )

        |   printICode {stream, printGenReg, printFPReg, ...}
                (ReturnResultFromFunction{ results, fpResults, returnReg, numStackArgs }) =
            (
                stream "\tReturnFromFunction\t"; printGenReg(returnReg); stream "with ";
                List.app(fn (reg, r) => (stream(regRepr r); stream "=>"; printGenReg reg; stream " ")) results;
                List.app(fn (reg, r) => (stream(vRegRepr r); stream "=>"; printFPReg reg; stream " ")) fpResults;
                stream("," ^ Int.toString numStackArgs)
            )

        |   printICode {stream, printGenReg, ...} (RaiseExceptionPacket{ packetReg }) =
            ( stream "\tRaiseException\t"; printGenReg(packetReg) )

        |   printICode {stream, printGenReg, ...} (PushToStack{ source, copies, container }) =
            (
                stream "\tPushToStack\t"; printGenReg source;
                if copies > 1 then (stream " * "; stream(Int.toString copies)) else ();
                stream " => "; printStackLoc(container, stream)
            )

        |   printICode {stream, printGenReg, ...} (LoadStack{ dest, wordOffset, container, field }) =
            (
                stream "\tLoadStack\t";
                printStackLoc(container, stream); stream " + ";
                stream(Int.toString field);
                stream " (";  stream(Int.toString wordOffset); stream ")";
                stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (StoreToStack{ source, container, field, stackOffset }) =
            (
                stream "\tStoreToStack\t"; printGenReg source;
                stream " => "; printStackLoc(container, stream); stream "+";
                stream (Int.toString field); stream "(";
                stream(Int.toString stackOffset); stream ")"
            )

        |   printICode {stream, printGenReg, ...} (ContainerAddress{ dest, container, stackOffset }) =
            (
                stream "\tContainerAddress\t";
                stream "@"; printStackLoc(container, stream);
                stream " (";  stream(Int.toString stackOffset); stream ") => ";
                printGenReg dest
            )

        |   printICode {stream, ...} (ResetStackPtr{ numWords }) =
            ( stream "\tResetStackPtr\t"; stream(Int.toString numWords) )

        |   printICode {stream, printGenReg, ...} (TagValue{ source, dest, isSigned, opSize }) =
            (
                stream "\tTag"; stream(if isSigned then "Signed" else "Unsigned");
                stream(arithRepr opSize); stream "\t";
                printGenReg source; stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (UntagValue{ source, dest, isSigned, opSize }) =
            (
                stream "\tUntag"; stream(if isSigned then "Signed" else "Unsigned");
                stream(arithRepr opSize); stream "\t";
                printGenReg source; stream " => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (BoxLarge{source, dest, saveRegs}) =
            (
                stream "\tBoxLarge\t";
                printGenReg source;
                stream " => ";
                printGenReg dest;
                stream " save="; printSaves(saveRegs, stream, printGenReg)
            )

        |   printICode {stream, printGenReg, ...} (UnboxLarge{source, dest}) =
            (
                stream "\tUnboxLarge\t";
                printGenReg source;
                stream " => ";
                printGenReg dest
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (BoxTagFloat{floatSize, source, dest, saveRegs}) =
            (
                stream "\tBoxTagFloat"; printFloatSize(floatSize, stream); stream "\t";
                printFPReg source;
                stream " => ";
                printGenReg dest;
                stream " save="; printSaves(saveRegs, stream, printGenReg)
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (UnboxTagFloat{floatSize, source, dest}) =
            (
                stream "\tUnboxTagFloat"; printFloatSize(floatSize, stream); stream "\t";
                printGenReg source;
                stream " => ";
                printFPReg dest
            )

        |   printICode {stream, printGenReg, ...} (LoadAcquire{ base, dest, loadType }) =
            (
                stream "\tLoadAcquire"; printLoadType(loadType, stream); stream "\t[";
                printGenReg base; stream "] => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (StoreRelease{ base, source, loadType }) =
            (
                stream "\tStoreRelease"; printLoadType(loadType, stream); stream "\t";
                printGenReg source; stream " => [";
                printGenReg base; stream "]"
            )

        |   printICode {stream, printGenReg, ...} (BitFieldShift{ source, dest, isSigned, length, immr, imms }) =
            (
                stream "\tBitShift"; stream(if isSigned then "Signed" else "Unsigned"); stream(arithRepr length); stream "\t";
                printGenReg source;
                stream " => "; printGenReg dest;
                stream " immr="; stream(Word.fmt StringCvt.DEC immr);
                stream " imms="; stream(Word.fmt StringCvt.DEC imms)
            )

        |   printICode {stream, printGenReg, ...} (BitFieldInsert{ source, dest, destAsSource, length, immr, imms }) =
            (
                stream "\tBitInsert";  stream(arithRepr length); stream "\t";
                printGenReg source; stream " with "; printGenReg destAsSource;
                stream " => "; printGenReg dest;
                stream " immr="; stream(Word.fmt StringCvt.DEC immr);
                stream " imms="; stream(Word.fmt StringCvt.DEC imms)
            )

        |   printICode {stream, printGenReg, ...} (IndexedCaseOperation{testReg}) =
                ( stream "\tIndexedCase\t"; printGenReg testReg )

        |   printICode {stream, ...} PushExceptionHandler = stream "\tPushExcHandler"

        |   printICode {stream, ...} PopExceptionHandler = stream "\tPopExcHandler"

        |   printICode {stream, printGenReg, ...} (BeginHandler{packetReg}) =
            (
                stream "\tBeginHandler\t";
                printGenReg packetReg
            )

        |   printICode {stream, printGenReg, ...} (CompareByteVectors{vec1Addr, vec2Addr, length, ccRef, ...}) =
            (
                stream "\tCompareByteVectors\t";
                printGenReg(vec1Addr); stream ",";
                printGenReg(vec2Addr); stream ",";
                printGenReg(length);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode {stream, printGenReg, ...} (BlockMove{srcAddr, destAddr, length, isByteMove}) =
            (
                stream(if isByteMove then "\tBlockByteMove\t" else "\tBlockWordMove\t");
                stream "src="; printGenReg(srcAddr);
                stream ",dest="; printGenReg(destAddr);
                stream ",len="; printGenReg(length)
            )

        |   printICode {stream, printGenReg, printOptGenReg, ...} (AddSubXSP{ source, dest, isAdd }) =
            (
                stream(if isAdd then "\tAdd\t" else "\tSubtract\t");
                printGenReg source; stream " XSP => ";
                printOptGenReg dest
            )

        |   printICode {stream, printGenReg, ...} (TouchValue{ source }) =
            ( stream "\tTouchValue\t"; printGenReg source )

        |   printICode {stream, printGenReg, ...} (LoadAcquireExclusive{ base, dest }) =
            (
                stream "\tLoadExclusive\t["; printGenReg base;
                stream "] => "; printGenReg dest
            )

        |   printICode {stream, printGenReg, printOptGenReg, ...} (StoreReleaseExclusive{ base, source, result }) =
            (
                stream "\tStoreExclusive\t";
                printOptGenReg source; stream " => [";
                printGenReg base; stream "] result => ";
                printGenReg result
            )

        |   printICode {stream, ...} MemoryBarrier = stream "\tMemoryBarrier"

        |   printICode {stream, printGenReg, printFPReg, ...} (ConvertIntToFloat{ source, dest, srcSize, destSize}) =
            (
                stream "\tConvert"; stream(arithRepr srcSize); stream "To";
                printFloatSize(destSize, stream); stream "\t";
                printGenReg source; stream " => "; printFPReg dest
            )

        |   printICode {stream, printGenReg, printFPReg, ...} (ConvertFloatToInt{ source, dest, srcSize, destSize, rounding}) =
            let
                open IEEEReal
            in
                stream "\tConvert"; printFloatSize(srcSize, stream); stream "To";
                stream(arithRepr destSize); stream "\t";
                printFPReg source; stream " => "; printGenReg dest;
                stream(
                    case rounding of
                        TO_NEAREST => " rounding"
                    |   TO_NEGINF => " rounding down"
                    |   TO_POSINF => " rounding up"
                    |   TO_ZERO => " truncating"
                )
            end

        |   printICode {stream, printFPReg, ...} (UnaryFloatingPt{ source, dest, fpOp}) =
            (
                stream(
                    case fpOp of
                        NegFloat => "\tNegFloat\t"
                    |   NegDouble => "\tNegDouble\t"
                    |   AbsFloat => "\tAbsFloat\t"
                    |   AbsDouble => "\tAbsDouble\t"
                    |   ConvFloatToDble => "\tFloatToDble\t"
                    |   ConvDbleToFloat => "\tDbleToFloat\t"
                    |   MoveDouble => "\tMoveDouble\t"
                    |   MoveFloat => "\tMoveFloat\t"
                );
                printFPReg source; stream " => "; printFPReg dest
            )

        |   printICode {stream, printFPReg, ...} (BinaryFloatingPoint{ arg1, arg2, dest, fpOp, opSize}) =
            (
                stream(
                    case fpOp of
                        MultiplyFP => "\tMultiply"
                    |   DivideFP => "\tDivide"
                    |   AddFP => "\tAdd"
                    |   SubtractFP => "\tSubtract"
                );
                printFloatSize(opSize, stream); stream "\t";
                printFPReg arg1; stream ", "; printFPReg arg2;
                stream " => "; printFPReg dest
            )

        |   printICode {stream, printFPReg, ...} (CompareFloatingPoint{ arg1, arg2, opSize, ccRef}) =
            (
                stream "\tCompare"; printFloatSize(opSize, stream); stream "\t";
                printFPReg arg1; stream ", "; printFPReg arg2;
                stream ", "; printCC(ccRef, stream)
            )

        |   printICode {stream, ...} CPUYield = stream "\tCpuYield"

        |   printICode {stream, printGenReg, printOptGenReg, ...} (AtomicOperation{ base, source, dest, atOp }) =
            (
                case atOp of
                    LoadAddAL => stream "\tLoadAddAL\t"
                |   LoadUmaxAL => stream "\tLoadUmaxAL\t"
                |   SwapAL => stream "\tSwapAL\t"
                |   LoadAddAcquire => stream "\tLoadAddAcquire\t"
                |   LoadUMaxAcquire => stream "\tLoadUMaxAcquire\t"
                |   SwapRelease => stream "\tSwapRelease\t";
                printOptGenReg source; stream ",["; printGenReg base; stream "] => "; printOptGenReg dest
            )

        and printCondition(cond,  stream)  = stream(condToString cond)

        (* Print a basic block. *)
        fun printBlock {stream, printGenReg, printOptGenReg, printFPReg} (blockNo, BasicBlock{block, flow, ...}) =
        (
            (* Put a label on all but the first. *)
            if blockNo <> 0 then stream("L" ^ Int.toString blockNo ^ ":") else ();
            List.app (fn icode =>
                (printICode {stream=stream, printGenReg=printGenReg, printOptGenReg=printOptGenReg, printFPReg=printFPReg} (icode); stream "\n")) block;
            case flow of
                Unconditional l => stream("\tJump\tL" ^ Int.toString l ^ "\n")
            |   Conditional {condition, trueJump, falseJump, ccRef, ...} =>
                (
                    stream "\tJump"; printCondition(condition, stream);
                    stream "\t"; printCC(ccRef, stream);
                    stream " L"; stream (Int.toString trueJump);
                    stream " else L"; stream (Int.toString falseJump); stream "\n"
                )
            |   ExitCode => ()
            |   IndexedBr _ => ()
            |   SetHandler{handler, continue} =>
                    stream(concat["\tSetHandler\tH", Int.toString handler, "\n",
                                  "\tJump\tL", Int.toString continue, "\n"])
            |   UnconditionalHandle handler => stream("\tJump\tH" ^ Int.toString handler ^ "\n")
            |   ConditionalHandle{handler, continue} =>
                    stream(concat["\tJump\tL", Int.toString continue, " or H", Int.toString handler, "\n"])
        )
    in
        fun printPReg stream (PReg i) = stream("R" ^ Int.toString i)
        fun printOptPReg stream ZeroReg = stream "Zero"
        |   printOptPReg stream (SomeReg reg) = printPReg stream reg
        
        fun printXReg stream (XReg w) = stream("X" ^ Int.toString(Word8.toInt w))
        |   printXReg stream XZero = stream "XZ"
        |   printXReg stream XSP = stream "XSP"

        fun printVReg stream (VReg w) = stream("V" ^ Int.toString(Word8.toInt w))

        fun printICodeAbstract(blockVec, stream) =
            Vector.appi(printBlock{stream=stream, printGenReg=printPReg stream, printOptGenReg=printOptPReg stream, printFPReg=printPReg stream}) blockVec
        
        and printICodeConcrete(blockVec, stream) =
            Vector.appi(printBlock{stream=stream, printGenReg=printXReg stream, printOptGenReg=printXReg stream, printFPReg=printVReg stream}) blockVec
    end

    (* Only certain bit patterns are allowed in a logical immediate instruction
       but the encoding is complex so it's easiest to inherit the test from
       the assembler layer. *)
    local
        fun optow OpSize32 = WordSize32 | optow OpSize64 = WordSize64
    in
        fun isEncodableBitPattern(v, w) = Arm64Code.isEncodableBitPattern(v, optow w)
    end

    (* This generates a  BitField instruction with the appropriate values for immr and imms. *)
    fun shiftConstant{ direction, dest, source, shift, opSize } =
    let
        val (isSigned, immr, imms) =
            case (direction, opSize) of
                (ShiftLeft, OpSize64) => (false, Word.~ shift mod 0w64, 0w64-0w1-shift)
            |   (ShiftLeft, OpSize32) => (false, Word.~ shift mod 0w32, 0w32-0w1-shift)
            |   (ShiftRightLogical, OpSize64) => (false, shift, 0wx3f)
            |   (ShiftRightLogical, OpSize32) => (false, shift, 0wx1f)
            |   (ShiftRightArithmetic, OpSize64) => (true, shift, 0wx3f)
            |   (ShiftRightArithmetic, OpSize32) => (true, shift, 0wx1f)
    in
        BitFieldShift{ source=source, dest=dest, isSigned=isSigned, length=opSize, immr=immr, imms=imms }
    end

    structure Sharing =
    struct
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

