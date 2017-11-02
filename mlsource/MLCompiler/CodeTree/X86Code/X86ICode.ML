(*
    Copyright David C. J. Matthews 2016-17

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

functor X86ICode(

    structure X86CODE: X86CODESIG

): ICodeSig =

struct
    open X86CODE

    open Address

    datatype fpMode = FPModeSSE2 | FPModeX87

    (* For the moment use SSE2 only on X86/64. Not all 32-bit processors support SSE2. *)
    val fpMode: fpMode = if isX64 then FPModeSSE2 else FPModeX87

    datatype preg = PReg of int (* A pseudo-register - an abstract register. *)

    (* A location on the stack.  May be more than word if this is a container or a handler entry. *)
    datatype stackLocn = StackLoc of {size: int, rno: int }
    
    (* This combines pregKind and stackLocn.  *)
    datatype regProperty =
        RegPropGeneral      (* A general register. *)
    |   RegPropUntagged     (* An untagged general register. *)
    |   RegPropStack of int (* A stack location or container. *)
    |   RegPropCacheTagged
    |   RegPropCacheUntagged
 
    datatype argument =
        RegisterArgument of preg
    |   AddressConstant of machineWord (* A constant that is an address. *)
    |   IntegerConstant of LargeInt.int (* A non-address constant.  Will usually be shifted and tagged. *)
    |   MemoryLocation of { base: preg, offset: int, index: memoryIndex, cache: preg option } (* A memory location.  Could be the stack. *)
        (* Offset on the stack.  The container is the stack location identifier, the field is an
           offset in a container.  cache is an optional cache register. *)
    |   StackLocation of { wordOffset: int, container: stackLocn, field: int, cache: preg option }
        (* Address of a container. *)
    |   ContainerAddr of { container: stackLocn, stackOffset: int }

    and memoryIndex =
        NoMemIndex | MemIndex1 of preg | MemIndex2 of preg | MemIndex4 of preg | MemIndex8 of preg

    (* Kinds of moves.
       MoveWord - Move a whole word - 64-bits in 64-bit mode, 32-bits in 32-bit mode.
       MoveByte - When loading, load a byte and zero extend.
       Move16Bit - Used for C-memory loads and stores.  Zero extends on load.
       Move32Bit - Used for C-memory loads and stores in 64-bit mode.
       MoveFloat - Load and store a single-precision value
       MoveDouble - Load and store a double-precision value. *)
    datatype moveKind =
        MoveWord | MoveByte | Move16Bit | Move32Bit | MoveFloat | MoveDouble

    (* The reference to a condition code. *)
    and ccRef = CcRef of int

    datatype boxKind = BoxLargeWord | BoxSSE2 | BoxX87

    datatype x86ICode =
        (* Move a value into a register. *)
        LoadArgument of { source: argument, dest: preg, kind: moveKind }
        
        (* Store a value into memory.  The source will usually be a register but could be
           a constant depending on the value.  If isMutable is true we're assigning to
           a ref and we need to flush the memory cache. *)
    |   StoreArgument of
            { source: argument, base: preg, offset: int, index: memoryIndex, kind: moveKind, isMutable: bool }

        (* Load an entry from the "memory registers".  Used just for ThreadSelf. *)
    |   LoadMemReg of { offset: int, dest: preg }

        (* Start of function.  Set the register arguments.  stackArgs is the list of
           stack arguments.  The last entry is the return address. *)
    |   BeginFunction of { regArgs: (preg * reg) list, stackArgs: stackLocn list }

        (* Call a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through rdx which has been loaded
           as one of the argument registers.  The result is stored in the destination register. *)
    |   FunctionCall of
            { callKind: callKinds, regArgs: (argument * reg) list,
              stackArgs: argument list, dest: preg}

        (* Jump to a tail-recursive function.  This is similar to FunctionCall
           but complicated for stack arguments because the stack and the return
           address need to be overwritten.
           stackAdjust is the number of words to remove (positive) or add
           (negative) to the stack before the call.
           currStackSize contains the number of items currently on the stack. *)
    |   TailRecursiveCall of
            { callKind: callKinds, regArgs: (argument * reg) list,
              stackArgs: {src: argument, stack: int} list,
              stackAdjust: int, currStackSize: int, workReg: preg }

        (* Allocate a fixed sized piece of memory.  The size is the number of words
           required.  This sets the length word including the flags bits.
           saveRegs is the list of registers that need to be saved if we
           need to do a garbage collection. *)
    |   AllocateMemoryOperation of { size: int, flags: Word8.word, dest: preg, saveRegs: preg list }

        (* Allocate a piece of memory whose size is not known at compile-time.  The size
           argument is the number of words. *)
    |   AllocateMemoryVariable of { size: preg, dest: preg, saveRegs: preg list }

        (* Initialise a piece of memory.  N.B. The size is an untagged value containing
           the number of words.  This uses REP STOSL/Q so addr must be rdi, size must be
           rcx and init must be rax. *)
    |   InitialiseMem of { size: preg, addr: preg, init: preg }

        (* Signal that a tuple has been fully initialised.  Really a check in the
           low-level code-generator. *)
    |   InitialisationComplete

        (* Set up the registers for a jump back to the start of a loop. *)
    |   JumpLoop of
            { regArgs: (argument * preg) list, stackArgs: (argument * int * stackLocn) list,
              checkInterrupt: preg list option, workReg: preg option }

        (* Raise an exception.  The packet is always loaded into rax. *)
    |   RaiseExceptionPacket of { packetReg: preg }

        (* Reserve a contiguous area on the stack to receive a result tuple. *)
    |   ReserveContainer of { size: int, container: stackLocn }

        (* Indexed case. *)
    |   IndexedCaseOperation of { testReg: preg, workReg: preg }

        (* Lock a mutable cell by turning off the mutable bit. *)
    |   LockMutable of { addr: preg }

        (* Compare two word values.  The first argument must be a register. *)
    |   WordComparison of { arg1: preg, arg2: argument, ccRef: ccRef }
    
        (* Exception handling.  - Set up an exception handler. *)
    |   PushExceptionHandler of { workReg: preg }

        (* End of a handled section.  Restore the previous handler. *)
    |   PopExceptionHandler of { workReg: preg }

        (* Marks the start of a handler.  This sets the stack pointer and
           restores the old handler.  Sets the exception packet register. *) 
    |   BeginHandler of { packetReg: preg, workReg: preg }

        (* Return from the function. *)
    |   ReturnResultFromFunction of { resultReg: preg, numStackArgs: int }
    
        (* Arithmetic or logical operation.  These can set the condition codes. *)
    |   ArithmeticFunction of
            { oper: arithOp, resultReg: preg, operand1: preg, operand2: argument, ccRef: ccRef }

        (* Test the tag bit of a word.  Sets the Zero bit if the value is an address i.e. untagged. *)
    |   TestTagBit of { arg: argument, ccRef: ccRef }

        (* Push a value to the stack.  Added during translation phase. *)
    |   PushValue of { arg: argument, container: stackLocn }

        (* Copy a value to a cache register.  LoadArgument could be used for this
           but it may be better to keep it separate. *)
    |   CopyToCache of { source: preg, dest: preg, kind: moveKind }

        (* Remove items from the stack.  Added during translation phase. *)
    |   ResetStackPtr of { numWords: int, preserveCC: bool }

        (* Store a value into the stack. *)
    |   StoreToStack of { source: argument, container: stackLocn, field: int, stackOffset: int }

        (* Tag a value by shifting and setting the tag bit. *)
    |   TagValue of { source: preg, dest: preg, isSigned: bool }

        (* Shift a value to remove the tag bit.  The cache is used if this is untagging a
           value that has previously been tagged. *)
    |   UntagValue of { source: preg, dest: preg, isSigned: bool, cache: preg option }

        (* This provides the LEA instruction which can be used for various sorts of arithmetic.
           The base register is optional in this case. *)
    |   LoadEffectiveAddress of { base: preg option, offset: int, index: memoryIndex, dest: preg }

        (* Shift a word by an amount that can either be a constant or a register. *)
    |   ShiftOperation of { shift: shiftType, resultReg: preg, operand: preg, shiftAmount: argument, ccRef: ccRef }

        (* Multiplication.  We can use signed multiplication for both fixed precision and word (unsigned)
           multiplication.  There are various forms of the instruction including a three-operand
           version. *)
    |   Multiplication of { resultReg: preg, operand1: preg, operand2: argument, ccRef: ccRef }

        (* Division.  This takes a register pair, always RDX:RAX, divides it by the operand register and
           puts the quotient in RAX and remainder in RDX.  At the abstract level we represent all of
           these by pRegs.  The divisor can be either a register or a memory location. *)
    |   Division of { isSigned: bool, dividend: preg, divisor: argument, quotient: preg, remainder: preg }

        (* Atomic exchange and addition.   This is executed with a lock prefix and is used
           for atomic increment and decrement for mutexes.
           Before the operation the source contains an increment.  After the operation
           the source contains the old value of the destination and the destination
           has been updated with its old value added to the increment.
           The destination is actually the word pointed at by "base". *)
    |   AtomicExchangeAndAdd of { base: preg, source: preg }

        (* Create a "box" of a single-word "byte" cell and store the source into it.
           This can be implemented using AllocateMemoryOperation but the idea is to
           allow the transform layer to recognise when a value is being boxed and
           then unboxed and remove unnecessary allocation. *)
    |   BoxValue of { boxKind: boxKind, source: preg, dest: preg, saveRegs: preg list }

        (* Compare two vectors of bytes and set the condition code on the result.
           In general vec1Addr and vec2Addr will be pointers inside memory cells
           so have to be untagged registers. *)
    |   CompareByteVectors of
            { vec1Addr: preg, vec2Addr: preg, length: preg, ccRef: ccRef }

        (* Move a block of bytes (isByteMove true) or words (isByteMove false).  The length is the
           number of items (bytes or words) to move. *)
    |   BlockMove of { srcAddr: preg, destAddr: preg, length: preg, isByteMove: bool }

        (* Floating point comparison. *)
    |   X87Compare of { arg1: preg, arg2: argument, ccRef: ccRef }

        (* Floating point comparison. *)
    |   SSE2Compare of { arg1: preg, arg2: argument, ccRef: ccRef }

        (* The X87 FP unit does not generate condition codes directly.  We have to
           load the cc into RAX and test it there. *)
    |   X87FPGetCondition of { ccRef: ccRef, dest: preg }

        (* Binary floating point operations on the X87. *)
    |   X87FPArith of { opc: fpOps, resultReg: preg, arg1: preg, arg2: argument }

        (* Floating point operations: negate and set sign positive. *)
    |   X87FPUnaryOps of { fpOp: fpUnaryOps, dest: preg, source: preg }

        (* Load a fixed point value as a floating point value. *)
    |   X87Float of { dest: preg, source: argument }

        (* Load a fixed point value as a floating point value. *)
    |   SSE2Float of { dest: preg, source: argument }

        (* Binary floating point operations using SSE2 instructions. *)
    |   SSE2FPArith of { opc: sse2Operations, resultReg: preg, arg1: preg, arg2: argument }

        (* Destinations at the end of a basic block. *)
    and controlFlow =
        (* Unconditional branch to a label - should be a merge point. *)
        Unconditional of int
        (* Conditional branch. Jumps to trueJump if the condional is false, falseJump if false. *)
    |   Conditional of { ccRef: ccRef, condition: branchOps, trueJump: int, falseJump: int }
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

    and basicBlock =
        BasicBlock of { block: x86ICode list, flow: controlFlow }

    (* Return the list of blocks that are the immediate successor of this. *)
    fun successorBlocks(Unconditional l) = [l]
    |   successorBlocks(Conditional{trueJump, falseJump, ...}) = [trueJump, falseJump]
    |   successorBlocks ExitCode = []
    |   successorBlocks(IndexedBr cases) = cases
    |   successorBlocks(SetHandler{handler, continue, ...}) = [handler, continue]
        (* We only need "handler" in SetHandler because we may have a handler that is never actually jumped to. *)
    |   successorBlocks(UnconditionalHandle handler) = [handler]
    |   successorBlocks(ConditionalHandle{handler, continue, ...}) = [handler, continue]

    datatype destinations = RegDest of reg | StackDest of int

    local        
        fun printIndex(NoMemIndex, _, _) = ()
        |   printIndex(MemIndex1 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*1]")
        |   printIndex(MemIndex2 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*2]")
        |   printIndex(MemIndex4 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*4]")
        |   printIndex(MemIndex8 i, stream, printReg) = (stream "["; printReg(i, stream); stream "*8]")
        
        fun printStackLoc(StackLoc{size, rno}, stream) =
            (stream "S"; stream(Int.toString rno); stream "("; stream(Int.toString size); stream ")")

        fun printArg(RegisterArgument reg, stream, printReg) = printReg(reg, stream)
        |   printArg(AddressConstant m, stream, _) = stream(stringOfWord m)
        |   printArg(IntegerConstant i, stream, _) = stream(LargeInt.toString i)
        |   printArg(MemoryLocation{base, offset, index, cache, ...}, stream, printReg) =
            (
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream, printReg);
                case cache of NONE => () | SOME r => (stream " cache "; printReg(r, stream))
            )
        |   printArg(StackLocation{wordOffset, container, field, cache, ...}, stream, printReg) =
            (
                printStackLoc(container, stream); stream " + ";
                stream(Int.toString field);
                stream " (";  stream(Int.toString wordOffset); stream ")";
                case cache of NONE => () | SOME r => (stream " cache "; printReg(r, stream))
            )
        |   printArg(ContainerAddr{stackOffset, container}, stream, _) =
            (
                stream "@"; printStackLoc(container, stream);
                stream " (";  stream(Int.toString stackOffset); stream ")"
            )
            
        fun printSaves([], _, _) = ()
        |   printSaves([areg], printReg, stream) = printReg(areg, stream)
        |   printSaves(areg::more, printReg, stream) =
                (printReg(areg, stream); stream ","; printSaves(more, printReg, stream))
        
        fun printKind(MoveWord, stream) = stream "Word"
        |   printKind(MoveByte, stream) = stream "Byte"
        |   printKind(Move16Bit, stream) = stream "16Bit"
        |   printKind(Move32Bit, stream) = stream "32Bit"
        |   printKind(MoveFloat, stream) = stream "Float"
        |   printKind(MoveDouble, stream) = stream "Double";

        fun printICode(LoadArgument{source, dest, kind}, stream, printReg) =
            (
                stream "\tLoad"; printKind(kind, stream); stream "\t";
                printArg(source,  stream, printReg);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(StoreArgument{source, base, offset, index, kind, ...}, stream, printReg) =
            (
                case kind of
                    MoveWord => stream "\tStoreWord\t"
                |   MoveByte => stream "\tStoreByte\t"
                |   Move16Bit => stream "\tStore16Bit\t"
                |   Move32Bit => stream "\tStore32Bit\t"
                |   MoveFloat => stream "\tStoreFloat\t"
                |   MoveDouble => stream "\tStoreDouble\t";
                printArg(source,  stream, printReg);
                stream " => ";
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream, printReg)
            )

        |   printICode(LoadMemReg { offset, dest}, stream, printReg) =
                ( stream "\tLoadMemReg\t"; stream(Int.toString offset); stream " => "; printReg(dest, stream) )

        |   printICode(BeginFunction {regArgs, stackArgs}, stream, printReg) =
            (
                stream "\tBeginFunction\t";
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printReg(arg, stream); stream " ")) regArgs;
                List.app(fn s => printStackLoc(s, stream)) stackArgs
            )

        |   printICode(FunctionCall{callKind, regArgs, stackArgs, dest}, stream, printReg) =
            (
                stream "\tFunctionCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ()
                |   DirectReg r => (stream(regRepr(GenReg r)); stream " ");
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream, printReg); stream " ")) regArgs;
                List.app(fn arg => (stream "p="; printArg(arg, stream, printReg); stream " ")) stackArgs;
                stream "=> "; printReg(dest, stream)
            )

        |   printICode(TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, currStackSize, workReg}, stream, printReg) =
            (
                stream "\tTailCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ()
                |   DirectReg r => (stream(regRepr(GenReg r)); stream " ");
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream, printReg); stream " ")) regArgs;
                List.app(fn {src, stack} => (stream (Int.toString stack); stream "<="; printArg(src, stream, printReg); stream " ")) stackArgs;
                stream "adjust="; stream(Int.toString stackAdjust);
                stream "stackSize="; stream(Int.toString currStackSize);
                stream " work reg="; printReg(workReg, stream)
            )

        |   printICode(AllocateMemoryOperation{size, flags, dest, saveRegs}, stream, printReg) =
            (
                stream "\tAllocateMemory\t";
                stream(concat["s=", Int.toString size, ",f=", Word8.toString flags, " => "]);
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(AllocateMemoryVariable{size, dest, saveRegs}, stream, printReg) =
            (
                stream "\tAllocateMemory\t";
                stream "s="; printReg(size, stream);
                stream " => "; printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(InitialiseMem{size, addr, init}, stream, printReg) =
            (
                stream "\tInitialiseMem\t";
                stream "s="; printReg(size, stream);
                stream ",i="; printReg(init, stream);
                stream ",a="; printReg(addr, stream)
            )

        |   printICode(InitialisationComplete, stream, _) = stream "\tInitComplete"

        |   printICode(JumpLoop{regArgs, stackArgs, checkInterrupt, workReg, ... }, stream, printReg) =
            (
                stream "\tJumpLoop\t";
                List.app(
                    fn (source, loopReg) => (printReg(loopReg, stream); stream "="; printArg(source, stream, printReg); stream " ")
                    ) regArgs;
                List.app(
                    fn (source, stack, stackLocn) =>
                        (printStackLoc(stackLocn, stream); stream("(sp" ^ Int.toString stack); stream ")="; printArg(source, stream, printReg); stream " ")
                    ) stackArgs;
                case checkInterrupt of
                    NONE => ()
                |   SOME saveRegs => (stream " Check:save="; printSaves(saveRegs, printReg, stream));
                case workReg of NONE => () | SOME r => (stream " work reg="; printReg(r, stream))
            )

        |   printICode(RaiseExceptionPacket{packetReg}, stream, printReg) = (stream "\tRaise\t"; printReg(packetReg, stream))

        |   printICode(ReserveContainer{size, container}, stream, _) =
            (stream "\tReserveContainer\t"; stream(Int.toString size); stream " => "; printStackLoc(container, stream))

        |   printICode(IndexedCaseOperation{testReg, workReg}, stream, printReg) =
            (
                stream "\tIndexedCase\t";
                stream "test="; printReg(testReg, stream);
                stream "work="; printReg(workReg, stream)
            )

        |   printICode(LockMutable{addr}, stream, printReg) = (stream "\tLockMutable\t"; printReg(addr, stream))

        |   printICode(WordComparison{arg1, arg2, ...}, stream, printReg) =
                (stream "\tWordComparison\t"; printReg(arg1, stream); stream ","; printArg(arg2, stream, printReg))

        |   printICode(PushExceptionHandler{workReg }, stream, printReg) =
            (
                stream "\tPushExcHandler\twith ";
                printReg(workReg, stream)
            )

        |   printICode(PopExceptionHandler{workReg}, stream, printReg) =
            (
                stream "\tPopExceptionHandler\t";
                stream "with ";
                printReg(workReg, stream)
            )

        |   printICode(BeginHandler{packetReg, workReg}, stream, printReg) =
            (
                stream "\tBeginHandler\t";
                printReg(packetReg, stream);
                stream " with ";
                printReg(workReg, stream)
            )

        |   printICode(ReturnResultFromFunction{resultReg, numStackArgs}, stream, printReg) =
                (stream "\tReturnFromFunction\t"; printReg(resultReg, stream); stream("," ^ Int.toString numStackArgs))

        |   printICode(ArithmeticFunction{oper, resultReg, operand1, operand2, ...}, stream, printReg) =
            (
                case oper of
                    ADD => stream "\tAdd\t"
                |   OR => stream "\tOrBits\t"
                |   AND => stream "\tAndBits\t"
                |   SUB => stream "\tSubtract\t"
                |   XOR => stream "\tExclusiveOrBits\t"
                |   CMP => stream "\tCompare\t";
                printReg(operand1, stream);
                stream ",";
                printArg(operand2, stream, printReg);
                stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(TestTagBit{arg, ...}, stream, printReg) = (stream "\tTestTagBit\t"; printArg(arg, stream, printReg))

        |   printICode(PushValue{arg, container}, stream, printReg) =
                (stream "\tPushValue\t"; printArg(arg, stream, printReg); stream " => "; printStackLoc(container, stream))

        |   printICode(CopyToCache{source, dest, kind}, stream, printReg) =
                (stream "\tCopyToCache"; printKind(kind, stream); stream "\t"; printReg(source, stream); stream " => "; printReg(dest, stream))

        |   printICode(ResetStackPtr{numWords, preserveCC}, stream, _) =
            (
                stream "\tResetStackPtr\t";
                stream(Int.toString numWords);
                if preserveCC then stream " preserve CC" else ()
            )

        |   printICode(StoreToStack{source, container, field, stackOffset}, stream, printReg) =
            (
                stream "\tStoreToStack\t"; printArg(source, stream, printReg);
                stream " => "; printStackLoc(container, stream); stream "+";
                stream (Int.toString field); stream "(";
                stream(Int.toString stackOffset); stream ")"
            )

        |   printICode(TagValue{source, dest, ...}, stream, printReg) =
                (stream "\tTagValue\t"; printReg(source, stream); stream " => "; printReg(dest, stream))

        |   printICode(UntagValue{source, dest, isSigned, cache}, stream, printReg) =
            (
                stream "\tUntag"; stream(if isSigned then "Signed\t" else "Unsigned\t");
                printReg(source, stream); stream " => "; printReg(dest, stream);
                case cache of NONE => () | SOME c => (stream " cache "; printReg(c, stream))
            )

        |   printICode(LoadEffectiveAddress{base, offset, index, dest}, stream, printReg) =
            (
                stream "\tLoadEffectiveAddr\t"; 
                stream(Int.toString offset ^ "(");
                case base of NONE => stream "_" | SOME b => printReg(b, stream);
                stream ")";
                printIndex(index, stream, printReg);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(ShiftOperation{shift, resultReg, operand, shiftAmount, ...}, stream, printReg) =
            (
                case shift of
                    SHL => stream "\tShiftLeft\t"
                |   SHR => stream "\tShiftRLogical\t"
                |   SAR => stream "\tShiftRArith\t";
                printReg(operand, stream); stream ",";
                printArg(shiftAmount, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(Multiplication{resultReg, operand1, operand2, ...}, stream, printReg) =
            (
                stream "\tMultiplication\t";
                printReg(operand1, stream);
                stream ",";
                printArg(operand2, stream, printReg);
                stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(Division{isSigned, dividend, divisor, quotient, remainder}, stream, printReg) =
            (
                stream "\tDivision"; stream(if isSigned then "Signed\t" else "Unsigned\t");
                printReg(dividend, stream); stream " by ";
                printArg(divisor, stream, printReg); stream " => ";
                printReg(quotient, stream); stream " rem ";
                printReg(remainder, stream)
            )

        |   printICode(AtomicExchangeAndAdd{base, source}, stream, printReg) =
            (
                stream "\tAtomicExchangeAndAdd\t";
                stream "addr=0("; printReg(base, stream);
                stream "),with="; printReg(source, stream)
            )

        |   printICode(BoxValue{boxKind, source, dest, saveRegs}, stream, printReg) =
            (
                stream(case boxKind of BoxLargeWord => "\tBoxLarge\t" | BoxX87 => "\tBoxX87\t" | BoxSSE2 => "\tBoxSSE2\t");
                printReg(source, stream);
                stream " => ";
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, printReg, stream)
            )

        |   printICode(CompareByteVectors{vec1Addr, vec2Addr, length, ...}, stream, printReg) =
            (
                stream "\tCompareByteVectors\t";
                printReg(vec1Addr, stream); stream ",";
                printReg(vec2Addr, stream); stream ",";
                printReg(length, stream)
            )

        |   printICode(BlockMove{srcAddr, destAddr, length, isByteMove}, stream, printReg) =
            (
                stream(if isByteMove then "\tBlockByteMove\t" else "\tBlockWordMove\t");
                stream "src="; printReg(srcAddr, stream);
                stream ",dest="; printReg(destAddr, stream);
                stream ",len="; printReg(length, stream)
            )

        |   printICode(X87Compare{arg1, arg2, ...}, stream, printReg) =
                (stream "\tX87Compare\t"; printReg(arg1, stream); stream ","; printArg(arg2, stream, printReg))

        |   printICode(SSE2Compare{arg1, arg2, ...}, stream, printReg) =
                (stream "\tSSE2Compare\t"; printReg(arg1, stream); stream ","; printArg(arg2, stream, printReg))

        |   printICode(X87FPGetCondition{dest, ...}, stream, printReg) = (stream "\tX87FPGetCondition\t=> "; printReg(dest, stream))

        |   printICode(X87FPArith{opc, resultReg, arg1, arg2}, stream, printReg) =
            (
                case opc of
                    FADD => stream "\tX87FPAdd\t"
                |   FMUL => stream "\tX87FPMul\t"
                |   FCOM => stream "\tX87FPCompare\t"
                |   FCOMP => stream "\tX87FPComparePop\t"
                |   FSUB => stream "\tX87FPSub\t"
                |   FSUBR => stream "\tX87FPRevSub\t"
                |   FDIV => stream "\tX87FPDiv\t"
                |   FDIVR => stream "\tX87FPRevDiv\t";
                printReg(arg1, stream); stream ",";
                printArg(arg2, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )
        
        |   printICode(X87FPUnaryOps{fpOp, dest, source}, stream, printReg) =
            (
                case fpOp of
                    FABS => stream "\tX87FPAbs\t"
                |   FCHS => stream "\tX87FPNegate\t"
                |   FLD1 => stream "\tX87FPLoad1\t"
                |   FLDZ => stream "\tX87FPLoad0\t";
                printReg(source, stream); stream " => ";
                printReg(dest, stream)
            )

        |   printICode(X87Float{dest, source}, stream, printReg) =
                (stream "\tX87Float\t"; printArg(source, stream, printReg); stream " => "; printReg(dest, stream))

        |   printICode(SSE2Float{dest, source}, stream, printReg) =
                (stream "\tSSE2Float\t"; printArg(source, stream, printReg); stream " => "; printReg(dest, stream))
        
        |   printICode(SSE2FPArith{opc, resultReg, arg1, arg2}, stream, printReg) =
            (
                case opc of
                    SSE2Move => stream "\tSSE2FPMove\t"
                |   SSE2Comp => stream "\tSSE2FPComp\t"
                |   SSE2Add => stream "\tSSE2FPAdd\t"
                |   SSE2Sub => stream "\tSSE2FPSub\t"
                |   SSE2Mul => stream "\tSSE2FPMul\t"
                |   SSE2Div => stream "\tSSE2FPDiv\t"
                |   SSE2Xor => stream "\tSSE2FPXor\t"
                |   SSE2And => stream "\tSSE2FPAnd\t"
                |   SSE2MoveSingle => stream "\tSSE2FPMoveSingle\t"
                |   SSE2DoubleToFloat => stream "\tSSE2FPDoubleToFloat\t";
                printReg(arg1, stream); stream ",";
                printArg(arg2, stream, printReg); stream " => ";
                printReg(resultReg, stream)
            )
            
        fun printAbstractReg(PReg i, stream) = stream("R" ^ Int.toString i)
        
        (* Print a basic block. *)
        fun printBlock (printItem, stream) (blockNo, BasicBlock{block, flow, ...}) =
        (
            (* Put a label on all but the first. *)
            if blockNo <> 0 then stream("L" ^ Int.toString blockNo ^ ":") else ();
            List.app (fn icode => (printICode(icode, stream, printItem); stream "\n")) block;
            case flow of
                Unconditional l => stream("\tJump\tL" ^ Int.toString l ^ "\n")
            |   Conditional {condition, trueJump, falseJump, ...} =>
                (
                    case condition of
                        JO => stream "\tJumpOverflow\t"
                    |   JNO => stream "\tJumpNoOverflow\t"
                    |   JE => stream "\tJumpEqual\t"
                    |   JNE => stream "\tJumpNotEqual\t"
                    |   JL => stream "\tJumpLessSigned\t"
                    |   JGE => stream "\tJumpGeqSigned\t"
                    |   JLE => stream "\tJumpLeqSigned\t"
                    |   JG => stream "\tJumpGrtSigned\t"
                    |   JB => stream "\tJumpLessUnsigned\t"
                    |   JNB => stream "\tJumpGeqUnsigned\t"
                    |   JNA => stream "\tJumpLeqUnsigned\t"
                    |   JA => stream "\tJumpGrtUnsigned\t"
                    |   JP => stream "\tJumpParitySet"
                    |   JNP => stream "\tJumpParityClear\t";
                    stream(concat["L", Int.toString trueJump, " else L",  Int.toString falseJump, "\n"])
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
        fun printICodeAbstract(blockVec, stream) =
            Vector.appi(printBlock(printAbstractReg, stream)) blockVec
    end

    (* We frequently just want to know the register. *)
    fun indexRegister NoMemIndex = NONE
    |   indexRegister (MemIndex1 r) = SOME r
    |   indexRegister (MemIndex2 r) = SOME r
    |   indexRegister (MemIndex4 r) = SOME r
    |   indexRegister (MemIndex8 r) = SOME r

    structure Sharing =
    struct
        type genReg         = genReg
        and argument        = argument
        and memoryIndex     = memoryIndex
        and x86ICode        = x86ICode
        and branchOps       = branchOps
        and reg             = reg
        and preg            = preg
        and destinations    = destinations
        and controlFlow     = controlFlow
        and basicBlock      = basicBlock
        and stackLocn       = stackLocn
        and regProperty     = regProperty
        and callKinds       = callKinds
        and arithOp         = arithOp
        and shiftType       = shiftType
        and repOps          = repOps
        and fpOps           = fpOps
        and fpUnaryOps      = fpUnaryOps
        and sse2Operations  = sse2Operations
        and ccRef           = ccRef
    end
end;

