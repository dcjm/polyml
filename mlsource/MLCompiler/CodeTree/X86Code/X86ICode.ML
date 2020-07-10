(*
    Copyright David C. J. Matthews 2016-20

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
    val fpMode: fpMode =
        case targetArch of Native32Bit => FPModeX87 | _ => FPModeSSE2
    
    val (polyWordOpSize, nativeWordOpSize) =
        case targetArch of
            Native32Bit     => (OpSize32, OpSize32)
        |   Native64Bit     => (OpSize64, OpSize64)
        |   ObjectId32Bit   => (OpSize32, OpSize64)

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
    |   RegPropMultiple     (* The result of a conditional or case. May be defined at multiple points. *)
 
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

    (* Generally this indicates the index register if present.  For 32-in-64
       the "index" may be ObjectIndex in which case the base is actually an
       object index. *)
    and memoryIndex =
        NoMemIndex | MemIndex1 of preg | MemIndex2 of preg |
        MemIndex4 of preg | MemIndex8 of preg | ObjectIndex

    (* Kinds of moves.
       Move32Bit - 32-bit loads and stores
       Move64Bit - 64-bit loads and stores
       MoveByte - When loading, load a byte and zero extend.
       Move16Bit - Used for C-memory loads and stores.  Zero extends on load.
       MoveFloat - Load and store a single-precision value
       MoveDouble - Load and store a double-precision value. *)
    datatype moveKind =
        MoveByte | Move16Bit | Move32Bit | Move64Bit | MoveFloat | MoveDouble

    (* The reference to a condition code. *)
    and ccRef = CcRef of int
    
    val (movePolyWord, moveNativeWord) =
        case targetArch of
            Native32Bit     => (Move32Bit, Move32Bit)
        |   Native64Bit     => (Move64Bit, Move64Bit)
        |   ObjectId32Bit   => (Move32Bit, Move64Bit)

    datatype boxKind = BoxLargeWord | BoxSSE2Double | BoxSSE2Float | BoxX87Double | BoxX87Float

    (* Size of operand.  OpSize64 is only valid in 64-bit mode. *)
    datatype opSize = datatype opSize
    
    datatype sse2UnaryOps = SSE2UDoubleToFloat | SSE2UFloatToDouble
    and      sse2BinaryOps = SSE2BAddDouble | SSE2BSubDouble | SSE2BMulDouble |
                SSE2BDivDouble | SSE2BXor | SSE2BAnd | SSE2BAddSingle |
                SSE2BSubSingle | SSE2BMulSingle | SSE2BDivSingle

    datatype callKinds =
        Recursive
    |   ConstantCode of machineWord
    |   FullCall

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
           stack arguments.  The last entry is the return address.  If the function
           has a real closure regArgs includes the closure register (rdx). *)
    |   BeginFunction of { regArgs: (preg * reg) list, stackArgs: stackLocn list }

        (* Call a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through rdx which has been loaded
           as one of the argument registers.  The result is stored in the destination register. *)
    |   FunctionCall of
            { callKind: callKinds, regArgs: (argument * reg) list,
              stackArgs: argument list, dest: preg, realDest: reg, saveRegs: preg list}

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

        (* Mark the beginning of a loop.  This is really only to prevent the initialisation code being
           duplicated in ICodeOptimise. *)
    |   BeginLoop

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
    |   WordComparison of { arg1: preg, arg2: argument, ccRef: ccRef, opSize: opSize }
    
        (* Compare with a literal.  This is generally used to compare a memory
           or stack location with a literal and overlaps to some extent
           with WordComparison. *)
    |   CompareLiteral of { arg1: argument, arg2: LargeInt.int, opSize: opSize, ccRef: ccRef }
    
        (* Compare a byte location with a literal.  This is the only operation that
           specifically deals with single bytes.  Other cases will use word
           operations. *)
    |   CompareByteMem of { arg1: { base: preg, offset: int, index: memoryIndex }, arg2: Word8.word, ccRef: ccRef }

        (* Exception handling.  - Set up an exception handler. *)
    |   PushExceptionHandler of { workReg: preg }

        (* End of a handled section.  Restore the previous handler. *)
    |   PopExceptionHandler of { workReg: preg }

        (* Marks the start of a handler.  This sets the stack pointer and
           restores the old handler.  Sets the exception packet register. *) 
    |   BeginHandler of { packetReg: preg, workReg: preg }

        (* Return from the function. *)
    |   ReturnResultFromFunction of { resultReg: preg, realReg: reg, numStackArgs: int }
    
        (* Arithmetic or logical operation.  These can set the condition codes. *)
    |   ArithmeticFunction of
            { oper: arithOp, resultReg: preg, operand1: preg, operand2: argument, ccRef: ccRef, opSize: opSize }

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
    |   TagValue of { source: preg, dest: preg, isSigned: bool, opSize: opSize }

        (* Shift a value to remove the tag bit.  The cache is used if this is untagging a
           value that has previously been tagged. *)
    |   UntagValue of { source: preg, dest: preg, isSigned: bool, cache: preg option, opSize: opSize }

        (* This provides the LEA instruction which can be used for various sorts of arithmetic.
           The base register is optional in this case. *)
    |   LoadEffectiveAddress of { base: preg option, offset: int, index: memoryIndex, dest: preg, opSize: opSize }

        (* Shift a word by an amount that can either be a constant or a register. *)
    |   ShiftOperation of { shift: shiftType, resultReg: preg, operand: preg, shiftAmount: argument, ccRef: ccRef, opSize: opSize }

        (* Multiplication.  We can use signed multiplication for both fixed precision and word (unsigned)
           multiplication.  There are various forms of the instruction including a three-operand
           version. *)
    |   Multiplication of { resultReg: preg, operand1: preg, operand2: argument, ccRef: ccRef, opSize: opSize }

        (* Division.  This takes a register pair, always RDX:RAX, divides it by the operand register and
           puts the quotient in RAX and remainder in RDX.  At the preg level we represent all of
           these by pRegs.  The divisor can be either a register or a memory location. *)
    |   Division of { isSigned: bool, dividend: preg, divisor: argument, quotient: preg, remainder: preg, opSize: opSize }

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
    |   X87Compare of { arg1: preg, arg2: argument, isDouble: bool, ccRef: ccRef }

        (* Floating point comparison. *)
    |   SSE2Compare of { arg1: preg, arg2: argument, isDouble: bool, ccRef: ccRef }

        (* The X87 FP unit does not generate condition codes directly.  We have to
           load the cc into RAX and test it there. *)
    |   X87FPGetCondition of { ccRef: ccRef, dest: preg }

        (* Binary floating point operations on the X87. *)
    |   X87FPArith of { opc: fpOps, resultReg: preg, arg1: preg, arg2: argument, isDouble: bool }

        (* Floating point operations: negate and set sign positive. *)
    |   X87FPUnaryOps of { fpOp: fpUnaryOps, dest: preg, source: preg }

        (* Load a fixed point value as a floating point value. *)
    |   X87Float of { dest: preg, source: argument }

        (* Load a fixed point value as a floating point value. *)
    |   SSE2Float of { dest: preg, source: argument }

        (* Binary floating point operations using SSE2 instructions. *)
    |   SSE2FPUnary of { opc: sse2UnaryOps, resultReg: preg, source: argument }

        (* Binary floating point operations using SSE2 instructions. *)
    |   SSE2FPBinary of { opc: sse2BinaryOps, resultReg: preg, arg1: preg, arg2: argument }

        (* Tag a 32-bit floating point value.  This is tagged by shifting left
           32-bits and then setting the bottom bit.  This allows memory operands
           to be untagged simply by loading the high-order word. *)
    |   TagFloat of { source: preg, dest: preg }

        (* Untag a 32-bit floating point value into a XMM register.  If the source
           is in memory we just need to load the high-order word. *)
    |   UntagFloat of { source: argument, dest: preg, cache: preg option }
    
        (* Get and set the control registers.  These all have to work through
           memory but it's simpler to assume they work through registers. *)
    |   GetSSE2ControlReg of { dest: preg }
    |   SetSSE2ControlReg of { source: preg }
    |   GetX87ControlReg of { dest: preg }
    |   SetX87ControlReg of { source: preg }
    
        (* Convert a floating point value to an integer. *)
    |   X87RealToInt of { source: preg, dest: preg }
        (* Convert a floating point value to an integer. *)
    |   SSE2RealToInt of { source: argument, dest: preg, isDouble: bool, isTruncate: bool }

        (* Sign extend a 32-bit value to 64-bits.  Not included in LoadArgument
           because that assumes that if we have the result in a register we can
           simply reuse the register. *)
    |   SignExtend32To64 of { source: argument, dest: preg }

        (* Touch an entry.  Actually doesn't do anything except make sure it is referenced. *)
    |   TouchArgument of { source: preg }

        (* Emulate a comparison of arbitrary precision values.  The first argument must be a register. *)
    |   LongArbComparison of { arg1: preg, arg2: argument, ccRef: ccRef, saveRegs: preg list }
    
        (* Emulate arithmetic of arbitrary precision values*)
    |   LongArbArithmetic of
            { oper: arithOp, resultReg: preg, operand1: preg, operand2: argument, saveRegs: preg list }

        (* Destinations at the end of a basic block. *)
    and controlFlow =
        (* Unconditional branch to a label - should be a merge point. *)
        Unconditional of int
        (* Conditional branch. Jumps to trueJump if the condional is true, falseJump if false. *)
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
        fun printReg(PReg i, stream) = stream("R" ^ Int.toString i)
        and printCC(CcRef ccRef, stream) = stream ("CC" ^ Int.toString ccRef)

        fun printIndex(NoMemIndex, _) = ()
        |   printIndex(MemIndex1 i, stream) = (stream "["; printReg(i, stream); stream "*1]")
        |   printIndex(MemIndex2 i, stream) = (stream "["; printReg(i, stream); stream "*2]")
        |   printIndex(MemIndex4 i, stream) = (stream "["; printReg(i, stream); stream "*4]")
        |   printIndex(MemIndex8 i, stream) = (stream "["; printReg(i, stream); stream "*8]")
        |   printIndex(ObjectIndex, stream) = stream "[objectindex]"
        
        fun printStackLoc(StackLoc{size, rno}, stream) =
            (stream "S"; stream(Int.toString rno); stream "("; stream(Int.toString size); stream ")")
        
        fun printOpsize(OpSize32, stream) = stream "32"
        |   printOpsize(OpSize64, stream) = stream "64"

        fun printArg(RegisterArgument reg, stream) = printReg(reg, stream)
        |   printArg(AddressConstant m, stream) = stream(stringOfWord m)
        |   printArg(IntegerConstant i, stream) = stream(LargeInt.toString i)
        |   printArg(MemoryLocation{base, offset, index, cache, ...}, stream) =
            (
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream);
                case cache of NONE => () | SOME r => (stream " cache "; printReg(r, stream))
            )
        |   printArg(StackLocation{wordOffset, container, field, cache, ...}, stream) =
            (
                printStackLoc(container, stream); stream " + ";
                stream(Int.toString field);
                stream " (";  stream(Int.toString wordOffset); stream ")";
                case cache of NONE => () | SOME r => (stream " cache "; printReg(r, stream))
            )
        |   printArg(ContainerAddr{stackOffset, container}, stream) =
            (
                stream "@"; printStackLoc(container, stream);
                stream " (";  stream(Int.toString stackOffset); stream ")"
            )
            
        fun printSaves([], _) = ()
        |   printSaves([areg], stream) = printReg(areg, stream)
        |   printSaves(areg::more, stream) =
                (printReg(areg, stream); stream ","; printSaves(more, stream))
        
        fun printKind(Move64Bit, stream) = stream "64Bit"
        |   printKind(MoveByte, stream) = stream "Byte"
        |   printKind(Move16Bit, stream) = stream "16Bit"
        |   printKind(Move32Bit, stream) = stream "32Bit"
        |   printKind(MoveFloat, stream) = stream "Float"
        |   printKind(MoveDouble, stream) = stream "Double";

        fun printICode(LoadArgument{source, dest, kind}, stream) =
            (
                stream "\tLoad"; printKind(kind, stream); stream "\t";
                printArg(source,  stream);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(StoreArgument{source, base, offset, index, kind, ...}, stream) =
            (
                case kind of
                    Move64Bit => stream "\tStore64Bit\t"
                |   MoveByte => stream "\tStoreByte\t"
                |   Move16Bit => stream "\tStore16Bit\t"
                |   Move32Bit => stream "\tStore32Bit\t"
                |   MoveFloat => stream "\tStoreFloat\t"
                |   MoveDouble => stream "\tStoreDouble\t";
                printArg(source,  stream);
                stream " => ";
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream)
            )

        |   printICode(LoadMemReg { offset, dest}, stream) =
                ( stream "\tLoadMemReg\t"; stream(Int.toString offset); stream " => "; printReg(dest, stream) )

        |   printICode(BeginFunction {regArgs, stackArgs}, stream) =
            (
                stream "\tBeginFunction\t";
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printReg(arg, stream); stream " ")) regArgs;
                List.app(fn s => printStackLoc(s, stream)) stackArgs
            )

        |   printICode(FunctionCall{callKind, regArgs, stackArgs, dest, realDest, saveRegs}, stream) =
            (
                stream "\tFunctionCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ();
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream); stream " ")) regArgs;
                List.app(fn arg => (stream "p="; printArg(arg, stream); stream " ")) stackArgs;
                stream "=> "; printReg(dest, stream); stream "="; stream(regRepr realDest);
                stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, currStackSize, workReg}, stream) =
            (
                stream "\tTailCall\t";
                case callKind of
                    Recursive => stream "recursive "
                |   ConstantCode m => (stream(stringOfWord m); stream " ")
                |   FullCall => ();
                List.app(fn (arg, r) => (stream(regRepr r); stream "="; printArg(arg, stream); stream " ")) regArgs;
                List.app(fn {src, stack} => (stream (Int.toString stack); stream "<="; printArg(src, stream); stream " ")) stackArgs;
                stream "adjust="; stream(Int.toString stackAdjust);
                stream "stackSize="; stream(Int.toString currStackSize);
                stream " work reg="; printReg(workReg, stream)
            )

        |   printICode(AllocateMemoryOperation{size, flags, dest, saveRegs}, stream) =
            (
                stream "\tAllocateMemory\t";
                stream(concat["s=", Int.toString size, ",f=", Word8.toString flags, " => "]);
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(AllocateMemoryVariable{size, dest, saveRegs}, stream) =
            (
                stream "\tAllocateMemory\t";
                stream "s="; printReg(size, stream);
                stream " => "; printReg(dest, stream);
                stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(InitialiseMem{size, addr, init}, stream) =
            (
                stream "\tInitialiseMem\t";
                stream "s="; printReg(size, stream);
                stream ",i="; printReg(init, stream);
                stream ",a="; printReg(addr, stream)
            )

        |   printICode(InitialisationComplete, stream) = stream "\tInitComplete"

        |   printICode(BeginLoop, stream) = stream "\tBeginLoop"

        |   printICode(JumpLoop{regArgs, stackArgs, checkInterrupt, workReg, ... }, stream) =
            (
                stream "\tJumpLoop\t";
                List.app(
                    fn (source, loopReg) => (printReg(loopReg, stream); stream "="; printArg(source, stream); stream " ")
                    ) regArgs;
                List.app(
                    fn (source, stack, stackLocn) =>
                        (printStackLoc(stackLocn, stream); stream("(sp" ^ Int.toString stack); stream ")="; printArg(source, stream); stream " ")
                    ) stackArgs;
                case checkInterrupt of
                    NONE => ()
                |   SOME saveRegs => (stream " Check:save="; printSaves(saveRegs, stream));
                case workReg of NONE => () | SOME r => (stream " work reg="; printReg(r, stream))
            )

        |   printICode(RaiseExceptionPacket{packetReg}, stream) = (stream "\tRaise\t"; printReg(packetReg, stream))

        |   printICode(ReserveContainer{size, container}, stream) =
            (stream "\tReserveContainer\t"; stream(Int.toString size); stream " => "; printStackLoc(container, stream))

        |   printICode(IndexedCaseOperation{testReg, workReg}, stream) =
            (
                stream "\tIndexedCase\t";
                stream "test="; printReg(testReg, stream);
                stream "work="; printReg(workReg, stream)
            )

        |   printICode(LockMutable{addr}, stream) = (stream "\tLockMutable\t"; printReg(addr, stream))

        |   printICode(WordComparison{arg1, arg2, ccRef, opSize, ...}, stream) =
            (
                stream "\tWordComparison"; printOpsize(opSize, stream); stream "\t";
                printReg(arg1, stream); stream ","; printArg(arg2, stream);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(CompareLiteral{arg1, arg2, opSize, ccRef, ...}, stream) =
            (
                stream "\tCompareLiteral"; printOpsize(opSize, stream); stream "\t";
                printArg(arg1, stream); stream ","; stream(LargeInt.toString arg2);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(CompareByteMem{arg1={base, offset, index, ...}, arg2, ccRef,...}, stream) =
            (
                stream "\tCompareByteMem\t";
                stream(Int.toString offset ^ "(");
                printReg(base, stream);
                stream ")";
                printIndex(index, stream);
                stream ","; stream(Word8.toString arg2);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(PushExceptionHandler{workReg }, stream) =
            (
                stream "\tPushExcHandler\twith ";
                printReg(workReg, stream)
            )

        |   printICode(PopExceptionHandler{workReg}, stream) =
            (
                stream "\tPopExceptionHandler\t";
                stream "with ";
                printReg(workReg, stream)
            )

        |   printICode(BeginHandler{packetReg, workReg}, stream) =
            (
                stream "\tBeginHandler\t";
                printReg(packetReg, stream);
                stream " with ";
                printReg(workReg, stream)
            )

        |   printICode(ReturnResultFromFunction{resultReg, realReg, numStackArgs}, stream) =
            (
                stream "\tReturnFromFunction\t";
                printReg(resultReg, stream); stream "="; stream(regRepr realReg);
                stream("," ^ Int.toString numStackArgs)
            )

        |   printICode(ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef, opSize, ...}, stream) =
            (
                case oper of
                    ADD => stream "\tAdd"
                |   OR => stream "\tOrBits"
                |   AND => stream "\tAndBits"
                |   SUB => stream "\tSubtract"
                |   XOR => stream "\tExclusiveOrBits"
                |   CMP => stream "\tCompare";
                printOpsize(opSize, stream);
                stream "\t";
                printReg(operand1, stream);
                stream ",";
                printArg(operand2, stream);
                stream " => ";
                printReg(resultReg, stream);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(TestTagBit{arg, ccRef, ...}, stream) =
            (stream "\tTestTagBit\t"; printArg(arg, stream); stream " => "; printCC(ccRef, stream))

        |   printICode(PushValue{arg, container}, stream) =
                (stream "\tPushValue\t"; printArg(arg, stream); stream " => "; printStackLoc(container, stream))

        |   printICode(CopyToCache{source, dest, kind}, stream) =
                (stream "\tCopyToCache"; printKind(kind, stream); stream "\t"; printReg(source, stream); stream " => "; printReg(dest, stream))

        |   printICode(ResetStackPtr{numWords, preserveCC}, stream) =
            (
                stream "\tResetStackPtr\t";
                stream(Int.toString numWords);
                if preserveCC then stream " preserve CC" else ()
            )

        |   printICode(StoreToStack{source, container, field, stackOffset}, stream) =
            (
                stream "\tStoreToStack\t"; printArg(source, stream);
                stream " => "; printStackLoc(container, stream); stream "+";
                stream (Int.toString field); stream "(";
                stream(Int.toString stackOffset); stream ")"
            )

        |   printICode(TagValue{source, dest, opSize, ...}, stream) =
            (
                stream "\tTagValue";
                printOpsize(opSize, stream);
                stream "\t";
                printReg(source, stream);
                stream " => "; printReg(dest, stream)
            )

        |   printICode(UntagValue{source, dest, isSigned, cache, opSize}, stream) =
            (
                stream "\tUntag"; stream(if isSigned then "Signed" else "Unsigned");
                printOpsize(opSize, stream);
                stream "\t";
                printReg(source, stream); stream " => "; printReg(dest, stream);
                case cache of NONE => () | SOME c => (stream " cache "; printReg(c, stream))
            )

        |   printICode(LoadEffectiveAddress{base, offset, index, dest, opSize}, stream) =
            (
                stream "\tLoadEffectiveAddr"; 
                printOpsize(opSize, stream);
                stream "\t";
                stream(Int.toString offset ^ "(");
                case base of NONE => stream "_" | SOME b => printReg(b, stream);
                stream ")";
                printIndex(index, stream);
                stream " => ";
                printReg(dest, stream)
            )

        |   printICode(ShiftOperation{shift, resultReg, operand, shiftAmount, ccRef, opSize, ...}, stream) =
            (
                case shift of
                    SHL => stream "\tShiftLeft"
                |   SHR => stream "\tShiftRLogical"
                |   SAR => stream "\tShiftRArith";
                printOpsize(opSize, stream);
                stream "\t";
                printReg(operand, stream); stream ",";
                printArg(shiftAmount, stream); stream " => ";
                printReg(resultReg, stream);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(Multiplication{resultReg, operand1, operand2, ccRef, opSize, ...}, stream) =
            (
                stream "\tMultiplication";
                printOpsize(opSize, stream);
                stream "\t";
                printReg(operand1, stream);
                stream ",";
                printArg(operand2, stream);
                stream " => ";
                printReg(resultReg, stream);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(Division{isSigned, dividend, divisor, quotient, remainder, opSize}, stream) =
            (
                stream "\tDivision"; stream(if isSigned then "Signed\t" else "Unsigned\t");
                printOpsize(opSize, stream);
                stream "\t";
                printReg(dividend, stream); stream " by ";
                printArg(divisor, stream); stream " => ";
                printReg(quotient, stream); stream " rem ";
                printReg(remainder, stream)
            )

        |   printICode(AtomicExchangeAndAdd{base, source}, stream) =
            (
                stream "\tAtomicExchangeAdd\t";
                stream "addr=0("; printReg(base, stream);
                stream "),with="; printReg(source, stream)
            )

        |   printICode(BoxValue{boxKind, source, dest, saveRegs}, stream) =
            (
                stream(
                    case boxKind of
                        BoxLargeWord => "\tBoxLarge\t"
                    |   BoxX87Double => "\tBoxX87Double\t"
                    |   BoxX87Float => "\tBoxX87Float\t"
                    |   BoxSSE2Double => "\tBoxSSE2Double\t"
                    |   BoxSSE2Float => "\tBoxSSE2Float\t"
                );
                printReg(source, stream);
                stream " => ";
                printReg(dest, stream);
                stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(CompareByteVectors{vec1Addr, vec2Addr, length, ccRef, ...}, stream) =
            (
                stream "\tCompareByteVectors\t";
                printReg(vec1Addr, stream); stream ",";
                printReg(vec2Addr, stream); stream ",";
                printReg(length, stream);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(BlockMove{srcAddr, destAddr, length, isByteMove}, stream) =
            (
                stream(if isByteMove then "\tBlockByteMove\t" else "\tBlockWordMove\t");
                stream "src="; printReg(srcAddr, stream);
                stream ",dest="; printReg(destAddr, stream);
                stream ",len="; printReg(length, stream)
            )

        |   printICode(X87Compare{arg1, arg2, isDouble, ccRef, ...}, stream) =
            (
                stream "\tX87Compare"; stream(if isDouble then "D\t" else "S\t"); printReg(arg1, stream);
                stream ","; printArg(arg2, stream);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(SSE2Compare{arg1, arg2, isDouble, ccRef, ...}, stream) =
            (
                stream "\tSSE2Compare"; stream(if isDouble then "D\t" else "S\t"); printReg(arg1, stream);
                stream ","; printArg(arg2, stream);
                stream " => "; printCC(ccRef, stream)
            )

        |   printICode(X87FPGetCondition{dest, ccRef, ...}, stream) =
            (stream "\tX87FPGetCondition\t=> "; printReg(dest, stream); stream " => "; printCC(ccRef, stream))

        |   printICode(X87FPArith{opc, resultReg, arg1, arg2, isDouble}, stream) =
            (
                case opc of
                    FADD => stream "\tX87FPAdd"
                |   FMUL => stream "\tX87FPMul"
                |   FCOM => stream "\tX87FPCompare"
                |   FCOMP => stream "\tX87FPComparePop"
                |   FSUB => stream "\tX87FPSub"
                |   FSUBR => stream "\tX87FPRevSub"
                |   FDIV => stream "\tX87FPDiv"
                |   FDIVR => stream "\tX87FPRevDiv";
                if isDouble then stream "D\t" else stream "S\t";
                printReg(arg1, stream); stream ",";
                printArg(arg2, stream); stream " => ";
                printReg(resultReg, stream)
            )
        
        |   printICode(X87FPUnaryOps{fpOp, dest, source}, stream) =
            (
                case fpOp of
                    FABS => stream "\tX87FPAbs\t"
                |   FCHS => stream "\tX87FPNegate\t"
                |   FLD1 => stream "\tX87FPLoad1\t"
                |   FLDZ => stream "\tX87FPLoad0\t";
                printReg(source, stream); stream " => ";
                printReg(dest, stream)
            )

        |   printICode(X87Float{dest, source}, stream) =
                (stream "\tX87Float\t"; printArg(source, stream); stream " => "; printReg(dest, stream))

        |   printICode(SSE2Float{dest, source}, stream) =
                (stream "\tSSE2Float\t"; printArg(source, stream); stream " => "; printReg(dest, stream))
        
        |   printICode(SSE2FPUnary{opc, resultReg, source}, stream) =
            (
                case opc of
                    SSE2UDoubleToFloat => stream "\tSSE2UDoubleToFloat\t"
                |   SSE2UFloatToDouble => stream "\tSSE2UFloatToDouble\t";
                printArg(source, stream); stream " => ";
                printReg(resultReg, stream)
            )
        
        |   printICode(SSE2FPBinary{opc, resultReg, arg1, arg2}, stream) =
            (
                case opc of
                    SSE2BAddDouble => stream "\tSSE2BAddDouble\t"
                |   SSE2BSubDouble => stream "\tSSE2BSubDouble\t"
                |   SSE2BMulDouble => stream "\tSSE2BMulDouble\t"
                |   SSE2BDivDouble => stream "\tSSE2BDivDouble\t"
                |   SSE2BAddSingle => stream "\tSSE2BAddSingle\t"
                |   SSE2BSubSingle => stream "\tSSE2BSubSingle\t"
                |   SSE2BMulSingle => stream "\tSSE2BMulSingle\t"
                |   SSE2BDivSingle => stream "\tSSE2BDivSingle\t"
                |   SSE2BXor => stream "\tSSE2BXor\t"
                |   SSE2BAnd => stream "\tSSE2BAnd\t";
                printReg(arg1, stream); stream ",";
                printArg(arg2, stream); stream " => ";
                printReg(resultReg, stream)
            )

        |   printICode(TagFloat{source, dest, ...}, stream) =
                (stream "\tTagFloat\t"; printReg(source, stream); stream " => "; printReg(dest, stream))

        |   printICode(UntagFloat{source, dest, cache}, stream) =
            (
                stream "\tUntagFloat\t";
                printArg(source, stream); stream " => "; printReg(dest, stream);
                case cache of NONE => () | SOME c => (stream " cache "; printReg(c, stream))
            )

        |   printICode(GetSSE2ControlReg{dest}, stream) = (stream "\tGetSSE2ControlReg\t"; printReg(dest, stream))
 
        |   printICode(SetSSE2ControlReg{source}, stream) = (stream "\tSetSSE2ControlReg\t"; printReg(source, stream))

        |   printICode(GetX87ControlReg{dest}, stream) = (stream "\tGetX87ControlReg\t"; printReg(dest, stream))
 
        |   printICode(SetX87ControlReg{source}, stream) = (stream "\tSetX87ControlReg\t"; printReg(source, stream))
 
        |   printICode(X87RealToInt{source, dest}, stream) =
            (stream "\tX87RealToInt\t"; printReg(source, stream); stream " => "; printReg(dest, stream))
 
        |   printICode(SSE2RealToInt{source, dest, isDouble, isTruncate}, stream) =
            (
                stream "\tSSE2RealToInt";
                if isTruncate then stream "Trunc" else ();
                if isDouble then stream "D\t" else stream "S\t";
                printArg(source, stream); stream " => "; printReg(dest, stream)
            )

        |   printICode(SignExtend32To64{source, dest}, stream) =
            (stream "\tSignExtend32To64\t"; printArg(source, stream); stream " => "; printReg(dest, stream))
 
        |   printICode(TouchArgument{source}, stream) =
            (stream "\tTouchArgument\t"; printReg(source, stream))

        |   printICode(LongArbComparison{arg1, arg2, ccRef, saveRegs, ...}, stream) =
            (
                stream "\tLongArbComparison"; stream "\t";
                printReg(arg1, stream); stream ","; printArg(arg2, stream);
                stream " => "; printCC(ccRef, stream);
                stream " save="; printSaves(saveRegs, stream)
            )

        |   printICode(LongArbArithmetic{oper, resultReg, operand1, operand2, saveRegs, ...}, stream) =
            (
                case oper of
                    ADD => stream "\tAddLongArb"
                |   SUB => stream "\tSubtractLongArb"
                |   _ => stream "????LongArb";
                stream "\t";
                printReg(operand1, stream);
                stream ",";
                printArg(operand2, stream);
                stream " => ";
                printReg(resultReg, stream);
                stream " save="; printSaves(saveRegs, stream)
            )

        and printCondition(JO,  stream)  = stream "Overflow"
        |   printCondition(JNO, stream)  = stream "NoOverflow"
        |   printCondition(JE,  stream)  = stream "Equal"
        |   printCondition(JNE, stream)  = stream "NotEqual"
        |   printCondition(JL,  stream)  = stream "LessSigned"
        |   printCondition(JGE, stream)  = stream "GeqSigned"
        |   printCondition(JLE, stream)  = stream "LeqSigned"
        |   printCondition(JG,  stream)  = stream "GrtSigned"
        |   printCondition(JB,  stream)  = stream "LessUnsigned"
        |   printCondition(JNB, stream)  = stream "GeqUnsigned"
        |   printCondition(JNA, stream)  = stream "LeqUnsigned"
        |   printCondition(JA,  stream)  = stream "GrtUnsigned"
        |   printCondition(JP,  stream)  = stream "ParitySet"
        |   printCondition(JNP, stream)  = stream "ParityClear"

        (* Print a basic block. *)
        fun printBlock stream (blockNo, BasicBlock{block, flow, ...}) =
        (
            (* Put a label on all but the first. *)
            if blockNo <> 0 then stream("L" ^ Int.toString blockNo ^ ":") else ();
            List.app (fn icode => (printICode(icode, stream); stream "\n")) block;
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
        fun printICodeAbstract(blockVec, stream) =
            Vector.appi(printBlock stream) blockVec
    end

    (* We frequently just want to know the register. *)
    fun indexRegister NoMemIndex = NONE
    |   indexRegister (MemIndex1 r) = SOME r
    |   indexRegister (MemIndex2 r) = SOME r
    |   indexRegister (MemIndex4 r) = SOME r
    |   indexRegister (MemIndex8 r) = SOME r
    |   indexRegister ObjectIndex = NONE

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
        and sse2UnaryOps    = sse2UnaryOps
        and sse2BinaryOps   = sse2BinaryOps
        and ccRef           = ccRef
        and opSize          = opSize
        and closureRef      = closureRef
    end
end;

