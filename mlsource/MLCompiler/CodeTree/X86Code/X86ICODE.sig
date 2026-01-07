(*
    Signature for the high-level X86 code
    Copyright David C. J. Matthews 2016-23, 2026

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

signature X86ICODE =
sig
    type machineWord = Address.machineWord
    type address = Address.address
    
    type closureRef

    (* Registers. *)
    datatype genReg = GeneralReg of Word8.word * bool
    and fpReg = FloatingPtReg of Word8.word
    and xmmReg = SSE2Reg of Word8.word
    
    datatype reg =
        GenReg of genReg
    |   FPReg of fpReg
    |   XMMReg of xmmReg
    
    val regRepr: reg -> string
    val nReg:   reg -> int

    val is32bit: LargeInt.int -> bool

    datatype targetArch = Native32Bit | Native64Bit | ObjectId32Bit2Word | ObjectId32Bit4Word
    val targetArch: targetArch

    (* Should we use SSE2 or X87 floating point? *)
    datatype fpMode = FPModeSSE2 | FPModeX87
    val fpMode: fpMode

    val eax: genReg and ebx: genReg and ecx: genReg and edx: genReg
    and edi: genReg and esi: genReg and esp: genReg and ebp: genReg
    and r8:  genReg and r9:  genReg and r10: genReg and r11: genReg
    and r12: genReg and r13: genReg and r14: genReg and r15: genReg
    and fp0: fpReg and fp1: fpReg and fp2: fpReg and fp3: fpReg
    and fp4: fpReg and fp5: fpReg and fp6: fpReg and fp7: fpReg
    and xmm0:xmmReg and xmm1:xmmReg and xmm2:xmmReg and xmm3:xmmReg
    and xmm4:xmmReg and xmm5:xmmReg and xmm6:xmmReg

    datatype branchOps =
        JO | JNO | JE | JNE | JL | JGE | JLE | JG | JB | JNB | JNA | JA | JP | JNP
    and      arithOp = ADD | OR (*|ADC | SBB*) | AND | SUB | XOR | CMP
    and      shiftType = SHL | SHR | SAR

    datatype boxKind = BoxLargeWord | BoxSSE2Double | BoxSSE2Float | BoxX87Double | BoxX87Float

    and      fpOps = FADD | FMUL | FCOM | FCOMP | FSUB | FSUBR | FDIV | FDIVR
    and      fpUnaryOps = FABS | FCHS | FLD1 | FLDZ
    
    datatype sse2UnaryOps = SSE2UDoubleToFloat | SSE2UFloatToDouble
    and      sse2BinaryOps = SSE2BAddDouble | SSE2BSubDouble | SSE2BMulDouble |
                SSE2BDivDouble | SSE2BXor | SSE2BAnd | SSE2BAddSingle |
                SSE2BSubSingle | SSE2BMulSingle | SSE2BDivSingle

    val memRegThreadSelf: int (* Copied from X86CodeSig *)
    and memRegExceptionPacket: int
    and memRegCStackPtr: int

    datatype callKinds =
        Recursive
    |   ConstantCode of machineWord
    |   FullCall

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
    |   MemoryLocation of { base: preg, offset: int, index: memoryIndex, cache: preg option } (* A memory location. *)
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
        MemIndex4 of preg | MemIndex8 of preg | ObjectIndex2 | ObjectIndex4

    (* Kinds of moves.
       Move32Bit - 32-bit loads and stores
       Move64Bit - 64-bit loads and stores
       MoveByte - When loading, load a byte and zero extend.
       Move16Bit - Used for C-memory loads and stores.  Zero extends on load.
       MoveFloat - Load and store a single-precision value
       MoveDouble - Load and store a double-precision value. *)
    datatype moveKind =
        MoveByte | Move16Bit | Move32Bit | Move64Bit | MoveFloat | MoveDouble

    val movePolyWord: moveKind and moveNativeWord: moveKind
    
    (* The reference to a condition code. *)
    datatype ccRef = CcRef of int
    
    (* Size of operand.  OpSize64 is only valid in 64-bit mode. *)
    datatype opSize = OpSize32 | OpSize64
    val polyWordOpSize: opSize and nativeWordOpSize: opSize

    datatype x86ICode =
        (* Move a value into a register. *)
        LoadArgument of { source: argument, dest: preg, kind: moveKind }
        
        (* Store a value into memory.  The source will usually be a register but could be
           a constant depending on the value.  If isMutable is true we're assigning to
           a ref and we need to flush the memory cache. *)
    |   StoreArgument of
            { source: argument, base: preg, offset: int, index: memoryIndex, kind: moveKind, isMutable: bool }

        (* Load an entry from the "memory registers".  Used for ThreadSelf and AllocCStack. *)
    |   LoadMemReg of { offset: int, dest: preg, kind: moveKind }

        (* Store a value into an entry in the "memory registers".  Used AllocCStack/FreeCStack. *)
    |   StoreMemReg of { offset: int, source: preg, kind: moveKind }

        (* Start of function.  Set the register arguments.  stackArgs is the list of
           stack arguments.  The last entry is the return address.  If the function
           has a real closure regArgs includes the closure register (rdx). *)
    |   BeginFunction of { regArgs: (preg * reg) list, stackArgs: stackLocn list }

        (* Call a function.  If the code address is a constant it is passed here.
           Otherwise the address is obtained by indirecting through rdx which has been loaded
           as one of the argument registers.  The results are stored in the destination
           registers.  Usually there is a single result but there could be zero or more
           than one if results are returned as containers. *)
    |   FunctionCall of
            { callKind: callKinds, regArgs: (argument * reg) list,
              stackArgs: argument list, results: (preg * reg) list, saveRegs: preg list}

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
    |   ReturnResultFromFunction of { results: (preg * reg) list, numStackArgs: int }
    
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
           the resultReg contains the old value of the destination and the destination
           has been updated with its old value added to the increment.
           The destination is actually the word pointed at by "base". *)
    |   AtomicExchangeAndAdd of { base: preg, source: preg, resultReg: preg }

        (* Atomic exchange.  Used to release a mutex.  The contents of the source
           register are written to the address and the resultReg contains the
           previous value. *)
    |   AtomicExchange of { base: preg, source: preg, resultReg: preg }

        (* Atomic compare-and-exchange.  Used to test a mutex and set it if it is currently unlocked.
           Compares the current value with the contents of "compare" and sets the mutex to
           "toStore" if it is equal.  If it is not it stores the current value of the
           mutex in resultReg.  Both compare and resultReg are actually RAX/EAX.
           Sets the condition codes to the results of the comparison. *)
    |   AtomicCompareAndExchange of { base: preg, compare: preg, toStore: preg, resultReg: preg, ccRef: ccRef }

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
    |   SSE2IntToReal of { dest: preg, source: argument, isDouble: bool }

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

        (* Pause instruction - used only in mutex spinlock *)
    |   PauseCPU

        (* Find the bit position of the highest bit set. This is always applied
           to a tagged value so it will return 63/31 if the argument is
           (tagged) zero. *)
    |   BitScanReverse of { source: argument, dest: preg }

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

    and basicBlock = BasicBlock of { block: x86ICode list, flow: controlFlow }
    
    (* Return the successor blocks from a control flow. *)
    val successorBlocks: controlFlow -> int list

    val printICodeAbstract: basicBlock vector * (string -> unit) -> unit
    
    val indexRegister: memoryIndex -> preg option
 
    structure Sharing:
    sig
        type genReg         = genReg
        and  argument       = argument
        and  memoryIndex    = memoryIndex
        and  x86ICode       = x86ICode
        and  branchOps      = branchOps
        and  reg            = reg
        and preg            = preg
        and controlFlow     = controlFlow
        and basicBlock      = basicBlock
        and stackLocn       = stackLocn
        and regProperty     = regProperty
        and callKinds       = callKinds
        and arithOp         = arithOp
        and shiftType       = shiftType
        and fpOps           = fpOps
        and fpUnaryOps      = fpUnaryOps
        and sse2UnaryOps    = sse2UnaryOps
        and sse2BinaryOps   = sse2BinaryOps
        and ccRef           = ccRef
        and opSize          = opSize
        and closureRef      = closureRef
   end
end;
