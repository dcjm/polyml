(*
    Copyright David C. J. Matthews 1989, 2000, 2009-10, 2012-13, 2015-17
    
    Based on original code:    
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(*
    Title:      Code Generator Routines.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1989
*)

(* This module contains the code vector and operations to insert code into
   it. Each procedure is compiled into a separate segment. Initially it is
   compiled into a fixed size segment, and then copied into a segment of the
   correct size at the end.
   This module contains all the definitions of the X86 opCodes and registers.
   It uses "codeseg" to create and operate on the segment itself.
 *)

functor X86OUTPUTCODE (
structure DEBUG: DEBUGSIG
structure PRETTY: PRETTYSIG (* for compilerOutTag *)

) : X86CODESIG =

struct
    open CODE_ARRAY
    open DEBUG;
    open Address
    open Misc;

    val isX64 = wordSize = 8 (* Generate X64 instructions if the word length is 8. *)

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op << = Word.<< and op >> = Word.>>
    val (*op <<+ = LargeWord.<< and *) op >>+ = LargeWord.>>
    val op <<- = Word8.<< and op >>- = Word8.>>

    val op orb8 = Word8.orb
    val op andb8 = Word8.andb

    val op andb = Word.andb (* and op andbL = LargeWord.andb *)
    and op orb  = Word.orb

    val wordToWord8 = Word8.fromLargeWord o Word.toLargeWord
    (*and word8ToWord = Word.fromLargeWord o Word8.toLargeWord*)

    val exp2_16 =        0x10000
    val exp2_31 =        0x80000000: LargeInt.int

    (* Returns true if this a 32-bit machine or if the constant is within 32-bits.
       This is exported to the higher levels.  N.B.  The test for not isX64
       avoids a significant overhead with arbitrary precision arithmetic on
       X86/32. *)
    fun is32bit v = not isX64 orelse ~exp2_31 <= v andalso v < exp2_31

    (* tag a short constant *)
    fun tag c = 2 * c + 1;

    fun is8BitL (n: LargeInt.int) = ~ 0x80 <= n andalso n < 0x80

    local
        val shift =
            if wordSize = 4
            then 0w2
            else if wordSize = 8
            then 0w3
            else raise InternalError "Invalid word size for x86_32 or x86+64"
    in
        fun wordsToBytes n = n << shift
        and bytesToWords n = n >> shift
    end

    infix 6 addrPlus addrMinus;
  
    (* All indexes into the code vector have type "addrs". This is really a legacy. *)
    type addrs = Word.word
  
    val addrZero = 0w0
 
    (* This is the external label type used when constructing operations. *)
    datatype label = Label of { labelNo: int }
   
  (* Constants which are too large to go inline in the code are put in
     a list and put at the end of the code. They are arranged so that
     the garbage collector can find them and change them as necessary.
     A reference to a constant is treated like a forward reference to a
     label. *)

    datatype code =
    Code of 
    {
        procName:       string,         (* Name of the procedure. *)
        printAssemblyCode:bool,            (* Whether to print the code when we finish. *)
        printStream:    string->unit,   (* The stream to use *)
        lowLevelOptimise: bool,         (* Whether to do the low-level optimisation pass *)
        profileObject   : machineWord  (* The profile object for this code. *)
    }

    (* Exported functions *)
    fun lowLevelOptimise(Code{lowLevelOptimise, ...}) = lowLevelOptimise

  (* EBP/RBP points to a structure that interfaces to the RTS.  These are
     offsets into that structure.  *)
    val memRegLocalMPointer       = 0 (* Not used in 64-bit *)
    and memRegHandlerRegister     = wordSize
    and memRegLocalMbottom        = 2 * wordSize
    and memRegStackLimit          = 3 * wordSize
    and memRegExceptionPacket     = 4 * wordSize
    and memRegCStackPtr           = 6 * wordSize
    and memRegThreadSelf          = 7 * wordSize
    and memRegStackPtr            = 8 * wordSize
    and memRegHeapOverflowCall    = 10 * wordSize
    and memRegStackOverflowCall   = 11 * wordSize
    and memRegStackOverflowCallEx = 12 * wordSize
    
    (* This can probably be much smaller now. *)
    and memRegSize                = if isX64 then 144 else 56 (* Size of area on the stack. *)

    (* create and initialise a code segment *)
    fun codeCreate (name : string, profObj, parameters) : code =
    let
        val printStream = PRETTY.getSimplePrinter(parameters, [])
    in
        Code
        { 
            procName       = name,
            printAssemblyCode = DEBUG.getParameter DEBUG.assemblyCodeTag parameters,
            printStream    = printStream,
            lowLevelOptimise = DEBUG.getParameter DEBUG.lowlevelOptimiseTag parameters,
            profileObject  = profObj
        }
    end

    (* Put 1 unsigned byte at a given offset in the segment. *)
    fun set8u (b, addr, seg) = byteVecSet (seg, addr,  b)
 
    (* Put 4 bytes at a given offset in the segment. *)
    (* b0 is the least significant byte. *)
    fun set4Bytes (b3, b2, b1, b0, addr, seg) =
    let
        val a = addr;
    in
        (* Little-endian *)
        byteVecSet (seg, a,     b0);
        byteVecSet (seg, a + 0w1, b1);
        byteVecSet (seg, a + 0w2, b2);
        byteVecSet (seg, a + 0w3, b3)
    end;

    (* Put 1 unsigned word at a given offset in the segment. *)
    fun set32u (ival: LargeWord.word, addr, seg) : unit =
    let
        val b3       = Word8.fromLargeWord (ival >>+ 0w24)
        val b2       = Word8.fromLargeWord (ival >>+ 0w16)
        val b1       = Word8.fromLargeWord (ival >>+ 0w8)
        val b0       = Word8.fromLargeWord ival
    in
        set4Bytes (b3, b2, b1, b0, addr, seg)
    end

    (* Put 1 signed word at a given offset in the segment. *)
    fun set32s (ival: LargeInt.int, addr, seg) = set32u(LargeWord.fromLargeInt ival, addr, seg)
    
    fun byteSigned ival =
        if ~0x80 <= ival andalso ival < 0x80
        then Word8.fromInt ival
        else raise InternalError "byteSigned: invalid byte"
    
    (* Convert a large-word value to a little-endian byte sequence. *)
    fun largeWordToBytes(_, 0) = []
    |   largeWordToBytes(ival: LargeWord.word, n) =
            Word8.fromLargeWord ival :: largeWordToBytes(ival >>+ 0w8, n-1)

    fun word32Unsigned(ival: LargeWord.word) = largeWordToBytes(ival, 4)

    fun int32Signed(ival: LargeInt.int) =
        if is32bit ival
        then word32Unsigned(LargeWord.fromLargeInt ival)
        else raise InternalError "int32Signed: invalid word"
    
    (* wordUnsigned is 8 bytes on 64-bits or 4-bytes on 32-bits. *)
    fun wordUnsigned(ival: LargeWord.word) = largeWordToBytes(ival, wordSize)

    (* Registers. *)
    datatype genReg = GeneralReg of Word8.word * bool
    and fpReg = FloatingPtReg of Word8.word
    and xmmReg = SSE2Reg of Word8.word
    
    datatype reg =
        GenReg of genReg
    |   FPReg of fpReg
    |   XMMReg of xmmReg

    (* These are the real registers we have.  The AMD extension encodes the
       additional registers through the REX prefix. *)
    val eax = GeneralReg (0w0, false)
    val ecx = GeneralReg (0w1, false)
    val edx = GeneralReg (0w2, false)
    val ebx = GeneralReg (0w3, false)
    val esp = GeneralReg (0w4, false)
    val ebp = GeneralReg (0w5, false)
    val esi = GeneralReg (0w6, false)
    val edi = GeneralReg (0w7, false)
    val r8  = GeneralReg (0w0, true)
    val r9  = GeneralReg (0w1, true)
    val r10 = GeneralReg (0w2, true)
    val r11 = GeneralReg (0w3, true)
    val r12 = GeneralReg (0w4, true)
    val r13 = GeneralReg (0w5, true)
    val r14 = GeneralReg (0w6, true)
    val r15 = GeneralReg (0w7, true)

    (* Floating point "registers".  Actually entries on the floating point stack.
       The X86 has a floating point stack with eight entries. *)
    val fp0 = FloatingPtReg 0w0
    and fp1 = FloatingPtReg 0w1
    and fp2 = FloatingPtReg 0w2
    and fp3 = FloatingPtReg 0w3
    and fp4 = FloatingPtReg 0w4
    and fp5 = FloatingPtReg 0w5
    and fp6 = FloatingPtReg 0w6
    and fp7 = FloatingPtReg 0w7

    (* SSE2 Registers.  These are used for floating point in 64-bity mode.
       We only use XMM0-6 because the others are callee save and we don't
       currently save them. *)
    val xmm0 = SSE2Reg 0w0
    and xmm1 = SSE2Reg 0w1
    and xmm2 = SSE2Reg 0w2
    and xmm3 = SSE2Reg 0w3
    and xmm4 = SSE2Reg 0w4
    and xmm5 = SSE2Reg 0w5
    and xmm6 = SSE2Reg 0w6

    val regClosure  = edx (* Addr. of closure for fn. call goes here. *)

    fun getReg (GeneralReg r) = r
    fun mkReg  n      = GeneralReg n  (* reg.up   *)
  
    (* The maximum size of the register vectors and masks.  Although the
       X86/32 has a floating point stack with eight entries it's much simpler
       to treat it as having seven "real" registers.  Items are pushed to the
       stack and then stored and popped into the current location.  It may be
       possible to improve the code by some peephole optimisation. *)
    val regs = 30 (* Include the X86/64 registers even if this is 32-bit. *)

    (* The nth register (counting from 0). *)
    (* Profiling shows that applying the constructors here creates a lot of
       garbage.  Create the entries once and then use vector indexing instead. *)
    local
        fun regN i =
            if i < 8
            then GenReg(GeneralReg(Word8.fromInt i, false))
            else if i < 16
            then GenReg(GeneralReg(Word8.fromInt(i-8), true))
            else if i < 23
            then FPReg(FloatingPtReg(Word8.fromInt(i-16)))
            else XMMReg(SSE2Reg(Word8.fromInt(i-23)))
        val regVec = Vector.tabulate(regs, regN)
    in
        fun regN i = Vector.sub(regVec, i) handle Subscript => raise InternalError "Bad register number"
    end
 
    (* The number of the register. *)
    fun nReg(GenReg(GeneralReg(r, false))) = Word8.toInt r
    |   nReg(GenReg(GeneralReg(r, true))) = Word8.toInt r + 8
    |   nReg(FPReg(FloatingPtReg r)) = Word8.toInt r + 16
    |   nReg(XMMReg(SSE2Reg r)) = Word8.toInt r + 23
        
    datatype opsize = SZByte | SZWord | SZDWord | SZQWord
    
    val sz32_64 = if isX64 then SZQWord else SZDWord

    fun genRegRepr(GeneralReg (0w0, false), SZByte) = "al"
    |   genRegRepr(GeneralReg (0w1, false), SZByte) = "cl"
    |   genRegRepr(GeneralReg (0w2, false), SZByte) = "dl"
    |   genRegRepr(GeneralReg (0w3, false), SZByte) = "bl"
    |   genRegRepr(GeneralReg (0w4, false), SZByte) = "ah" (* TODO: May be different if there's a rex code *)
    |   genRegRepr(GeneralReg (0w5, false), SZByte) = "ch"
    |   genRegRepr(GeneralReg (0w6, false), SZByte) = "dh"
    |   genRegRepr(GeneralReg (0w7, false), SZByte) = "bh"
    |   genRegRepr(GeneralReg (reg, true),  SZByte) = "r" ^ Int.toString(Word8.toInt reg +8) ^ "b"
    |   genRegRepr(GeneralReg (0w0, false), SZDWord) = "eax"
    |   genRegRepr(GeneralReg (0w1, false), SZDWord) = "ecx"
    |   genRegRepr(GeneralReg (0w2, false), SZDWord) = "edx"
    |   genRegRepr(GeneralReg (0w3, false), SZDWord) = "ebx"
    |   genRegRepr(GeneralReg (0w4, false), SZDWord) = "esp"
    |   genRegRepr(GeneralReg (0w5, false), SZDWord) = "ebp"
    |   genRegRepr(GeneralReg (0w6, false), SZDWord) = "esi"
    |   genRegRepr(GeneralReg (0w7, false), SZDWord) = "edi"
    |   genRegRepr(GeneralReg (reg, true),  SZDWord) = "r" ^ Int.toString(Word8.toInt reg +8) ^ "d"
    |   genRegRepr(GeneralReg (0w0, false), SZQWord) = "rax"
    |   genRegRepr(GeneralReg (0w1, false), SZQWord) = "rcx"
    |   genRegRepr(GeneralReg (0w2, false), SZQWord) = "rdx"
    |   genRegRepr(GeneralReg (0w3, false), SZQWord) = "rbx"
    |   genRegRepr(GeneralReg (0w4, false), SZQWord) = "rsp"
    |   genRegRepr(GeneralReg (0w5, false), SZQWord) = "rbp"
    |   genRegRepr(GeneralReg (0w6, false), SZQWord) = "rsi"
    |   genRegRepr(GeneralReg (0w7, false), SZQWord) = "rdi"
    |   genRegRepr(GeneralReg (reg, true),  SZQWord) = "r" ^ Int.toString(Word8.toInt reg +8)
    |   genRegRepr(GeneralReg (0w0, false), SZWord) = "ax"
    |   genRegRepr(GeneralReg (0w1, false), SZWord) = "cx"
    |   genRegRepr(GeneralReg (0w2, false), SZWord) = "dx"
    |   genRegRepr(GeneralReg (0w3, false), SZWord) = "bx"
    |   genRegRepr(GeneralReg (0w4, false), SZWord) = "sp"
    |   genRegRepr(GeneralReg (0w5, false), SZWord) = "bp"
    |   genRegRepr(GeneralReg (0w6, false), SZWord) = "si"
    |   genRegRepr(GeneralReg (0w7, false), SZWord) = "di"
    |   genRegRepr(GeneralReg (reg, true),  SZWord) = "r" ^ Int.toString(Word8.toInt reg +8) ^ "w"
    |   genRegRepr _ = "unknown" (* Suppress warning because word values are not exhaustive. *)

    and fpRegRepr(FloatingPtReg n) = "fp" ^ Word8.toString n
    
    and xmmRegRepr(SSE2Reg n) = "xmm" ^ Word8.toString n

    fun regRepr(GenReg r) = genRegRepr (r, sz32_64)
    |   regRepr(FPReg r) = fpRegRepr r
    |   regRepr(XMMReg r) = xmmRegRepr r

    (* Install a pretty printer.  This is simply for when this code is being
       run under the debugger.  N.B. We need PolyML.PrettyString here. *)
    val () = PolyML.addPrettyPrinter(fn _ => fn _ => fn r => PolyML.PrettyString(regRepr r))
    
    datatype argType = ArgGeneral | ArgFP

    structure RegSet =
    struct
        (* Implement a register set as a bit mask. *)
        datatype regSet = RegSet of word
        fun singleton r = RegSet(0w1 << Word.fromInt(nReg r))
        fun regSetUnion(RegSet r1, RegSet r2) = RegSet(Word.orb(r1, r2))
        fun regSetIntersect(RegSet r1, RegSet r2) = RegSet(Word.andb(r1, r2))

        local
            fun addReg(acc, n) =
                if n = regs then acc else addReg(regSetUnion(acc, singleton(regN n)), n+1)
        in
            val allRegisters = addReg(RegSet 0w0, 0)
        end

        val noRegisters = RegSet 0w0

        fun inSet(r, rs) = regSetIntersect(singleton r, rs) <> noRegisters
        
        fun regSetMinus(RegSet s1, RegSet s2) = RegSet(Word.andb(s1, Word.notb s2))
        
        val listToSet = List.foldl (fn(r, rs) => regSetUnion(singleton r, rs)) noRegisters

        val generalRegisters = (* Registers checked by the GC. *)
            if isX64
            then listToSet(map GenReg [eax, ecx, edx, ebx, esi, edi, r8, r9, r10, r11, r12, r13, r14])
            else listToSet(map GenReg [eax, ecx, edx, ebx, esi, edi])
        
        (* The floating point stack.  Note that this excludes one item so it is always
           possible to load a value onto the top of the FP stack. *)
        val floatingPtRegisters =
            listToSet(map FPReg [fp0, fp1, fp2, fp3, fp4, fp5, fp6(*, fp7*)])

        val sse2Registers =
            listToSet(map XMMReg [xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6])

        fun isAllRegs rs = rs = allRegisters

        fun setToList (RegSet regSet)=
        let
            fun testBit (n, bit, res) =
                if n = regs
                then res
                else testBit(n+1, bit << 0w1, 
                        if (regSet andb bit) <> 0w0
                        then regN n :: res else res)
        in
            testBit(0, 0w1, [])
        end

        val cardinality = List.length o setToList

        (* Choose one of the set.  This chooses the least value which means that
           the ordering of the registers is significant.  This is a hot-spot
           so is coded directly with the word operations. *)
        fun oneOf(RegSet regSet) =
        let
            fun find(n, bit) =
                if n = Word.fromInt regs then raise InternalError "oneOf: empty"
                else if Word.andb(bit, regSet) <> 0w0 then n
                else find(n+0w1, Word.<<(bit, 0w1))
        in
            regN(Word.toInt(find(0w0, 0w1)))
        end
        
        fun regSetRepr regSet =
        let
            val regs = setToList regSet
        in
            "[" ^ String.concatWith "," (List.map regRepr regs) ^ "]"
        end
        
        (* Install a pretty printer for when this code is being debugged. *)
        val () = PolyML.addPrettyPrinter(fn _ => fn _ => fn r => PolyML.PrettyString(regSetRepr r))
     end

    open RegSet

    datatype arithOp = ADD | OR (*|ADC | SBB*) | AND | SUB | XOR | CMP
  
    fun arithOpToWord ADD = 0w0: Word8.word
    |   arithOpToWord OR  = 0w1
    |   arithOpToWord AND = 0w4
    |   arithOpToWord SUB = 0w5
    |   arithOpToWord XOR = 0w6
    |   arithOpToWord CMP = 0w7

    fun arithOpRepr ADD = "Add"
    |   arithOpRepr OR  = "Or"
    |   arithOpRepr AND = "And"
    |   arithOpRepr SUB = "Sub"
    |   arithOpRepr XOR = "Xor"
    |   arithOpRepr CMP = "Cmp"

    datatype shiftType = SHL | SHR | SAR

    fun shiftTypeToWord SHL = 0w4: Word8.word
    |   shiftTypeToWord SHR = 0w5
    |   shiftTypeToWord SAR = 0w7

    fun shiftTypeRepr SHL = "Shift Left Logical"
    |   shiftTypeRepr SHR = "Shift Right Logical"
    |   shiftTypeRepr SAR = "Shift Right Arithemetic"

    datatype repOps = CMPSB | MOVSB | MOVSL | STOSB | STOSL
    
    fun repOpsToWord CMPSB = 0wxa6: Word8.word
    |   repOpsToWord MOVSB = 0wxa4
    |   repOpsToWord MOVSL = 0wxa5
    |   repOpsToWord STOSB = 0wxaa
    |   repOpsToWord STOSL = 0wxab

    fun repOpsRepr CMPSB = "CompareBytes"
    |   repOpsRepr MOVSB = "MoveBytes"
    |   repOpsRepr MOVSL = "MoveWords"
    |   repOpsRepr STOSB = "StoreBytes"
    |   repOpsRepr STOSL = "StoreWords"

    datatype fpOps = FADD | FMUL | FCOM | FCOMP | FSUB | FSUBR | FDIV | FDIVR

    fun fpOpToWord FADD  = 0w0: Word8.word
    |   fpOpToWord FMUL  = 0w1
    |   fpOpToWord FCOM  = 0w2
    |   fpOpToWord FCOMP = 0w3
    |   fpOpToWord FSUB  = 0w4
    |   fpOpToWord FSUBR = 0w5
    |   fpOpToWord FDIV  = 0w6
    |   fpOpToWord FDIVR = 0w7

    fun fpOpRepr FADD  = "FPAdd"
    |   fpOpRepr FMUL  = "FPMultiply"
    |   fpOpRepr FCOM  = "FPCompare"
    |   fpOpRepr FCOMP = "FPCompareAndPop"
    |   fpOpRepr FSUB  = "FPSubtract"
    |   fpOpRepr FSUBR = "FPReverseSubtract"
    |   fpOpRepr FDIV  = "FPDivide"
    |   fpOpRepr FDIVR = "FPReverseDivide"

    datatype fpUnaryOps = FCHS | FABS | FLD1 | FLDZ
    
    fun fpUnaryToWords FCHS   = {rm=0w0:Word8.word, nnn=0w4: Word8.word}
    |   fpUnaryToWords FABS   = {rm=0w1, nnn=0w4}
    |   fpUnaryToWords FLD1   = {rm=0w0, nnn=0w5}
    |   fpUnaryToWords FLDZ   = {rm=0w6, nnn=0w5}

    fun fpUnaryRepr FCHS   = "FPChangeSign"
    |   fpUnaryRepr FABS   = "FPAbs"
    |   fpUnaryRepr FLD1   = "FPLoadOne"
    |   fpUnaryRepr FLDZ   = "FPLoadZero"

    datatype branchOps = JO | JNO | JE | JNE | JL | JGE | JLE | JG | JB | JNB | JNA | JA | JP | JNP

    fun branchOpToWord JO   = 0wx0: Word8.word
    |   branchOpToWord JNO  = 0wx1
    |   branchOpToWord JB   = 0wx2
    |   branchOpToWord JNB  = 0wx3
    |   branchOpToWord JE   = 0wx4
    |   branchOpToWord JNE  = 0wx5
    |   branchOpToWord JNA  = 0wx6
    |   branchOpToWord JA   = 0wx7
    |   branchOpToWord JP   = 0wxa
    |   branchOpToWord JNP  = 0wxb
    |   branchOpToWord JL   = 0wxc
    |   branchOpToWord JGE  = 0wxd
    |   branchOpToWord JLE  = 0wxe
    |   branchOpToWord JG   = 0wxf
 
    fun branchOpRepr JO = "JumpOverflow"
    |   branchOpRepr JNO = "JumpNotOverflow"
    |   branchOpRepr JE = "JumpEqual"
    |   branchOpRepr JNE = "JumpNotEqual"
    |   branchOpRepr JL = "JumpLess"
    |   branchOpRepr JGE = "JumpGreaterOrEqual"
    |   branchOpRepr JLE = "JumpLessOrEqual"
    |   branchOpRepr JG = "JumpGreater"
    |   branchOpRepr JB = "JumpBefore"
    |   branchOpRepr JNB= "JumpNotBefore"
    |   branchOpRepr JNA = "JumpNotAfter"
    |   branchOpRepr JA = "JumpAfter"
    |   branchOpRepr JP = "JumpParity"
    |   branchOpRepr JNP = "JumpNoParity"

    datatype sse2Operations =
        SSE2Move | SSE2Comp | SSE2Add | SSE2Sub | SSE2Mul | SSE2Div | SSE2Xor |
        SSE2And | SSE2MoveSingle | SSE2DoubleToFloat
    
    fun sse2OpRepr SSE2Move = "SSE2Move"
    |   sse2OpRepr SSE2Comp = "SSE2Comp"
    |   sse2OpRepr SSE2Add  = "SSE2Add"
    |   sse2OpRepr SSE2Sub  = "SSE2Sub"
    |   sse2OpRepr SSE2Mul  = "SSE2Mul"
    |   sse2OpRepr SSE2Div  = "SSE2Div"
    |   sse2OpRepr SSE2Xor  = "SSE2Xor"
    |   sse2OpRepr SSE2And  = "SSE2And"
    |   sse2OpRepr SSE2MoveSingle = "SSE2MoveSingle"
    |   sse2OpRepr SSE2DoubleToFloat = "SSE2DoubleToFloat"

    (* Primary opCodes.  N.B. only opCodes actually used are listed here.
       If new instruction are added check they will be handled by the
       run-time system in the event of trap. *)
    datatype opCode =
        Group1_8_A
    |   Group1_32_A
    |   Group1_8_a
    |   JMP_8
    |   JMP_32
    |   CALL_32
    |   MOVL_A_R
    |   MOVL_R_A
    |   MOVL_R_A16
    |   MOVL_R_A32
    |   MOVB_R_A of {forceRex: bool}
    |   PUSH_R of Word8.word
    |   POP_R  of Word8.word
    |   Group5
    |   NOP
    |   LEAL
    |   MOVL_32_64_R of Word8.word
    |   MOVL_32_A
    |   MOVB_8_A
    |   POP_A
    |   RET
    |   RET_16
    |   CondJump of branchOps
    |   CondJump32 of branchOps
    |   Arith of arithOp * Word8.word
    |   Group3_A
    |   Group3_a
    |   Group2_8_A
    |   Group2_CL_A
    |   Group2_1_A
    |   PUSH_8
    |   PUSH_32
    |   TEST_ACC8
    |   LOCK_XADD
    |   FPESC of Word8.word
    |   XCHNG
    |   REP (* Rep prefix *)
    |   MOVZB (* Needs escape code. *)
    |   MOVZW (* Needs escape code. *)
    |   MOVL_A_R32 (* As MOVL_A_R but without RAX.W *)
    |   IMUL (* Needs escape code. *)
    |   SSE2StoreSingle (* movss with memory destination - needs escape sequence. *)
    |   SSE2StoreDouble (* movsd with memory destination - needs escape sequence. *)
    |   CQO_CDQ (* Sign extend before divide.. *)
    |   SSE2Ops of sse2Operations (* SSE2 instructions. *)
    |   CVTSI2SD
    |   HLT     (* End of code marker. *)
    |   IMUL_C8
    |   IMUL_C32

    fun opToInt Group1_8_A    =  0wx83
    |   opToInt Group1_32_A   =  0wx81
    |   opToInt Group1_8_a    =  0wx80
    |   opToInt JMP_8         =  0wxeb
    |   opToInt JMP_32        =  0wxe9
    |   opToInt CALL_32       =  0wxe8
    |   opToInt MOVL_A_R      =  0wx8b
    |   opToInt MOVL_R_A      =  0wx89
    |   opToInt MOVL_R_A16    =  0wx89 (* Also has an OPSIZE prefix. *)
    |   opToInt MOVL_R_A32    =  0wx89 (* Suppresses the REX.W prefix. *)
    |   opToInt (MOVB_R_A _)  =  0wx88
    |   opToInt (PUSH_R reg)  =  0wx50 + reg
    |   opToInt (POP_R  reg)  =  0wx58 + reg
    |   opToInt Group5        =  0wxff
    |   opToInt NOP           =  0wx90
    |   opToInt LEAL          =  0wx8d
    |   opToInt (MOVL_32_64_R reg) =  0wxb8 + reg
    |   opToInt MOVL_32_A     =  0wxc7
    |   opToInt MOVB_8_A      =  0wxc6
    |   opToInt POP_A         =  0wx8f
    |   opToInt RET           = 0wxc3
    |   opToInt RET_16        = 0wxc2
    |   opToInt (CondJump opc) = 0wx70 + branchOpToWord opc
    |   opToInt (CondJump32 opc) = 0wx80 + branchOpToWord opc (* Needs 0F prefix *)
    |   opToInt (Arith (ao,dw)) = arithOpToWord ao * 0w8 + dw
    |   opToInt Group3_A      = 0wxf7
    |   opToInt Group3_a      = 0wxf6
    |   opToInt Group2_8_A    = 0wxc1
    |   opToInt Group2_1_A    = 0wxd1
    |   opToInt Group2_CL_A   = 0wxd3
    |   opToInt PUSH_8        = 0wx6a
    |   opToInt PUSH_32       = 0wx68
    |   opToInt TEST_ACC8     = 0wxa8
    |   opToInt LOCK_XADD     = 0wxC1 (* Needs lock and escape prefixes. *)
    |   opToInt (FPESC n)     = 0wxD8 orb8 n
    |   opToInt XCHNG         = 0wx87
    |   opToInt REP           = 0wxf3
    |   opToInt MOVZB         = 0wxb6 (* Needs escape code. *)
    |   opToInt MOVZW         = 0wxb7 (* Needs escape code. *)
    |   opToInt MOVL_A_R32    = 0wx8b
    |   opToInt IMUL          = 0wxaf (* Needs escape code. *)
    |   opToInt SSE2StoreSingle     = 0wx11 (* Needs F3 0F escape. *)
    |   opToInt SSE2StoreDouble     = 0wx11 (* Needs F2 0F escape. *)
    |   opToInt CQO_CDQ       = 0wx99
    |   opToInt (SSE2Ops SSE2Move) = 0wx10 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2Comp) = 0wx2E (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2Add)  = 0wx58 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2Sub)  = 0wx5c (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2Mul)  = 0wx59 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2Div)  = 0wx5e (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2And)  = 0wx54 (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2Xor)  = 0wx57 (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2MoveSingle)  = 0wx5A (* Needs F3 0F escape. *)
    |   opToInt (SSE2Ops SSE2DoubleToFloat)  = 0wx5A (* Needs F2 0F escape. *)
    |   opToInt CVTSI2SD      = 0wx2a (* Needs F2 0F escape. *)
    |   opToInt HLT           = 0wxf4
    |   opToInt IMUL_C8       = 0wx6b
    |   opToInt IMUL_C32      = 0wx69

    datatype mode =
        Based0   (* mod = 0 *)
    |   Based8   (* mod = 1 *)
    |   Based32  (* mod = 2 *)
    |   Register (* mod = 3 *) ;

    (* Put together the three fields which make up the mod r/m byte. *)
    fun modrm (md : mode, rg: Word8.word, rm : Word8.word) : Word8.word =
    let
        val _ = if rg > 0w7 then raise InternalError "modrm: bad rg" else ()
        val _ = if rm > 0w7 then raise InternalError "modrm: bad rm" else ()
        val modField: Word8.word = 
            case md of 
                Based0   => 0w0
            |   Based8   => 0w1
            |   Based32  => 0w2
            |   Register => 0w3
    in
        (modField <<- 0w6) orb8 (rg <<- 0w3) orb8 rm
    end

    (* REX prefix *)
    fun rex {w,r,x,b} =
        0wx40 orb8 (if w then 0w8 else 0w0) orb8 (if r then 0w4 else 0w0) orb8
            (if x then 0w2 else 0w0) orb8 (if b then 0w1 else 0w0)

    (* The X86 has the option to include an index register and to scale it. *)
    datatype indexType =
        NoIndex | Index1 of genReg | Index2 of genReg | Index4 of genReg | Index8 of genReg

    (* Lock, Opsize and REPNE prefixes come before the REX. *)
    fun opcodePrefix LOCK_XADD                  = [0wxF0] (* Requires LOCK prefix. *)
    |   opcodePrefix MOVL_R_A16                 = [0wx66] (* Requires OPSIZE prefix. *)
    |   opcodePrefix SSE2StoreSingle            = [0wxf3]
    |   opcodePrefix SSE2StoreDouble            = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2Comp)          = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2And)           = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2Xor)           = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2MoveSingle)    = [0wxf3]
    |   opcodePrefix(SSE2Ops _)                 = [0wxf2]
    |   opcodePrefix CVTSI2SD                   = [0wxf2]
    |   opcodePrefix _                          = []

    (* A few instructions require an escape.  Escapes come after the REX. *)
    fun escapePrefix MOVZB                      = [0wx0f]
    |   escapePrefix MOVZW                      = [0wx0f]
    |   escapePrefix LOCK_XADD                  = [0wx0f]
    |   escapePrefix IMUL                       = [0wx0f]
    |   escapePrefix(CondJump32 _)              = [0wx0f]
    |   escapePrefix SSE2StoreSingle            = [0wx0f]
    |   escapePrefix SSE2StoreDouble            = [0wx0f]
    |   escapePrefix(SSE2Ops SSE2Comp)          = [0wx0f]
    |   escapePrefix(SSE2Ops SSE2And)           = [0wx0f]
    |   escapePrefix(SSE2Ops SSE2Xor)           = [0wx0f]
    |   escapePrefix(SSE2Ops SSE2MoveSingle)    = [0wx0f]
    |   escapePrefix(SSE2Ops _)                 = [0wx0f]
    |   escapePrefix CVTSI2SD                   = [0wx0f]
    |   escapePrefix _                          = []

    (* Generate an opCode byte after doing any pending operations. *)
    fun opCodeBytes(opb:opCode, rx) =
    let
        val rexByte = 
            case rx of
                NONE => []
            |   SOME rxx =>
                if isX64 then [rex rxx]
                else raise InternalError "opCodeBytes: rex prefix in 32 bit mode";
    in
        opcodePrefix opb @ rexByte @ escapePrefix opb @ [opToInt opb]
    end

    fun rexByte(opb, rrX, rbX, riX) =
    let
        (* We need a rex prefix if we need to set the length to 64-bit. *)
        val need64bit =
            case opb of
                Group1_8_A => isX64 (* Arithmetic operations - must be 64-bit *)
            |   Group1_32_A => isX64 (* Arithmetic operations - must be 64-bit *)
            |   Group2_1_A => isX64 (* 1-bit shifts - must be 64-bit *)
            |   Group2_8_A => isX64 (* n-bit shifts - must be 64-bit *)
            |   Group2_CL_A => isX64 (* Shifts by value in CL *)
            |   Group3_A => isX64 (* Test, Not, Mul etc. *)
            |   Arith _ => isX64
            |   MOVL_A_R => isX64 (* Needed *)
            |   MOVL_R_A => isX64 (* Needed *)
            |   XCHNG => isX64
            |   LEAL => isX64 (* Needed to ensure the result is 64-bits *)
            |   MOVZB => isX64 (* Needed to ensure the result is 64-bits *)
            |   MOVZW => isX64 (* Needed to ensure the result is 64-bits *)
            |   MOVL_32_64_R _ => isX64 (* Needed *)
            |   MOVL_32_A => isX64 (* Needed *)
            |   IMUL => isX64 (* Needed to ensure the result is 64-bits *)
            |   LOCK_XADD => isX64 (* Needed to ensure the result is 64-bits *)
            |   CQO_CDQ => isX64 (* It's only CQO if there's a Rex prefix. *)
            |   CVTSI2SD => isX64 (* This affects the size of the integer source. *)
            |   IMUL_C8 => isX64
            |   IMUL_C32 => isX64
            (* Group5 - We only use 2/4/6 and they don't need prefix *)
            |   _ => false
        (* If we are using MOVB_R_A with SIL or DIL we need to force a REX prefix.
           That's only possible in 64-bit mode. *)
        val forceRex =
            case opb of
                MOVB_R_A {forceRex=true} => (* This is allowed in X86/64 but not in X86/32. *)
                    if isX64
                    then true
                    else raise InternalError "rexByte: MOVB_R_A accessing low order byte of ESI/EDI"
            |   _ => false
    in
        if need64bit orelse rrX orelse rbX orelse riX orelse forceRex
        then [rex{w=need64bit, r=rrX, b=rbX, x = riX}]
        else []
    end

    (* Register/register operation. *)
    fun opReg(opb:opCode, (*dest*)GeneralReg(rrC, rrX), (*source*)GeneralReg(rbC, rbX)) =
    let
        val pref = opcodePrefix opb (* Any opsize or lock prefix. *)
        val rex = rexByte(opb, rrX, rbX, false)
        val esc = escapePrefix opb (* Generate the ESCAPE code if needed. *)
        val opc = opToInt opb
        val mdrm = modrm(Register, rrC, rbC)
    in
        pref @ rex @ esc @ [opc, mdrm]
    end

    (* Operations on a register where the second "register" is actually an operation code. *)
    fun opRegPlus2(opb:opCode, rd: genReg, op2: Word8.word) =
    let
        val (rrC, rrX) = getReg rd
        val pref = opcodePrefix opb (* Any opsize or lock prefix. *)
        val rex = rexByte(opb, false, rrX, false)
        val opc = opToInt opb
        val mdrm = modrm(Register, op2, rrC)
    in
        pref @ rex @ [opc, mdrm]
    end

    local
        (* General instruction form with modrm and optional sib bytes.  rb is an option since the
           base register may be omitted.  This is used with LEA to tag integers. *)
        fun opIndexedGen (opb:opCode, offset: LargeInt.int, rb: genReg option, ri: indexType, (rrC, rrX)) =
        let
            (* Base encoding.  (Based0, 0w5) means "no base" so if we need ebp as the
               base we have to use Based8 at least. *)
            val (offsetCode, rbC, rbX) =
                case rb of
                    NONE => (Based0, 0w5 (* no base register *), false)
                |   SOME rb =>
                    let
                        val (rbC, rbX) = getReg rb
                        val base =
                            if offset = 0 andalso rbC <> 0wx5 (* Can't use ebp with Based0 *)
                            then Based0    (* no disp field *)
                            else if is8BitL offset
                            then Based8   (* use 8-bit disp field *)
                            else Based32 (* use 32-bit disp field *)
                    in
                        (base, rbC, rbX)
                    end

            (* Index coding.  esp can't be used as an index so (0w4, false) means "no index".
               But r12 (0w4, true) CAN be. *)
            val ((riC, riX), scaleFactor) =
                case ri of
                    NoIndex  => ((0w4, false), 0w0)
                |   Index1 i => (getReg i, 0w0)
                |   Index2 i => (getReg i, 0w1)
                |   Index4 i => (getReg i, 0w2)
                |   Index8 i => (getReg i, 0w3)
            
            (* If the base register is esp or r12 we have to use a sib byte even if
               there's no index.  That's because 0w4 as a base register means "there's
               a SIB byte". *)
            val modRmAndOptionalSib =
                if rbC = 0w4  (* Code for esp and r12 *) orelse riC <> 0w4 orelse riX
                then
                let
                    val mdrm = modrm(offsetCode, rrC, 0w4 (* s-i-b *))
                    val sibByte = (scaleFactor <<- 0w6) orb8 (riC <<- 0w3) orb8 rbC
                in
                    [mdrm, sibByte]
                end
                else [modrm(offsetCode, rrC, rbC)]
    
            (* Generate the disp field (if any) *)
            val dispField =
                case (offsetCode, rb) of
                    (Based8, _)  => [Word8.fromLargeInt offset]
                |   (Based32, _) => int32Signed offset
                |   (_, NONE)    => (* 32 bit absolute used as base *) int32Signed offset
                |    _ => []
        in
            opcodePrefix opb @ rexByte(opb, rrX, rbX, riX) @ escapePrefix opb @
                opToInt opb :: modRmAndOptionalSib @ dispField
        end
    in
        fun opEA(opb, offset, rb, r) = opIndexedGen(opb, offset, SOME rb, NoIndex, getReg r)
        (* Generate a opcode plus a second modrm byte but where the "register" field in
           the modrm byte is actually a code.  *)
        and opPlus2(opb, offset, rb, op2) = opIndexedGen(opb, offset, SOME rb, NoIndex, (op2, false))
        
        fun opIndexed (opb, offset, rb, ri, rd) =
            opIndexedGen(opb, offset, rb, ri, getReg rd) 
        
        fun opAddress(opb, offset, rb, ri, rd) = opIndexedGen (opb, offset, SOME rb, ri, getReg rd)
        and mMXAddress(opb, offset, rb, ri, SSE2Reg rrC) = opIndexedGen(opb, offset, SOME rb, ri, (rrC, false))
        and opAddressPlus2(opb, offset, rb, ri, op2) =
            opIndexedGen(opb, offset, SOME rb, ri, (op2, false))
    end

    fun immediateOperand (opn: arithOp, rd: genReg, imm: LargeInt.int) =
    if is8BitL imm
    then (* Can use one byte immediate *)
        opRegPlus2(Group1_8_A, rd, arithOpToWord opn) @ [Word8.fromLargeInt imm]
    else if is32bit imm
    then (* Need 32 bit immediate. *)
        opRegPlus2(Group1_32_A, rd, arithOpToWord opn) @ int32Signed imm
    else (* It won't fit in the immediate; put it in the non-address area. *)
    let
        val (rc, rx) = getReg rd
        val opb = opCodeBytes(Arith (opn, 0w3 (* r/m to reg *)), SOME{w=true, r=rx, b=false, x = false})
        val mdrm = modrm (Based0, rc, 0w5 (* PC-relative *))
    in
        opb @ [mdrm] @ int32Signed(tag 0)
    end
    
    fun arithOpReg(opn: arithOp, rd: genReg, rs: genReg) = opReg (Arith (opn, 0w3 (* r/m to reg *)), rd, rs)

    type handlerLab = addrs ref

    datatype callKinds =
        Recursive           (* The function calls itself. *)
    |   ConstantCode of machineWord (* A function that doesn't need a closure *)
    |   FullCall            (* Full closure call *)
    |   DirectReg of genReg  (* Currently used within ForeignCall to call the RTS *)

    fun floatingPtOp{escape, md, nnn, rm} =
        opCodeBytes(FPESC escape, NONE) @ [(md <<- 0w6) orb8 (nnn <<- 0w3) orb8 rm]

    datatype trapEntries =
        StackOverflowCall
    |   StackOverflowCallEx
    |   HeapOverflowCall

    (* RTS call.  We need to save any registers that may contain addresses to the stack.
       All the registers are preserved but not seen by the GC. *)
    fun rtsCall(rtsEntry, regSet) =
        let
            val entry =
                case rtsEntry of
                    StackOverflowCall   => memRegStackOverflowCall
                |   StackOverflowCallEx => memRegStackOverflowCallEx
                |   HeapOverflowCall    => memRegHeapOverflowCall
            val regSet = List.foldl(fn (r, a) => (0w1 << Word.fromInt(nReg(GenReg r))) orb a) 0w0 regSet
            val callInstr =
                opPlus2(Group5, LargeInt.fromInt entry, ebp, 0w2 (* call *))
            val regSetInstr =
                if regSet >= 0w256
                then [0wxca, (* This is actually a FAR RETURN *)
                        wordToWord8 regSet, (* Low byte*) wordToWord8 (regSet >> 0w8) (* High byte*)]
                else if regSet <> 0w0
                then [0wxcd, (* This is actually INT n *) wordToWord8 regSet]
                else []
        in
            callInstr @ regSetInstr
        end

    (* Operations. *)
    type cases = word * label

    type memoryAddress = { base: genReg, offset: int, index: indexType }

    datatype branchPrediction = PredictNeutral | PredictTaken | PredictNotTaken

    datatype 'reg regOrMemoryArg =
        RegisterArg of 'reg
    |   MemoryArg of memoryAddress
    |   NonAddressConstArg of LargeInt.int
    |   AddressConstArg of machineWord
    
    datatype nonWordSize = Size8Bit | Size16Bit | Size32Bit
    and fpSize = SinglePrecision | DoublePrecision

    datatype operation =
        MoveToRegister of { source: genReg regOrMemoryArg, output: genReg }
    |   LoadNonWord of { size: nonWordSize, source: memoryAddress, output: genReg }
    |   PushToStack of genReg regOrMemoryArg
    |   PopR of genReg
    |   ArithToGenReg of { opc: arithOp, output: genReg, source: genReg regOrMemoryArg }
    |   ArithMemConst of { opc: arithOp, offset: int, base: genReg, source: LargeInt.int }
    |   ArithMemLongConst of { opc: arithOp, offset: int, base: genReg, source: machineWord }
    |   ShiftConstant of { shiftType: shiftType, output: genReg, shift: Word8.word }
    |   ShiftVariable of { shiftType: shiftType, output: genReg } (* Shift amount is in ecx *)
    |   ConditionalBranch of { test: branchOps, label: label, predict: branchPrediction }
    |   LockMutableSegment of genReg
    |   LoadAddress of { output: genReg, offset: int, base: genReg option, index: indexType }
    |   TestTagR of genReg
    |   TestByteMem of { base: genReg, offset: int, bits: word }
    |   CallRTS of {rtsEntry: trapEntries, saveRegs: genReg list }
    |   StoreRegToMemory of { toStore: genReg, address: memoryAddress }
    |   StoreConstToMemory of { toStore: LargeInt.int, address: memoryAddress }
    |   StoreLongConstToMemory of { toStore: machineWord, address: memoryAddress }
    |   StoreNonWord of { size: nonWordSize, toStore: genReg, address: memoryAddress }
    |   StoreNonWordConst of { size: nonWordSize, toStore: LargeInt.int, address: memoryAddress }
    |   AllocStore of { size: int, output: genReg, saveRegs: genReg list }
    |   AllocStoreVariable of { output: genReg, saveRegs: genReg list }
    |   StoreInitialised
    |   CallFunction of callKinds
    |   JumpToFunction of callKinds
    |   ReturnFromFunction of int
    |   RaiseException
    |   UncondBranch of label
    |   ResetStack of { numWords: int, preserveCC: bool }
    |   JumpLabel of label
    |   LoadLabelAddress of { label: label, output: genReg }
    |   RepeatOperation of repOps
    |   DivideAccR of {arg: genReg, isSigned: bool }
    |   DivideAccM of {base: genReg, offset: int, isSigned: bool }
    |   AtomicXAdd of {base: genReg, output: genReg}
    |   FPLoadFromMemory of { address: memoryAddress, precision: fpSize }
    |   FPLoadFromFPReg of { source: fpReg, lastRef: bool }
    |   FPLoadFromConst of real
    |   FPStoreToFPReg of { output: fpReg, andPop: bool }
    |   FPStoreToMemory of { address: memoryAddress, precision: fpSize, andPop: bool }
    |   FPArithR of { opc: fpOps, source: fpReg }
    |   FPArithConst of { opc: fpOps, source: machineWord }
    |   FPArithMemory of { opc: fpOps, base: genReg, offset: int }
    |   FPUnary of fpUnaryOps
    |   FPStatusToEAX
    |   FPLoadInt of { base: genReg, offset: int }
    |   FPFree of fpReg
    |   MultiplyR of { source: genReg regOrMemoryArg, output: genReg }
    |   XMMArith of { opc: sse2Operations, source: xmmReg regOrMemoryArg, output: xmmReg }
    |   XMMStoreToMemory of { toStore: xmmReg, address: memoryAddress, precision: fpSize }
    |   XMMConvertFromInt of { source: genReg, output: xmmReg }
    |   SignExtendForDivide
    |   XChng of { reg: genReg, arg: genReg regOrMemoryArg }
    |   Negative of { output: genReg }
    |   JumpTable of { cases: label list, jumpSize: jumpSize ref }
    |   IndexedJumpCalc of { addrReg: genReg, indexReg: genReg, jumpSize: jumpSize ref }

    and jumpSize = JumpSize2 | JumpSize8

    type operations = operation list

    fun printOperation(operation, stream) =
    let
        fun printGReg r = stream(genRegRepr(r, sz32_64))
        val printFPReg = stream o fpRegRepr
        and printXMMReg = stream o xmmRegRepr
        fun printBaseOffset(b, x, i) =
        (
            stream(Int.toString i); stream "("; printGReg b; stream ")";
            case x of
                NoIndex => ()
            |   Index1 x => (stream "["; printGReg x; stream "]")
            |   Index2 x => (stream "["; printGReg x; stream "*2]")
            |   Index4 x => (stream "["; printGReg x; stream "*4]")
            |   Index8 x => (stream "["; printGReg x; stream "*8]")
        )
        fun printMemAddress({ base, offset, index }) = printBaseOffset(base, index, offset)
        
        fun printRegOrMemoryArg printReg (RegisterArg r) = printReg r
        |   printRegOrMemoryArg _ (MemoryArg{ base, offset, index }) = printBaseOffset(base, index, offset)
        |   printRegOrMemoryArg _ (NonAddressConstArg c) = stream(LargeInt.toString c)
        |   printRegOrMemoryArg _ (AddressConstArg c) = stream(Address.stringOfWord c)
        
        fun printCallKind Recursive = stream "Recursive"
        |   printCallKind (ConstantCode w) = (stream "code="; stream(stringOfWord w))
        |   printCallKind FullCall = stream "via ClosureReg"
        |   printCallKind (DirectReg reg) = printGReg reg
        
        fun printSize Size8Bit = "Byte"
        |   printSize Size16Bit = "16Bit"
        |   printSize Size32Bit = "32Bit"
     in
        case operation of
            MoveToRegister { source, output } =>
                (stream "MoveRR "; printGReg output; stream " <= "; printRegOrMemoryArg printGReg source)

        |   LoadNonWord { size, source, output } =>
                (stream "Load"; printSize size; stream " "; printGReg output; stream " <= "; printMemAddress source )

        |   ArithToGenReg { opc, output, source } =>
                (stream (arithOpRepr opc ^ "RR "); printGReg output; stream " <= "; printRegOrMemoryArg printGReg source )

        |   ArithMemConst { opc, offset, base, source } =>
            (
                stream (arithOpRepr opc ^ "MC "); printBaseOffset(base, NoIndex, offset);
                stream " "; stream(LargeInt.toString source)
            )

        |   ArithMemLongConst { opc, offset, base, source } =>
            (
                stream (arithOpRepr opc ^ "MC ");
                printBaseOffset(base, NoIndex, offset);
                stream " <= "; stream(Address.stringOfWord source)
            )

        |   ShiftConstant { shiftType, output, shift } =>
            (
                stream(shiftTypeRepr shiftType); stream " "; printGReg output;
                stream " by "; stream(Word8.toString shift)
            )

        |   ShiftVariable { shiftType, output } => (* Shift amount is in ecx *)
            (
                stream(shiftTypeRepr shiftType); stream " "; printGReg output; stream " by ECX"
            )

        |   ConditionalBranch { test, label=Label{labelNo, ...}, predict } =>
            (
                stream(branchOpRepr test); stream " L"; stream(Int.toString labelNo);
                case predict of
                    PredictNeutral => ()
                |   PredictTaken => stream " PredictTaken"
                |   PredictNotTaken => stream " PredictNotTaken"
            )

        |   LockMutableSegment reg => (stream "LockMutableSegment "; printGReg reg)

        |   PushToStack source => (stream "Push "; printRegOrMemoryArg printGReg source)

        |   PopR dest => (stream "PopR "; printGReg dest)

        |   StoreRegToMemory { toStore, address } =>
            (
                stream "StoreRegToMemory "; printMemAddress address;
                stream " <= "; printGReg toStore
            )

        |   StoreConstToMemory { toStore, address } =>
            (
                stream "StoreConstToMemory "; printMemAddress address;
                stream " <= "; stream(LargeInt.toString toStore)
            )

        |   StoreLongConstToMemory { address, toStore } =>
            (
                stream "StoreLongConstToMemory "; printMemAddress address; stream " <= "; stream(Address.stringOfWord toStore)
            )

        |   StoreNonWord { size, toStore, address } =>
            (
                stream "Store"; printSize size; stream " "; printMemAddress address;
                stream " <= "; stream(genRegRepr(toStore, SZByte))
            )

        |   StoreNonWordConst { size, toStore, address } =>
            (
                stream "StoreConst"; printSize size; stream " "; printMemAddress address;
                stream " <= "; stream(LargeInt.toString toStore)
            )

        |   LoadAddress{ output, offset, base, index } =>
            (
                stream "LoadAddress ";
                case base of NONE => () | SOME r => (printGReg r; stream " + ");
                stream(Int.toString offset);
                case index of
                    NoIndex => ()
                |   Index1 x => (stream " + "; printGReg x)
                |   Index2 x => (stream " + "; printGReg x; stream "*2 ")
                |   Index4 x => (stream " + "; printGReg x; stream "*4 ")
                |   Index8 x => (stream " + "; printGReg x; stream "*8 ");
                stream " => "; printGReg output
            )

        |   TestTagR reg => ( stream "TestTagR "; printGReg reg )

        |   TestByteMem { base, offset, bits } =>
                ( stream "TestByteMem "; printBaseOffset(base, NoIndex, offset); stream " 0x"; stream(Word.toString bits) )

        |   CallRTS {rtsEntry, ...} =>
            (
                stream "CallRTS ";
                case rtsEntry of
                    StackOverflowCall => stream "StackOverflowCall"
                |   HeapOverflowCall => stream "HeapOverflow"
                |   StackOverflowCallEx => stream "StackOverflowCallEx"
            )

        |   AllocStore { size, output, ... } =>
                (stream "AllocStore "; stream(Int.toString size); stream " => "; printGReg output )

        |   AllocStoreVariable { output, ...} => (stream "AllocStoreVariable "; printGReg output )
        
        |   StoreInitialised => stream "StoreInitialised"

        |   CallFunction callKind => (stream "CallFunction "; printCallKind callKind)

        |   JumpToFunction callKind => (stream "JumpToFunction "; printCallKind callKind)

        |   ReturnFromFunction argsToRemove =>
                (stream "ReturnFromFunction "; stream(Int.toString argsToRemove))

        |   RaiseException =>
                stream "RaiseException"
        |   UncondBranch(Label{labelNo, ...})=>
                (stream "UncondBranch L"; stream(Int.toString labelNo))
        |   ResetStack{numWords, preserveCC} =>
                (stream "ResetStack "; stream(Int.toString numWords); if preserveCC then stream " preserve CC" else ())
        |   JumpLabel(Label{labelNo, ...}) =>
                (stream "L"; stream(Int.toString labelNo); stream ":")
        |   LoadLabelAddress{ label=Label{labelNo, ...}, output } =>
                (stream "LoadLabelAddress L"; stream(Int.toString labelNo); stream "=>"; printGReg output)
        |   RepeatOperation repOp => (stream "Repeat "; stream(repOpsRepr repOp))
        |   DivideAccR{arg, isSigned} => ( stream(if isSigned then "DivideSigned" else "DivideUnsigned"); stream " "; printGReg arg)
        |   DivideAccM{base, offset, isSigned} => ( stream(if isSigned then "DivideSigned" else "DivideUnsigned"); stream " "; printBaseOffset(base, NoIndex, offset))
        |   AtomicXAdd{base, output} => (stream "LockedXAdd ("; printGReg base; stream ") <=> "; printGReg output)
        |   FPLoadFromMemory{address, precision=DoublePrecision} => (stream "FPLoadDouble "; printMemAddress address)
        |   FPLoadFromMemory{address, precision=SinglePrecision} => (stream "FPLoadSingle "; printMemAddress address)
        |   FPLoadFromFPReg {source, lastRef} =>
                (stream "FPLoad "; printFPReg source; if lastRef then stream " (LAST)" else())
        |   FPLoadFromConst const => (stream "FPLoad "; stream(Real.toString const) )
        |   FPStoreToFPReg{ output, andPop } =>
                (if andPop then stream "FPStoreAndPop => " else stream "FPStore => "; printFPReg output)
        |   FPStoreToMemory{ address, precision=DoublePrecision, andPop: bool } =>
            (
                if andPop then stream "FPStoreDoubleAndPop => " else stream "FPStoreDouble => ";
                printMemAddress address
            )
        |   FPStoreToMemory{ address, precision=SinglePrecision, andPop: bool } =>
            (
                if andPop then stream "FPStoreSingleAndPop => " else stream "FPStoreSingle => ";
                printMemAddress address
            )
        |   FPArithR{ opc, source } => (stream(fpOpRepr opc); stream " "; printFPReg source)
        |   FPArithConst{ opc, source } => (stream(fpOpRepr opc); stream(Address.stringOfWord source))
        |   FPArithMemory{ opc, base, offset } => (stream(fpOpRepr opc); stream " "; printBaseOffset(base, NoIndex, offset))
        |   FPUnary opc => stream(fpUnaryRepr opc)
        |   FPStatusToEAX => (stream "FPStatus "; printGReg eax)
        |   FPLoadInt { base, offset} => (stream "FPLoadInt "; printBaseOffset(base, NoIndex, offset))
        |   FPFree reg => (stream "FPFree "; printFPReg reg)
        |   MultiplyR {source, output } => (stream "MultiplyR"; stream " "; printRegOrMemoryArg printGReg source; stream " *=>"; printGReg output)
        |   XMMArith { opc, source, output } =>
            (
                stream (sse2OpRepr opc ^ "RM "); printXMMReg output; stream " <= "; printRegOrMemoryArg printXMMReg source
            )
        |   XMMStoreToMemory { toStore, address, precision=DoublePrecision } =>
            (
                stream "MoveDouble "; printXMMReg toStore; stream " => "; printMemAddress address
            )
        |   XMMStoreToMemory { toStore, address, precision=SinglePrecision } =>
            (
                stream "MoveSingle "; printXMMReg toStore; stream " => "; printMemAddress address
            )
        |   XMMConvertFromInt { source, output } =>
            (
                stream "ConvertFromInt "; printGReg source; stream " => "; printXMMReg output
            )
        |   SignExtendForDivide => stream "SignExtendForDivide"
        |   XChng { reg, arg } => (stream "XChng "; printGReg reg; stream " <=> "; printRegOrMemoryArg printGReg arg)
        |   Negative { output } => (stream "Negative "; printGReg output)
        |   JumpTable{cases, ...} =>
                List.app(fn(Label{labelNo, ...}) => (stream "UncondBranch L"; stream(Int.toString labelNo); stream "\n")) cases
        |   IndexedJumpCalc { addrReg, indexReg, jumpSize=ref jumpSize } =>
            (
                stream "IndexedJumpCalc "; printGReg addrReg; stream " += "; printGReg indexReg;
                print (case jumpSize of JumpSize2 => " * 2" | JumpSize8 => " * 8 ")
            )
        ;
 
        stream "\n"
    end

    datatype implement = ImplementGeneral | ImplementLiteral of machineWord

    fun printLowLevelCode(ops, Code{printAssemblyCode, printStream, procName, ...}) =
        if printAssemblyCode
        then
        (
            if procName = "" (* No name *) then printStream "?" else printStream procName;
            printStream ":\n";
            List.app(fn i => printOperation(i, printStream)) ops;
            printStream "\n"
        )
        else ()

    (* Code generate a list of operations.  The list is in reverse order i.e. last instruction first. *)
    fun codeGenerate ops =
    let
        fun cgOp(LockMutableSegment baseReg) =
                (* Remove the mutable bit from the flag byte. *)(*andb CONST(0xff-0x40),-1[Reax]*)
                opPlus2(Group1_8_a, ~1, baseReg, arithOpToWord AND) @ [0wxff - 0wx40]

        |   cgOp(MoveToRegister{ source=RegisterArg source, output }) =
                (* Move from one general register to another. *)
                opReg(MOVL_R_A, source, output)

        |   cgOp(MoveToRegister{ source=NonAddressConstArg source, output}) =
            if isX64
            then
            (
                if source >= ~0x40000000 andalso source < 0x40000000
                then (* Signed 32-bits. *)
                    (* This is not scanned in 64-bit mode because 32-bit values aren't
                       big enough to contain addresses. *)
                    opRegPlus2(MOVL_32_A, output, 0w0) @ int32Signed source
                else (* Too big for 32-bits; put it in the non-word area. *)
                let
                    val (rc, rx) = getReg output
                    val opb = opCodeBytes(MOVL_A_R, SOME{w=true, r=rx, b=false, x = false})
                    val mdrm = modrm (Based0, rc, 0w5 (* PC-relative *))
                in
                    opb @ [mdrm] @ int32Signed(tag 0)
                end
            )
            else (* 32-bit mode. *)
            (
                (* The RTS scans for possible addresses in MOV instructions so we
                   can only use MOV if this is a tagged value.  If it isn't we have
                   to use something else such as XOR/ADD.  In particular this is used
                   before LOCK XADD for atomic inc/dec.
                   TODO: There are various special cases such as setting to -1 can
                   be done with an OR whatever the initial value.  INC can be used
                   in 32-bit mode instead of ADD for 1. LEAL isn't a good idea. *)
                if source mod 2 = 0
                then arithOpReg(XOR, output, output) @
                        (if source = 0 then []
                         else immediateOperand(ADD, output, source))
                else
                let
                    val (rc, _) = getReg output
                    val opb = opCodeBytes(MOVL_32_64_R rc, NONE)
                in
                    opb @ int32Signed source
                end
            )

        |   cgOp(MoveToRegister{ source=AddressConstArg _, output }) =
            let
                val (rc, rx) = getReg output
                val opb =
                    opCodeBytes(MOVL_32_64_R rc,
                            if isX64 then SOME {w=true, r=false, b=rx, x=false} else NONE)
                val constnt =
                    if wordSize = 8
                    then wordUnsigned(LargeWord.fromLargeInt(tag 0))
                    else int32Signed(tag 0)
            in
                opb @ constnt
            end

        |   cgOp(MoveToRegister{source=MemoryArg{base, offset, index}, output }) =
                opAddress(MOVL_A_R, LargeInt.fromInt offset, base, index, output)

        |   cgOp(LoadNonWord{size=Size8Bit, source={base, offset, index}, output }) =
                opAddress(MOVZB, LargeInt.fromInt offset, base, index, output)

        |   cgOp(LoadNonWord{size=Size16Bit, source={base, offset, index}, output }) =
                opAddress(MOVZW, LargeInt.fromInt offset, base, index, output)

        |   cgOp(LoadNonWord{size=Size32Bit, source={base, offset, index}, output }) =
                opAddress(MOVL_A_R32, LargeInt.fromInt offset, base, index, output)

        |   cgOp(LoadAddress{ offset, base, index, output }) =
                (* This provides a mixture of addition and multiplication in a single
                   instruction. *)
                opIndexed(LEAL, LargeInt.fromInt offset, base, index, output)

        |   cgOp(ArithToGenReg{ opc, output, source=RegisterArg source }) =
                arithOpReg (opc, output, source)

        |   cgOp(ArithToGenReg{ opc, output, source=NonAddressConstArg source }) =
            let
                (* On the X86/32 we use CMP with literal sources to compare with an
                   address and the RTS searches for them in the code.  Any
                   non-address constant must be tagged.  Most will be but we
                   might want to use this to compare with the contents of a
                   LargeWord value. *)
                val _ =
                    if isX64 orelse is8BitL source orelse opc <> CMP orelse IntInf.andb(source, 1) = 1
                    then ()
                    else raise InternalError "CMP with constant that looks like an address"
            in
                immediateOperand(opc, output, source)
            end

        |   cgOp(ArithToGenReg{ opc, output, source=AddressConstArg _ }) =
            (* This is only used for opc=CMP to compare addresses for equality. *)
            let
                val (rc, rx) = getReg output
            in
                if isX64
                then
                let
                    val opb = opCodeBytes(Arith (opc, 0w3), SOME {w=true, r=rx, b=false, x=false})
			        val mdrm = modrm(Based0, rc, 0w5 (* Immediate address. *))
                in
                    opb @ [mdrm] @ int32Signed(tag 0) (* This is the address of the constant. *)
                end
                else
                let
                    val opb = opCodeBytes(Group1_32_A (* group1, 32 bit immediate *), NONE)
                    val mdrm = modrm(Register, arithOpToWord opc, rc)
                in
                    opb @ [mdrm] @ int32Signed(tag 0)
                end
            end

        |   cgOp(ArithToGenReg{ opc, output, source=MemoryArg{offset, base, index} }) =
                opAddress(Arith (opc, 0w3), LargeInt.fromInt offset, base, index, output)

        |   cgOp(ArithMemConst{ opc, offset, base, source }) =
                if is8BitL source
                then (* Can use one byte immediate *) 
                    opPlus2(Group1_8_A (* group1, 8 bit immediate *),
                               LargeInt.fromInt offset, base, arithOpToWord opc) @ [Word8.fromLargeInt source]
                else (* Need 32 bit immediate. *)
                    opPlus2(Group1_32_A (* group1, 32 bit immediate *), 
                               LargeInt.fromInt offset, base, arithOpToWord opc) @ int32Signed source

        |   cgOp(ArithMemLongConst{ opc, offset, base, ... }) =
                (* Currently this is always a comparison.  It is only valid in 32-bit mode because
                   the constant is only 32-bits. *)
                if isX64
                then raise InternalError "ArithMemLongConst in 64-bit mode"
                else 
                let
                    val opb = opPlus2 (Group1_32_A, LargeInt.fromInt offset, base, arithOpToWord opc)
                in
                    opb @ int32Signed(tag 0)
                end

        |   cgOp(ShiftConstant { shiftType, output, shift }) =
                if shift = 0w1
                then opRegPlus2(Group2_1_A, output, shiftTypeToWord shiftType)
                else opRegPlus2(Group2_8_A, output, shiftTypeToWord shiftType) @ [shift]

        |   cgOp(ShiftVariable { shiftType, output }) =
                opRegPlus2(Group2_CL_A, output, shiftTypeToWord shiftType)

        |   cgOp(TestTagR reg) =
            let
            (* Test the bottom bit and jump depending on its value.  This is used
               for tag tests in arbitrary precision operations and also for testing
               for short/long values. *)
                val (regNum, rx) = getReg reg
            in
                if reg = eax
                then (* Special instruction for testing accumulator.  Can use an 8-bit test. *)
                    opCodeBytes(TEST_ACC8, NONE) @ [0w1]
                else if isX64
                then 
                let
                    (* We can use a REX code to force it to always use the low order byte. *)
                    val opb = opCodeBytes(Group3_a,
                        if rx orelse regNum >= 0w4 then SOME{w=false, r=false, b=rx, x=false} else NONE)
                    val mdrm = modrm (Register, 0w0 (* test *), regNum)
                in
                    opb @ [mdrm, 0w1]
                end
                else if reg = ebx orelse reg = ecx orelse reg = edx (* can we use an 8-bit test? *)
                then (* Yes. The register value refers to low-order byte. *)
                let
                    val opb = opCodeBytes(Group3_a, NONE)
                    val mdrm = modrm(Register, 0w0 (* test *), regNum)
                in
                    opb @ [mdrm, 0w1]
                end
                else
                let
                    val opb = opCodeBytes(Group3_A, NONE)
                    val mdrm = modrm (Register, 0w0 (* test *), regNum)
                in
                    opb @ mdrm :: word32Unsigned 0w1
                end
            end

        |   cgOp(TestByteMem{base, offset, bits}) =
                (* Test the tag bit and set the condition code. *)
                opPlus2(Group3_a, LargeInt.fromInt offset, base, 0w0 (* test *)) @ [wordToWord8 bits]

        |   cgOp(ConditionalBranch{ test=opc, ... }) = opCodeBytes(CondJump32 opc, NONE) @ word32Unsigned 0w0

        |   cgOp(CallRTS{rtsEntry, saveRegs}) = rtsCall(rtsEntry, saveRegs)

        |   cgOp(RepeatOperation repOp) =
            let
                (* We don't explicitly clear the direction flag.  Should that be done? *)
                val opb = opCodeBytes(REP, NONE)
                (* Put in a rex prefix to force 64-bit mode. *)
                val optRex =
                    if isX64 andalso (case repOp of STOSL => true | MOVSL => true | _ => false)
                    then [rex{w=true, r=false, b=false, x=false}]
                    else []
                val repOp = repOpsToWord repOp
            in
                opb @ optRex @ [repOp]
            end

        |   cgOp(DivideAccR{arg, isSigned}) = opRegPlus2(Group3_A, arg, if isSigned then 0w7 else 0w6)

        |   cgOp(DivideAccM{base, offset, isSigned}) =
                opPlus2(Group3_A, LargeInt.fromInt offset, base, if isSigned then 0w7 else 0w6)

        |   cgOp(AtomicXAdd{base, output}) =
                (* Locked exchange-and-add.  We need the lock prefix before the REX prefix. *)
                opEA(LOCK_XADD, 0, base, output)

        |   cgOp(PushToStack(RegisterArg reg)) =
            let
                val (rc, rx) = getReg reg
            in
                (* Always 64-bit but a REX prefix may be needed for the register. *)
                opCodeBytes(PUSH_R rc, if rx then SOME{w=false, b = true, x=false, r = false } else NONE)
            end

        |   cgOp(PushToStack(MemoryArg{base, offset, index})) =
                opAddressPlus2(Group5, LargeInt.fromInt offset, base, index, 0w6 (* push *))

        |   cgOp(PushToStack(NonAddressConstArg constnt)) = 
                if is8BitL constnt
                then opCodeBytes(PUSH_8, NONE) @ [Word8.fromLargeInt constnt]
                else if is32bit constnt
                then opCodeBytes(PUSH_32, NONE) @ int32Signed constnt
                else (* It won't fit in the immediate; put it in the non-address area. *)
                let
                    val opb = opCodeBytes(Group5, NONE)
                    val mdrm = modrm(Based0, 0w6 (* push *), 0w5 (* PC rel *))
                in
                    opb @ [mdrm] @ int32Signed(tag 0)
                end

        |   cgOp(PushToStack(AddressConstArg _)) = 
                if isX64
                then (* Put it in the constant area. *)
		        let
                    val opb = opCodeBytes(Group5, NONE)
                    val mdrm = modrm(Based0, 0w6 (* push *), 0w5 (* PC rel *));
                in
                    opb @ [mdrm] @ int32Signed(tag 0)
                end
                else (* 32-bit *) opCodeBytes(PUSH_32, NONE) @ int32Signed(tag 0)

        |   cgOp(PopR reg ) =
                let
                    val (rc, rx) = getReg reg
                in
                    (* Always 64-bit but a REX prefix may be needed for the register. *)
                    opCodeBytes(POP_R rc, if rx then SOME{w=false, b = true, x=false, r = false } else NONE)
                end

        |   cgOp(StoreRegToMemory{toStore, address={offset, base, index}}) =
                opAddress(MOVL_R_A, LargeInt.fromInt offset, base, index, toStore)

        |   cgOp(StoreConstToMemory{ toStore=toStore, address={offset, base, index} }) =
            (
                (* Short constant.  In 32-bit mode this is scanned as a possible address.  That means
                   we can't have an untagged constant in it.  That's not a problem in 64-bit mode.
                   There's a special check for using this to set the length word on newly allocated
                   memory. *)
                isX64 orelse toStore = 0 orelse toStore mod 2 = 1 orelse offset = ~wordSize
                    orelse raise InternalError "cgOp: StoreConstToMemory not tagged";
                opAddressPlus2(MOVL_32_A, LargeInt.fromInt offset, base, index, 0w0) @
                            int32Signed toStore
            )

        |   cgOp(StoreLongConstToMemory{ address={offset, base, index}, ... }) =
                (* We can only store a 32-bit constant so we can't use this for 64-bit addresses. *)
                if isX64
                then raise InternalError "StoreLongConstToMemory in 64-bit mode"
                else opAddressPlus2(MOVL_32_A, LargeInt.fromInt offset, base, index, 0w0) @ int32Signed (tag 0)

        |   cgOp(StoreNonWord{ size = Size8Bit, toStore, address={offset, base, index} }) =
            let
                val (rrC, _) = getReg toStore
                (* In 64-bit mode we can specify the low-order byte of RSI/RDI but we
                   must use a REX prefix.  This isn't possible in 32-bit mode. *)
                val opcode = MOVB_R_A{forceRex= rrC >= 0w4}
                val _ = isX64 orelse rrC < 0w4 orelse raise InternalError "High byte register"
            in
                opAddress(opcode, LargeInt.fromInt offset, base, index, toStore)
            end

        |   cgOp(StoreNonWord{ size = Size16Bit, toStore, address={offset, base, index}}) =
                opAddress(MOVL_R_A16, LargeInt.fromInt offset, base, index, toStore)
            
        |   cgOp(StoreNonWord{ size = Size32Bit, toStore, address={offset, base, index}}) =
                opAddress(MOVL_R_A32, LargeInt.fromInt offset, base, index, toStore)

        |   cgOp(StoreNonWordConst{ size=Size8Bit, toStore=toStore, address={offset, base, index}}) =
                opAddressPlus2(MOVB_8_A, LargeInt.fromInt offset, base, index, 0w0) @
                    [Word8.fromLargeInt toStore]

        |   cgOp(StoreNonWordConst{ size=Size16Bit, ... }) =
                raise InternalError "StoreNonWordConst: 16Bit"

        |   cgOp(StoreNonWordConst{ size=Size32Bit, ... }) =
                raise InternalError "StoreNonWordConst: 32Bit"

            (* Allocation is dealt with by expanding the code. *)
        |   cgOp(AllocStore _) = raise InternalError "cgOp: AllocStore"

        |   cgOp(AllocStoreVariable _) = raise InternalError "cgOp: AllocStoreVariable"

        |   cgOp StoreInitialised = raise InternalError "cgOp: StoreInitialised"

        |   cgOp(CallFunction Recursive) = (* Call to the start of the code.  Offset is patched in later. *)
                opCodeBytes (CALL_32, NONE) @ int32Signed 0
     
        |   cgOp(CallFunction FullCall) =
                (* Indirect call to the address held in the first word of the closure. *)
                opAddressPlus2(Group5, 0, regClosure, NoIndex, 0w2 (* call *))

        |   cgOp(CallFunction(ConstantCode _)) =
            if isX64
            then
            let
                val opc = opCodeBytes(Group5, NONE)
                val mdrm = modrm(Based0, 0w2 (* call *), 0w5 (* PC rel *))
            in
                opc @ [mdrm] @ int32Signed(tag 0)
            end
            (* Because this is a relative branch we need to point this at itself.
               Until it is set to the relative offset of the destination it
               needs to contain an address within the code and this could
               be the last instruction. *)
            else opCodeBytes (CALL_32, NONE) @ int32Signed ~5
            
        |   cgOp(CallFunction(DirectReg reg)) = opRegPlus2(Group5, reg, 0w2 (* call *))

        |   cgOp(JumpToFunction Recursive) =
                (* Jump to the start of the current function.  Offset is patched in later. *)
                opCodeBytes (JMP_32, NONE) @ int32Signed 0
           
        |   cgOp(JumpToFunction FullCall) = opAddressPlus2(Group5, 0, regClosure, NoIndex, 0w4 (* jmp *))

        |   cgOp(JumpToFunction (ConstantCode _)) =
            if isX64
            then
            let
                val opb = opCodeBytes (Group5, NONE)
                val mdrm = modrm(Based0, 0w4 (* jmp *), 0w5 (* PC rel *))
            in
                opb @ [mdrm] @ int32Signed(tag 0)
            end
            else opCodeBytes (JMP_32, NONE) @ int32Signed ~5 (* As with Call. *)
            
        |   cgOp(JumpToFunction (DirectReg reg)) =
            (* Used as part of indexed case - not for entering a function. *)
                opRegPlus2(Group5, reg, 0w4 (* jmp *))
    
        |   cgOp(ReturnFromFunction args) =
            if args = 0
            then opCodeBytes(RET, NONE)
            else
            let
                val offset = Word.fromInt(args * wordSize)
            in
                opCodeBytes(RET_16, NONE) @ [wordToWord8 offset, wordToWord8(offset >> 0w8)]
            end

        |   cgOp RaiseException =
            let
                (* Load the current handler into ebx.  Any register will do since we
                   don't preserve registers across exceptions. *)
                val loadInstr = opEA(MOVL_A_R, LargeInt.fromInt memRegHandlerRegister, ebp, ebx)
                val opb = opCodeBytes(Group5, NONE)
                val mdrm = modrm (Based0, 0w4 (* jmp *), #1 (getReg ebx))
            in
                loadInstr @ opb @ [mdrm]
            end

        |   cgOp(UncondBranch _) = opToInt JMP_32 :: word32Unsigned 0w0

        |   cgOp(ResetStack{numWords, preserveCC}) =
            let
                val bytes = Word.toLargeInt(wordsToBytes(Word.fromInt numWords))
            in
                (* If we don't need to preserve the CC across the reset we use ADD since
                   it's shorter. *)
                if preserveCC
                then opEA(LEAL, bytes, esp, esp)
                else immediateOperand(ADD, esp, bytes)
            end

        |   cgOp(JumpLabel _) = [] (* No code. *)

        |   cgOp(LoadLabelAddress{ output, ... }) =
            (* Load the address of a label.  Used when setting up an exception handler or
               in indexed cases. *)
            let
                val (rc, rx) = getReg output
            in
                (* On X86/64 we can use pc-relative addressing to set the start of the handler.
                   On X86/32 we have to load the address of the start of the code and add an offset. *)
                if isX64
                then
                let
                    val opb = opCodeBytes(LEAL, SOME {w=true, r=rx, b=false, x=false})
                    val mdrm = modrm(Based0, rc, 0w5 (* Immediate address. *))
                in
                    opb @ mdrm :: int32Signed 0
                end
                else opCodeBytes(MOVL_32_64_R rc, NONE) @ int32Signed(tag 0) @
                        opRegPlus2(Group1_32_A, output, arithOpToWord ADD) @ int32Signed 0
            end

        |   cgOp (FPLoadFromMemory {address={ base, offset, index }, precision}) =
            let
                val loadInstr =
                    case precision of
                        DoublePrecision => FPESC 0w5
                    |   SinglePrecision => FPESC 0w1
            in
                opAddressPlus2(loadInstr, LargeInt.fromInt offset, base, index, 0wx0)
            end

        |   cgOp (FPLoadFromFPReg{source=FloatingPtReg fp, ...}) =
                (* Assume there's nothing currently on the stack. *)
                floatingPtOp({escape=0w1, md=0w3, nnn=0w0, rm= fp + 0w0}) (* FLD ST(r1) *)

        |   cgOp (FPLoadFromConst realValue ) =
            let
                open Real
                infix ==
            in
                (* Treat +/- 0,1 as special cases. *)
                if realValue == 0.0 (* This is also true for -0.0 *)
                then floatingPtOp({escape=0w1, md=0w3, nnn=0w5, rm=0w6}) (* FLDZ *) @
                        (if signBit realValue
                         then floatingPtOp({escape=0w1, md=0w3, nnn=0w4, rm=0w0})
                         else [])
                else if realValue == 1.0
                then floatingPtOp({escape=0w1, md=0w3, nnn=0w5, rm=0w0}) (* FLD1 *)
                else if realValue == ~1.0
                then
                    floatingPtOp({escape=0w1, md=0w3, nnn=0w5, rm=0w0}) @ (* FLD1 *)
                        floatingPtOp({escape=0w1, md=0w3, nnn=0w4, rm=0w0}) (* FCHS *)
                else
                (* The real constant here is actually the address of an 8-byte memory
                   object.  FLD takes the address as the argument and in 32-bit mode
                   we use an absolute address.  In 64-bit mode we need to put the
                   constant at the end of the code segment and use PC-relative
                   addressing which happens to be encoded in the same way. *)
                let
                    val opb = opCodeBytes(FPESC 0w5, NONE) (* FLD [Constant] *)
                    val mdrm = modrm (Based0, 0w0, 0w5 (* constant address/PC-relative *))
                in
                    opb @ [mdrm] @ int32Signed(tag 0)
                end
            end

        |   cgOp (FPStoreToFPReg{ output=FloatingPtReg dest, andPop }) =
                (* Assume there's one item on the stack. *)
                floatingPtOp({escape=0w5, md=0w3, nnn=if andPop then 0wx3 else 0wx2,
                               rm = dest+0w1(* One item *)}) (* FSTP ST(n+1) *)

        |   cgOp (FPStoreToMemory{address={ base, offset, index}, precision, andPop }) =
            let
                val storeInstr =
                    case precision of
                        DoublePrecision => FPESC 0w5
                    |   SinglePrecision => FPESC 0w1
                val subInstr = if andPop then 0wx3 else 0wx2
            in
                opAddressPlus2(storeInstr, LargeInt.fromInt offset, base, index, subInstr)
            end

        |   cgOp (FPArithR{ opc, source = FloatingPtReg src}) =
                floatingPtOp({escape=0w0, md=0w3, nnn=fpOpToWord opc,
                        rm=src + 0w1 (* One item already there *)})

        |   cgOp (FPArithConst{ opc, ... }) =
                (* See comment on FPLoadFromConst *)
            let
                val opb = opCodeBytes(FPESC 0w4, NONE) (* FADD etc [constnt] *)
                val mdrm = modrm (Based0, fpOpToWord opc, 0w5 (* constant address *))
            in
                opb @ [mdrm] @ int32Signed(tag 0)
            end

        |   cgOp (FPArithMemory{ opc, base, offset }) =
                opPlus2(FPESC 0w4, LargeInt.fromInt offset, base, fpOpToWord opc) (* FADD/FMUL etc [r2] *)

        |   cgOp (FPUnary opc ) =
            let
                val {rm, nnn} = fpUnaryToWords opc
            in
                floatingPtOp({escape=0w1, md=0w3, nnn=nnn, rm=rm}) (* FCHS etc *)
            end

        |   cgOp (FPStatusToEAX ) =
                opCodeBytes(FPESC 0w7, NONE) @ [0wxe0] (* FNSTSW AX *)

        |   cgOp (FPFree(FloatingPtReg reg)) =
                floatingPtOp({escape=0w5, md=0w3, nnn=0w0, rm=reg}) (* FFREE FP(n) *)

        |   cgOp (FPLoadInt{base, offset}) =
                (* fildl (esp) in 32-bit mode or fildq (esp) in 64-bit mode. *)
                if isX64
                then opPlus2(FPESC 0w7, LargeInt.fromInt offset, base, 0w5)
                else opPlus2(FPESC 0w3, LargeInt.fromInt offset, base, 0w0)

        |   cgOp (MultiplyR {source=RegisterArg srcReg, output}) =
                (* We use the 0F AF form of IMUL rather than the Group3 MUL or IMUL
                   because the former allows us to specify the destination register.
                   The Group3 forms produce double length results in RAX:RDX/EAX:EDX
                   but we only ever want the low-order half. *)
                opReg(IMUL (* 2 byte opcode *), output, srcReg)

        |   cgOp (MultiplyR {source=MemoryArg{base, offset, index}, output}) =
                (* This may be used for large-word multiplication. *)
                opAddress(IMUL (* 2 byte opcode *), LargeInt.fromInt offset, base, index, output)
        
        |   cgOp(MultiplyR {source=NonAddressConstArg constnt, output}) =
                (* If the constant is an 8-bit or 32-bit value we are actually using a
                   three-operand instruction where the argument can be a register or memory
                   and the destination register does not need to be the same as the source. *)
                if is8BitL constnt
                then opReg(IMUL_C8, output, output) @ [Word8.fromLargeInt constnt]
                else if is32bit constnt
                then opReg(IMUL_C32, output, output) @ int32Signed constnt
                else
                let
                    val (rc, rx) = getReg output
                    val opb = opCodeBytes(IMUL, SOME{w=true, r=rx, b=false, x = false})
                    val mdrm = modrm(Based0, rc, 0w5 (* PC rel *))
                in
                    opb @ [mdrm] @ int32Signed(tag 0)
                end

        |   cgOp(MultiplyR {source=AddressConstArg _, ...}) =
                raise InternalError "Multiply - address constant"

        |   cgOp (XMMArith { opc, source=MemoryArg{base, offset, index}, output }) =
                mMXAddress(SSE2Ops opc, LargeInt.fromInt offset, base, index, output)

        |   cgOp (XMMArith { opc, source=AddressConstArg _, output=SSE2Reg rrC }) =
            let
               (* The real constant here is actually the address of an 8-byte memory
                   object.  The SSE2 instruction takes the address as the argument and in 32-bit mode
                   we use an absolute address.  In 64-bit mode we need to put the
                   constant at the end of the code segment and use PC-relative
                   addressing which happens to be encoded in the same way. *)
                val opb = opCodeBytes(SSE2Ops opc, NONE)
                val mdrm = modrm (Based0, rrC, 0w5 (* constant address/PC-relative *))
            in
                opb @ [mdrm] @ int32Signed(tag 0)
            end

        |   cgOp (XMMArith { opc, source=RegisterArg(SSE2Reg rrS), output=SSE2Reg rrC }) =
            let
                val oper = SSE2Ops opc
                val pref = opcodePrefix oper
                val esc = escapePrefix oper
                val opc = opToInt oper
                val mdrm = modrm(Register, rrC, rrS)
            in
                pref @ esc @ [opc, mdrm]
            end

        |   cgOp (XMMArith _) = raise InternalError "cgOp: XMMArith"

        |   cgOp (XMMStoreToMemory { toStore, address={base, offset, index}, precision }) =
            let
                val oper =
                    case precision of
                        DoublePrecision => SSE2StoreDouble
                    |   SinglePrecision => SSE2StoreSingle
            in
                mMXAddress(oper, LargeInt.fromInt offset, base, index, toStore)
            end

        |   cgOp (XMMConvertFromInt { source, output=SSE2Reg rrC }) =
            let
                (* The source is a general register and the output a XMM register. *)
                (* TODO: The source can be a memory location. *)
                val (rbC, rbX) = getReg source
            in
                (* This is a special case with both an XMM and general register. *)
                opcodePrefix CVTSI2SD @ rexByte(CVTSI2SD, false, rbX, false) @
                    escapePrefix CVTSI2SD @ [opToInt CVTSI2SD, modrm(Register, rrC, rbC)]
            end

        |   cgOp SignExtendForDivide =
                opCodeBytes(CQO_CDQ, if isX64 then SOME {w=true, r=false, b=false, x=false} else NONE)
            
        |   cgOp (XChng { reg, arg=RegisterArg regY }) = opReg(XCHNG, reg, regY)
            
        |   cgOp (XChng { reg, arg=MemoryArg{offset, base, index} }) =
                opAddress(XCHNG, LargeInt.fromInt offset, base, index, reg)
                
        |   cgOp (XChng _) = raise InternalError "cgOp: XChng"

        |   cgOp (Negative {output}) =
                opRegPlus2(Group3_A, output, 0w3 (* neg *))

        |   cgOp (JumpTable{cases, jumpSize=ref jumpSize}) =
            let
                val _ = jumpSize = JumpSize8 orelse raise InternalError "cgOp: JumpTable"
                (* Make one jump for each case and pad it 8 bytes with Nops. *)
                fun makeJump (_, l) = opToInt JMP_32 :: word32Unsigned 0w0 @ [opToInt NOP, opToInt NOP, opToInt NOP] @ l
            in
                List.foldl makeJump [] cases
            end

        |   cgOp(IndexedJumpCalc{ addrReg, indexReg, jumpSize=ref jumpSize }) =
            (
                jumpSize = JumpSize8 orelse raise InternalError "cgOp: IndexedJumpCalc";
                (* Should currently be JumpSize8 which requires a multiplier of 4 and
                   4 to be subtracted to remove the shifted tag. *)
                opAddress(LEAL, ~4, addrReg, Index4 indexReg, addrReg)
            )
    in
        List.rev(List.foldl (fn (c, list) => Word8Vector.fromList(cgOp c) :: list) [] ops)
    end
    
    (* General function to process the code.  ic is the byte counter within the original code. *)
    fun foldCode foldFn n (ops, byteList) =
    let
        fun doFold(oper :: operList, bytes :: byteList, ic, acc) =
            doFold(operList, byteList, ic + Word.fromInt(Word8Vector.length bytes),
                foldFn(oper, bytes, ic, acc))
        |   doFold(_, _, _, n) = n
    in
        doFold(ops, byteList, 0w0, n)
    end

    (* Go through the code and update branch and similar instructions with the destinations
       of the branches.  Long branches are converted to short where possible and the code
       is reprocessed.  That might repeat if the effect of shorting one branch allows
       another to be shortened. *)
    fun fixupLabels(ops, bytesList, labelCount) =
    let
        (* Label array - initialise to 0wxff... .  Every label should be defined
           but just in case, this is more likely to be detected in int32Signed. *)
        val labelArray = Array.array(labelCount, ~ 0w1)

        (* First pass - Set the addresses of labels. *)
        fun setLabelAddresses(oper :: operList, bytes :: byteList, ic) =
            (
                case oper of
                    JumpLabel(Label{labelNo, ...}) => Array.update(labelArray, labelNo, ic)
                |   _ => ();
                setLabelAddresses(operList, byteList, ic + Word.fromInt(Word8Vector.length bytes))
            )
        |   setLabelAddresses(_, _, ic) = ic (* Return the length of the code. *)

        fun fixup32(destination, bytes, ic) =
        let
            val brLength = Word8Vector.length bytes
            (* The offset is relative to the end of the branch instruction. *)
            val diff = Word.toInt destination - Word.toInt ic - brLength
        in
            Word8VectorSlice.concat[
                Word8VectorSlice.slice(bytes, 0, SOME(brLength-4)), (* The original opcode. *)
                Word8VectorSlice.full(Word8Vector.fromList(int32Signed(LargeInt.fromInt diff)))
            ]
        end
        
        fun fixupAddress(UncondBranch(Label{labelNo, ...}), bytes, ic, list) =
            let
                val destination = Array.sub(labelArray, labelNo)
                val brLength = Word8Vector.length bytes
                (* The offset is relative to the end of the branch instruction. *)
                val diff = Word.toInt destination - Word.toInt ic - brLength
            in
                if brLength = 2
                then (* It's a short branch.  Take the original operand and set the relative offset. *)
                    Word8Vector.fromList [opToInt JMP_8, byteSigned diff] :: list
                else if brLength <> 5
                then raise InternalError "fixupAddress"
                else (* 32-bit offset.  If it will fit in a byte we can use a short branch.
                        If this is a reverse branch we can actually use values up to -131
                        here because we've calculated using the end of the long branch. *)
                    if diff <= 127 andalso diff >= ~(128 + 3)
                then Word8Vector.fromList [opToInt JMP_8, 0w0 (* Fixed on next pass *)] :: list
                else Word8Vector.fromList(opToInt JMP_32 :: int32Signed(LargeInt.fromInt diff)) :: list
            end

        |   fixupAddress(ConditionalBranch{label=Label{labelNo, ...}, test, ...}, bytes, ic, list) =
            let
                val destination = Array.sub(labelArray, labelNo)
                val brLength = Word8Vector.length bytes
                (* The offset is relative to the end of the branch instruction. *)
                val diff = Word.toInt destination - Word.toInt ic - brLength
            in
                if brLength = 2
                then (* It's a short branch.  Take the original operand and set the relative offset. *)
                    Word8Vector.fromList [opToInt(CondJump test), byteSigned diff]  :: list
                else if brLength <> 6
                then raise InternalError "fixupAddress"
                else if diff <= 127 andalso diff >= ~(128+4)
                then Word8Vector.fromList[opToInt(CondJump test), 0w0 (* Fixed on next pass *)] :: list
                else Word8Vector.fromList(opCodeBytes(CondJump32 test, NONE) @ int32Signed(LargeInt.fromInt diff))  :: list
            end

        |   fixupAddress(LoadLabelAddress{ label=Label{labelNo, ...}, ... }, brCode, ic, list) =
            let
                val destination = Array.sub(labelArray, labelNo)
            in
                if isX64
                then (* This is a relative offset on the X86/64. *)
                    fixup32(destination, brCode, ic) :: list
                else (* On X86/32 the address is relative to the start of the code so we simply put in
                        the destination address. *)
                    Word8VectorSlice.concat[
                        Word8VectorSlice.slice(brCode, 0, SOME(Word8Vector.length brCode-4)),
                        Word8VectorSlice.full(Word8Vector.fromList(int32Signed(Word.toLargeInt destination)))] :: list
            end

        |   fixupAddress(JumpTable{cases, jumpSize as ref JumpSize8}, brCode: Word8Vector.vector, ic, list) =
            let
                (* Each branch is a 32-bit jump padded up to 8 bytes. *)
                fun processCase(Label{labelNo, ...} :: cases, offset, ic) =
                    fixup32(Array.sub(labelArray, labelNo),
                        Word8VectorSlice.vector(Word8VectorSlice.slice(brCode, offset, SOME 5)), ic) ::
                    Word8VectorSlice.vector(Word8VectorSlice.slice(brCode, offset+5, SOME 3)) ::
                    processCase(cases, offset+8, ic+0w8)
                |   processCase _ = []
                (* Could we use short branches?  If all of the branches were short the
                   table would be smaller so the offsets we use would be less.
                   Ignore backwards branches - could only occur if we have linked labels
                   in a loop. *)
                val newStartOfCode = ic + Word.fromInt(List.length cases * 6)
                fun tryShort(Label{labelNo, ...} :: cases, ic) =
                    let
                        val destination = Array.sub(labelArray, labelNo)
                    in
                        if destination > ic + 0w2 andalso destination - ic - 0w2 < 0w127
                        then tryShort(cases, ic+0w2)
                        else false
                    end
                |   tryShort _ = true

                val newCases =
                    if tryShort(cases, newStartOfCode)
                    then
                    (
                        jumpSize := JumpSize2;
                        (* Generate a short branch table. *)
                        List.map(fn _ => Word8Vector.fromList [opToInt JMP_8, 0w0 (* Fixed on next pass *)]) cases
                    )
                    else processCase(cases, 0, ic)
            in
                Word8Vector.concat newCases :: list
            end

        |   fixupAddress(JumpTable{cases, jumpSize=ref JumpSize2}, _, ic, list) =
            let
                (* Each branch is a short jump. *)
                fun processCase(Label{labelNo, ...} :: cases, offset, ic) =
                    let
                        val destination = Array.sub(labelArray, labelNo)
                        val brLength = 2
                        val diff = Word.toInt destination - Word.toInt ic - brLength
                    in
                        Word8Vector.fromList[opToInt JMP_8, byteSigned diff] :: processCase(cases, offset+2, ic+0w2)
                    end
                |   processCase _ = []
            in
                Word8Vector.concat(processCase(cases, 0, ic)) :: list
            end

            (* If we've shortened a jump table we have to change the indexing. *)
        |   fixupAddress(IndexedJumpCalc{ addrReg, indexReg, jumpSize=ref JumpSize2 }, _, _, list) =
                (* On x86/32 it might be shorter to use DEC addrReg; ADD addrReg, indexReg. *)
                Word8Vector.fromList(opAddress(LEAL, ~1, addrReg, Index1 indexReg, addrReg)) :: list

        |   fixupAddress(CallFunction Recursive, brCode, ic, list) =
            let
                val brLen = Word8Vector.length brCode
            in
                (* Call to the start of the code.  Offset is -(bytes to start). *)
                Word8VectorSlice.concat[
                    Word8VectorSlice.slice(brCode, 0, SOME(brLen-4)), (* The original opcode. *)
                    Word8VectorSlice.full(Word8Vector.fromList(int32Signed(LargeInt.fromInt(~(Word.toInt ic+brLen)))))
                ] :: list
            end

        |   fixupAddress(JumpToFunction Recursive, brCode, ic, list) =
            let
                val brLen = Word8Vector.length brCode
            in
                (* Call to the start of the code.  Offset is -(bytes to start). *)
                Word8VectorSlice.concat[
                    Word8VectorSlice.slice(brCode, 0, SOME(brLen-4)), (* The original opcode. *)
                    Word8VectorSlice.full(Word8Vector.fromList(int32Signed(LargeInt.fromInt(~(Word.toInt ic+brLen)))))
                ] :: list
            end

        |   fixupAddress(_, bytes, _, list) = bytes :: list
        
        fun reprocess(bytesList, lastCodeSize) =
        let
            val fixedList = List.rev(foldCode fixupAddress [] (ops, bytesList))
            val newCodeSize = setLabelAddresses(ops, fixedList, 0w0)
        in
            if newCodeSize = lastCodeSize
            then (fixedList, lastCodeSize)
            else if newCodeSize > lastCodeSize
            then raise InternalError "reprocess - size increased"
            else reprocess(fixedList, newCodeSize)
        end
    in
        reprocess(bytesList, setLabelAddresses(ops, bytesList, 0w0))
    end
 
    (* The handling of constants generally differs between 32- and 64-bits.  In 32-bits we put all constants
       inline and the GC processes the code to find the addresss.  For real values the "constant" is actually
       the address of the boxed real value.
       In 64-bit mode only the MOV instruction allows a 64-bit quantity so in all other cases only non-addresses
       that will fit in 32-bits are inline.  Other constants are stored in one of two areas at the end of the
       code segment.  Non-addresses, including the actual values of reals, are stored in the non-address area
       and addresses go in the address area.  Only the latter is scanned by the GC.
       The address area is also used in 32-bit mode but only has the address of the function name and the
       address of the profile ref in it. *)
    datatype constnts =
        SelfAddress                             (* The address of the start of the code - inline absolute address 32-bit only *)
    |   InlineAbsoluteAddress of machineWord    (* An address in the code *)
    |   InlineRelativeAddress of machineWord    (* A relative address: 32-bit only. *)
    |   ConstantArea of machineWord             (* A PC-relative address to the constant area - 64-bit only. *)
    |   RealConstant of machineWord             (* A PC-relative address to the non-address area - 64-bit only.*)
    |   IntConstant of LargeInt.int             (* A PC-relative address to the non-address area - 64-bit only.*)

    local       
        fun getConstant(MoveToRegister{ source=NonAddressConstArg source, ...}, bytes, ic, l) =
            if isX64
            then
            (
                if source >= ~0x40000000 andalso source < 0x40000000
                then (* Signed 32-bits. *) l
                else (* Too big for 32-bits. *)
                    (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, IntConstant source) :: l
            )
            else l (* 32-bit mode.  The constant will always be inline even if we've had to use XOR r,r; ADD r,c *)

        |   getConstant(MoveToRegister{ source=AddressConstArg source, ... }, bytes, ic, l) =
                (* This is the only InlineAbsolute constant form that is valid on X86/64 as well as X86/32. *)
                (ic + Word.fromInt(Word8Vector.length bytes) - Word.fromInt wordSize, InlineAbsoluteAddress source) :: l

        |   getConstant(ArithToGenReg{ source=NonAddressConstArg source, ... }, bytes, ic, l) =
                if is32bit source
                then l
                else (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, IntConstant source) :: l

        |   getConstant(ArithToGenReg{ source=AddressConstArg source, ... }, bytes, ic, l) =
                (ic + Word.fromInt(Word8Vector.length bytes) - 0w4,
                    if isX64 then ConstantArea source else InlineAbsoluteAddress source) :: l

        |   getConstant(ArithMemLongConst{ source, ... }, bytes, ic, l) = (* 32-bit only. *)
                (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineAbsoluteAddress source) :: l

        |   getConstant(PushToStack(NonAddressConstArg constnt), bytes, ic, l) =
                if is32bit constnt then l
                else (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, IntConstant constnt) :: l

        |   getConstant(PushToStack(AddressConstArg constnt), bytes, ic, l) =
                (ic + Word.fromInt(Word8Vector.length bytes) - 0w4,
                    if isX64 then ConstantArea constnt else InlineAbsoluteAddress constnt) :: l

        |   getConstant(StoreLongConstToMemory{ toStore, ... }, bytes, ic, l) = (* 32-bit only. *)
                (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineAbsoluteAddress toStore) :: l

        |   getConstant(CallFunction(ConstantCode w), bytes, ic, l) =
                (ic + Word.fromInt(Word8Vector.length bytes) - 0w4,
                    if isX64 then ConstantArea w else InlineRelativeAddress w) :: l

        |   getConstant(JumpToFunction(ConstantCode w), bytes, ic, l) =
                (ic + Word.fromInt(Word8Vector.length bytes) - 0w4,
                    if isX64 then ConstantArea w else InlineRelativeAddress w) :: l

        |   getConstant(LoadLabelAddress _, _, ic, l) =
                (* We need the address of the code itself but it's in the first of a pair of instructions. *)
                if isX64 then l else (ic + 0w1, SelfAddress) :: l

        |   getConstant(FPLoadFromConst realValue, bytes, ic, l) =
                if Real.==(realValue, 0.0) orelse Real.==(realValue, 1.0) orelse Real.==(realValue, ~1.0)
                then l
                else (ic + Word.fromInt(Word8Vector.length bytes) - 0w4,
                        if isX64 then RealConstant(toMachineWord realValue) else InlineAbsoluteAddress (toMachineWord realValue)) :: l

        |   getConstant(FPArithConst{ source, ... }, bytes, ic, l) =
                (ic + Word.fromInt(Word8Vector.length bytes) - 0w4,
                        if isX64 then RealConstant(toMachineWord source) else InlineAbsoluteAddress (toMachineWord source)) :: l

        |   getConstant(XMMArith { source=AddressConstArg constVal, ... }, bytes, ic, l) =
                (ic + Word.fromInt(Word8Vector.length bytes) - 0w4,
                        if isX64 then RealConstant(toMachineWord constVal) else InlineAbsoluteAddress (toMachineWord constVal)) :: l

        |   getConstant(MultiplyR{ source=NonAddressConstArg source, ... }, bytes, ic, l) =
                if is32bit source
                then l
                else (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, IntConstant source) :: l

        |   getConstant(_, _, _, l) = l        
    in
        val getConstants = foldCode getConstant []
    end

    (* It is convenient to have AllocStore and AllocStoreVariable as primitives at the higher
       level but at this point it's better to expand them into their basic instructions. *)
    fun expandComplexOperations(instrs, oldLabelCount) =
    let
        val labelCount = ref oldLabelCount
        fun mkLabel() = Label{labelNo= !labelCount} before labelCount := !labelCount + 1

         (* On X86/64 the local pointer is in r15.  On X86/32 it's in memRegs. *)
        val localPointer =
            if isX64 then RegisterArg r15 else MemoryArg{base=ebp, offset=memRegLocalMPointer, index=NoIndex}

        fun allocStoreCommonCode (resultReg, isVarAlloc, regSaveSet: genReg list) =
        let
            val compare =
                ArithToGenReg{opc=CMP, output=resultReg, source=MemoryArg{base=ebp, offset=memRegLocalMbottom, index=NoIndex}}
            (* Normally we won't have run out of store so we want the default
               branch prediction to skip the test here. However doing that
               involves adding an extra branch which lengthens the code so
               it's probably not worth while. *)
            (* Just checking against the lower limit can fail
               in the situation where the heap pointer is at the low end of
               the address range and the store required is so large that the
               subtraction results in a negative number.  In that case it
               will be > (unsigned) lower_limit so in addition we have
               to check that the result is < (unsigned) heap_pointer.
               This actually happened on Windows with X86-64.
               In theory this can happen with fixed-size allocations as
               well as variable allocations but in practice fixed-size
               allocations are going to be small enough that it's not a
               problem.  *)
            val destLabel = mkLabel()
            val branches =
                if isVarAlloc
                then
                let
                    val extraLabel = mkLabel()
                in
                    [ConditionalBranch{test=JB, label=extraLabel, predict=PredictNotTaken},
                     ArithToGenReg{opc=CMP, output=resultReg, source=localPointer},
                     ConditionalBranch{test=JB, label=destLabel, predict=PredictTaken},
                     JumpLabel extraLabel]
                end
                else [ConditionalBranch{test=JNB, label=destLabel, predict=PredictTaken}]
            val callRts = CallRTS{rtsEntry=HeapOverflowCall, saveRegs=regSaveSet}
            val fixup = JumpLabel destLabel
            (* Update the heap pointer now we have the store.  This is also
               used by the RTS in the event of a trap to work out how much
               store was being allocated. *)
            val update =
                if isX64 then MoveToRegister{source=RegisterArg resultReg, output=r15}
                else StoreRegToMemory{toStore=resultReg, address={base=ebp, offset=memRegLocalMPointer, index=NoIndex}}
        in
            compare :: branches @ [callRts, fixup, update]
        end
        
        fun doExpansion([], code, _) = code

        |   doExpansion(AllocStore {size, output, saveRegs} :: instrs, code, inAllocation) =
            let
                val _ = inAllocation andalso raise InternalError "doExpansion: Allocation started but not complete"
                val () = if List.exists (fn r => r = output) saveRegs then raise InternalError "AllocStore: in set" else ()
                val bytes = (size + 1) * wordSize
                val startCode =
                    if isX64
                    then [LoadAddress{output=output, offset = ~ bytes, base=SOME r15, index=NoIndex}]
                             (* TODO: What if it's too big to fit? *)
                    else
                        [MoveToRegister{source=MemoryArg{base=ebp, offset=memRegLocalMPointer, index=NoIndex}, output=output},
                         LoadAddress{output=output, offset = ~ bytes, base=SOME output, index=NoIndex}]
                val resultCode = startCode @ allocStoreCommonCode(output, false, saveRegs)
            in
                doExpansion(instrs, (List.rev resultCode) @ code, true)
            end

        |   doExpansion(AllocStoreVariable {output, saveRegs} :: instrs, code, inAllocation) =
            let
                (* Allocates memory whose size as a number of bytes is in the output register. *)
                val _ = inAllocation andalso raise InternalError "doExpansion: Allocation started but not complete"
                val () = if List.exists (fn r => r = output) saveRegs then raise InternalError "AllocStore: in set" else ()
                (* Negate the length and add it to the current heap pointer. *)
                val startCode =
                    [Negative{output=output}, ArithToGenReg{opc=ADD, output=output, source=localPointer}]
                val resultCode = startCode @ allocStoreCommonCode(output, true, saveRegs)
            in
                doExpansion(instrs, (List.rev resultCode) @ code, true)
            end

        |   doExpansion(StoreInitialised :: instrs, code, _) = doExpansion(instrs, code, false)

        |   doExpansion(instr :: instrs, code, inAlloc) = doExpansion(instrs, instr::code, inAlloc)
        
        val expanded = List.rev(doExpansion(instrs, [], false))
    in
        (expanded, !labelCount)
    end


    fun printCode (Code{procName, printStream, ...}, seg) =
    let
        val print = printStream
        val ptr = ref 0w0;
        (* prints a string representation of a number *)
        fun printValue v =
            if v < 0 then (print "-"; print(LargeInt.toString  (~ v))) else print(LargeInt.toString v)

        infix 3 +:= ;
        fun (x +:= y) = (x := !x + (y:word));

        fun get16s (a, seg) : int =
        let
            val b0  = Word8.toInt (codeVecGet (seg, a));
            val b1  = Word8.toInt (codeVecGet (seg, a + 0w1));
            val b1' = if b1 >= 0x80 then b1 - 0x100 else b1;
        in
            (b1' * 0x100) + b0
        end
        
        fun get16u(a, seg) : int =
            Word8.toInt (codeVecGet (seg, a + 0w1)) * 0x100 + Word8.toInt (codeVecGet (seg, a))
        
        (* Get 1 unsigned byte from the given offset in the segment. *)
        fun get8u (a, seg) : Word8.word = codeVecGet (seg, a);

        (* Get 1 signed byte from the given offset in the segment. *)
        fun get8s (a, seg) : int = Word8.toIntX (codeVecGet (seg, a));
     
        (* Get 1 signed 32 bit word from the given offset in the segment. *)
        fun get32s (a, seg) : LargeInt.int =
        let
            val b0  = Word8.toLargeInt (codeVecGet (seg, a));
            val b1  = Word8.toLargeInt (codeVecGet (seg, a + 0w1));
            val b2  = Word8.toLargeInt (codeVecGet (seg, a + 0w2));
            val b3  = Word8.toLargeInt (codeVecGet (seg, a + 0w3));
            val b3' = if b3 >= 0x80 then b3 - 0x100 else b3;
            val topHw    = (b3' * 0x100) + b2;
            val bottomHw = (b1 * 0x100) + b0;
        in
            (topHw * exp2_16) + bottomHw
        end
 
        fun get64s (a, seg) : LargeInt.int =
        let
            val b0  = Word8.toLargeInt (codeVecGet (seg, a));
            val b1  = Word8.toLargeInt (codeVecGet (seg, a + 0w1));
            val b2  = Word8.toLargeInt (codeVecGet (seg, a + 0w2));
            val b3  = Word8.toLargeInt (codeVecGet (seg, a + 0w3));
            val b4  = Word8.toLargeInt (codeVecGet (seg, a + 0w4));
            val b5  = Word8.toLargeInt (codeVecGet (seg, a + 0w5));
            val b6  = Word8.toLargeInt (codeVecGet (seg, a + 0w6));
            val b7  = Word8.toLargeInt (codeVecGet (seg, a + 0w7));
            val b7' = if b7 >= 0x80 then b7 - 0x100 else b7;
        in
            ((((((((b7' * 0x100 + b6) * 0x100 + b5) * 0x100 + b4) * 0x100 + b3)
                 * 0x100 + b2) * 0x100) + b1) * 0x100) + b0
        end
 
        fun print32 () = printValue (get32s (!ptr, seg)) before (ptr +:= 0w4)
        and print64 () = printValue (get64s (!ptr, seg)) before (ptr +:= 0w8)
        and print16 () = printValue (LargeInt.fromInt(get16s (!ptr, seg)) before (ptr +:= 0w2))
        and print8 () = printValue (LargeInt.fromInt(get8s (!ptr, seg)) before (ptr +:= 0w1))
 
        fun printJmp () =
        let
            val valu = get8s (!ptr, seg)  before ptr +:= 0w1
        in
            print (Word.fmt StringCvt.HEX (Word.fromInt valu + !ptr))
        end
 
        (* Print an effective address.  The register field may designate a general register
           or an xmm register depending on the instruction. *)
        fun printEAGeneral printRegister (rex, sz) =
        let
            val modrm = codeVecGet (seg, !ptr)
            val () = ptr +:= 0w1
            (* Decode the Rex prefix if present. *)
            val rexX = (rex andb8 0wx2) <> 0w0
            val rexB = (rex andb8 0wx1) <> 0w0
            val prefix =
                case sz of
                    SZByte  => "byte ptr "
                |   SZWord  => "word ptr "
                |   SZDWord => "dword ptr "
                |   SZQWord => "qword ptr "
        in
            case (modrm >>- 0w6, modrm andb8 0w7, isX64) of
                (0w3, rm, _) => printRegister(rm, rexB, sz)
      
            |   (md, 0w4, _) =>
                let (* s-i-b present. *)
                    val sib = codeVecGet (seg, !ptr)
                    val () = ptr +:= 0w1
                    val ss    = sib >>- 0w6
                    val index = (sib  >>- 0w3) andb8 0w7
                    val base   = sib andb8 0w7
                in
                    print prefix;

                    case (md, base, isX64) of
                        (0w1, _, _) => print8 ()
                    |   (0w2, _, _) => print32 ()
                    |   (0w0, 0w5, _) => print32 () (* Absolute in 32-bit mode.  PC-relative in 64-bit ?? *)
                    |   _ => ();
          
                    print "[";
        
                    if md <> 0w0 orelse base <> 0w5
                    then
                    (
                        print (genRegRepr (mkReg (base, rexB), sz32_64));
                        if index = 0w4 then () else print ","
                    )
                    else ();
        
                    if index = 0w4 andalso not rexX (* No index. *)
                    then ()
                    else print (genRegRepr (mkReg(index, rexX), sz32_64) ^ 
                            (if ss = 0w0 then "*1"
                            else if ss = 0w1 then "*2"
                            else if ss = 0w2 then "*4"
                            else "*8"));
        
                    print "]"
                end
      
            |   (0w0, 0w5, false) => (* Absolute address.*) (print prefix; print32 ())

            |   (0w0, 0w5, _) => (* PC-relative in 64-bit  *)
                        (print prefix; print ".+"; print32 ())
            
            |   (md, rm, _) => (* register plus offset. *)
                (
                    print prefix;
                    if md = 0w1 then print8 ()
                    else if md = 0w2 then print32 ()
                    else ();
         
                    print ("[" ^ genRegRepr (mkReg(rm, rexB), sz32_64) ^ "]")
                )
        end
        
        (* For most instructions we want to print a general register. *)
        val printEA =
            printEAGeneral (fn (rm, rexB, sz) => print (genRegRepr (mkReg(rm, rexB), sz)))
        and printEAxmm =
            printEAGeneral (fn (rm, _, _) => print (xmmRegRepr(SSE2Reg rm)))
 
        fun printArith opc =
            print
               (case opc of
                  0 => "add "
                | 1 => "or  "
                | 2 => "adc "
                | 3 => "sbb "
                | 4 => "and "
                | 5 => "sub "
                | 6 => "xor "
                | _ => "cmp "
               )

        fun printGvEv (opByte, rex, rexR, sz) =
        let
            (* Register is in next byte. *)
            val nb = codeVecGet (seg, !ptr)
            val reg = (nb >>- 0w3) andb8 0w7
        in
            printArith(Word8.toInt((opByte div 0w8) mod 0w8));
            print "\t";
            print (genRegRepr (mkReg(reg, rexR), sz));
            print ",";
            printEA(rex, sz)
        end
        
        fun printMovCToR (opByte, sz, rexB) =
        (
            print "mov \t";
            print(genRegRepr (mkReg (opByte mod 0w8, rexB), sz));
            print ",";
            case sz of SZDWord => print32 () | SZQWord => print64 () | _ => print "???"
        )
        
        fun printShift (opByte, rex, sz) =
        let
            (* Opcode is determined by next byte. *)
            val nb = Word8.toInt (codeVecGet (seg, !ptr))
            val opc = (nb div 8) mod 8
        in
            print
               (case opc of
                  4 => "shl "
                | 5 => "shr "
                | 7 => "sar "
                | _ => "???"
               );
            print "\t";
            printEA(rex, sz);
            print ",";
            if opByte = opToInt Group2_1_A then print "1"
            else if opByte = opToInt Group2_CL_A then print "cl"
            else print8 ()
        end
        
        fun printFloat (opByte, rex) =
        let
            (* Opcode is in next byte. *)
            val opByte2  = codeVecGet (seg, !ptr)
            val nnn = (opByte2 >>- 0w3) andb8 0w7
            val escNo = opByte andb8 0wx7
        in
            if (opByte2 andb8 0wxC0) = 0wxC0
            then (* mod = 11 *)
            (
                case (escNo, nnn, opByte2 andb8 0wx7 (* modrm *)) of
                    (0w1, 0w4, 0w0) => print "fchs"
                |   (0w1, 0w4, 0w1) => print "fabs"
                |   (0w1, 0w5, 0w6) => print "fldz"
                |   (0w1, 0w5, 0w1) => print "flf1"
                |   (0w7, 0w4, 0w0) => print "fnstsw\tax"
                |   (0w1, 0w5, 0w0) => print "fld1"
                |   (0w1, 0w6, 0w3) => print "fpatan"
                |   (0w1, 0w7, 0w2) => print "fsqrt"
                |   (0w1, 0w7, 0w6) => print "fsin"
                |   (0w1, 0w7, 0w7) => print "fcos"
                |   (0w1, 0w6, 0w7) => print "fincstp"
                |   (0w1, 0w6, 0w6) => print "fdecstp"
                |   (0w5, 0w2, rno) => print ("fst \tst(" ^ Word8.toString rno ^ ")")
                |   (0w5, 0w3, rno) => print ("fstp\tst(" ^ Word8.toString rno ^ ")")
                |   (0w1, 0w0, rno) => print ("fld \tst(" ^ Word8.toString rno ^ ")")
                |   (0w1, 0w1, rno) => print ("fxch\tst(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w3, rno) => print ("fcomp\tst(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w0, rno) => print ("fadd\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w1, rno) => print ("fmul\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w4, rno) => print ("fsub\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w5, rno) => print ("fsubr\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w6, rno) => print ("fdiv\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w0, 0w7, rno) => print ("fdivr\tst,st(" ^ Word8.toString rno ^ ")")
                |   (0w5, 0w0, rno) => print ("ffree\tst(" ^ Word8.toString rno ^ ")")
                |   _ => (printValue(Word8.toLargeInt opByte); printValue(Word8.toLargeInt opByte2));
                ptr +:= 0w1
            )
            else (* mod = 00, 01, 10 *)
            (
                case (escNo, nnn) of
                    (0w1, 0w0) => (print "fld \t"; printEA(rex, SZDWord)) (* Single precision. *)
                |   (0w1, 0w2) => (print "fst\t"; printEA(rex, SZDWord))
                |   (0w1, 0w3) => (print "fstp\t"; printEA(rex, SZDWord))
                |   (0w3, 0w0) => (print "fildl\t"; printEA(rex, SZQWord))
                |   (0w7, 0w5) => (print "fildq\t"; printEA(rex, SZQWord))
                |   (0w4, 0w0) => (print "fadd\t"; printEA(rex, SZQWord))
                |   (0w4, 0w1) => (print "fmul\t"; printEA(rex, SZQWord))
                |   (0w4, 0w3) => (print "fcomp\t"; printEA(rex, SZQWord))
                |   (0w4, 0w4) => (print "fsub\t"; printEA(rex, SZQWord))
                |   (0w4, 0w5) => (print "fsubr\t"; printEA(rex, SZQWord))
                |   (0w4, 0w6) => (print "fdiv\t"; printEA(rex, SZQWord))
                |   (0w4, 0w7) => (print "fdivr\t"; printEA(rex, SZQWord))
                |   (0w5, 0w0) => (print "fld \t"; printEA(rex, SZQWord)) (* Double precision. *)
                |   (0w5, 0w2) => (print "fst\t"; printEA(rex, SZQWord))
                |   (0w5, 0w3) => (print "fstp\t"; printEA(rex, SZQWord))
                |   _ => (printValue(Word8.toLargeInt opByte); printValue(Word8.toLargeInt opByte2))
            )
        end
        
        fun printJmp32 oper =
        let
            val valu = get32s (!ptr, seg) before (ptr +:= 0w4)
        in
            print oper; print "\t";
            print (Word.fmt StringCvt.HEX (!ptr + Word.fromLargeInt valu))
        end

        fun printMask mask =
        let
            val wordMask = Word.fromInt mask
            fun printAReg n =
                if n = regs then ()
                else
                (
                    if (wordMask andb (0w1 << Word.fromInt n)) <> 0w0
                    then (print(regRepr(regN n)); print " ")
                    else ();
                    printAReg(n+1)
                )
        in
            printAReg 0
        end

    in

        if procName = "" (* No name *) then print "?" else print procName;
        print ":\n";
 
        while get8u (!ptr, seg) <> 0wxf4 (* HLT. *) do
        let
            val () = print (Word.fmt StringCvt.HEX (!ptr)) (* The address in hex. *)
            val () = print "\t"

            (* See if we have a lock prefix. *)
            val () =
                if get8u (!ptr, seg) = 0wxF0
                then (print "lock "; ptr := !ptr + 0w1)
                else ()
                
            val legacyPrefix =
                let
                    val p = get8u (!ptr, seg)
                in
                    if p = 0wxF2 orelse p = 0wxF3 orelse p = 0wx66
                    then (ptr := !ptr + 0w1; p)
                    else 0wx0
                end

            (* See if we have a REX byte. *)
            val rex =
            let
               val b = get8u (!ptr, seg);
            in
               if b >= 0wx40 andalso b <= 0wx4f
               then (ptr := !ptr + 0w1; b)
               else 0w0
            end
        
            val rexW = (rex andb8 0wx8) <> 0w0
            val rexR = (rex andb8 0wx4) <> 0w0
            val rexB = (rex andb8 0wx1) <> 0w0

            val opByte = get8u (!ptr, seg) before ptr +:= 0w1
            
            val sizeFromRexW =
                if rexW then SZQWord else if legacyPrefix = 0wx66 then SZWord else SZDWord
        in
            case opByte of
                0wx03 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx0b => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx0f => (* ESCAPE *)
                let
                    (* Opcode is in next byte. *)
                    val opByte2  = codeVecGet (seg, !ptr)
                    val () = (ptr +:= 0w1)
                in
                    case legacyPrefix of
                        0w0 =>
                        (
                            case opByte2 of
                                0wxC1 =>
                                let
                                    val nb = codeVecGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    (* The address argument comes first in the assembly code. *)
                                    print "xadd\t";
                                    printEA (rex, sizeFromRexW);
                                    print ",";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW))
                                end

                            |   0wxB6 =>
                                let
                                    val nb = codeVecGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "movzx\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, SZByte)
                                end

                            |   0wxB7 =>
                                let
                                    val nb = codeVecGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "movzx\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, SZWord)
                                end

                            |   0wxAF =>
                                let
                                    val nb = codeVecGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "imul\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, sizeFromRexW)
                                end

                            |   0wx80 => printJmp32 "jo  "
                            |   0wx81 => printJmp32 "jno "
                            |   0wx82 => printJmp32 "jb  "
                            |   0wx83 => printJmp32 "jnb "
                            |   0wx84 => printJmp32 "je  "
                            |   0wx85 => printJmp32 "jne "
                            |   0wx86 => printJmp32 "jna "
                            |   0wx87 => printJmp32 "ja  " 
                            |   0wx88 => printJmp32 "js  "
                            |   0wx89 => printJmp32 "jns " 
                            |   0wx8a => printJmp32 "jp  "
                            |   0wx8b => printJmp32 "jnp " 
                            |   0wx8c => printJmp32 "jl  "
                            |   0wx8d => printJmp32 "jge "
                            |   0wx8e => printJmp32 "jle "
                            |   0wx8f => printJmp32 "jg  "
                            
                            |   _ => (print "esc\t"; printValue(Word8.toLargeInt opByte2))
                        )
                    
                    |   0wxf2 => (* SSE2 instruction *)
                        let
                            val nb = codeVecGet (seg, !ptr)
                            val reg = SSE2Reg((nb >>- 0w3) andb8 0w7)
                        in
                            case opByte2 of
                                0wx10 => ( print "movsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx11 => ( print "movsd\t"; printEAxmm(rex, SZQWord); print ","; print(xmmRegRepr reg)  )
                            |   0wx2a => ( print "cvtsi2sd\t"; print(xmmRegRepr reg); print ","; printEA(rex, sizeFromRexW)  )
                            |   0wx58 => ( print "addsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx59 => ( print "mulsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx5a => ( print "cvtsd2ss\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx5c => ( print "subsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx5e => ( print "divsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   b => (print "F2\n"; print "0F\n"; print(Word8.fmt StringCvt.HEX b))
                        end

                    |   0wxf3 => (* SSE2 instruction. *)
                        let
                            val nb = codeVecGet (seg, !ptr)
                            val reg = SSE2Reg((nb >>- 0w3) andb8 0w7)
                        in
                            case opByte2 of
                                0wx5a => ( print "cvtss2sd\t"; print(xmmRegRepr reg); print ","; printEA(rex, sizeFromRexW)  )
                            |   0wx11 => ( print "movss\t"; printEAxmm(rex, SZDWord); print ","; print(xmmRegRepr reg)  )
                            |   b => (print "F3\n"; print "0F\n"; print(Word8.fmt StringCvt.HEX b))
                        end

                    |   0wx66 => (* SSE2 instruction *)
                        let
                            val nb = codeVecGet (seg, !ptr)
                            val reg = SSE2Reg((nb >>- 0w3) andb8 0w7)
                        in
                            case opByte2 of
                                0wx2e => ( print "ucomisd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx54 => ( print "andpd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx57 => ( print "xorpd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   b => (print "66\n"; print "0F\n"; print(Word8.fmt StringCvt.HEX b))
                        end

                    |   _ => (print "esc\t"; printValue(Word8.toLargeInt opByte2))
                end (* ESCAPE *)

            |   0wx13 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx1b => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx23 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx2b => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx33 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx3b => printGvEv (opByte, rex, rexR, sizeFromRexW)

                (* Push and Pop.  These are 64-bit on X86/64 whether there is REX prefix or not. *)
            |   0wx50 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx51 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx52 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx53 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx54 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx55 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx56 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx57 => print ("push\t" ^  genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))

            |   0wx58 => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx59 => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5a => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5b => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5c => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5d => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5e => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))
            |   0wx5f => print ("pop \t" ^ genRegRepr (mkReg (opByte mod 0w8, rexB), sz32_64))

            |   0wx68 => (print "push\t"; print32 ())
            |   0wx69 =>
                let
                    (* Register is in next byte. *)
                    val nb = codeVecGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "imul\t"; print(genRegRepr (mkReg(reg, rexR), sizeFromRexW)); print ",";
                    printEA(rex, sizeFromRexW); print ","; print32 ()
                end
            |   0wx6a => (print "push\t"; print8 ())
            |   0wx6b =>
                let
                    (* Register is in next byte. *)
                    val nb = codeVecGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "imul\t"; print(genRegRepr (mkReg(reg, rexR), sizeFromRexW)); print ",";
                    printEA(rex, sizeFromRexW); print ","; print8 ()
                end

            |   0wx70 => (print "jo  \t"; printJmp())
            |   0wx71 => (print "jno \t"; printJmp())
            |   0wx72 => (print "jb  \t"; printJmp())
            |   0wx73 => (print "jnb \t"; printJmp())
            |   0wx74 => (print "je  \t"; printJmp())
            |   0wx75 => (print "jne \t"; printJmp())
            |   0wx76 => (print "jna \t"; printJmp())
            |   0wx77 => (print "ja  \t"; printJmp())
            |   0wx78 => (print "js  \t"; printJmp())
            |   0wx79 => (print "jns \t"; printJmp())
            |   0wx7a => (print "jp  \t"; printJmp())
            |   0wx7b => (print "jnp \t"; printJmp())
            |   0wx7c => (print "jl  \t"; printJmp())
            |   0wx7d => (print "jge \t"; printJmp())
            |   0wx7e => (print "jle \t"; printJmp())
            |   0wx7f => (print "jg  \t"; printJmp())

            |   0wx80 => (* Group1_8_a *)
                let (* Memory, byte constant *)
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (codeVecGet (seg, !ptr))
                in
                    printArith ((nb div 8) mod 8);
                    print "\t";
                    printEA(rex, SZByte);
                    print ",";
                    print8 ()
                end

            |   0wx81 =>
                let (* Memory, 32-bit constant *)
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (codeVecGet (seg, !ptr))
                in
                    printArith ((nb div 8) mod 8);
                    print "\t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print32 ()
                end

            |   0wx83 =>
                let (* Word memory, 8-bit constant *)
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (codeVecGet (seg, !ptr))
                in
                    printArith ((nb div 8) mod 8);
                    print "\t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print8 ()
                end

            |   0wx87 =>
                let (* xchng *)
                    (* Register is in next byte. *)
                    val nb = codeVecGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "xchng \t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW))
                end

            |   0wx88 =>
                let (* mov eb,gb i.e a store *)
                    (* Register is in next byte. *)
                    val nb = Word8.toInt (codeVecGet (seg, !ptr));
                    val reg = (nb div 8) mod 8;
                in
                    print "mov \t";
                    printEA(rex, SZByte);
                    print ",";
                    if rexR
                    then print ("r" ^ Int.toString(reg+8) ^ "B")
                    else case reg of
                        0 => print "al"
                    |   1 => print "cl"
                    |   2 => print "dl"
                    |   3 => print "bl"
                         (* If there is a REX byte these select the low byte of the registers. *)
                    |   4 => print (if rex = 0w0 then "ah" else "sil")
                    |   5 => print (if rex = 0w0 then "ch" else "dil")
                    |   6 => print (if rex = 0w0 then "dh" else "bpl")
                    |   7 => print (if rex = 0w0 then "bh" else "spl")
                    |   _ => print ("r" ^ Int.toString reg)
                end

            |   0wx89 =>
                let (* mov ev,gv i.e. a store *)
                    (* Register is in next byte. *)
                    val nb = codeVecGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "mov \t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW))
                end
         
            |   0wx8b =>
                let (* mov gv,ev i.e. a load *)
                    (* Register is in next byte. *)
                    val nb = codeVecGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "mov \t";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                    print ",";
                    printEA(rex, sizeFromRexW)
                end

            |   0wx8d =>
                let (* lea gv.M *)
                    (* Register is in next byte. *)
                    val nb = codeVecGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "lea \t";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                    print ",";
                    printEA(rex, sizeFromRexW)
                end
         
            |   0wx8f => (print "pop \t"; printEA(rex, sz32_64))
            |   0wx90 => print "nop"
            
            |   0wx99 => if rexW then print "cqo" else print "cdq"
            
            |   0wx9e => print "sahf\n"

            |   0wxa4 => (if legacyPrefix = 0wxf3 then print "rep " else (); print "movsb")
            |   0wxa5 => (if legacyPrefix = 0wxf3 then print "rep " else (); print "movsl")
            |   0wxa6 => (if legacyPrefix = 0wxf3 then print "repe " else (); print "cmpsb")

            |   0wxa8 => (print "test\tal,"; print8 ())

            |   0wxaa => (if legacyPrefix = 0wxf3 then print "rep " else (); print "stosb")
            |   0wxab =>
                (
                    if legacyPrefix = 0wxf3 then print "rep " else ();
                    if rexW then print "stosq" else print "stosl"
                )

            |   0wxb8 => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxb9 => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxba => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbb => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbc => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbd => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbe => printMovCToR (opByte, sizeFromRexW, rexB)
            |   0wxbf => printMovCToR (opByte, sizeFromRexW, rexB)
   
            |   0wxc1 => (* Group2_8_A *) printShift (opByte, rex, sizeFromRexW)

            |   0wxc2 => (print "ret \t"; print16 ())
            |   0wxc3 => print "ret"
         
            |   0wxc6 => (* move 8-bit constant to memory *)
                (
                    print "mov \t";
                    printEA(rex, SZByte);
                    print ",";
                    print8 ()
                )

            |   0wxc7 => (* move 32/64-bit constant to memory *)
                (
                    print "mov \t";
                    printEA(rex, sizeFromRexW);
                    print ",";
                    print32 ()
                )
            
            |   0wxca => (* Register mask *)
                let
                    val mask = get16u (!ptr, seg) before (ptr +:= 0w2)
                in
                    print "SAVE\t";
                    printMask mask
                end

            |   0wxcd => (* Register mask *)
                let
                    val mask = get8u (!ptr, seg) before (ptr +:= 0w1)
                in
                    print "SAVE\t";
                    printMask(Word8.toInt mask)
                end

            |   0wxd1 => (* Group2_1_A *) printShift (opByte, rex, sizeFromRexW)

            |   0wxd3 => (* Group2_CL_A *) printShift (opByte, rex, sizeFromRexW)
           
            |   0wxd8 => printFloat (opByte, rex) (* Floating point escapes *)
            |   0wxd9 => printFloat (opByte, rex)
            |   0wxda => printFloat (opByte, rex)
            |   0wxdb => printFloat (opByte, rex)
            |   0wxdc => printFloat (opByte, rex)
            |   0wxdd => printFloat (opByte, rex)
            |   0wxde => printFloat (opByte, rex)
            |   0wxdf => printFloat (opByte, rex)

            |   0wxe8 =>
                let (* 32-bit relative call. *)
                    val valu = get32s (!ptr, seg) before (ptr +:= 0w4)
                in
                    print "call\t";
                    print (Word.fmt StringCvt.HEX (!ptr + Word.fromLargeInt valu))
                end

            |   0wxe9 =>
                let (* 32-bit relative jump. *)
                    val valu = get32s (!ptr, seg) before (ptr +:= 0w4)
                in
                    print "jmp \t";
                    print (Word.fmt StringCvt.HEX (!ptr + Word.fromLargeInt valu))
                end

            |   0wxeb => (print "jmp \t"; printJmp())
            
            |   0wxf4 => print "hlt" (* Marker to indicate end-of-code. *)
        
            |   0wxf6 => (* Group3_a *)
                let
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (codeVecGet (seg, !ptr))
                    val opc = (nb div 8) mod 8
                in
                    print
                      (case opc of
                         0 => "test"
                       | 3 => "neg"
                       | _ => "???"
                      );
                    print "\t";
                    printEA(rex, SZByte);
                    if opc = 0 then (print ","; print8 ()) else ()
                end

            |   0wxf7 => (* Group3_A *)
                let
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (codeVecGet (seg, !ptr))
                    val opc = (nb div 8) mod 8
                in
                    print
                      (case opc of
                         0 => "test"
                       | 3 => "neg "
                       | 4 => "mul "
                       | 5 => "imul"
                       | 6 => "div "
                       | 7 => "idiv"
                       | _ => "???"
                      );
                    print "\t";
                    printEA(rex, sizeFromRexW);
                    (* Test has an immediate operand.  It's 32-bits even in 64-bit mode. *)
                    if opc = 0 then (print ","; print32 ()) else ()
                end
         
            |   0wxff => (* Group5 *)
                let
                    (* Opcode is determined by next byte. *)
                    val nb = Word8.toInt (codeVecGet (seg, !ptr))
                    val opc = (nb div 8) mod 8
                in
                    print
                      (case opc of
                         2 => "call"
                       | 4 => "jmp "
                       | 6 => "push"
                       | _ => "???"
                      );
                    print "\t";
                    printEA(rex, sz32_64) (* None of the cases we use need a prefix. *)
                end
 
            |   _ => print(Word8.fmt StringCvt.HEX opByte);
      
            print "\n"
        end; (* end of while loop *)

        print "\n"

    end (* printCode *);

    (* This actually does the final code-generation. *)
    fun generateCode
        {ops=operations,
         code=cvec as Code{procName, printAssemblyCode, printStream, profileObject, ...},
         labelCount} : address =
    let
        val (expanded, newLabelCount) = expandComplexOperations (operations, labelCount)

        val () = printLowLevelCode(expanded, cvec)
        local
            val initialBytesList = codeGenerate expanded
        in
            (* Fixup labels and shrink long branches to short. *)
            val (bytesList, codeSize) = fixupLabels(expanded, initialBytesList, newLabelCount)
        end

        val constantList = getConstants(expanded, bytesList)
        (* Get the number of constants that need to be added to the address area. *)
        val constsInConstArea = List.foldl (fn ((_, ConstantArea _), n) => n+1 | (_, n) => n) 0 constantList

        local
            (* Add one byte for the HLT and round up to a number of words. *)
            val endOfCode = (codeSize+Word.fromInt wordSize) div Word.fromInt wordSize
            val realSize = 0w8 div Word.fromInt wordSize
            fun addSizes((_, RealConstant _), n) = n+realSize
            |   addSizes((_, IntConstant _), n) = n+0w1
            |   addSizes(_, n) = n
            val numOfNonAddrWords = List.foldl addSizes 0w0 constantList
        in
            val endOfByteArea = endOfCode + numOfNonAddrWords
            (* +4 for function name, register mask (no longer used), profile object and count of constants. *)
            val segSize = endOfByteArea + Word.fromInt constsInConstArea + 0w4
        end

        (* Create a byte vector and copy the data in.  This is a byte area and not scanned
           by the GC so cannot contain any addresses. *)
        val byteVec = byteVecMake segSize
        val ic = ref 0w0
        
        local
            fun genByte (ival: Word8.word) = set8u (ival, !ic, byteVec) before ic := !ic + 0w1
        in
            fun genBytes l = Word8Vector.app (fn i => genByte i) l
            val () = List.app (fn b => genBytes b) bytesList
            val () = genBytes(Word8Vector.fromList(opCodeBytes(HLT, NONE))) (* Marker - this is used by ScanConstants in the RTS. *)
        end
    
        (* Align ic onto a fullword boundary. *)
        val ()   = ic := ((!ic + Word.fromInt wordSize - 0w1) andb ~ (Word.fromInt wordSize))
        
        (* Copy the non-address constants.  These are only used in 64-bit mode and are
           either real constants or integers that are too large to fit in a 32-bit
           inline constants.  We don't use this for real constants in 32-bit mode because
           we don't have relative addressing.  Instead a real constant is the inline
           address of a boxed real number. *)
        local
            fun putNonAddrConst(addrs, RealConstant m) =
                let
                    val addrOfConst = ! ic
                    val c = toMachineWord m
                    val cAsAddr = toAddress c
                    (* For the moment this should always be a real number contained in
                       a byte segment.  If this changes in the future we may need to
                       align this back onto a 4/8-byte boundary. *)
                    val cLength = length cAsAddr * Word.fromInt wordSize
                    val _ = (cLength = 0w8 andalso flags cAsAddr = F_bytes) orelse
                                raise InternalError "putNonAddrConst: Not a real number"
                    fun doCopy n =
                        if n = cLength then ()
                        else (genBytes(Word8Vector.fromList[loadByte(cAsAddr, n)]); doCopy(n+0w1))
                    val () = doCopy 0w0
                in
                    set32s(Word.toLargeInt(addrOfConst - addrs - 0w4), addrs, byteVec)
                end

            |   putNonAddrConst(addrs, IntConstant imm) =
                let
                    val addrOfConst = ! ic
                    fun setMem(m, n) = 
                    if n = Word.fromInt wordSize then ()
                    else 
                    (
                        genBytes(Word8Vector.fromList[Word8.fromLargeInt(m mod 0x100)]);
                        setMem(m div 0x100, n+0w1)
                    )

                    val () = setMem(imm, 0w0)
                in
                    set32s(Word.toLargeInt(addrOfConst - addrs - 0w4), addrs, byteVec)
                end

            |   putNonAddrConst _ = ()
        in
            val () = List.app putNonAddrConst constantList
        end

        val _ = bytesToWords(! ic) = endOfByteArea orelse raise InternalError "mismatch"
        
        (* Put in the number of constants. This must go in before we actually put
           in any constants.  In 32-bit mode there are only three constants: the 
           function name and the register mask, now unused and the profile object.
           All other constants are in the code. *)
        local
            val addr = wordsToBytes(endOfByteArea + 0w3 + Word.fromInt constsInConstArea)

            fun setBytes(_, _, 0) = ()
            |   setBytes(ival, offset, count) =
                (
                    byteVecSet(byteVec, offset, Word8.fromLargeInt(ival mod 256));
                    setBytes(ival div 256, offset+0w1, count-1)
                )
        in
            val () = setBytes(LargeInt.fromInt(3 + constsInConstArea), addr, wordSize)
        end;

        (* We've put in all the byte data so it is safe to convert this to a mutable code
           cell that can contain addresses and will be scanned by the GC. *)
        val codeSeg = byteVecToCodeVec byteVec

        (* Various RTS functions assume that the first constant is the function name.
           The profiler assumes that the third word is the address of the mutable that
           contains the profile count.  The second word used to be used for the register
           mask but is no longer used. *)
        val () = codeVecPutWord (codeSeg, endOfByteArea, toMachineWord procName)
        val () = codeVecPutWord (codeSeg, endOfByteArea + 0w1, toMachineWord 1 (* No longer used. *))
        (* Next the profile object. *)
        val () = codeVecPutWord (codeSeg, endOfByteArea + 0w2, profileObject)
    in
        let
            fun setBytes(_, _, 0w0) = ()
            |   setBytes(b, addr, count) =
                (
                    codeVecSet (codeSeg, addr, wordToWord8 b);
                    setBytes(b >> 0w8, addr+0w1, count-0w1)
                )

            fun putConst ((addrs, SelfAddress), constAddr) =
                    (* Self address goes inline. *)
                    (codeVecPutConstant (codeSeg, addrs, toMachineWord(codeVecAddr codeSeg), ConstAbsolute); constAddr)

            |   putConst ((addrs, InlineAbsoluteAddress m), constAddr) =
                    (codeVecPutConstant (codeSeg, addrs, m, ConstAbsolute); constAddr)

            |   putConst ((addrs, InlineRelativeAddress m), constAddr) =
                    (codeVecPutConstant (codeSeg, addrs, m, ConstX86Relative); constAddr)

            |   putConst ((addrs, ConstantArea m), constAddr) =
                    (* Not inline.  Put the constant in the constant area and set the original address
                        to be the relative offset to the constant itself. *)
                    (
                        codeVecPutWord (codeSeg, constAddr, m);
                        (* Put in the 32-bit offset - always unsigned since the destination
                           is after the reference. *)
                        setBytes(constAddr * Word.fromInt wordSize - addrs - 0w4, addrs, 0w4);
                        constAddr+0w1
                    )

            |   putConst (_, constOffset) = constOffset

            (* Put the constants.  Any values in the constant area start at +3 i.e. after the profile. *)
            val _ = List.foldl putConst (endOfByteArea+0w3) constantList

            val () = 
                if printAssemblyCode
                then (* print out the code *)
                (
                    printCode(cvec, codeSeg);
                    printStream "\n\n"
                )
            else ()
        in
            (* Finally lock the code and return the code address. *)
            codeVecLockAndGetExecutable codeSeg 
        end (* the result *)
    end (* generateCode *)
 
    structure Sharing =
    struct
        type code           = code
        and  reg            = reg
        and  genReg         = genReg
        and  fpReg          = fpReg
        and  addrs          = addrs
        and  operation      = operation
        and  regSet         = RegSet.regSet
        and  label          = label
        and  branchOps      = branchOps
        and  callKinds      = callKinds
        and  arithOp        = arithOp
        and  shiftType      = shiftType
        and  repOps         = repOps
        and  fpOps          = fpOps
        and  fpUnaryOps     = fpUnaryOps
        and  sse2Operations = sse2Operations
    end

end (* struct *) (* CODECONS *);
