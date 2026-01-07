(*
    Copyright David C. J. Matthews 1989, 2000, 2009-10, 2012-13, 2015-21, 2023

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

functor X86OutputCode (
structure Debug: DEBUG
structure Pretty: PRETTY (* for compilerOutTag *)
structure CodeArray: CODEARRAY

) : X86CODE =

struct
    open CodeArray
    open Debug
    open Address
    open Misc

    (* May be targeted at native 32-bit, native 64-bit or X86/64 with 32-bit words
       and addresses as object Ids. *)
    datatype targetArch = Native32Bit | Native64Bit | ObjectId32Bit2Word | ObjectId32Bit4Word


    val targetArch =
        case (PolyML.architecture(), Address.compact32UnitSize) of
            ("I386", _)      => Native32Bit
        |   ("X86_64", _)    => Native64Bit
        |   ("X86_64_32", 0w2) => ObjectId32Bit2Word
        |   ("X86_64_32", 0w4) => ObjectId32Bit4Word
        |   _ => raise InternalError "Unknown target architecture or invalid unit length"
    
    (* Some checks - *)
    val () =
        case (targetArch, wordSize, nativeWordSize) of
            (Native32Bit, 0w4, 0w4) => ()
        |   (Native64Bit, 0w8, 0w8) => ()
        |   (ObjectId32Bit2Word, 0w4, 0w8) => ()
        |   (ObjectId32Bit4Word, 0w4, 0w8) => ()
        |   _ => raise InternalError "Mismatch of architecture and word-length"

    val hostIsX64 = targetArch <> Native32Bit

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
    fun is32bit v = not hostIsX64 orelse ~exp2_31 <= v andalso v < exp2_31

    (* tag a short constant *)
    fun tag c = 2 * c + 1;

    fun is8BitL (n: LargeInt.int) = ~ 0x80 <= n andalso n < 0x80

    local
        val shift =
            if wordSize = 0w4
            then 0w2
            else if wordSize = 0w8
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
     offsets into that structure. *)
    val memRegLocalMPointer       = 0 (* Not used in 64-bit *)
    and memRegHandlerRegister     = Word.toInt nativeWordSize
    and memRegLocalMbottom        = 2 * Word.toInt nativeWordSize
    and memRegStackLimit          = 3 * Word.toInt nativeWordSize
    and memRegExceptionPacket     = 4 * Word.toInt nativeWordSize
    and memRegCStackPtr           = 6 * Word.toInt nativeWordSize
    and memRegThreadSelf          = 7 * Word.toInt nativeWordSize
    and memRegStackPtr            = 8 * Word.toInt nativeWordSize
    and memRegHeapOverflowCall    = 10 * Word.toInt nativeWordSize
    and memRegStackOverflowCall   = 11 * Word.toInt nativeWordSize
    and memRegStackOverflowCallEx = 12 * Word.toInt nativeWordSize
    and memRegArbPrecisionCall    = 13 * Word.toInt nativeWordSize
    and memRegSavedRbx            = 15 * Word.toInt nativeWordSize (* Heap base in 32-in-64. *)

    (* create and initialise a code segment *)
    fun codeCreate (name : string, profObj, parameters) : code =
    let
        val printStream = Pretty.getSimplePrinter(parameters, [])
    in
        Code
        { 
            procName       = name,
            printAssemblyCode = Debug.getParameter Debug.assemblyCodeTag parameters,
            printStream    = printStream,
            lowLevelOptimise = Debug.getParameter Debug.lowlevelOptimiseTag parameters,
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
    val rax = GeneralReg (0w0, false)
    val rcx = GeneralReg (0w1, false)
    val rdx = GeneralReg (0w2, false)
    val rbx = GeneralReg (0w3, false)
    val rsp = GeneralReg (0w4, false)
    val rbp = GeneralReg (0w5, false)
    val rsi = GeneralReg (0w6, false)
    val rdi = GeneralReg (0w7, false)
    val eax = rax and ecx = rcx and edx = rdx and ebx = rbx
    and esp = rsp and ebp = rbp and esi = rsi and edi = rdi
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
    and xmm7 = SSE2Reg 0w7

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
    
    (* Default size when printing regs. *)
    val sz32_64 = if hostIsX64 then SZQWord else SZDWord

    fun genRegRepr(GeneralReg (0w0, false), SZByte) = "al"
    |   genRegRepr(GeneralReg (0w1, false), SZByte) = "cl"
    |   genRegRepr(GeneralReg (0w2, false), SZByte) = "dl"
    |   genRegRepr(GeneralReg (0w3, false), SZByte) = "bl"
    |   genRegRepr(GeneralReg (0w4, false), SZByte) = "ah"
    |   genRegRepr(GeneralReg (0w5, false), SZByte) = "ch"
    |   genRegRepr(GeneralReg (0w6, false), SZByte) = "sil" (* Assume there's a Rex code that forces low-order reg *)
    |   genRegRepr(GeneralReg (0w7, false), SZByte) = "dil"
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
    
    (* Size of operand.  OpSize64 is only valid in 64-bit mode. *)
    datatype opSize = OpSize32 | OpSize64


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

    datatype repOps = CMPS8 | MOVS8 | MOVS32 | STOS8 | STOS32 | MOVS64 | STOS64
    
    fun repOpsToWord CMPS8  = 0wxa6: Word8.word
    |   repOpsToWord MOVS8  = 0wxa4
    |   repOpsToWord MOVS32 = 0wxa5
    |   repOpsToWord MOVS64 = 0wxa5 (* Plus Rex.w *)
    |   repOpsToWord STOS8  = 0wxaa
    |   repOpsToWord STOS32 = 0wxab
    |   repOpsToWord STOS64 = 0wxab (* Plus Rex.w *)

    fun repOpsRepr CMPS8    = "CompareBytes"
    |   repOpsRepr MOVS8    = "MoveBytes"
    |   repOpsRepr MOVS32   = "MoveWords32"
    |   repOpsRepr MOVS64   = "MoveWords64"
    |   repOpsRepr STOS8    = "StoreBytes"
    |   repOpsRepr STOS32   = "StoreWords32"
    |   repOpsRepr STOS64   = "StoreWords64"

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
 
    fun branchOpRepr JO = "Overflow"
    |   branchOpRepr JNO = "NotOverflow"
    |   branchOpRepr JE = "Equal"
    |   branchOpRepr JNE = "NotEqual"
    |   branchOpRepr JL = "Less"
    |   branchOpRepr JGE = "GreaterOrEqual"
    |   branchOpRepr JLE = "LessOrEqual"
    |   branchOpRepr JG = "Greater"
    |   branchOpRepr JB = "Before"
    |   branchOpRepr JNB= "NotBefore"
    |   branchOpRepr JNA = "NotAfter"
    |   branchOpRepr JA = "After"
    |   branchOpRepr JP = "Parity"
    |   branchOpRepr JNP = "NoParity"
    
    (* Invert a test.  This is used if we want to change the
       sense of a test from jumping if the condition is true to
       jumping if it is false. *)
    fun invertTest JE  = JNE
    |   invertTest JNE = JE
    |   invertTest JA  = JNA
    |   invertTest JB  = JNB
    |   invertTest JNA = JA
    |   invertTest JNB = JB
    |   invertTest JL  = JGE
    |   invertTest JG  = JLE
    |   invertTest JLE = JG
    |   invertTest JGE = JL
    |   invertTest JO  = JNO
    |   invertTest JNO = JO
    |   invertTest JP  = JNP
    |   invertTest JNP = JP

    datatype sse2Operations =
        SSE2MoveDouble | SSE2MoveFloat | SSE2CompDouble | SSE2AddDouble |
        SSE2SubDouble | SSE2MulDouble | SSE2DivDouble |
        SSE2Xor | SSE2And | SSE2FloatToDouble | SSE2DoubleToFloat |
        SSE2CompSingle | SSE2AddSingle | SSE2SubSingle | SSE2MulSingle | SSE2DivSingle
    
    fun sse2OpRepr SSE2MoveDouble   = "SSE2MoveDouble"
    |   sse2OpRepr SSE2MoveFloat    = "SSE2MoveFloat"
    |   sse2OpRepr SSE2CompDouble   = "SSE2CompDouble"
    |   sse2OpRepr SSE2AddDouble    = "SSE2AddDouble"
    |   sse2OpRepr SSE2SubDouble    = "SSE2SubDouble"
    |   sse2OpRepr SSE2MulDouble    = "SSE2MulDouble"
    |   sse2OpRepr SSE2DivDouble    = "SSE2DivDouble"
    |   sse2OpRepr SSE2Xor          = "SSE2Xor"
    |   sse2OpRepr SSE2And          = "SSE2And"
    |   sse2OpRepr SSE2CompSingle   = "SSE2CompSingle"
    |   sse2OpRepr SSE2AddSingle    = "SSE2AddSingle"
    |   sse2OpRepr SSE2SubSingle    = "SSE2SubSingle"
    |   sse2OpRepr SSE2MulSingle    = "SSE2MulSingle"
    |   sse2OpRepr SSE2DivSingle    = "SSE2DivSingle"
    |   sse2OpRepr SSE2FloatToDouble = "SSE2FloatToDouble"
    |   sse2OpRepr SSE2DoubleToFloat = "SSE2DoubleToFloat"

    (* Primary opCodes.  N.B. only opCodes actually used are listed here.
       If new instruction are added check they will be handled by the
       run-time system in the event of trap. *)    
    datatype opCode =
        Group1_8_A32
    |   Group1_8_A64
    |   Group1_32_A32
    |   Group1_32_A64
    |   Group1_8_a
    |   JMP_8
    |   JMP_32
    |   CALL_32
    |   MOVL_A_R32
    |   MOVL_A_R64
    |   MOVL_R_A32
    |   MOVL_R_A64
    |   MOVL_R_A16
    |   MOVB_R_A32
    |   MOVB_R_A64 of {forceRex: bool}
    |   PUSH_R of Word8.word
    |   POP_R  of Word8.word
    |   Group5
    |   NOP
    |   LEAL32
    |   LEAL64
    |   MOVL_32_R of Word8.word
    |   MOVL_64_R of Word8.word
    |   MOVL_32_A32
    |   MOVL_32_A64
    |   MOVB_8_A
    |   POP_A
    |   RET
    |   RET_16
    |   CondJump of branchOps
    |   CondJump32 of branchOps
    |   SetCC of branchOps
    |   Arith32 of arithOp * Word8.word
    |   Arith64 of arithOp * Word8.word
    |   Group3_A32
    |   Group3_A64
    |   Group3_a
    |   Group2_8_A32
    |   Group2_8_A64
    |   Group2_CL_A32
    |   Group2_CL_A64
    |   Group2_1_A32
    |   Group2_1_A64
    |   PUSH_8
    |   PUSH_32
    |   TEST_ACC8
    |   LOCK_XADD32
    |   LOCK_XADD64
    |   LOCK_CMPXCHG32
    |   LOCK_CMPXCHG64
    |   FPESC of Word8.word
    |   XCHNG32
    |   XCHNG64
    |   REP (* Rep prefix *)
    |   MOVZB (* Needs escape code. *)
    |   MOVZW (* Needs escape code. *)
    |   MOVSXB32 (* Needs escape code. *)
    |   MOVSXW32 (* Needs escape code. *)
    |   MOVSXB64 (* Needs escape code. *)
    |   MOVSXW64 (* Needs escape code. *)
    |   IMUL32 (* Needs escape code. *)
    |   IMUL64 (* Needs escape code. *)
    |   SSE2StoreSingle (* movss with memory destination - needs escape sequence. *)
    |   SSE2StoreDouble (* movsd with memory destination - needs escape sequence. *)
    |   CQO_CDQ32 (* Sign extend before divide.. *)
    |   CQO_CDQ64 (* Sign extend before divide.. *)
    |   SSE2Ops of sse2Operations (* SSE2 instructions. *)
    |   CVTSI2SD32 (* 32 bit int to double *)
    |   CVTSI2SD64 (* 64 bit int to double *)
    |   CVTSI2SS32 (* 32 bit int to single *)
    |   CVTSI2SS64 (* 64 bit int to single *)
    |   HLT     (* End of code marker. *)
    |   IMUL_C8_32
    |   IMUL_C8_64
    |   IMUL_C32_32
    |   IMUL_C32_64
    |   MOVDFromXMM (* move 32 bit value from XMM to general reg. *)
    |   MOVQToXMM   (* move 64 bit value from general reg.to XMM *)
    |   PSRLDQ (* Shift XMM register *)
    |   LDSTMXCSR
    |   CVTSD2SI32 (* Double to 32-bit int *)
    |   CVTSD2SI64 (* Double to 64-bit int *)
    |   CVTSS2SI32 (* Single to 32-bit int *)
    |   CVTSS2SI64 (* Single to 64-bit int *)
    |   CVTTSD2SI32 (* Double to 32-bit int - truncate towards zero *)
    |   CVTTSD2SI64 (* Double to 64-bit int - truncate towards zero *)
    |   CVTTSS2SI32 (* Single to 32-bit int - truncate towards zero *)
    |   CVTTSS2SI64 (* Single to 64-bit int - truncate towards zero *)
    |   MOVSXD
    |   CMOV32 of branchOps
    |   CMOV64 of branchOps
    |   PAUSE
    |   BSR32 (* Needs escape code *)
    |   BSR64 (* Needs escape code *)


    fun opToInt Group1_8_A32    =  0wx83
    |   opToInt Group1_8_A64    =  0wx83
    |   opToInt Group1_32_A32   =  0wx81
    |   opToInt Group1_32_A64   =  0wx81
    |   opToInt Group1_8_a      =  0wx80
    |   opToInt JMP_8           =  0wxeb
    |   opToInt JMP_32          =  0wxe9
    |   opToInt CALL_32         =  0wxe8
    |   opToInt MOVL_A_R32      =  0wx8b
    |   opToInt MOVL_A_R64      =  0wx8b
    |   opToInt MOVL_R_A32      =  0wx89
    |   opToInt MOVL_R_A64      =  0wx89
    |   opToInt MOVL_R_A16      =  0wx89 (* Also has an OPSIZE prefix. *)
    |   opToInt MOVB_R_A32      =  0wx88
    |   opToInt (MOVB_R_A64 _)  =  0wx88
    |   opToInt (PUSH_R reg)    =  0wx50 + reg
    |   opToInt (POP_R  reg)    =  0wx58 + reg
    |   opToInt Group5          =  0wxff
    |   opToInt NOP             =  0wx90
    |   opToInt LEAL32          =  0wx8d
    |   opToInt LEAL64          =  0wx8d
    |   opToInt (MOVL_32_R reg) =  0wxb8 + reg
    |   opToInt (MOVL_64_R reg) =  0wxb8 + reg
    |   opToInt MOVL_32_A32     =  0wxc7
    |   opToInt MOVL_32_A64     =  0wxc7
    |   opToInt MOVB_8_A        =  0wxc6
    |   opToInt POP_A           =  0wx8f
    |   opToInt RET             = 0wxc3
    |   opToInt RET_16          = 0wxc2
    |   opToInt (CondJump opc)  = 0wx70 + branchOpToWord opc
    |   opToInt (CondJump32 opc) = 0wx80 + branchOpToWord opc (* Needs 0F prefix *)
    |   opToInt (SetCC opc)     = 0wx90 + branchOpToWord opc (* Needs 0F prefix *)
    |   opToInt (Arith32 (ao,dw)) = arithOpToWord ao * 0w8 + dw
    |   opToInt (Arith64 (ao,dw)) = arithOpToWord ao * 0w8 + dw
    |   opToInt Group3_A32      = 0wxf7
    |   opToInt Group3_A64      = 0wxf7
    |   opToInt Group3_a        = 0wxf6
    |   opToInt Group2_8_A32    = 0wxc1
    |   opToInt Group2_8_A64    = 0wxc1
    |   opToInt Group2_1_A32    = 0wxd1
    |   opToInt Group2_1_A64    = 0wxd1
    |   opToInt Group2_CL_A32   = 0wxd3
    |   opToInt Group2_CL_A64   = 0wxd3
    |   opToInt PUSH_8          = 0wx6a
    |   opToInt PUSH_32         = 0wx68
    |   opToInt TEST_ACC8       = 0wxa8
    |   opToInt LOCK_XADD32     = 0wxC1 (* Needs lock and escape prefixes. *)
    |   opToInt LOCK_XADD64     = 0wxC1 (* Needs lock and escape prefixes. *)
    |   opToInt LOCK_CMPXCHG32  = 0wxB1 (* Needs lock and escape prefixes. *)
    |   opToInt LOCK_CMPXCHG64  = 0wxB1 (* Needs lock and escape prefixes. *)
    |   opToInt (FPESC n)       = 0wxD8 orb8 n
    |   opToInt XCHNG32         = 0wx87
    |   opToInt XCHNG64         = 0wx87
    |   opToInt REP             = 0wxf3
    |   opToInt MOVZB           = 0wxb6 (* Needs escape code. *)
    |   opToInt MOVZW           = 0wxb7 (* Needs escape code. *)
    |   opToInt MOVSXB32        = 0wxbe (* Needs escape code. *)
    |   opToInt MOVSXW32        = 0wxbf (* Needs escape code. *)
    |   opToInt MOVSXB64        = 0wxbe (* Needs escape code. *)
    |   opToInt MOVSXW64        = 0wxbf (* Needs escape code. *)
    |   opToInt IMUL32          = 0wxaf (* Needs escape code. *)
    |   opToInt IMUL64          = 0wxaf (* Needs escape code. *)
    |   opToInt SSE2StoreSingle = 0wx11 (* Needs F3 0F escape. *)
    |   opToInt SSE2StoreDouble = 0wx11 (* Needs F2 0F escape. *)
    |   opToInt CQO_CDQ32       = 0wx99
    |   opToInt CQO_CDQ64       = 0wx99
    |   opToInt (SSE2Ops SSE2MoveDouble) = 0wx10 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2MoveFloat) = 0wx10 (* Needs F3 0F escape. *)
    |   opToInt (SSE2Ops SSE2CompDouble) = 0wx2E (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2AddDouble)  = 0wx58 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2SubDouble)  = 0wx5c (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2MulDouble)  = 0wx59 (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2DivDouble)  = 0wx5e (* Needs F2 0F escape. *)
    |   opToInt (SSE2Ops SSE2CompSingle) = 0wx2E (* Needs 0F escape. *)
    |   opToInt (SSE2Ops SSE2AddSingle)  = 0wx58 (* Needs F3 0F escape. *)
    |   opToInt (SSE2Ops SSE2SubSingle)  = 0wx5c (* Needs F3 0F escape. *)
    |   opToInt (SSE2Ops SSE2MulSingle)  = 0wx59 (* Needs F3 0F escape. *)
    |   opToInt (SSE2Ops SSE2DivSingle)  = 0wx5e (* Needs F3 0F escape. *)
    |   opToInt (SSE2Ops SSE2And)  = 0wx54 (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2Xor)  = 0wx57 (* Needs 66 0F escape. *)
    |   opToInt (SSE2Ops SSE2FloatToDouble)  = 0wx5A (* Needs F3 0F escape. *)
    |   opToInt (SSE2Ops SSE2DoubleToFloat)  = 0wx5A (* Needs F2 0F escape. *)
    |   opToInt CVTSI2SD32      = 0wx2a (* Needs F2 0F escape. *)
    |   opToInt CVTSI2SD64      = 0wx2a (* Needs F2 0F escape. *)
    |   opToInt CVTSI2SS32      = 0wx2a (* Needs F3 0F escape. *)
    |   opToInt CVTSI2SS64      = 0wx2a (* Needs F3 0F escape. *)
    |   opToInt HLT             = 0wxf4
    |   opToInt IMUL_C8_32      = 0wx6b
    |   opToInt IMUL_C8_64      = 0wx6b
    |   opToInt IMUL_C32_32     = 0wx69
    |   opToInt IMUL_C32_64     = 0wx69
    |   opToInt MOVDFromXMM     = 0wx7e  (* Needs 66 0F escape. *)
    |   opToInt MOVQToXMM       = 0wx6e  (* Needs 66 0F escape. *)
    |   opToInt PSRLDQ          = 0wx73  (* Needs 66 0F escape. *)
    |   opToInt LDSTMXCSR       = 0wxae  (* Needs 0F prefix. *)
    |   opToInt CVTSD2SI32      = 0wx2d  (* Needs F2 0F prefix. *)
    |   opToInt CVTSD2SI64      = 0wx2d  (* Needs F2 0F prefix and rex.w. *)
    |   opToInt CVTSS2SI32      = 0wx2d  (* Needs F3 0F prefix. *)
    |   opToInt CVTSS2SI64      = 0wx2d  (* Needs F3 0F prefix and rex.w. *)
    |   opToInt CVTTSD2SI32     = 0wx2c  (* Needs F2 0F prefix. *)
    |   opToInt CVTTSD2SI64     = 0wx2c  (* Needs F2 0F prefix. *)
    |   opToInt CVTTSS2SI32     = 0wx2c  (* Needs F3 0F prefix. *)
    |   opToInt CVTTSS2SI64     = 0wx2c  (* Needs F3 0F prefix and rex.w. *)
    |   opToInt MOVSXD          = 0wx63
    |   opToInt (CMOV32 opc)    = 0wx40 + branchOpToWord opc (* Needs 0F prefix *)
    |   opToInt (CMOV64 opc)    = 0wx40 + branchOpToWord opc (* Needs 0F prefix and rex.w *)
    |   opToInt PAUSE           = 0wx90 (* Needs F3 prefix *)
    |   opToInt BSR32           = 0wxbd (* Needs 0F prefix *)
    |   opToInt BSR64           = 0wxbd (* Needs 0F prefix *)

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
    fun opcodePrefix LOCK_XADD32                = [0wxF0] (* Requires LOCK prefix. *)
    |   opcodePrefix LOCK_XADD64                = [0wxF0] (* Requires LOCK prefix. *)
    |   opcodePrefix LOCK_CMPXCHG32             = [0wxF0] (* Requires LOCK prefix. *)
    |   opcodePrefix LOCK_CMPXCHG64             = [0wxF0] (* Requires LOCK prefix. *)
    |   opcodePrefix MOVL_R_A16                 = [0wx66] (* Requires OPSIZE prefix. *)
    |   opcodePrefix SSE2StoreSingle            = [0wxf3]
    |   opcodePrefix SSE2StoreDouble            = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2CompDouble)    = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2And)           = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2Xor)           = [0wx66]
    |   opcodePrefix(SSE2Ops SSE2CompSingle)    = [] (* No prefix *)
    |   opcodePrefix(SSE2Ops SSE2MoveDouble)    = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2AddDouble)     = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2SubDouble)     = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2MulDouble)     = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2DivDouble)     = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2DoubleToFloat) = [0wxf2]
    |   opcodePrefix(SSE2Ops SSE2MoveFloat)     = [0wxf3]
    |   opcodePrefix(SSE2Ops SSE2AddSingle)     = [0wxf3]
    |   opcodePrefix(SSE2Ops SSE2SubSingle)     = [0wxf3]
    |   opcodePrefix(SSE2Ops SSE2MulSingle)     = [0wxf3]
    |   opcodePrefix(SSE2Ops SSE2DivSingle)     = [0wxf3]
    |   opcodePrefix(SSE2Ops SSE2FloatToDouble) = [0wxf3]
    |   opcodePrefix CVTSI2SD32                 = [0wxf2]
    |   opcodePrefix CVTSI2SD64                 = [0wxf2]
    |   opcodePrefix CVTSI2SS32                 = [0wxf3]
    |   opcodePrefix CVTSI2SS64                 = [0wxf3]
    |   opcodePrefix MOVDFromXMM                = [0wx66]
    |   opcodePrefix MOVQToXMM                  = [0wx66]
    |   opcodePrefix PSRLDQ                     = [0wx66]
    |   opcodePrefix CVTSD2SI32                 = [0wxf2]
    |   opcodePrefix CVTSD2SI64                 = [0wxf2]
    |   opcodePrefix CVTSS2SI32                 = [0wxf3]
    |   opcodePrefix CVTSS2SI64                 = [0wxf3]
    |   opcodePrefix CVTTSD2SI32                = [0wxf2]
    |   opcodePrefix CVTTSD2SI64                = [0wxf2]
    |   opcodePrefix CVTTSS2SI32                = [0wxf3]
    |   opcodePrefix CVTTSS2SI64                = [0wxf3]
    |   opcodePrefix PAUSE                      = [0wxf3]
    |   opcodePrefix _                          = []

    (* A few instructions require an escape.  Escapes come after the REX. *)
    fun escapePrefix MOVZB                      = [0wx0f]
    |   escapePrefix MOVZW                      = [0wx0f]
    |   escapePrefix MOVSXB32                   = [0wx0f]
    |   escapePrefix MOVSXW32                   = [0wx0f]
    |   escapePrefix MOVSXB64                   = [0wx0f]
    |   escapePrefix MOVSXW64                   = [0wx0f]
    |   escapePrefix LOCK_XADD32                = [0wx0f]
    |   escapePrefix LOCK_XADD64                = [0wx0f]
    |   escapePrefix LOCK_CMPXCHG32             = [0wx0f]
    |   escapePrefix LOCK_CMPXCHG64             = [0wx0f]
    |   escapePrefix IMUL32                     = [0wx0f]
    |   escapePrefix IMUL64                     = [0wx0f]
    |   escapePrefix(CondJump32 _)              = [0wx0f]
    |   escapePrefix(SetCC _)                   = [0wx0f]
    |   escapePrefix SSE2StoreSingle            = [0wx0f]
    |   escapePrefix SSE2StoreDouble            = [0wx0f]
    |   escapePrefix(SSE2Ops _)                 = [0wx0f]
    |   escapePrefix CVTSI2SD32                 = [0wx0f]
    |   escapePrefix CVTSI2SD64                 = [0wx0f]
    |   escapePrefix CVTSI2SS32                 = [0wx0f]
    |   escapePrefix CVTSI2SS64                 = [0wx0f]
    |   escapePrefix MOVDFromXMM                = [0wx0f]
    |   escapePrefix MOVQToXMM                  = [0wx0f]
    |   escapePrefix PSRLDQ                     = [0wx0f]
    |   escapePrefix LDSTMXCSR                  = [0wx0f]
    |   escapePrefix CVTSD2SI32                 = [0wx0f]
    |   escapePrefix CVTSD2SI64                 = [0wx0f]
    |   escapePrefix CVTSS2SI32                 = [0wx0f]
    |   escapePrefix CVTSS2SI64                 = [0wx0f]
    |   escapePrefix CVTTSD2SI32                = [0wx0f]
    |   escapePrefix CVTTSD2SI64                = [0wx0f]
    |   escapePrefix CVTTSS2SI32                = [0wx0f]
    |   escapePrefix CVTTSS2SI64                = [0wx0f]
    |   escapePrefix(CMOV32 _)                  = [0wx0f]
    |   escapePrefix(CMOV64 _)                  = [0wx0f]
    |   escapePrefix BSR32                      = [0wx0f]
    |   escapePrefix BSR64                      = [0wx0f]
    |   escapePrefix _                          = []

    (* Generate an opCode byte after doing any pending operations. *)
    fun opCodeBytes(opb:opCode, rx) =
    let
        val rexByte = 
            case rx of
                NONE => []
            |   SOME rxx =>
                if hostIsX64 then [rex rxx]
                else raise InternalError "opCodeBytes: rex prefix in 32 bit mode";
    in
        opcodePrefix opb @ rexByte @ escapePrefix opb @ [opToInt opb]
    end

    fun rexByte(opb, rrX, rbX, riX) =
    let
        (* We need a rex prefix if we need to set the length to 64-bit. *)
        val need64bit =
            case opb of
                Group1_8_A64 => true (* Arithmetic operations - must be 64-bit *)
            |   Group1_32_A64 => true (* Arithmetic operations - must be 64-bit *)
            |   Group2_1_A64 => true (* 1-bit shifts - must be 64-bit *)
            |   Group2_8_A64 => true (* n-bit shifts - must be 64-bit *)
            |   Group2_CL_A64 => true (* Shifts by value in CL *)
            |   Group3_A64 => true (* Test, Not, Mul etc. *)
            |   Arith64 (_, _) => true
            |   MOVL_A_R64 => true (* Needed *)
            |   MOVL_R_A64 => true (* Needed *)
            |   XCHNG64 => true
            |   LEAL64 => true (* Needed to ensure the result is 64-bits *)
            |   MOVL_64_R _ => true (* Needed *)
            |   MOVL_32_A64 => true (* Needed *)
            |   IMUL64 => true (* Needed to ensure the result is 64-bits *)
            |   LOCK_XADD64 => true (* Needed to ensure the result is 64-bits *)
            |   LOCK_CMPXCHG64 => true (* Needed to ensure the result is 64-bits *)
            |   CQO_CDQ64 => true (* It's only CQO if there's a Rex prefix. *)
            |   CVTSI2SD64 => true (* This affects the size of the integer source. *)
            |   CVTSI2SS64 => true (* This affects the size of the integer source. *)
            |   IMUL_C8_64 => true
            |   IMUL_C32_64 => true
            |   MOVQToXMM => true
            |   CVTSD2SI64 => true (* This affects the size of the integer source. *)
            |   CVTSS2SI64 => true 
            |   CVTTSD2SI64 => true
            |   CVTTSS2SI64 => true
            |   MOVSXD      => true
            |   CMOV64 _    => true
            |   MOVSXB64   => true
            |   MOVSXW64   => true
            |   BSR64      => true
            (* Group5 - We only use 2/4/6 and they don't need prefix *)
            |   _ => false
        (* If we are using MOVB_R_A with SIL or DIL we need to force a REX prefix.
           That's only possible in 64-bit mode.  This also applies with Test and SetCC
           but they are dealt with elsewhere. *)
        val forceRex =
            case opb of
                MOVB_R_A64 {forceRex=true} => true (* This is allowed in X86/64 but not in X86/32. *)
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
        
        and opIndexedPlus2(opb, offset, rb, ri, op2) = opIndexedGen(opb, offset, SOME rb, ri, (op2, false))
        
        fun opIndexed (opb, offset, rb, ri, rd) =
            opIndexedGen(opb, offset, rb, ri, getReg rd) 
        
        fun opAddress(opb, offset, rb, ri, rd) = opIndexedGen (opb, offset, SOME rb, ri, getReg rd)
        and mMXAddress(opb, offset, rb, ri, SSE2Reg rrC) = opIndexedGen(opb, offset, SOME rb, ri, (rrC, false))
        and opAddressPlus2(opb, offset, rb, ri, op2) =
            opIndexedGen(opb, offset, SOME rb, ri, (op2, false))
    end

    (* An operation with an operand that needs to go in the constant area, or in the case of
       native 32-bit, where the constant is stored in an object and the address of the
       object is inline.  This just puts in the instruction and the address.  The details
       of the constant are dealt with in putConst. *)
    fun opConstantOperand(opb, (*dest*)GeneralReg(rrC, rrX)) =
    let
        val pref = opcodePrefix opb (* Any opsize or lock prefix. *)
        val rex = rexByte(opb, rrX, false, false)
        val esc = escapePrefix opb (* Generate the ESCAPE code if needed. *)
        val opc = opToInt opb
        val mdrm = modrm(Based0, rrC, 0w5 (* PC-relative or absolute *))
    in
        pref @ rex @ esc @ [opc, mdrm] @ int32Signed(tag 0)
    end

    fun immediateOperand (opn: arithOp, rd: genReg, imm: LargeInt.int, opSize) =
    if is8BitL imm
    then (* Can use one byte immediate *)
        opRegPlus2(case opSize of OpSize64 => Group1_8_A64 | OpSize32 => Group1_8_A32, rd, arithOpToWord opn) @ [Word8.fromLargeInt imm]
    else if is32bit imm
    then (* Need 32 bit immediate. *)
        opRegPlus2(case opSize of OpSize64 => Group1_32_A64 | OpSize32 => Group1_32_A32, rd, arithOpToWord opn) @ int32Signed imm
    else (* It won't fit in the immediate; put it in the non-address area. *)
    let
        val opc = case opSize of OpSize64 => Arith64 | OpSize32 => Arith32
    in
        opConstantOperand(opc(opn, 0w3 (* r/m to reg *)), rd)
    end
    
    fun arithOpReg(opn: arithOp, rd: genReg, rs: genReg, opIs64) =
        opReg ((if opIs64 then Arith64 else Arith32) (opn, 0w3 (* r/m to reg *)), rd, rs)

    type handlerLab = addrs ref

    fun floatingPtOp{escape, md, nnn, rm} =
        opCodeBytes(FPESC escape, NONE) @ [(md <<- 0w6) orb8 (nnn <<- 0w3) orb8 rm]

    datatype trapEntries =
        StackOverflowCall
    |   StackOverflowCallEx
    |   HeapOverflowCall
    |   ArbPrecisionEmulate

    (* RTS call.  We need to save any registers that may contain addresses to the stack.
       All the registers are preserved but not seen by the GC. *)
    fun rtsCall(rtsEntry, regSet) =
        let
            val entry =
                case rtsEntry of
                    StackOverflowCall   => memRegStackOverflowCall
                |   StackOverflowCallEx => memRegStackOverflowCallEx
                |   HeapOverflowCall    => memRegHeapOverflowCall
                |   ArbPrecisionEmulate => memRegArbPrecisionCall
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

    datatype 'reg regOrMemoryArg =
        RegisterArg of 'reg
    |   MemoryArg of memoryAddress
    |   NonAddressConstArg of LargeInt.int
    |   AddressConstArg of machineWord

    datatype moveSize =
        Move64 | Move32 | Move8 | Move16 | Move32X64 | Move8X32 | Move8X64 | Move16X32 | Move16X64
    and fpSize = SinglePrecision | DoublePrecision

    datatype operation =
        Move of { source: genReg regOrMemoryArg, destination: genReg regOrMemoryArg, moveSize: moveSize }
    |   PushToStack of genReg regOrMemoryArg
    |   PopR of genReg
    |   ArithToGenReg of { opc: arithOp, output: genReg, source: genReg regOrMemoryArg, opSize: opSize }
    |   ArithMemConst of { opc: arithOp, address: memoryAddress, source: LargeInt.int, opSize: opSize }
    |   ArithMemLongConst of { opc: arithOp, address: memoryAddress, source: machineWord }
    |   ArithByteMemConst of { opc: arithOp, address: memoryAddress, source: Word8.word }
    |   ShiftConstant of { shiftType: shiftType, output: genReg, shift: Word8.word, opSize: opSize }
    |   ShiftVariable of { shiftType: shiftType, output: genReg, opSize: opSize } (* Shift amount is in ecx *)
    |   ConditionalBranch of { test: branchOps, label: label }
    |   SetCondition of { output: genReg, test: branchOps }
    |   LoadAddress of { output: genReg, offset: int, base: genReg option, index: indexType, opSize: opSize }
    |   TestByteBits of { arg: genReg regOrMemoryArg, bits: Word8.word }
    |   CallRTS of {rtsEntry: trapEntries, saveRegs: genReg list }
    |   AllocStore of { size: int, output: genReg, saveRegs: genReg list }
    |   AllocStoreVariable of { size: genReg, output: genReg, saveRegs: genReg list }
    |   StoreInitialised
    |   CallAddress of genReg regOrMemoryArg
    |   JumpAddress of genReg regOrMemoryArg
    |   ReturnFromFunction of int
    |   RaiseException of { workReg: genReg }
    |   UncondBranch of label
    |   ResetStack of { numWords: int, preserveCC: bool }
    |   JumpLabel of label
    |   LoadLabelAddress of { label: label, output: genReg }
    |   RepeatOperation of repOps
    |   DivideAccR of {arg: genReg, isSigned: bool, opSize: opSize }
    |   DivideAccM of {base: genReg, offset: int, isSigned: bool, opSize: opSize }
    |   AtomicXAdd of {address: memoryAddress, output: genReg, opSize: opSize }
    |   AtomicCmpXChng of {address: memoryAddress, source: genReg, opSize: opSize }
    |   FPLoadFromMemory of { address: memoryAddress, precision: fpSize }
    |   FPLoadFromFPReg of { source: fpReg, lastRef: bool }
    |   FPLoadFromConst of { constant: machineWord, precision: fpSize }
    |   FPStoreToFPReg of { output: fpReg, andPop: bool }
    |   FPStoreToMemory of { address: memoryAddress, precision: fpSize, andPop: bool }
    |   FPArithR of { opc: fpOps, source: fpReg }
    |   FPArithConst of { opc: fpOps, source: machineWord, precision: fpSize }
    |   FPArithMemory of { opc: fpOps, base: genReg, offset: int, precision: fpSize }
    |   FPUnary of fpUnaryOps
    |   FPStatusToEAX
    |   FPLoadInt of { base: genReg, offset: int, opSize: opSize }
    |   FPFree of fpReg
    |   MultiplyR of { source: genReg regOrMemoryArg, output: genReg, opSize: opSize }
    |   XMMArith of { opc: sse2Operations, source: xmmReg regOrMemoryArg, output: xmmReg }
    |   XMMStoreToMemory of { toStore: xmmReg, address: memoryAddress, precision: fpSize }
    |   XMMConvertFromInt of { source: genReg, output: xmmReg, opSize: opSize, precision: fpSize }
    |   SignExtendForDivide of opSize
    |   XChng of { reg: genReg, arg: genReg regOrMemoryArg, opSize: opSize }
    |   Negative of { output: genReg, opSize: opSize }
    |   JumpTable of { cases: label list, jumpSize: jumpSize ref }
    |   IndexedJumpCalc of { addrReg: genReg, indexReg: genReg, jumpSize: jumpSize ref }
    |   MoveXMMRegToGenReg of { source: xmmReg, output: genReg }
    |   MoveGenRegToXMMReg of { source: genReg, output: xmmReg }
    |   XMMShiftRight of { output: xmmReg, shift: Word8.word }
    |   FPLoadCtrlWord of memoryAddress (* Load FP control word. *)
    |   FPStoreCtrlWord of memoryAddress (* Store FP control word. *)
    |   XMMLoadCSR of memoryAddress (* Load combined control/status word. *)
    |   XMMStoreCSR of memoryAddress (* Store combined control/status word. *)
    |   FPStoreInt of memoryAddress
    |   XMMStoreInt of { source: xmmReg regOrMemoryArg, output: genReg, precision: fpSize, isTruncate: bool }
    |   CondMove of { test: branchOps, output: genReg, source: genReg regOrMemoryArg, opSize: opSize }
    |   LoadAbsolute of { destination: genReg, value: machineWord }
    |   PauseForSpinLock
    |   BitScanR of { output: genReg, source: genReg regOrMemoryArg, opSize: opSize }

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
        
        fun printOpSize OpSize32 = "32"
        |   printOpSize OpSize64 = "64"
     in
        case operation of
            Move { source, destination, moveSize } =>
            (
                case moveSize of
                    Move64  => stream "Move64 "
                |   Move32  => stream "Move32 "
                |   Move8   => stream "Move8 "
                |   Move16  => stream "Move16 "
                |   Move32X64 => stream "Move32X64 "
                |   Move8X32  => stream "Move8X32 "
                |   Move8X64  => stream "Move8X64 "
                |   Move16X32 => stream "Move16X32 "
                |   Move16X64 => stream "Move16X64 ";
                printRegOrMemoryArg printGReg destination; stream " <= "; printRegOrMemoryArg printGReg source
            )

        |   ArithToGenReg { opc, output, source, opSize } =>
                (stream (arithOpRepr opc); stream "RR"; stream(printOpSize opSize); stream " "; printGReg output; stream " <= "; printRegOrMemoryArg printGReg source )

        |   ArithMemConst { opc, address, source, opSize } =>
            (
                stream (arithOpRepr opc); stream "MC"; stream(printOpSize opSize); stream " ";
                printMemAddress address;
                stream " "; stream(LargeInt.toString source)
            )

        |   ArithMemLongConst { opc, address, source } =>
            (
                stream (arithOpRepr opc ^ "MC "); printMemAddress address;
                stream " <= "; stream(Address.stringOfWord source)
            )

        |   ArithByteMemConst { opc, address, source } =>
            (
                stream (arithOpRepr opc); stream "MC8"; stream " ";
                printMemAddress address; stream " "; stream(Word8.toString source)
            )

        |   ShiftConstant { shiftType, output, shift, opSize } =>
            (
                stream(shiftTypeRepr shiftType); stream(printOpSize opSize); stream " "; printGReg output;
                stream " by "; stream(Word8.toString shift)
            )

        |   ShiftVariable { shiftType, output, opSize } => (* Shift amount is in ecx *)
            (
                stream(shiftTypeRepr shiftType); stream(printOpSize opSize); stream " "; printGReg output; stream " by ECX"
            )

        |   ConditionalBranch { test, label=Label{labelNo, ...} } =>
            (
                stream "Jump"; stream(branchOpRepr test); stream " L"; stream(Int.toString labelNo)
            )

        |   SetCondition { output, test } =>
            (
                stream "SetCC"; stream(branchOpRepr test); stream " => "; printGReg output
            )

        |   PushToStack source => (stream "Push "; printRegOrMemoryArg printGReg source)

        |   PopR dest => (stream "PopR "; printGReg dest)

        |   LoadAddress{ output, offset, base, index, opSize } =>
            (
                stream "LoadAddress"; stream(printOpSize opSize); stream " "; 
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

        |   TestByteBits { arg, bits } =>
                ( stream "TestByteBits "; printRegOrMemoryArg printGReg arg; stream " 0x"; stream(Word8.toString bits) )

        |   CallRTS {rtsEntry, ...} =>
            (
                stream "CallRTS ";
                case rtsEntry of
                    StackOverflowCall => stream "StackOverflowCall"
                |   HeapOverflowCall => stream "HeapOverflow"
                |   StackOverflowCallEx => stream "StackOverflowCallEx"
                |   ArbPrecisionEmulate => stream "ArbPrecisionEmulate"
            )

        |   AllocStore { size, output, ... } =>
                (stream "AllocStore "; stream(Int.toString size); stream " => "; printGReg output )

        |   AllocStoreVariable { output, size, ...} =>
                (stream "AllocStoreVariable "; printGReg size; stream " => "; printGReg output )
        
        |   StoreInitialised => stream "StoreInitialised"

        |   CallAddress source => (stream "CallAddress "; printRegOrMemoryArg printGReg source)
        |   JumpAddress source => (stream "JumpAddress "; printRegOrMemoryArg printGReg source)

        |   ReturnFromFunction argsToRemove =>
                (stream "ReturnFromFunction "; stream(Int.toString argsToRemove))

        |   RaiseException { workReg } => (stream "RaiseException "; printGReg workReg)
        |   UncondBranch(Label{labelNo, ...})=>
                (stream "UncondBranch L"; stream(Int.toString labelNo))
        |   ResetStack{numWords, preserveCC} =>
                (stream "ResetStack "; stream(Int.toString numWords); if preserveCC then stream " preserve CC" else ())
        |   JumpLabel(Label{labelNo, ...}) =>
                (stream "L"; stream(Int.toString labelNo); stream ":")
        |   LoadLabelAddress{ label=Label{labelNo, ...}, output } =>
                (stream "LoadLabelAddress L"; stream(Int.toString labelNo); stream "=>"; printGReg output)
        |   RepeatOperation repOp => (stream "Repeat "; stream(repOpsRepr repOp))
        |   DivideAccR{arg, isSigned, opSize} =>
                ( stream(if isSigned then "DivideSigned" else "DivideUnsigned"); stream(printOpSize opSize); stream " "; printGReg arg)
        |   DivideAccM{base, offset, isSigned, opSize} =>
                ( stream(if isSigned then "DivideSigned" else "DivideUnsigned"); stream(printOpSize opSize); stream " "; printBaseOffset(base, NoIndex, offset))
        |   AtomicXAdd{address, output, opSize} =>
                (stream "LockedXAdd"; stream(printOpSize opSize); printMemAddress address; stream " <=> "; printGReg output)
        |   AtomicCmpXChng{address, source, opSize} =>
                (stream "LockedCmpXchng"; stream(printOpSize opSize); printMemAddress address; stream ", "; printGReg source)
        |   FPLoadFromMemory{address, precision=DoublePrecision} => (stream "FPLoadDouble "; printMemAddress address)
        |   FPLoadFromMemory{address, precision=SinglePrecision} => (stream "FPLoadSingle "; printMemAddress address)
        |   FPLoadFromFPReg {source, lastRef} =>
                (stream "FPLoad "; printFPReg source; if lastRef then stream " (LAST)" else())
        |   FPLoadFromConst{constant, precision} =>
            (
                case precision of DoublePrecision => stream "FPLoadD " |  SinglePrecision => stream "FPLoadS";
                stream(Address.stringOfWord constant)
            )
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
        |   FPArithConst{ opc, source, precision } =>
                (stream(fpOpRepr opc); case precision of DoublePrecision => stream "D " |  SinglePrecision => stream "S "; stream(Address.stringOfWord source))
        |   FPArithMemory{ opc, base, offset, precision } =>
                (stream(fpOpRepr opc); case precision of DoublePrecision => stream "D " |  SinglePrecision => stream "S "; printBaseOffset(base, NoIndex, offset))
        |   FPUnary opc => stream(fpUnaryRepr opc)
        |   FPStatusToEAX => (stream "FPStatus "; printGReg eax)
        |   FPLoadInt { base, offset, opSize} =>
                (stream "FPLoadInt"; stream(printOpSize opSize); stream " "; printBaseOffset(base, NoIndex, offset))
        |   FPFree reg => (stream "FPFree "; printFPReg reg)
        |   MultiplyR {source, output, opSize } =>
                (stream "MultiplyR"; stream(printOpSize opSize); stream " "; printRegOrMemoryArg printGReg source; stream " *=>"; printGReg output)
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
        |   XMMConvertFromInt { source, output, opSize, precision } =>
            (
                stream (case precision of DoublePrecision => "ConvertFromIntToDouble " | SinglePrecision => "ConvertFromIntToSingle ");
                stream(printOpSize opSize); stream " "; printGReg source; stream " => "; printXMMReg output
            )
        |   SignExtendForDivide opSize => ( stream "SignExtendForDivide"; stream(printOpSize opSize) )
        |   XChng { reg, arg, opSize } =>
                (stream "XChng"; stream(printOpSize opSize); stream " "; printGReg reg; stream " <=> "; printRegOrMemoryArg printGReg arg)
        |   Negative { output, opSize } =>
                (stream "Negative"; stream(printOpSize opSize); stream " "; printGReg output)
        |   JumpTable{cases, ...} =>
                List.app(fn(Label{labelNo, ...}) => (stream "UncondBranch L"; stream(Int.toString labelNo); stream "\n")) cases
        |   IndexedJumpCalc { addrReg, indexReg, jumpSize=ref jumpSize } =>
            (
                stream "IndexedJumpCalc "; printGReg addrReg; stream " += "; printGReg indexReg;
                stream (case jumpSize of JumpSize2 => " * 2" | JumpSize8 => " * 8 ")
            )
        |   MoveXMMRegToGenReg { source, output } =>
            (
                stream "MoveXMMRegToGenReg "; printXMMReg source; stream " => "; printGReg output
            )
        |   MoveGenRegToXMMReg { source, output } =>
            (
                stream "MoveGenRegToXMMReg "; printGReg source; stream " => "; printXMMReg output
            )
        |   XMMShiftRight { output, shift } =>
            (
                stream "XMMShiftRight "; printXMMReg output; stream " by "; stream(Word8.toString shift)
            )
        |   FPLoadCtrlWord address =>
            (
                stream "FPLoadCtrlWord "; stream " => "; printMemAddress address
            )
        |   FPStoreCtrlWord address =>
            (
                stream "FPStoreCtrlWord "; stream " <= "; printMemAddress address
            )
        |   XMMLoadCSR address =>
            (
                stream "XMMLoadCSR "; stream " => "; printMemAddress address
            )
        |   XMMStoreCSR address =>
            (
                stream "XMMStoreCSR "; stream " <= "; printMemAddress address
            )
        |   FPStoreInt address =>
            (
                stream "FPStoreInt "; stream " <= "; printMemAddress address
            )
        |   XMMStoreInt{ source, output, precision, isTruncate } =>
            (
                stream "XMMStoreInt";
                case precision of SinglePrecision => stream "Single" | DoublePrecision => stream "Double";
                if isTruncate then stream "Truncate " else stream " ";
                printGReg output; stream " <= "; printRegOrMemoryArg printXMMReg source
            )
        |   CondMove { test, output, source, opSize } =>
            (
                stream "CondMove"; stream(branchOpRepr test); stream(printOpSize opSize);
                printGReg output; stream " <= "; printRegOrMemoryArg printGReg source 
            )
        |   LoadAbsolute { destination, value } =>
                ( stream "LoadAbsolute "; printGReg destination; stream " <= "; stream(Address.stringOfWord value) )
        |   PauseForSpinLock => stream "PauseForSpinLock"
        |   BitScanR { output, source, opSize } =>
            (
                stream "BitScanR"; stream(printOpSize opSize);
                printGReg output; stream " <= "; printRegOrMemoryArg printGReg source
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

    val isLocalCodeAddress: machineWord -> bool = RunCall.rtsCallFast1 "PolyX86IsLocalCode"

    (* Code generate a list of operations.  The list is in reverse order i.e. last instruction first. *)
    fun codeGenerate ops =
    let
        fun cgOp(Move{source=RegisterArg source, destination=RegisterArg output, moveSize=Move64 }) =
                (* Move from one general register to another.  N.B. Because we're using the
                   "store" version of the Move the source and output are reversed. *)
                opReg(MOVL_R_A64, source, output)

        |   cgOp(Move{source=RegisterArg source, destination=RegisterArg output, moveSize=Move32 }) =
                opReg(MOVL_R_A32, source, output)

        |   cgOp(Move{ source=NonAddressConstArg source, destination=RegisterArg output, moveSize=Move64}) =
            if targetArch <> Native32Bit
            then
            (
                (* N.B. There is related code in getConstant that deals with PC-relative values and
                   also checks the range of constants that need to be in the constant area. *)
                if source >= 0 andalso source < 0x100000000
                then (* Unsigned 32 bits.  We can use a 32-bit instruction to set the
                        value because it will zero extend to 64-bits.
                        This may also allow us to save a rex byte. *)
                let
                    val (rc, rx) = getReg output
                    val opb = opCodeBytes(MOVL_32_R rc, if rx then SOME{w=false, r=false, b=rx, x=false} else NONE)
                in
                    opb @ word32Unsigned(LargeWord.fromLargeInt source)
                end
                else if source >= ~0x80000000 andalso source < 0
                then (* Signed 32-bits. *)
                    (* This is not scanned in 64-bit mode because 32-bit values aren't
                       big enough to contain addresses. *)
                    opRegPlus2(MOVL_32_A64, output, 0w0) @ int32Signed source
                else (* Too big for 32-bits; put it in the non-word area. *)
                    opConstantOperand(MOVL_A_R64, output)
            )
            else (* 32-bit mode. *)
            (
                (* The RTS scans for possible addresses in MOV instructions so we
                   can only use MOV if this is a tagged value.  If it isn't we have
                   to use something else such as XOR/ADD.  In particular this is used
                   before LOCK XADD for atomic inc/dec.
                   We expect Move to preserve the CC so shouldn't use anything that
                   affects it.  There was a previous comment that said that using
                   LEA wasn't a good idea.  Perhaps because it takes 6 bytes. *)
                if source mod 2 = 0
                then opIndexed(LEAL32, source, NONE, NoIndex, output)
                else
                let
                    val (rc, rx) = getReg output
                    val opb = opCodeBytes(MOVL_32_R rc, if rx then SOME{w=false, r=false, b=rx, x=false} else NONE)
                in
                    opb @ int32Signed source
                end
            )

        |   cgOp(Move{ source=NonAddressConstArg source, destination=RegisterArg output, moveSize=Move32}) =
            if targetArch <> Native32Bit
            then
            (
                (* N.B. There is related code in getConstant that deals with PC-relative values and
                   also checks the range of constants that need to be in the constant area. *)
                if source >= 0 andalso source < 0x100000000
                then (* Unsigned 32 bits.  We can use a 32-bit instruction to set the
                        value because it will zero extend to 64-bits.
                        This may also allow us to save a rex byte. *)
                let
                    val (rc, rx) = getReg output
                    val opb = opCodeBytes(MOVL_32_R rc, if rx then SOME{w=false, r=false, b=rx, x=false} else NONE)
                in
                    opb @ word32Unsigned(LargeWord.fromLargeInt source)
                end
                else if source >= ~0x80000000 andalso source < 0
                then (* Signed 32-bits. *)
                    (* This is not scanned in 64-bit mode because 32-bit values aren't
                       big enough to contain addresses. *)
                    opRegPlus2(MOVL_32_A64, output, 0w0) @ int32Signed source
                else (* Too big for 32-bits; put it in the non-word area. *)
                    opConstantOperand(MOVL_A_R64, output)
            )
            else (* 32-bit mode. *)
            (
                (* The RTS scans for possible addresses in MOV instructions so we
                   can only use MOV if this is a tagged value.  If it isn't we have
                   to use something else such as XOR/ADD.  In particular this is used
                   before LOCK XADD for atomic inc/dec.
                   We expect Move to preserve the CC so shouldn't use anything that
                   affects it.  There was a previous comment that said that using
                   LEA wasn't a good idea.  Perhaps because it takes 6 bytes. *)
                if source mod 2 = 0
                then opIndexed(LEAL32, source, NONE, NoIndex, output)
                else
                let
                    val (rc, rx) = getReg output
                    val opb = opCodeBytes(MOVL_32_R rc, if rx then SOME{w=false, r=false, b=rx, x=false} else NONE)
                in
                    opb @ int32Signed source
                end
            )

        |   cgOp(Move{ source=AddressConstArg _, destination=RegisterArg output, moveSize=Move64 }) =
            (
                (* The constant area is currently PolyWords.  That means we MUST use
                   a 32-bit load in 32-in-64. *)
                targetArch = Native64Bit orelse raise InternalError "Move64 in 32-bit";
                (* Put address constants in the constant area. *)
                opConstantOperand(MOVL_A_R64, output)
            )
 
        |   cgOp(Move{ source=AddressConstArg _, destination=RegisterArg output, moveSize=Move32 }) =
            (
                case targetArch of
                    Native64Bit => raise InternalError "Move32 - AddressConstArg"

                |   ObjectId32Bit2Word =>
                        (* Put address constants in the constant area. *)
                        (* The constant area is currently PolyWords.  That means we MUST use
                           a 32-bit load in 32-in-64. *)
                        opConstantOperand(MOVL_A_R32, output)

                |   ObjectId32Bit4Word =>
                        (* Put address constants in the constant area. *)
                        (* The constant area is currently PolyWords.  That means we MUST use
                           a 32-bit load in 32-in-64. *)
                        opConstantOperand(MOVL_A_R32, output)

                |   Native32Bit =>
                        (* Immediate constant *)
                    let
                        val (rc, _) = getReg output
                    in
                        opCodeBytes(MOVL_32_R rc, NONE) @ int32Signed(tag 0)
                    end
            )

        |   cgOp(LoadAbsolute{ destination, ... }) =
            (
                (* Immediate address constant.  This is currently only used the special case of loading
                   the address of PolyX86GetThreadData in a callback when we don't have rbx in 32-in-64. *)
                case targetArch of
                    Native32Bit =>
                    let
                        val (rc, _) = getReg destination
                    in
                        opCodeBytes(MOVL_32_R rc, NONE) @ int32Signed(tag 0)
                    end
                
                |   Native64Bit => opConstantOperand(MOVL_A_R64, destination)

                |   ObjectId32Bit2Word =>
                    let
                        val (rc, rx) = getReg destination
                    in
                        opCodeBytes(MOVL_64_R rc, SOME{w=true, r=false, b=rx, x=false}) @ largeWordToBytes(LargeWord.fromLargeInt(tag 0), 8)
                    end

                |   ObjectId32Bit4Word =>
                    let
                        val (rc, rx) = getReg destination
                    in
                        opCodeBytes(MOVL_64_R rc, SOME{w=true, r=false, b=rx, x=false}) @ largeWordToBytes(LargeWord.fromLargeInt(tag 0), 8)
                    end

            )

        |   cgOp(Move{source=MemoryArg{base, offset, index}, destination=RegisterArg output, moveSize=Move32 }) =
                opAddress(MOVL_A_R32, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{source=MemoryArg{base, offset, index}, destination=RegisterArg output, moveSize=Move64 }) =
                opAddress(MOVL_A_R64, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{source=MemoryArg{base, offset, index}, destination=RegisterArg output, moveSize=Move8 }) =
                (* We don't need a REX.W bit here because the top 32-bits of a
                   64-bit register will always be zeroed. *)
                opAddress(MOVZB, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{source=RegisterArg source, destination=RegisterArg output, moveSize=Move8 }) =
            let
                (* Zero extend an 8-bit value in a register to 32/64 bits. *)
                val (rrC, rrX) = getReg output
                val (rbC, rbX) = getReg source
                (* We don't need a REX.W bit here because the top 32-bits of a
                   64-bit register will always be zeroed but we may need a REX byte
                   if we're using esi or edi. *)
                val rexByte =
                    if rrC < 0w4 andalso not rrX andalso not rbX
                    then NONE
                    else if hostIsX64
                    then SOME {w=false, r=rrX, b=rbX, x=false}
                    else raise InternalError "Move8 with esi/edi"
            in
                opCodeBytes(MOVZB, rexByte) @ [modrm(Register, rrC, rbC)]
            end

        |   cgOp(Move{source=MemoryArg{base, offset, index}, destination=RegisterArg output, moveSize=Move8X32 }) =
                opAddress(MOVSXB32, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{source=MemoryArg{base, offset, index}, destination=RegisterArg output, moveSize=Move8X64 }) =
                (* But we will need a Rex.W here. *)
                opAddress(MOVSXB64, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{moveSize=Move16, source=MemoryArg{base, offset, index}, destination=RegisterArg output }) =
                (* No need for Rex.W *)
                opAddress(MOVZW, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{moveSize=Move16X32, source=MemoryArg{base, offset, index}, destination=RegisterArg output }) =
                opAddress(MOVSXW32, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{moveSize=Move16X64, source=MemoryArg{base, offset, index}, destination=RegisterArg output }) =
                (* But we do need Rex.W here *)
                opAddress(MOVSXW64, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{moveSize=Move32X64, source=RegisterArg source, destination=RegisterArg output }) =
                (* We should have a REX.W bit here. *)
                opReg(MOVSXD, output, source)

        |   cgOp(Move{moveSize=Move32X64, source=MemoryArg{base, offset, index}, destination=RegisterArg output }) =
                (* We should have a REX.W bit here. *)
                opAddress(MOVSXD, LargeInt.fromInt offset, base, index, output)

        |   cgOp(Move{moveSize=Move32X64, ...}) = raise InternalError "cgOp: LoadNonWord Size32Bit"

        |   cgOp(LoadAddress{ offset, base, index, output, opSize }) =
                (* This provides a mixture of addition and multiplication in a single
                   instruction. *)
                opIndexed(case opSize of OpSize64 => LEAL64 | OpSize32 => LEAL32, LargeInt.fromInt offset, base, index, output)

        |   cgOp(ArithToGenReg{ opc, output, source=RegisterArg source, opSize }) =
                arithOpReg (opc, output, source, opSize=OpSize64)

        |   cgOp(ArithToGenReg{ opc, output, source=NonAddressConstArg source, opSize }) =
            let
                (* On the X86/32 we use CMP with literal sources to compare with an
                   address and the RTS searches for them in the code.  Any
                   non-address constant must be tagged.  Most will be but we
                   might want to use this to compare with the contents of a
                   LargeWord value. *)
                val _ =
                    if hostIsX64 orelse is8BitL source orelse opc <> CMP orelse IntInf.andb(source, 1) = 1
                    then ()
                    else raise InternalError "CMP with constant that looks like an address"
            in
                immediateOperand(opc, output, source, opSize)
            end

        |   cgOp(ArithToGenReg{ opc, output, source=AddressConstArg _, opSize }) =
            (* This is only used for opc=CMP to compare addresses for equality. *)
            if hostIsX64
            then (* We use this in 32-in-64 as well as native 64-bit. *)
                opConstantOperand(
                    (case opSize of OpSize64 => Arith64 | OpSize32 => Arith32) (opc, 0w3), output)
            else
            let
                val (rc, _) = getReg output
                val opb = opCodeBytes(Group1_32_A32 (* group1, 32 bit immediate *), NONE)
                val mdrm = modrm(Register, arithOpToWord opc, rc)
            in
                opb @ [mdrm] @ int32Signed(tag 0)
            end

        |   cgOp(ArithToGenReg{ opc, output, source=MemoryArg{offset, base, index}, opSize }) =
                opAddress((case opSize of OpSize64 => Arith64 | OpSize32 => Arith32) (opc, 0w3),
                    LargeInt.fromInt offset, base, index, output)

        |   cgOp(ArithByteMemConst{ opc, address={offset, base, index}, source }) =
                opIndexedPlus2(Group1_8_a (* group1, 8 bit immediate *),
                               LargeInt.fromInt offset, base, index, arithOpToWord opc) @ [source]

        |   cgOp(ArithMemConst{ opc, address={offset, base, index}, source, opSize }) =
                if is8BitL source
                then (* Can use one byte immediate *) 
                    opIndexedPlus2(case opSize of OpSize64 => Group1_8_A64 | OpSize32 => Group1_8_A32 (* group1, 8 bit immediate *),
                               LargeInt.fromInt offset, base, index, arithOpToWord opc) @ [Word8.fromLargeInt source]
                else (* Need 32 bit immediate. *)
                    opIndexedPlus2(case opSize of OpSize64 => Group1_32_A64 | OpSize32 => Group1_32_A32(* group1, 32 bit immediate *), 
                               LargeInt.fromInt offset, base, index, arithOpToWord opc) @ int32Signed source

        |   cgOp(ArithMemLongConst{ opc, address={offset, base, index}, ... }) =
                (* Currently this is always a comparison.  It is only valid in 32-bit mode because
                   the constant is only 32-bits. *)
                if hostIsX64
                then raise InternalError "ArithMemLongConst in 64-bit mode"
                else 
                let
                    val opb = opIndexedPlus2 (Group1_32_A32, LargeInt.fromInt offset, base, index, arithOpToWord opc)
                in
                    opb @ int32Signed(tag 0)
                end

        |   cgOp(ShiftConstant { shiftType, output, shift, opSize }) =
                if shift = 0w1
                then opRegPlus2(case opSize of OpSize64 => Group2_1_A64 | OpSize32 => Group2_1_A32, output, shiftTypeToWord shiftType)
                else opRegPlus2(case opSize of OpSize64 => Group2_8_A64 | OpSize32 => Group2_8_A32, output, shiftTypeToWord shiftType) @ [shift]

        |   cgOp(ShiftVariable { shiftType, output, opSize }) =
                opRegPlus2(case opSize of OpSize64 => Group2_CL_A64 | OpSize32 => Group2_CL_A32, output, shiftTypeToWord shiftType)

        |   cgOp(TestByteBits{arg=RegisterArg reg, bits}) =
            let
            (* Test the bottom bit and jump depending on its value.  This is used
               for tag tests in arbitrary precision operations and also for testing
               for short/long values. *)
                val (regNum, rx) = getReg reg
            in
                if reg = eax
                then (* Special instruction for testing accumulator.  Can use an 8-bit test. *)
                    opCodeBytes(TEST_ACC8, NONE) @ [bits]
                else if hostIsX64
                then 
                let
                    (* We can use a REX code to force it to always use the low order byte. *)
                    val opb = opCodeBytes(Group3_a,
                        if rx orelse regNum >= 0w4 then SOME{w=false, r=false, b=rx, x=false} else NONE)
                    val mdrm = modrm (Register, 0w0 (* test *), regNum)
                in
                    opb @ [mdrm, bits]
                end
                else if reg = ebx orelse reg = ecx orelse reg = edx (* can we use an 8-bit test? *)
                then (* Yes. The register value refers to low-order byte. *)
                let
                    val opb = opCodeBytes(Group3_a, NONE)
                    val mdrm = modrm(Register, 0w0 (* test *), regNum)
                in
                    opb @ [mdrm, bits]
                end
                else
                let
                    val opb = opCodeBytes(Group3_A32, NONE)
                    val mdrm = modrm (Register, 0w0 (* test *), regNum)
                in
                    opb @ mdrm :: word32Unsigned(Word8.toLarge bits)
                end
            end

        |   cgOp(TestByteBits{arg=MemoryArg{base, offset, index}, bits}) =
                (* Test the tag bit and set the condition code. *)
                opIndexedPlus2(Group3_a, LargeInt.fromInt offset, base, index, 0w0 (* test *)) @ [ bits]

        |   cgOp(TestByteBits _) = raise InternalError "cgOp: TestByteBits"                

        |   cgOp(ConditionalBranch{ test=opc, ... }) = opCodeBytes(CondJump32 opc, NONE) @ word32Unsigned 0w0
        
        |   cgOp(SetCondition{ output, test}) =
            let
                val (rrC, rx) = getReg output
                (* In 64-bit mode we can specify the low-order byte of RSI/RDI but we
                   must use a REX prefix.  This isn't possible in 32-bit mode. *)
            in
                if hostIsX64 orelse rrC < 0w4
                then
                let
                    val opb = opCodeBytes(SetCC test,
                        if rx orelse rrC >= 0w4 then SOME{w=false, r=false, b=rx, x=false} else NONE)
                    val mdrm = modrm (Register, 0w0, rrC)
                in
                    opb @ [mdrm]
                end
                else raise InternalError "High byte register"
            end

        |   cgOp(CallRTS{rtsEntry, saveRegs}) = rtsCall(rtsEntry, saveRegs)

        |   cgOp(RepeatOperation repOp) =
            let
                (* We don't explicitly clear the direction flag.  Should that be done? *)
                val opb = opCodeBytes(REP, NONE)
                (* Put in a rex prefix to force 64-bit mode. *)
                val optRex =
                    if case repOp of STOS64 => true | MOVS64 => true | _ => false
                    then [rex{w=true, r=false, b=false, x=false}]
                    else []
                val repOp = repOpsToWord repOp
            in
                opb @ optRex @ [repOp]
            end

        |   cgOp(DivideAccR{arg, isSigned, opSize}) =
                opRegPlus2(case opSize of OpSize64 => Group3_A64 | OpSize32 => Group3_A32, arg, if isSigned then 0w7 else 0w6)

        |   cgOp(DivideAccM{base, offset, isSigned, opSize}) =
                opPlus2(case opSize of OpSize64 => Group3_A64 | OpSize32 => Group3_A32, LargeInt.fromInt offset, base, if isSigned then 0w7 else 0w6)

        |   cgOp(AtomicXAdd{address={offset, base, index}, output, opSize}) =
                (* Locked exchange-and-add.  We need the lock prefix before the REX prefix. *)
                opAddress(case opSize of OpSize64 => LOCK_XADD64 | OpSize32 => LOCK_XADD32, LargeInt.fromInt offset, base, index, output)

        |   cgOp(AtomicCmpXChng{address={offset, base, index}, source, opSize}) =
                (* Locked compare and exchange. *)
                opAddress(case opSize of OpSize64 => LOCK_CMPXCHG64 | OpSize32 => LOCK_CMPXCHG32, LargeInt.fromInt offset,
                         base, index, source)

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
            (
                case targetArch of
                    Native64Bit => (* Put it in the constant area. *)
    		        let
                        val opb = opCodeBytes(Group5, NONE)
                        val mdrm = modrm(Based0, 0w6 (* push *), 0w5 (* PC rel *));
                    in
                        opb @ [mdrm] @ int32Signed(tag 0)
                    end
                |   Native32Bit => opCodeBytes(PUSH_32, NONE) @ int32Signed(tag 0)
                |   ObjectId32Bit2Word =>
                    (* We can't do this.  The constant area contains 32-bit quantities
                       and 32-bit literals are sign-extended rather than zero-extended. *)
                        raise InternalError "PushToStack:AddressConstArg"
                |   ObjectId32Bit4Word =>
                        raise InternalError "PushToStack:AddressConstArg"
           )

        |   cgOp(PopR reg ) =
                let
                    val (rc, rx) = getReg reg
                in
                    (* Always 64-bit but a REX prefix may be needed for the register.
                       Because the register is encoded in the instruction the rex bit for
                       the register is b not r. *)
                    opCodeBytes(POP_R rc, if rx then SOME{w=false, b = true, x=false, r = false } else NONE)
                end

        |   cgOp(Move{source=RegisterArg toStore, destination=MemoryArg{offset, base, index}, moveSize=Move64}) =
                opAddress(MOVL_R_A64, LargeInt.fromInt offset, base, index, toStore)

        |   cgOp(Move{source=RegisterArg toStore, destination=MemoryArg{offset, base, index}, moveSize=Move32}) =
                opAddress(MOVL_R_A32, LargeInt.fromInt offset, base, index, toStore)

        |   cgOp(Move{source=NonAddressConstArg toStore, destination=MemoryArg{offset, base, index}, moveSize=Move64 }) =
            (
                (* Short constant.  In 32-bit mode this is scanned as a possible address.  That means
                   we can't have an untagged constant in it.  That's not a problem in 64-bit mode.
                   There's a special check for using this to set the length word on newly allocated
                   memory. *)
                targetArch <> Native32Bit orelse toStore = 0 orelse toStore mod 2 = 1 orelse offset = ~ (Word.toInt wordSize)
                    orelse raise InternalError "cgOp: StoreConstToMemory not tagged";
                opAddressPlus2(MOVL_32_A64, LargeInt.fromInt offset, base, index, 0w0) @ int32Signed toStore
            )

        |   cgOp(Move{source=NonAddressConstArg toStore, destination=MemoryArg{offset, base, index}, moveSize=Move32 }) =
            (
                (* Short constant.  In 32-bit mode this is scanned as a possible address.  That means
                   we can't have an untagged constant in it.  That's not a problem in 64-bit mode.
                   There's a special check for using this to set the length word on newly allocated
                   memory. *)
                targetArch <> Native32Bit orelse toStore = 0 orelse toStore mod 2 = 1 orelse offset = ~ (Word.toInt wordSize)
                    orelse raise InternalError "cgOp: StoreConstToMemory not tagged";
                opAddressPlus2(MOVL_32_A32, LargeInt.fromInt offset, base, index, 0w0) @ int32Signed toStore
            )

        |   cgOp(Move{source=AddressConstArg _, destination=MemoryArg{offset, base, index}, moveSize=Move32}) =
                (* This is not used for addresses even in 32-in-64.  We don't scan for addresses after MOVL_32_A. *)
                if targetArch <> Native32Bit
                then raise InternalError "StoreLongConstToMemory in 64-bit mode"
                else opAddressPlus2(MOVL_32_A32, LargeInt.fromInt offset, base, index, 0w0) @ int32Signed (tag 0)

        |   cgOp(Move{source=AddressConstArg _, destination=MemoryArg _, ...}) =
                raise InternalError "cgOp: Move - AddressConstArg => MemoryArg"

        |   cgOp(Move{ moveSize = Move8, source=RegisterArg toStore, destination=MemoryArg{offset, base, index} }) =
            let
                val (rrC, _) = getReg toStore
                (* In 64-bit mode we can specify the low-order byte of RSI/RDI but we
                   must use a REX prefix.  This isn't possible in 32-bit mode. *)
                val opcode =
                    if hostIsX64 then MOVB_R_A64{forceRex= rrC >= 0w4}
                    else if rrC < 0w4 then MOVB_R_A32
                    else raise InternalError "High byte register"
            in
                opAddress(opcode, LargeInt.fromInt offset, base, index, toStore)
            end

        |   cgOp(Move{ moveSize = Move16, source=RegisterArg toStore, destination=MemoryArg{offset, base, index}}) =
                opAddress(MOVL_R_A16, LargeInt.fromInt offset, base, index, toStore)

        |   cgOp(Move{ moveSize = Move8, source=NonAddressConstArg toStore, destination=MemoryArg{offset, base, index}}) =
                opAddressPlus2(MOVB_8_A, LargeInt.fromInt offset, base, index, 0w0) @
                    [Word8.fromLargeInt toStore]

        |   cgOp(Move _) = raise InternalError "Move: Unimplemented arguments"

            (* Allocation is dealt with by expanding the code. *)
        |   cgOp(AllocStore _) = raise InternalError "cgOp: AllocStore"

        |   cgOp(AllocStoreVariable _) = raise InternalError "cgOp: AllocStoreVariable"

        |   cgOp StoreInitialised = raise InternalError "cgOp: StoreInitialised"

        |   cgOp(CallAddress(NonAddressConstArg _)) = (* Call to the start of the code.  Offset is patched in later. *)
                opCodeBytes (CALL_32, NONE) @ int32Signed 0

        |   cgOp(CallAddress(AddressConstArg address)) =
            if targetArch = Native64Bit andalso not (isLocalCodeAddress address)
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
            
        |   cgOp(CallAddress(RegisterArg reg)) = opRegPlus2(Group5, reg, 0w2 (* call *))
     
        |   cgOp(CallAddress(MemoryArg{base, offset, index})) =
                opAddressPlus2(Group5, LargeInt.fromInt offset, base, index, 0w2 (* call *))

        |   cgOp(JumpAddress(NonAddressConstArg _)) =
                (* Jump to the start of the current function.  Offset is patched in later. *)
                opCodeBytes (JMP_32, NONE) @ int32Signed 0

        |   cgOp(JumpAddress (AddressConstArg address)) =
            if targetArch = Native64Bit andalso not (isLocalCodeAddress address)
            then
            let
                val opb = opCodeBytes (Group5, NONE)
                val mdrm = modrm(Based0, 0w4 (* jmp *), 0w5 (* PC rel *))
            in
                opb @ [mdrm] @ int32Signed(tag 0)
            end
            else opCodeBytes (JMP_32, NONE) @ int32Signed ~5 (* As with Call. *)
            
        |   cgOp(JumpAddress (RegisterArg reg)) =
            (* Used as part of indexed case - not for entering a function. *)
                opRegPlus2(Group5, reg, 0w4 (* jmp *))

        |   cgOp(JumpAddress(MemoryArg{base, offset, index})) =
                opAddressPlus2(Group5, LargeInt.fromInt offset, base, index, 0w4 (* jmp *))
    
        |   cgOp(ReturnFromFunction args) =
            if args = 0
            then opCodeBytes(RET, NONE)
            else
            let
                val offset = Word.fromInt args * nativeWordSize
            in
                opCodeBytes(RET_16, NONE) @ [wordToWord8 offset, wordToWord8(offset >> 0w8)]
            end

        |   cgOp (RaiseException { workReg }) =
                opEA(if hostIsX64 then MOVL_A_R64 else MOVL_A_R32, LargeInt.fromInt memRegHandlerRegister, ebp, workReg) @
                    opAddressPlus2(Group5, 0, workReg, NoIndex, 0w4 (* jmp *))

        |   cgOp(UncondBranch _) = opToInt JMP_32 :: word32Unsigned 0w0

        |   cgOp(ResetStack{numWords, preserveCC}) =
            let
                val bytes = Word.toLargeInt(Word.fromInt numWords * nativeWordSize)
            in
                (* If we don't need to preserve the CC across the reset we use ADD since
                   it's shorter. *)
                if preserveCC
                then opEA(if hostIsX64 then LEAL64 else LEAL32, bytes, esp, esp)
                else immediateOperand(ADD, esp, bytes, if hostIsX64 then OpSize64 else OpSize32)
            end

        |   cgOp(JumpLabel _) = [] (* No code. *)

        |   cgOp(LoadLabelAddress{ output, ... }) =
            (* Load the address of a label.  Used when setting up an exception handler or
               in indexed cases. *)
            (* On X86/64 we can use pc-relative addressing to set the start of the handler.
               On X86/32 we have to load the address of the start of the code and add an offset. *)
            if hostIsX64
            then opConstantOperand(LEAL64, output)
            else
            let
                val (rc, _) = getReg output
            in
                opCodeBytes(MOVL_32_R rc , NONE) @ int32Signed(tag 0) @
                        opRegPlus2(Group1_32_A32, output, arithOpToWord ADD) @ int32Signed 0
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

        |   cgOp (FPLoadFromConst {precision, ...} ) =
            (* The real constant here is actually the address of a memory
               object.  FLD takes the address as the argument and in 32-bit mode
               we use an absolute address.  In 64-bit mode we need to put the
               constant at the end of the code segment and use PC-relative
               addressing which happens to be encoded in the same way.
               There are special cases for zero and one but it's probably too
               much work to detect them. *)
            let
                val esc = case precision of SinglePrecision => 0w1 | DoublePrecision => 0w5
                val opb = opCodeBytes(FPESC esc, NONE) (* FLD [Constant] *)
                val mdrm = modrm (Based0, 0w0, 0w5 (* constant address/PC-relative *))
            in
                opb @ [mdrm] @ int32Signed(tag 0)
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

        |   cgOp (FPArithConst{ opc, precision, ... }) =
                (* See comment on FPLoadFromConst *)
            let
                val fpesc = case precision of DoublePrecision => 0w4 | SinglePrecision => 0w0
                val opb = opCodeBytes(FPESC fpesc, NONE) (* FADD etc [constnt] *)
                val mdrm = modrm (Based0, fpOpToWord opc, 0w5 (* constant address *))
            in
                opb @ [mdrm] @ int32Signed(tag 0)
            end

        |   cgOp (FPArithMemory{ opc, base, offset, precision }) =
            let
                val fpesc = case precision of DoublePrecision => 0w4 | SinglePrecision => 0w0
            in
                opPlus2(FPESC fpesc, LargeInt.fromInt offset, base, fpOpToWord opc) (* FADD/FMUL etc [r2] *)
            end

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

        |   cgOp (FPLoadInt{base, offset, opSize=OpSize64}) =
                (* fildl (esp) in 32-bit mode or fildq (esp) in 64-bit mode. *)
                opPlus2(FPESC 0w7, LargeInt.fromInt offset, base, 0w5)

        |   cgOp (FPLoadInt{base, offset, opSize=OpSize32}) =
                (* fildl (esp) in 32-bit mode or fildq (esp) in 64-bit mode. *)
                opPlus2(FPESC 0w3, LargeInt.fromInt offset, base, 0w0)

        |   cgOp (MultiplyR {source=RegisterArg srcReg, output, opSize}) =
                (* We use the 0F AF form of IMUL rather than the Group3 MUL or IMUL
                   because the former allows us to specify the destination register.
                   The Group3 forms produce double length results in RAX:RDX/EAX:EDX
                   but we only ever want the low-order half. *)
                opReg(case opSize of OpSize64 => IMUL64 | OpSize32 => IMUL32 (* 2 byte opcode *), output, srcReg)

        |   cgOp (MultiplyR {source=MemoryArg{base, offset, index}, output, opSize}) =
                (* This may be used for large-word multiplication. *)
                opAddress(case opSize of OpSize64 => IMUL64 | OpSize32 => IMUL32 (* 2 byte opcode *), LargeInt.fromInt offset, base, index, output)
        
        |   cgOp(MultiplyR {source=NonAddressConstArg constnt, output, opSize}) =
                (* If the constant is an 8-bit or 32-bit value we are actually using a
                   three-operand instruction where the argument can be a register or memory
                   and the destination register does not need to be the same as the source. *)
                if is8BitL constnt
                then opReg(case opSize of OpSize64 => IMUL_C8_64 | OpSize32 => IMUL_C8_32, output, output) @ [Word8.fromLargeInt constnt]
                else if is32bit constnt
                then opReg(case opSize of OpSize64 => IMUL_C32_64 | OpSize32 => IMUL_C32_32, output, output) @ int32Signed constnt
                else opConstantOperand(case opSize of OpSize64 => IMUL64 | OpSize32 => IMUL32, output)

        |   cgOp(MultiplyR {source=AddressConstArg _, ...}) =
                raise InternalError "Multiply - address constant"

        |   cgOp (XMMArith { opc, source=MemoryArg{base, offset, index}, output }) =
                mMXAddress(SSE2Ops opc, LargeInt.fromInt offset, base, index, output)

        |   cgOp (XMMArith { opc, source=AddressConstArg _, output=SSE2Reg rrC }) =
            let
                (* The real constant here is actually the address of an 8-byte memory
                   object.  In 32-bit mode we put this address into the code and retain
                   this memory object.  In 64-bit mode we copy the real value out of the
                   memory object into the non-address constant area and use
                   PC-relative addressing.  These happen to be encoded the same
                   way. *)
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

        |   cgOp (XMMArith { opc, source=NonAddressConstArg _, output=SSE2Reg rrC }) =
            let
                val _ = hostIsX64 orelse raise InternalError "XMMArith-NonAddressConstArg in 32-bit mode"
                (* This is currently used for 32-bit float arguments but can equally be
                   used for 64-bit values since the actual argument will always be put
                   in the 64-bit constant area. *)
                val opb = opCodeBytes(SSE2Ops opc, NONE)
                val mdrm = modrm (Based0, rrC, 0w5 (* constant address/PC-relative *))
            in
                opb @ [mdrm] @ int32Signed(tag 0)
            end

        |   cgOp (XMMStoreToMemory { toStore, address={base, offset, index}, precision }) =
            let
                val oper =
                    case precision of
                        DoublePrecision => SSE2StoreDouble
                    |   SinglePrecision => SSE2StoreSingle
            in
                mMXAddress(oper, LargeInt.fromInt offset, base, index, toStore)
            end

        |   cgOp (XMMConvertFromInt { source, output=SSE2Reg rrC, opSize, precision }) =
            let
                (* The source is a general register and the output a XMM register. *)
                (* TODO: The source can be a memory location. *)
                val (rbC, rbX) = getReg source
                val oper =
                    case (opSize, precision) of
                        (OpSize64, DoublePrecision) => CVTSI2SD64
                    |   (OpSize32, DoublePrecision) => CVTSI2SD32
                    |   (OpSize64, SinglePrecision) => CVTSI2SS64
                    |   (OpSize32, SinglePrecision) => CVTSI2SS32
            in
                (* This is a special case with both an XMM and general register. *)
                opcodePrefix oper @ rexByte(oper, false, rbX, false) @
                    escapePrefix oper @ [opToInt oper, modrm(Register, rrC, rbC)]
            end

        |   cgOp (SignExtendForDivide OpSize64) =
                opCodeBytes(CQO_CDQ64, SOME {w=true, r=false, b=false, x=false})

        |   cgOp (SignExtendForDivide OpSize32) =
                opCodeBytes(CQO_CDQ32, NONE)
            
        |   cgOp (XChng { reg, arg=RegisterArg regY, opSize }) =
                opReg(case opSize of OpSize64 => XCHNG64 | OpSize32 => XCHNG32, reg, regY)
            
        |   cgOp (XChng { reg, arg=MemoryArg{offset, base, index}, opSize }) =
                opAddress(case opSize of OpSize64 => XCHNG64 | OpSize32 => XCHNG32, LargeInt.fromInt offset, base, index, reg)
                
        |   cgOp (XChng _) = raise InternalError "cgOp: XChng"

        |   cgOp (Negative {output, opSize}) =
                opRegPlus2(case opSize of OpSize64 => Group3_A64 | OpSize32 => Group3_A32, output, 0w3 (* neg *))

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
                opAddress(if hostIsX64 then LEAL64 else LEAL32, ~4, addrReg, Index4 indexReg, addrReg)
            )

        |   cgOp(MoveXMMRegToGenReg { source=SSE2Reg rrC, output }) =
            let
                (* The source is a XMM register and the output a general register. *)
                val (rbC, rbX) = getReg output
                val oper = MOVDFromXMM
            in
                (* This is a special case with both an XMM and general register. *)
                opcodePrefix oper @ rexByte(oper, false, rbX, false) @
                    escapePrefix oper @ [opToInt oper, modrm(Register, rrC, rbC)]
            end

        |   cgOp(MoveGenRegToXMMReg { source, output=SSE2Reg rrC }) =
            let
                (* The source is a general register and the output a XMM register. *)
                val (rbC, rbX) = getReg source
                val oper = MOVQToXMM
            in
                (* This is a special case with both an XMM and general register. *)
                (* This needs to move the whole 64-bit value.  TODO: This is inconsistent
                   with MoveXMMRegToGenReg *)
                opcodePrefix oper @ rexByte(oper, false, rbX, false) @
                    escapePrefix oper @ [opToInt oper, modrm(Register, rrC, rbC)]
            end

        |   cgOp(XMMShiftRight { output=SSE2Reg rrC, shift }) =
            let
                val oper = PSRLDQ
            in
                opcodePrefix oper @ escapePrefix oper @ [opToInt oper, modrm(Register, 0w3, rrC), shift]
            end

        |   cgOp(FPLoadCtrlWord {base, offset, index}) =
                opIndexedPlus2(FPESC 0w1, LargeInt.fromInt offset, base, index, 0w5)

        |   cgOp(FPStoreCtrlWord  {base, offset, index}) =
                opIndexedPlus2(FPESC 0w1, LargeInt.fromInt offset, base, index, 0w7)

        |   cgOp(XMMLoadCSR  {base, offset, index}) =
                opIndexedPlus2(LDSTMXCSR, LargeInt.fromInt offset, base, index, 0w2)

        |   cgOp(XMMStoreCSR  {base, offset, index}) =
                opIndexedPlus2(LDSTMXCSR, LargeInt.fromInt offset, base, index, 0w3)

        |   cgOp(FPStoreInt {base, offset, index}) =
                (* fistp dword ptr [esp] in 32-bit mode or fistp qword ptr [rsp] in 64-bit mode. *)
                if hostIsX64
                then opIndexedPlus2(FPESC 0w7, LargeInt.fromInt offset, base, index, 0w7)
                else opIndexedPlus2(FPESC 0w3, LargeInt.fromInt offset, base, index, 0w3)

        |   cgOp(XMMStoreInt {source, output, precision, isTruncate}) =
            let
                (* The destination is a general register.  The source is an XMM register or memory. *)
                val (rbC, rbX) = getReg output
                val oper =
                    case (hostIsX64, precision, isTruncate) of
                        (false, DoublePrecision, false) => CVTSD2SI32
                    |   (true, DoublePrecision, false)  => CVTSD2SI64
                    |   (false, SinglePrecision, false) => CVTSS2SI32
                    |   (true, SinglePrecision, false)  => CVTSS2SI64
                    |   (false, DoublePrecision, true)  => CVTTSD2SI32
                    |   (true, DoublePrecision, true)   => CVTTSD2SI64
                    |   (false, SinglePrecision, true)  => CVTTSS2SI32
                    |   (true, SinglePrecision, true)   => CVTTSS2SI64
            in
                case source of
                    MemoryArg{base, offset, index} =>
                        opAddress(oper, LargeInt.fromInt offset, base, index, output)
                |   RegisterArg(SSE2Reg rrS) =>
                        opcodePrefix oper @ rexByte(oper, rbX, false, false) @
                            escapePrefix oper @ [opToInt oper, modrm(Register, rbC, rrS)]
                |   _ => raise InternalError "XMMStoreInt: Not register or memory"
            end

        |   cgOp(CondMove { test, output, source=RegisterArg source, opSize=OpSize32 }) =
                opReg(CMOV32 test, output, source)

        |   cgOp(CondMove { test, output, source=RegisterArg source, opSize=OpSize64 }) =
                opReg(CMOV64 test, output, source)

        |   cgOp(CondMove { test, output, source=NonAddressConstArg _, opSize }) =
            (
                (* We currently support only native-64 bit and put the constant in the
                   non-address constant area.  These are 64-bit values both in native
                   64-bit and in 32-in-64.  To support it in 32-bit mode we'd have to
                   put the constant in a single-word object and put its absolute
                   address into the code. *)
                targetArch <> Native32Bit orelse
                            raise InternalError "CondMove: constant in 32-bit mode";
                opConstantOperand((case opSize of OpSize32 => CMOV32 | OpSize64 => CMOV64) test, output)
            )

        |   cgOp(CondMove { test, output, source=AddressConstArg _, opSize=OpSize64 }) =
                (* An address constant.  The opSize must match the size of a polyWord since
                   the value it going into the constant area. *)
            (
                targetArch = Native64Bit orelse raise InternalError "CondMove: AddressConstArg";
                opConstantOperand(CMOV64 test, output)
            )

        |   cgOp(CondMove { test, output, source=AddressConstArg _, opSize=OpSize32 }) =
            (
                (* We only support address constants in 32-in-64. *)
                targetArch = ObjectId32Bit2Word orelse targetArch = ObjectId32Bit4Word orelse raise InternalError "CondMove: AddressConstArg";
                opConstantOperand(CMOV32 test, output)
            )

        |   cgOp(CondMove { test, output, source=MemoryArg{base, offset, index}, opSize=OpSize32 }) =
                opAddress(CMOV32 test, LargeInt.fromInt offset, base, index, output)

        |   cgOp(CondMove { test, output, source=MemoryArg{base, offset, index}, opSize=OpSize64 }) =
                opAddress(CMOV64 test, LargeInt.fromInt offset, base, index, output)
        
        |   cgOp PauseForSpinLock = opCodeBytes(PAUSE, NONE)

        |   cgOp (BitScanR { output, source=RegisterArg source, opSize=OpSize32 }) =
                opReg(BSR32, output, source)

        |   cgOp (BitScanR { output, source=RegisterArg source, opSize=OpSize64 }) =
                opReg(BSR64, output, source)

        |   cgOp (BitScanR { output, source=MemoryArg{base, offset, index}, opSize=OpSize32 }) =
                opAddress(BSR32, LargeInt.fromInt offset, base, index, output)

        |   cgOp (BitScanR { output, source=MemoryArg{base, offset, index}, opSize=OpSize64 }) =
                opAddress(BSR64, LargeInt.fromInt offset, base, index, output)

        |   cgOp (BitScanR _) =
                raise InternalError "cgOp: BitScanR"
                

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
                if hostIsX64
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
                Word8Vector.fromList(opAddress(if hostIsX64 then LEAL64 else LEAL32, ~1, addrReg, Index1 indexReg, addrReg)) :: list

        |   fixupAddress(CallAddress(NonAddressConstArg _), brCode, ic, list) =
            let
                val brLen = Word8Vector.length brCode
            in
                (* Call to the start of the code.  Offset is -(bytes to start). *)
                Word8VectorSlice.concat[
                    Word8VectorSlice.slice(brCode, 0, SOME(brLen-4)), (* The original opcode. *)
                    Word8VectorSlice.full(Word8Vector.fromList(int32Signed(LargeInt.fromInt(~(Word.toInt ic+brLen)))))
                ] :: list
            end

        |   fixupAddress(JumpAddress(NonAddressConstArg _), brCode, ic, list) =
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
       In 64-bit mode inline constants were used with the MOV instruction but this has now been removed.
       All constants are stored in one of two areas at the end of the
       code segment.  Non-addresses, including the actual values of reals, are stored in the non-address area
       and addresses go in the address area.  Only the latter is scanned by the GC.
       The address area is also used in 32-bit mode but only has the address of the function name and the
       address of the profile ref in it. *)
    datatype inline32constants =
        SelfAddress                             (* The address of the start of the code - inline absolute address 32-bit only *)
    |   InlineAbsoluteAddress of machineWord    (* An address in the code: 32-bit only *)
    |   InlineRelativeAddress of machineWord    (* A relative address: 32-bit only. *)

    local
        (* Turn an integer constant into an 8-byte vector. *)
        fun intConst ival = LargeWord.fromLargeInt ival

        (* Copy a real constant from memory into an 8-byte vector. *)
        fun realConst c =
        let
            val cAsAddr = toAddress c
            (* This may be a boxed real or, in 32-in-64 mode, a boxed float. *)
            val cLength = length cAsAddr * wordSize
            val _ = ((cLength = 0w8 orelse cLength = 0w4) andalso flags cAsAddr = F_bytes) orelse
                        raise InternalError "realConst: Not a real number"
            fun getBytes(i, a) =
                if i = 0w0 then a
                else getBytes(i-0w1, a*0w256 + Word8.toLargeWord(loadByte(cAsAddr, i-0w1)))
        in
            getBytes(cLength, 0w0)
        end

        fun getConstant(Move{ source=NonAddressConstArg source, moveSize=Move32, ...}, bytes, ic, (inl, addr, na)) =
            if targetArch <> Native32Bit
            then
            (
                if source >= ~0x80000000 andalso source < 0x100000000
                then (* Signed or unsigned 32-bits. *) (inl, addr, na)
                else (* Too big for 32-bits. *)
                    (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, intConst source) :: na)
            )
            else (inl, addr, na) (* 32-bit mode.  The constant will always be inline even if we've had to use LEA r,c *)

        |   getConstant(Move{ source=NonAddressConstArg source, moveSize=Move64, ...}, bytes, ic, (inl, addr, na)) =
            if targetArch <> Native32Bit
            then
            (
                if source >= ~0x80000000 andalso source < 0x100000000
                then (* Signed or unsigned 32-bits. *) (inl, addr, na)
                else (* Too big for 32-bits. *)
                    (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, intConst source) :: na)
            )
            else (inl, addr, na) (* 32-bit mode.  The constant will always be inline even if we've had to use XOR r,r; ADD r,c *)

        |   getConstant(Move{ source=AddressConstArg source, ... }, bytes, ic, (inl, addr, na)) =
            if targetArch <> Native32Bit
            then (* Address constants go in the constant area. *)
                (inl, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, source) :: addr, na)
            else ((ic + Word.fromInt(Word8Vector.length bytes) - wordSize, InlineAbsoluteAddress source) :: inl, addr, na)

        |   getConstant(LoadAbsolute{value, ...}, bytes, ic, (inl, addr, na)) =
            if targetArch = Native64Bit
            then (* Address constants go in the constant area. *)
                (inl, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, value) :: addr, na)
                (* This is the only case of an inline constant in 32-in-64 *)
            else ((ic + Word.fromInt(Word8Vector.length bytes) - nativeWordSize, InlineAbsoluteAddress value) :: inl, addr, na)

        |   getConstant(ArithToGenReg{ source=NonAddressConstArg source, ... }, bytes, ic, (inl, addr, na)) =
                if is32bit source
                then (inl, addr, na)
                else (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, intConst source) :: na)

        |   getConstant(ArithToGenReg{ source=AddressConstArg source, ... }, bytes, ic, (inl, addr, na)) =
                if hostIsX64
                then (inl, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, source) :: addr, na)
                else ((ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineAbsoluteAddress source) :: inl, addr, na)

        |   getConstant(ArithMemLongConst{ source, ... }, bytes, ic, (inl, addr, na)) = (* 32-bit only. *)
                ((ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineAbsoluteAddress source) :: inl, addr, na)

        |   getConstant(PushToStack(NonAddressConstArg constnt), bytes, ic, (inl, addr, na)) =
                if is32bit constnt then (inl, addr, na)
                else (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, intConst constnt) ::  na)

        |   getConstant(PushToStack(AddressConstArg constnt), bytes, ic, (inl, addr, na)) =
                if targetArch = Native64Bit
                then (inl, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, constnt) :: addr, na)
                else ((ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineAbsoluteAddress constnt) :: inl, addr, na)

        |   getConstant(CallAddress(AddressConstArg w), bytes, ic, (inl, addr, na)) =
                if targetArch = Native64Bit andalso not (isLocalCodeAddress w)
                then (inl, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, w) :: addr, na)
                else ((ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineRelativeAddress w) :: inl, addr, na)

        |   getConstant(JumpAddress(AddressConstArg w), bytes, ic, (inl, addr, na)) =
                if targetArch = Native64Bit andalso not (isLocalCodeAddress w)
                then (inl, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, w) :: addr, na)
                else ((ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineRelativeAddress w) :: inl, addr, na)

        |   getConstant(LoadLabelAddress _, _, ic, (inl, addr, na)) =
                (* We need the address of the code itself but it's in the first of a pair of instructions. *)
                if hostIsX64 then (inl, addr, na) else ((ic + 0w1, SelfAddress) :: inl, addr, na)

        |   getConstant(FPLoadFromConst{constant, ...}, bytes, ic, (inl, addr, na)) =
               if hostIsX64
               then (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, realConst constant) :: na)
               else ((ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineAbsoluteAddress constant) :: inl, addr, na)

        |   getConstant(FPArithConst{ source, ... }, bytes, ic, (inl, addr, na)) =
                if hostIsX64
                then (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, realConst source) :: na)
                else ((ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineAbsoluteAddress source) :: inl, addr, na)

        |   getConstant(XMMArith { source=AddressConstArg constVal, ... }, bytes, ic, (inl, addr, na)) =
                (* Real.real constant or, with 32-bit words, a Real32.real constant. *)
                if hostIsX64
                then (inl, addr,  (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, realConst constVal) :: na)
                else ((ic + Word.fromInt(Word8Vector.length bytes) - 0w4, InlineAbsoluteAddress constVal) :: inl, addr, na)

        |   getConstant(XMMArith { source=NonAddressConstArg constVal, ... }, bytes, ic, (inl, addr, na)) =
                (* Real32.real constant in native 64-bit. *)
                (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, intConst constVal) :: na)

        |   getConstant(MultiplyR{ source=NonAddressConstArg source, ... }, bytes, ic, (inl, addr, na)) =
                if is32bit source
                then (inl, addr, na)
                else (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, intConst source) :: na)

        |   getConstant(CondMove{ source=NonAddressConstArg source, ... }, bytes, ic, (inl, addr, na)) =
            if targetArch <> Native32Bit
            then (inl, addr, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, intConst source) :: na)
            else (inl, addr, na) (* 32-bit mode.  The constant will always be inline. *)

        |   getConstant(CondMove{ source=AddressConstArg source, ... }, bytes, ic, (inl, addr, na)) =
            if targetArch <> Native32Bit
            then (* Address constants go in the constant area. *)
                (inl, (ic + Word.fromInt(Word8Vector.length bytes) - 0w4, source) :: addr, na)
            else ((ic + Word.fromInt(Word8Vector.length bytes) - wordSize, InlineAbsoluteAddress source) :: inl, addr, na)

        |   getConstant(_, _, _, l) = l        
    in
        val getConstants = foldCode getConstant ([], [], [])
    end

    (* It is convenient to have AllocStore and AllocStoreVariable as primitives at the higher
       level but at this point it's better to expand them into their basic instructions. *)
    fun expandComplexOperations(instrs, oldLabelCount) =
    let
        val labelCount = ref oldLabelCount
        fun mkLabel() = Label{labelNo= !labelCount} before labelCount := !labelCount + 1

         (* On X86/64 the local pointer is in r15.  On X86/32 it's in memRegs. *)
        val localPointer =
            if hostIsX64 then RegisterArg r15 else MemoryArg{base=ebp, offset=memRegLocalMPointer, index=NoIndex}
        
        val nativeWordOpSize = if hostIsX64 then OpSize64 else OpSize32

        fun allocStoreCommonCode (resultReg, isVarAlloc, regSaveSet: genReg list) =
        let
            val compare =
                ArithToGenReg{opc=CMP, output=resultReg,
                    source=MemoryArg{base=ebp, offset=memRegLocalMbottom, index=NoIndex}, opSize=nativeWordOpSize}
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
                    [ConditionalBranch{test=JB, label=extraLabel},
                     ArithToGenReg{opc=CMP, output=resultReg, source=localPointer, opSize=nativeWordOpSize},
                     ConditionalBranch{test=JB, label=destLabel},
                     JumpLabel extraLabel]
                end
                else [ConditionalBranch{test=JNB, label=destLabel}]
            val callRts = CallRTS{rtsEntry=HeapOverflowCall, saveRegs=regSaveSet}
            val fixup = JumpLabel destLabel
            (* Update the heap pointer now we have the store.  This is also
               used by the RTS in the event of a trap to work out how much
               store was being allocated. *)
            val update =
                if hostIsX64 then Move{source=RegisterArg resultReg, destination=RegisterArg r15, moveSize=Move64}
                else Move{source=RegisterArg resultReg,
                        destination=MemoryArg{base=ebp, offset=memRegLocalMPointer, index=NoIndex}, moveSize=Move32}
        in
            compare :: branches @ [callRts, fixup, update]
        end
        
        fun doExpansion([], code, _) = code

        |   doExpansion(AllocStore {size, output, saveRegs} :: instrs, code, inAllocation) =
            let
                val _ = inAllocation andalso raise InternalError "doExpansion: Allocation started but not complete"
                val () = if List.exists (fn r => r = output) saveRegs then raise InternalError "AllocStore: in set" else ()

                val startCode =
                    case targetArch of
                        Native64Bit =>
                        let
                            val bytes = (size + 1) * Word.toInt wordSize
                        in
                            [LoadAddress{output=output, offset = ~ bytes, base=SOME r15, index=NoIndex, opSize=OpSize64}]
                             (* TODO: What if it's too big to fit? *)
                        end
                    |   Native32Bit =>
                        let
                            val bytes = (size + 1) * Word.toInt wordSize
                        in
                            [Move{source=MemoryArg{base=ebp, offset=memRegLocalMPointer, index=NoIndex},
                                destination=RegisterArg output, moveSize=Move32},
                             LoadAddress{output=output, offset = ~ bytes, base=SOME output, index=NoIndex, opSize=OpSize32}]
                        end
                    |   ObjectId32Bit2Word =>
                        let
                            (* We must allocate an even number of words. *)
                            val heapWords = if Int.rem(size, 2) = 1 then size+1 else size+2
                            val bytes = heapWords * Word.toInt wordSize
                        in
                            [LoadAddress{output=output, offset = ~ bytes, base=SOME r15, index=NoIndex, opSize=OpSize64}]
                        end
                    |   ObjectId32Bit4Word =>
                        let
                            (* We must allocate in multiples of four words. Have to add one for the length word. *)
                            val heapWords = Int.div(size+1+3, 4) * 4
                            val bytes = heapWords * Word.toInt wordSize
                        in
                            [LoadAddress{output=output, offset = ~ bytes, base=SOME r15, index=NoIndex, opSize=OpSize64}]
                        end
                            
                val resultCode = startCode @ allocStoreCommonCode(output, false, saveRegs)
            in
                doExpansion(instrs, (List.rev resultCode) @ code, true)
            end

        |   doExpansion(AllocStoreVariable {size, output, saveRegs} :: instrs, code, inAllocation) =
            let
                (* Allocates memory.  The "size" register contains the number of words as a tagged int. *)
                val _ = inAllocation andalso raise InternalError "doExpansion: Allocation started but not complete"
                val () = if List.exists (fn r => r = output) saveRegs then raise InternalError "AllocStore: in set" else ()
                (* Negate the length and add it to the current heap pointer. *)
                (* Compute the number of bytes into dReg. The length in sReg is the number
                   of words as a tagged value so we need to multiply it, add wordSize to
                   include one word for the header then subtract the, multiplied, tag.
                   We use LEA here but want to avoid having an empty base register. *)
                val _ = size = output andalso raise InternalError "AllocStoreVariable : same register for size and output"
                val startCode =
                    if wordSize = 0w8 (* 8-byte words *)
                    then
                    [
                        ArithToGenReg{opc=XOR, output=output, source=RegisterArg output, opSize=OpSize32 (* Rest is zeroed *)},
                        ArithToGenReg{opc=SUB, output=output, source=RegisterArg size, opSize=OpSize64},
                        LoadAddress{output=output, base=SOME r15, offset= ~(Word.toInt wordSize-4), index=Index4 output, opSize=OpSize64 }
                    ]
                    else (* 4 byte words *)
                    [
                        LoadAddress{output=output, base=SOME size, offset=Word.toInt wordSize-2,
                            index=Index1 size, opSize=nativeWordOpSize },
                        Negative{output=output, opSize=nativeWordOpSize},
                        ArithToGenReg{opc=ADD, output=output, source=localPointer, opSize=nativeWordOpSize}
                    ]
                (* If this is 32-in-64 we need to round down to the next 8 or 16-byte boundary. *)
                val roundCode =
                    case targetArch of
                        ObjectId32Bit2Word => [ArithToGenReg{opc=AND, output=output, source=NonAddressConstArg ~8, opSize=OpSize64 }]
                    |   ObjectId32Bit4Word => [ArithToGenReg{opc=AND, output=output, source=NonAddressConstArg ~16, opSize=OpSize64 }]
                    |   Native32Bit => []
                    |   Native64Bit => []
                val resultCode = startCode @ roundCode @ allocStoreCommonCode(output, true, saveRegs)
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
            case (modrm >>- 0w6, modrm andb8 0w7, hostIsX64) of
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

                    case (md, base, hostIsX64) of
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
            if opByte = opToInt Group2_1_A32 then print "1"
            else if opByte = opToInt Group2_CL_A32 then print "cl"
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
                |   (0w3, 0w4, 0w2) => print "fnclex"
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
                    (0w0, 0w0) => (print "fadd\t"; printEA(rex, SZDWord)) (* Single precision. *)
                |   (0w0, 0w1) => (print "fmul\t"; printEA(rex, SZDWord))
                |   (0w0, 0w3) => (print "fcomp\t"; printEA(rex, SZDWord))
                |   (0w0, 0w4) => (print "fsub\t"; printEA(rex, SZDWord))
                |   (0w0, 0w5) => (print "fsubr\t"; printEA(rex, SZDWord))
                |   (0w0, 0w6) => (print "fdiv\t"; printEA(rex, SZDWord))
                |   (0w0, 0w7) => (print "fdivr\t"; printEA(rex, SZDWord))
                |   (0w1, 0w0) => (print "fld \t"; printEA(rex, SZDWord))
                |   (0w1, 0w2) => (print "fst\t"; printEA(rex, SZDWord))
                |   (0w1, 0w3) => (print "fstp\t"; printEA(rex, SZDWord))
                |   (0w1, 0w5) => (print "fldcw\t"; printEA(rex, SZWord)) (* Control word is 16 bits *)
                |   (0w1, 0w7) => (print "fstcw\t"; printEA(rex, SZWord)) (* Control word is 16 bits *)
                |   (0w3, 0w0) => (print "fild\t"; printEA(rex, SZDWord)) (* 32-bit int. *)
                |   (0w7, 0w5) => (print "fild\t"; printEA(rex, SZQWord)) (* 64-bit int. *)
                |   (0w3, 0w3) => (print "fistp\t"; printEA(rex, SZDWord)) (* 32-bit int. *)
                |   (0w7, 0w7) => (print "fistp\t"; printEA(rex, SZQWord)) (* 64-bit int. *)
                |   (0w4, 0w0) => (print "fadd\t"; printEA(rex, SZQWord)) (* Double precision. *)
                |   (0w4, 0w1) => (print "fmul\t"; printEA(rex, SZQWord))
                |   (0w4, 0w3) => (print "fcomp\t"; printEA(rex, SZQWord))
                |   (0w4, 0w4) => (print "fsub\t"; printEA(rex, SZQWord))
                |   (0w4, 0w5) => (print "fsubr\t"; printEA(rex, SZQWord))
                |   (0w4, 0w6) => (print "fdiv\t"; printEA(rex, SZQWord))
                |   (0w4, 0w7) => (print "fdivr\t"; printEA(rex, SZQWord))
                |   (0w5, 0w0) => (print "fld \t"; printEA(rex, SZQWord))
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
            
            val sizeFromRexW = if rexW then SZQWord else SZDWord
        in
            case opByte of
                0wx03 => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx0b => printGvEv (opByte, rex, rexR, sizeFromRexW)

            |   0wx0f => (* ESCAPE *)
                let
                    (* Opcode is in next byte. *)
                    val opByte2  = codeVecGet (seg, !ptr)
                    val () = (ptr +:= 0w1)
                    
                    fun printcmov movop =
                    let
                        val nb = codeVecGet (seg, !ptr)
                        val reg = (nb >>- 0w3) andb8 0w7
                    in
                        print movop;
                        print "\t";
                        print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                        print ",";
                        printEA(rex, sizeFromRexW)
                    end
                in
                    case legacyPrefix of
                        0w0 =>
                        (
                            case opByte2 of
                                0wx2e =>
                                let (* ucomiss doesn't have a prefix. *)
                                    val nb = codeVecGet (seg, !ptr)
                                    val reg = SSE2Reg((nb >>- 0w3) andb8 0w7)
                                in
                                    print "ucomiss\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZDWord)
                                end

                            |   0wx40 => printcmov "cmovo"
                            |   0wx41 => printcmov "cmovno"
                            |   0wx42 => printcmov "cmovb"
                            |   0wx43 => printcmov "cmovnb"
                            |   0wx44 => printcmov "cmove"
                            |   0wx45 => printcmov "cmovne"
                            |   0wx46 => printcmov "cmovna"
                            |   0wx47 => printcmov "cmova"
                            |   0wx48 => printcmov "cmovs"
                            |   0wx49 => printcmov "cmovns"
                            |   0wx4a => printcmov "cmovp"
                            |   0wx4b => printcmov "cmovnp" 
                            |   0wx4c => printcmov "cmovl"
                            |   0wx4d => printcmov "cmovge"
                            |   0wx4e => printcmov "cmovle"
                            |   0wx4f => printcmov "cmovg"

                            |   0wxB1 =>
                                let
                                    val nb = codeVecGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    (* The address argument comes first in the assembly code. *)
                                    print "cmpxchg\t";
                                    printEA (rex, sizeFromRexW);
                                    print ",";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW))
                                end

                            |   0wxC1 =>
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

                            |   0wxBD =>
                                let
                                    val nb = codeVecGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "bsr\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, sizeFromRexW)
                                end

                            |   0wxBE =>
                                let
                                    val nb = codeVecGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "movsx\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, SZByte)
                                end

                            |   0wxBF =>
                                let
                                    val nb = codeVecGet (seg, !ptr);
                                    val reg = (nb >>- 0w3) andb8 0w7
                                in
                                    print "movsx\t";
                                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                                    print ",";
                                    printEA (rex, SZWord)
                                end

                            |   0wxAE =>
                                let
                                    (* Opcode is determined by the next byte. *)
                                    val opByte2 = codeVecGet (seg, !ptr);
                                    val nnn = (opByte2 >>- 0w3) andb8 0w7
                                in
                                    case nnn of
                                        0wx2 => (print "ldmxcsr\t"; printEA(rex, SZDWord))
                                    |   0wx3 => (print "stmxcsr\t"; printEA(rex, SZDWord))
                                    |   _ => (printValue(Word8.toLargeInt opByte); printValue(Word8.toLargeInt opByte2))
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

                            |   0wx90 => (print "seto\t";  printEA (rex, SZByte))
                            |   0wx91 => (print "setno\t"; printEA (rex, SZByte))
                            |   0wx92 => (print "setb\t";  printEA (rex, SZByte))
                            |   0wx93 => (print "setnb\t"; printEA (rex, SZByte))
                            |   0wx94 => (print "sete\t";  printEA (rex, SZByte))
                            |   0wx95 => (print "setne\t"; printEA (rex, SZByte))
                            |   0wx96 => (print "setna\t"; printEA (rex, SZByte))
                            |   0wx97 => (print "seta\t";  printEA (rex, SZByte))
                            |   0wx98 => (print "sets\t";  printEA (rex, SZByte))
                            |   0wx99 => (print "setns\t"; printEA (rex, SZByte))
                            |   0wx9a => (print "setp\t";  printEA (rex, SZByte))
                            |   0wx9b => (print "setnp\t"; printEA (rex, SZByte)) 
                            |   0wx9c => (print "setl\t";  printEA (rex, SZByte))
                            |   0wx9d => (print "setge\t"; printEA (rex, SZByte))
                            |   0wx9e => (print "setle\t"; printEA (rex, SZByte))
                            |   0wx9f => (print "setg\t";  printEA (rex, SZByte))
                            
                            |   _ => (print "esc\t"; printValue(Word8.toLargeInt opByte2))
                        )
                    
                    |   0wxf2 => (* SSE2 instruction *)
                        let
                            val nb = codeVecGet (seg, !ptr)
                            val rr = (nb >>- 0w3) andb8 0w7
                            val reg = SSE2Reg rr
                        in
                            case opByte2 of
                                0wx10 => ( print "movsd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZQWord) )
                            |   0wx11 => ( print "movsd\t"; printEAxmm(rex, SZQWord); print ","; print(xmmRegRepr reg)  )
                            |   0wx2a => ( print "cvtsi2sd\t"; print(xmmRegRepr reg); print ","; printEA(rex, sizeFromRexW)  )
                            |   0wx2c =>
                                    ( print "cvttsd2si\t"; print (genRegRepr (mkReg(rr, rexR), sizeFromRexW)); print ","; printEAxmm(rex, sizeFromRexW)  )
                            |   0wx2d =>
                                    ( print "cvtsd2si\t"; print (genRegRepr (mkReg(rr, rexR), sizeFromRexW)); print ","; printEAxmm(rex, sizeFromRexW)  )
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
                            val rr = (nb >>- 0w3) andb8 0w7
                            val reg = SSE2Reg rr
                        in
                            case opByte2 of
                                0wx10 => ( print "movss\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZDWord) )
                            |   0wx11 => ( print "movss\t"; printEAxmm(rex, SZDWord); print ","; print(xmmRegRepr reg)  )
                            |   0wx2a => ( print "cvtsi2ss\t"; print(xmmRegRepr reg); print ","; printEA(rex, sizeFromRexW)  )
                            |   0wx2c =>
                                    ( print "cvttss2si\t"; print (genRegRepr (mkReg(rr, rexR), sizeFromRexW)); print ","; printEAxmm(rex, sizeFromRexW)  )
                            |   0wx2d =>
                                    ( print "cvtss2si\t"; print (genRegRepr (mkReg(rr, rexR), sizeFromRexW)); print ","; printEAxmm(rex, sizeFromRexW)  )
                            |   0wx5a => ( print "cvtss2sd\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZDWord)  )
                            |   0wx58 => ( print "addss\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZDWord) )
                            |   0wx59 => ( print "mulss\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZDWord) )
                            |   0wx5c => ( print "subss\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZDWord) )
                            |   0wx5e => ( print "divss\t"; print(xmmRegRepr reg); print ","; printEAxmm(rex, SZDWord) )
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
                            |   0wx6e => ( print (if rexW then "movq\t" else "movd\t"); print(xmmRegRepr reg); print ","; printEA(rex, sizeFromRexW) )
                            |   0wx7e => ( print (if rexW then "movq\t" else "movd\t"); printEA(rex, sizeFromRexW); print ","; print(xmmRegRepr reg)  )
                            |   0wx73 => ( print "psrldq\t"; printEAxmm(rex, SZQWord); print ","; print8 ())
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
            
            |   0wx63 => (* MOVSXD. This is ARPL in 32-bit mode but that's never used here. *)
                let
                    val nb = codeVecGet (seg, !ptr)
                    val reg = (nb >>- 0w3) andb8 0w7
                in
                    print "movsxd\t";
                    print (genRegRepr (mkReg(reg, rexR), sizeFromRexW));
                    print ",";
                    printEA(rex, SZDWord)
                end

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
                    (* This may have an opcode prefix. *)
                    printEA(rex, if legacyPrefix = 0wx66 then SZWord else sizeFromRexW);
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

            |   0wx90 => if legacyPrefix = 0wxf3 then print "pause" else print "nop"
            
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

    (* Although this is used locally it must be defined at the top level
       otherwise a new RTS function will be compiler every time the
       containing function is called *)
    val sortFunction: (machineWord * word) array -> bool =
        RunCall.rtsCallFast1 "PolySortArrayOfAddresses"

    (* This actually does the final code-generation. *)
    fun generateCode
        {ops=operations,
         code=cvec as Code{procName, printAssemblyCode, printStream, profileObject, ...},
         labelCount, resultClosure} : unit =
    let
        val (expanded, newLabelCount) = expandComplexOperations (operations, labelCount)

        val () = printLowLevelCode(expanded, cvec)
        local
            val initialBytesList = codeGenerate expanded
        in
            (* Fixup labels and shrink long branches to short. *)
            val (bytesList, codeSize) = fixupLabels(expanded, initialBytesList, newLabelCount)
        end

        local
            (* Extract the constants and the location of the references from the code. *)
            val (inlineConstants, addressConstants, nonAddressConstants) = getConstants(expanded, bytesList)
            
            (* Sort the non-address constants to remove duplicates.  There don't seem to be
               many in practice.
               Since we're not actually interested in the order but only
               sorting to remove duplicates we can use a stripped-down Quicksort. *)
            fun sort([], out) = out
            |   sort((addr, median) :: tl, out) = partition(median, tl, [addr], [], [], out)

            and partition(median, [], addrs, less, greater, out) =
                    sort(less, sort(greater, (addrs, median) :: out))
            |   partition(median, (entry as (addr, value)) :: tl, addrs, less, greater, out) =
                    if value = median
                    then partition(median, tl, addr::addrs, less, greater, out)
                    else if value < median
                    then partition(median, tl, addrs, entry :: less, greater, out)
                    else partition(median, tl, addrs, less, entry :: greater, out)
            
            (* Non-address constants.  We can't use any ordering on them because a GC could
               change the values half way through the sort.  Instead we use a simple search
               for a small number of constants and use an RTS call for larger numbers.  We
               want to avoid quadratic cost when there are large numbers. *)

            val sortedConstants =
                if List.length addressConstants < 10
                then
                let
                    fun findDups([], out) = out
                    |   findDups((addr, value) :: tl, out) =
                        let
                            fun partition(e as (a, v), (eq, neq)) =
                                if PolyML.pointerEq(value, v)
                                then (a :: eq, neq)
                                else (eq, e :: neq)
                            val (eqAddr, neq) = List.foldl partition ([addr], []) tl
                        in
                            findDups(neq, (eqAddr, value) :: out)
                        end
                in
                    findDups(addressConstants, [])
                end
                else
                let
                    fun swap (a, b) = (b, a)
                    val arrayToSort: (machineWord * word) array =
                        Array.fromList (List.map swap addressConstants)
                    val _ = sortFunction arrayToSort
                    
                    fun makeList((v, a), []) = [([a], v)]
                    |   makeList((v, a), l as (aa, vv) :: tl) =
                        if PolyML.pointerEq(v, vv)
                        then (a :: aa, vv) :: tl
                        else ([a], v) :: l
                in
                    Array.foldl makeList [] arrayToSort
                end
        in
            val inlineConstants = inlineConstants
            and addressConstants = sortedConstants
            and nonAddressConstants = sort(nonAddressConstants, [])
        end

        (* Get the number of constants that need to be added to the address area. *)
        val constsInConstArea = List.length addressConstants

        local
            (* Add one byte for the HLT and round up to a number of words. *)
            val endOfCode = (codeSize+nativeWordSize) div nativeWordSize * (nativeWordSize div wordSize)
            val numOfNonAddrWords = Word.fromInt(List.length nonAddressConstants)
            (* Each entry in the non-address constant area is 8 bytes. *)
            val intSize = 0w8 div wordSize
        in
            val endOfByteArea = endOfCode + numOfNonAddrWords * intSize
            (* +4 for no of consts. function name, profile object and offset to start of consts. *)
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
        val ()   = ic := ((!ic + nativeWordSize - 0w1) andb ~nativeWordSize)
        
        (* Copy the non-address constants.  These are only used in 64-bit mode and are
           either real constants or integers that are too large to fit in a 32-bit
           inline constants.  We don't use this for real constants in 32-bit mode because
           we don't have relative addressing.  Instead a real constant is the inline
           address of a boxed real number. *)
        local
            fun putNonAddrConst(addrs, constant) =
                let
                    val addrOfConst = ! ic
                    val () = genBytes(Word8Vector.fromList(largeWordToBytes(constant, 8)))
                    fun setAddr addr = set32s(Word.toLargeInt(addrOfConst - addr - 0w4), addr, byteVec)
                in
                    List.app setAddr addrs
                end
        in
            val () = List.app putNonAddrConst nonAddressConstants
        end

        val _ = bytesToWords(! ic) = endOfByteArea orelse raise InternalError "mismatch"
        
        (* Put in the number of constants. This must go in before we actually put
           in any constants.  In 32-bit mode there are only two constants: the 
           function name and the profile object.
           All other constants are in the code. *)
        local
            val lastWord = wordsToBytes(endOfByteArea + 0w3 + Word.fromInt constsInConstArea)

            fun setBytes(_, _, 0) = ()
            |   setBytes(ival, offset, count) =
                (
                    byteVecSet(byteVec, offset, Word8.fromLargeInt(ival mod 256));
                    setBytes(ival div 256, offset+0w1, count-1)
                )
        in
            val () = setBytes(LargeInt.fromInt(2 + constsInConstArea), wordsToBytes endOfByteArea, Word.toInt wordSize)
            (* Set the last word of the code to the (negative) byte offset of the start of the code area
               from the end of this word. *)
            val () = setBytes(Word.toLargeIntX(wordsToBytes endOfByteArea - lastWord), lastWord, Word.toInt wordSize)
        end;

        (* We've put in all the byte data so it is safe to convert this to a mutable code
           cell that can contain addresses and will be scanned by the GC. *)
        val codeSeg = byteVecToCodeVec(byteVec, resultClosure)

        (* Various RTS functions assume that the first constant is the function name.
           The profiler assumes that the second word is the address of the mutable that
           contains the profile count. *)
        val () = codeVecPutWord (codeSeg, endOfByteArea + 0w1, toMachineWord procName)
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

            (*  Inline constants - native 32-bit only plus one special case in 32-in-64 *)
            fun putInlConst (addrs, SelfAddress) =
                    (* Self address goes inline. *)
                    codeVecPutConstant (codeSeg, addrs, toMachineWord(codeVecAddr codeSeg), ConstAbsolute)
            |   putInlConst (addrs, InlineAbsoluteAddress m) =
                    codeVecPutConstant (codeSeg, addrs, m, ConstAbsolute)
            |   putInlConst (addrs, InlineRelativeAddress m) =
                    codeVecPutConstant (codeSeg, addrs, m, ConstX86Relative)
            
            val _ = List.app putInlConst inlineConstants

            (* Address constants - native 64-bit and 32-in-64. *)
            fun putAddrConst ((addrs, m), constAddr) =
            (*  Put the constant in the constant area and set the original address
                to be the relative offset to the constant itself. *)
            (
                codeVecPutWord (codeSeg, constAddr, m);
                (* Put in the 32-bit offset - always unsigned since the destination
                   is after the reference. *)
                List.app(fn addr => setBytes(constAddr * wordSize - addr - 0w4, addr, 0w4)) addrs;
                constAddr+0w1
            )

            (* Put the constants.  Any values in the constant area start at +3 i.e. after the profile. *)
            val _ = List.foldl putAddrConst (endOfByteArea+0w3) addressConstants

            val () = 
                if printAssemblyCode
                then (* print out the code *)
                (
                    printCode(cvec, codeSeg);
                    printStream "\n\n"
                )
            else ()
        in
            (* Finally lock the code. *)
            codeVecLock(codeSeg, resultClosure)
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
        and  label          = label
        and  branchOps      = branchOps
        and  arithOp        = arithOp
        and  shiftType      = shiftType
        and  repOps         = repOps
        and  fpOps          = fpOps
        and  fpUnaryOps     = fpUnaryOps
        and  sse2Operations = sse2Operations
        and  opSize         = opSize
        and  closureRef     = closureRef
    end

end (* struct *) (* CODECONS *);
