(*
    Copyright (c) 2021-3, 2026 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    Licence version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public Licence for more details.
    
    You should have received a copy of the GNU Lesser General Public
    Licence along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

functor Arm64Assembly (
    structure Debug: DEBUG
    and       Pretty: PRETTY
    and       CodeArray: CODEARRAY
) : ARM64ASSEMBLY =

struct
    open CodeArray Address
    
	datatype archType = ArchNative | ArchC32 of word
    val archType =
        case Address.compact32UnitSize of
            0w0 => ArchNative
        |   0w2 => ArchC32 0w2
        |   0w4 => ArchC32 0w3
        |   0w8 => ArchC32 0w4
        |   _ => raise Misc.InternalError "Unsupported compact 32 unit size"

    val wordsPerNativeWord: word = Address.nativeWordSize div Address.wordSize
    
    local
        (* Almost every ARM64 platform is little-endian but it is possible to
           run it in big-endian mode.  Instructions are always little-endian.
           The value of isBigEndian will be determined when the structure is
           constructed.  That's not a problem since it will be built on the
           target machine. *)
        val isBigEndian: unit -> bool = RunCall.rtsCallFast1 "PolyIsBigEndian"
    in
        val isBigEndian = isBigEndian()
    end
    
    exception InternalError = Misc.InternalError

    infix 5 << <<+ <<- >> >>+ >>- ~>> ~>>+ ~>>- (* Shift operators *)
    infix 3 andb orb xorb andbL orbL xorbL andb8 orb8 xorb8
    
    val op << = Word32.<< and op >> = Word32.>> and op ~>> = Word32.~>>
    and op andb = Word32.andb and op orb = Word32.orb

    val word32ToWord8 = Word8.fromLargeWord o Word32.toLargeWord
    and word8ToWord32 = Word32.fromLargeWord o Word8.toLargeWord
    and word32ToWord = Word.fromLargeWord o Word32.toLargeWord
    and wordToWord32 = Word32.fromLargeWord o Word.toLargeWord
    and word8ToWord = Word.fromLargeWord o Word8.toLargeWord

    (* The maximum positive number that will fit in a signed "bits" field. *)
    fun maxSigned bits = Word.<<(0w1, bits-0w1) - 0w1
    fun willFitInRange(offset, bits) = offset <= Word.toInt(maxSigned bits) andalso offset >= ~ (Word.toInt(maxSigned bits)) - 1
   
    (* XReg is used for fixed point registers since X0 and W0 are
       the same register. *)
    datatype xReg = XReg of Word8.word | XZero | XSP
    (* VReg is used for the floating point registers since V0, D0 and
       S0 are the same register. *)
    and vReg = VReg of Word8.word

    (* A Label is a ref that is later set to the location.
       Several labels can be linked together so that they are only set
       at a single point.
       Only forward jumps are linked so when we come to finally set the
       label we will have the full list. *)
    type labels = Word.word ref list ref

    (* Condition codes. *)

    (* N.B. On subtraction and comparison the ARM uses an inverted carry
       flag for borrow.  The C flag is set if there is NO borrow.
       This is the reverse of the X86. *)
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

    (* The negation of a test just involves inverting the bottom bit. *)
    fun invertTest CondEqual            = CondNotEqual
    |   invertTest CondNotEqual         = CondEqual
    |   invertTest CondCarrySet         = CondCarryClear
    |   invertTest CondCarryClear       = CondCarrySet
    |   invertTest CondNegative         = CondPositive
    |   invertTest CondPositive         = CondNegative
    |   invertTest CondOverflow         = CondNoOverflow
    |   invertTest CondNoOverflow       = CondOverflow
    |   invertTest CondUnsignedHigher   = CondUnsignedLowOrEq
    |   invertTest CondUnsignedLowOrEq  = CondUnsignedHigher
    |   invertTest CondSignedGreaterEq  = CondSignedLess
    |   invertTest CondSignedLess       = CondSignedGreaterEq
    |   invertTest CondSignedGreater    = CondSignedLessEq
    |   invertTest CondSignedLessEq     = CondSignedGreater

    fun condToString CondEqual            = "EQ"
    |   condToString CondNotEqual         = "NE"
    |   condToString CondCarrySet         = "CS"
    |   condToString CondCarryClear       = "CC"
    |   condToString CondNegative         = "MI"
    |   condToString CondPositive         = "PL"
    |   condToString CondOverflow         = "VS"
    |   condToString CondNoOverflow       = "VC"
    |   condToString CondUnsignedHigher   = "HI"
    |   condToString CondUnsignedLowOrEq  = "LS"
    |   condToString CondSignedGreaterEq  = "GE"
    |   condToString CondSignedLess       = "LT"
    |   condToString CondSignedGreater    = "GT"
    |   condToString CondSignedLessEq     = "LE"


    (* Condition codes to binary encoding. *)
    fun cCode CondEqual           = 0wx0: Word32.word
    |   cCode CondNotEqual        = 0wx1
    |   cCode CondCarrySet        = 0wx2 (* C=1 *)
    |   cCode CondCarryClear      = 0wx3 (* C=0 *)
    |   cCode CondNegative        = 0wx4 (* N=1 *)
    |   cCode CondPositive        = 0wx5 (* N=0 imcludes zero *)
    |   cCode CondOverflow        = 0wx6 (* V=1 *)
    |   cCode CondNoOverflow      = 0wx7 (* V=0 *)
    |   cCode CondUnsignedHigher  = 0wx8 (* C=1 && Z=0 *)
    |   cCode CondUnsignedLowOrEq = 0wx9 (* ! (C=1 && Z=0) *)
    |   cCode CondSignedGreaterEq = 0wxa (* N=V *)
    |   cCode CondSignedLess      = 0wxb (* N<>V *)
    |   cCode CondSignedGreater   = 0wxc (* Z==0 && N=V *)
    |   cCode CondSignedLessEq    = 0wxd (* !(Z==0 && N=V) *)


    (* Offsets in the assembly code interface pointed at by X26
       These are in units of 64-bits NOT bytes. *)
    val heapOverflowCallOffset  = 1
    and stackOverflowCallOffset = 2
    and stackOverflowXCallOffset= 3
    and exceptionHandlerOffset  = 5
    and stackLimitOffset        = 6
    and exceptionPacketOffset   = 7
    and threadIdOffset          = 8
    and heapLimitPtrOffset      = 42
    and heapAllocPtrOffset      = 43
    and mlStackPtrOffset        = 44

    (* 31 in the register field can either mean the zero register or
       the hardware stack pointer.  Which meaning depends on the instruction. *)
    fun xRegOrXZ(XReg w) = w
    |   xRegOrXZ XZero = 0w31
    |   xRegOrXZ XSP = raise InternalError "XSP not valid here"
    
    and xRegOrXSP(XReg w) = w
    |   xRegOrXSP XZero = raise InternalError "XZero not valid here"
    |   xRegOrXSP XSP = 0w31
    
    (* There are cases where it isn't clear. *)
    and xRegOnly (XReg w) = w
    |   xRegOnly XZero = raise InternalError "XZero not valid here"
    |   xRegOnly XSP = raise InternalError "XSP not valid here"

    val X0  = XReg 0w0  and X1  = XReg 0w1  and X2 = XReg 0w2   and X3  = XReg 0w3
    and X4  = XReg 0w4  and X5  = XReg 0w5  and X6 = XReg 0w6   and X7  = XReg 0w7
    and X8  = XReg 0w8  and X9  = XReg 0w9  and X10= XReg 0w10  and X11 = XReg 0w11
    and X12 = XReg 0w12 and X13 = XReg 0w13 and X14= XReg 0w14  and X15 = XReg 0w15
    and X16 = XReg 0w16 and X17 = XReg 0w17 and X18= XReg 0w18  and X19 = XReg 0w19
    and X20 = XReg 0w20 and X21 = XReg 0w21 and X22= XReg 0w22  and X23 = XReg 0w23
    and X24 = XReg 0w24 and X25 = XReg 0w25 and X26= XReg 0w26  and X27 = XReg 0w27
    and X28 = XReg 0w28 and X29 = XReg 0w29 and X30= XReg 0w30
    
    val X_MLHeapLimit       = X25 (* ML Heap limit pointer *)
    and X_MLAssemblyInt     = X26 (* ML assembly interface pointer. *)
    and X_MLHeapAllocPtr    = X27 (* ML Heap allocation pointer. *)
    and X_MLStackPtr        = X28 (* ML Stack pointer. *)
    and X_LinkReg           = X30 (* Link reg - return address *)
    and X_Base32in64        = X24 (* X24 is used for the heap base in 32-in-64. *)
    
    fun vReg(VReg v) = v
    (* Only the first eight registers are currently used by ML. *)
    val V0  = VReg 0w0  and V1  = VReg 0w1 and V2 = VReg 0w2   and V3  = VReg 0w3
    and V4  = VReg 0w4  and V5  = VReg 0w5 and V6 = VReg 0w6   and V7  = VReg 0w7

    (* Some data instructions include a possible shift. *)
    datatype shiftType =
        ShiftLSL of Word8.word
    |   ShiftLSR of Word8.word
    |   ShiftASR of Word8.word
    |   ShiftNone

    local
        fun checkImm6 w = if w > 0w63 then raise InternalError "shift > 63" else w
    in 
        fun shiftEncode(ShiftLSL w) = (0w0, checkImm6 w)
        |   shiftEncode(ShiftLSR w) = (0w1, checkImm6 w)
        |   shiftEncode(ShiftASR w) = (0w2, checkImm6 w)
        |   shiftEncode ShiftNone   = (0w0, 0w0)
    end

    (* Other instructions include an extension i.e. a sign- or zero-extended
       value from one of the argument registers.  When an extension is encoded
       there can also be a left shift which applies after the extension.
       I don't understand what difference, if any, there is between UXTX
       and SXTX.
       There's no ExtNone because we need to use either UXTW or UXTX depending
       on the length *)
    datatype 'a extend =
        ExtUXTB of 'a (* Unsigned extend byte *)
    |   ExtUXTH of 'a (* Unsigned extend byte *)
    |   ExtUXTW of 'a (* Unsigned extend byte *)
    |   ExtUXTX of 'a (* Left shift *)
    |   ExtSXTB of 'a (* Sign extend byte *)
    |   ExtSXTH of 'a (* Sign extend halfword *)
    |   ExtSXTW of 'a (* Sign extend word *)
    |   ExtSXTX of 'a (* Left shift *)

    (* Load/store instructions have only a single bit for the shift.  For byte
       operations this is one bit shift; for others it scales by the size of
       the operand if set. *)
    datatype scale =
        ScaleOrShift
    |   NoScale

    local
        (* Although there are three bits it seems that the shift is limited to 0 to 4. *)
        fun checkImm3 w = if w > 0w4 then raise InternalError "extend shift > 4" else w
    in
        fun extendArithEncode(ExtUXTB w) = (0w0, checkImm3 w)
        |   extendArithEncode(ExtUXTH w) = (0w1, checkImm3 w)
        |   extendArithEncode(ExtUXTW w) = (0w2, checkImm3 w)
        |   extendArithEncode(ExtUXTX w) = (0w3, checkImm3 w)
        |   extendArithEncode(ExtSXTB w) = (0w4, checkImm3 w)
        |   extendArithEncode(ExtSXTH w) = (0w5, checkImm3 w)
        |   extendArithEncode(ExtSXTW w) = (0w6, checkImm3 w)
        |   extendArithEncode(ExtSXTX w) = (0w7, checkImm3 w)
        
        fun extendLSEncode(ExtUXTB v) = (0w0, v)
        |   extendLSEncode(ExtUXTH v) = (0w1, v)
        |   extendLSEncode(ExtUXTW v) = (0w2, v)
        |   extendLSEncode(ExtUXTX v) = (0w3, v)
        |   extendLSEncode(ExtSXTB v) = (0w4, v)
        |   extendLSEncode(ExtSXTH v) = (0w5, v)
        |   extendLSEncode(ExtSXTW v) = (0w6, v)
        |   extendLSEncode(ExtSXTX v) = (0w7, v)
    end

    datatype wordSize = WordSize32 | WordSize64

    (* Bit patterns on the ARM64 are encoded using a complicated scheme and
       only certain values can be encoded.  An element can be 2, 4, 8, 16, 32 or
       64 bits and must be a sequence of at least one zero bits followed by at
       least one one bit.  This sequence can then be rotated within the element.
       Finally the element is replicated within the register up to 32 or
       64 bits.  All this information is encoded in 13 bits.
       N.B. Bit patterns of all zeros or all ones cannot be encoded. *)

    (* Encode the value if it is possible. *)
    fun encodeBitPattern(value, sf (* size flag *)) =
    (* Can't encode 0 or all ones. *)
    if value = 0w0 orelse value = Word64.notb 0w0
    then NONE
    (* If this is 32-bits we can't encode all ones in the
       low-order 32-bits or any value that won't fit in 32-bits, *)
    else if sf = WordSize32 andalso value >= 0wxffffffff
    then NONE
    else
    let
        val regSize = case sf of WordSize32 => 0w32 | WordSize64 => 0w64
        (* Get the element size.  Look for the repeat of the
           pattern. *)
        fun getElemSize size =
        let
            val ns = size div 0w2
            val mask = Word64.<<(0w1, ns)  - 0w1
        in
            if Word64.andb(value, mask) <> Word64.andb(Word64.>>(value, ns), mask)
            then size
            else if ns <= 0w2
            then ns
            else getElemSize ns
        end
        val elemSize = getElemSize regSize
        fun log2 0w1 = 0w0 | log2 n = 0w1 + log2(Word.>>(n, 0w1))
        val elemBits = log2 elemSize

        (* Find the rotation that puts as many of the zero bits in the
           element at the top. *)
        val elemMask = Word64.>>(Word64.notb 0w0, 0w64-elemSize)
        fun ror elt =
            Word64.orb((Word64.<<(Word64.andb(elt, 0w1), elemSize-0w1),
                Word64.>>(elt, 0w1)))
        and rol elt =
            Word64.orb(Word64.andb(elemMask, Word64.<<(elt, 0w1)),
                Word64.>>(elt, elemSize-0w1))

        fun findRotation(v, n) =
            if ror v < v then findRotation(ror v, (n-0w1) mod elemSize)
            else if rol v < v then findRotation(rol v, n+0w1)
            else (v, n)

        val (rotated, rotation) = findRotation(Word64.andb(value, elemMask), 0w0)

        (* Count out the low order ones.  If the result is zero
           then we;ve got a valid sequence of zeros followed by ones
           but if we discover a zero bit and the result isn't zero
           then we can't encode this. *)
        fun countLowOrderOnes(v, n) =
            if v = 0w0
            then SOME n
            else if Word64.andb(v, 0w1) = 0w1
            then countLowOrderOnes(Word64.>>(v, 0w1), n+0w1)
            else NONE
     in
        case countLowOrderOnes(rotated, 0w0) of
            NONE => NONE
        |   SOME lowOrderOnes =>
            let
                (* Encode the element size. *)
                val elemSizeEnc = 0wx7f - (Word.<<(0w1, elemBits+0w1) - 0w1)
                val n = if Word.andb(elemSizeEnc, 0wx40) = 0w0 then 0w1 else 0w0
                val imms = Word.andb(Word.orb(elemSizeEnc, lowOrderOnes-0w1), 0wx3f)
            in
                SOME{n=n, imms=imms, immr=rotation}
            end
    end;

    (* Decode a pattern for printing. *)
    fun decodeBitPattern{sf, n, immr, imms} =
    let
        (* Find the highest bit set in N:NOT(imms) *)
        fun highestBitSet 0w0 = 0
        |   highestBitSet n = 1+highestBitSet(Word32.>>(n, 0w1))
        val len = highestBitSet(Word32.orb(Word32.<<(n, 0w6), Word32.xorb(imms, 0wx3f))) - 1
        val _ = if len < 0 then raise InternalError "decodeBitPattern: invalid" else ()
        val size = Word32.<<(0w1, Word.fromInt len)
        val r = Word32.andb(immr, size-0w1)
        and s = Word32.andb(imms, size-0w1)
        val _ = if s = size-0w1 then raise InternalError "decodeBitPattern: invalid" else ()
        val pattern = Word64.<<(0w1, word32ToWord(s+0w1)) - 0w1
        (* Rotate right: shift left and put the top bit in the high order bit*)
        fun ror elt =
            Word64.orb((Word64.<<(Word64.andb(elt, 0w1), word32ToWord(size-0w1)),
                Word64.>>(elt, 0w1)))

        fun rotateBits(value, 0w0) = value
        |   rotateBits(value, n) = rotateBits(ror value, n-0w1)

        val rotated = rotateBits(pattern, r)

        val regSize = if sf = 0w0 then 0w32 else 0w64

        (* Replicate the rotated pattern to fill the register. *)
        fun replicate(pattern, size) =
            if size >= regSize
            then pattern
            else replicate(Word64.orb(pattern, Word64.<<(pattern, word32ToWord size)), size * 0w2)
    in
        replicate(rotated, size)
    end

    val isEncodableBitPattern = isSome o encodeBitPattern


    datatype instr =
        SimpleInstr of Word32.word
    |   LoadAddressLiteral of {reg: xReg, value: machineWord, length: brLength ref}
    |   LoadNonAddressLiteral of {reg: xReg, value: Word64.word, length: brLength ref}
    |   LoadFPLiteral of {reg: vReg, value: Word64.word, length: brLength ref, isDouble: bool, work: xReg}
    |   Label of labels
    |   UnconditionalBranch of {label: labels, andLink: bool}
    |   ConditionalBranch of { label: labels, jumpCondition: condition, length: brLength ref }
    |   LoadLabelAddress of { label: labels, reg: xReg, length: brLength ref }
    |   TestBitBranch of { label: labels, bitNo: Word8.word, brNonZero: bool, reg: xReg, length: brLength ref }
    |   CompareBranch of { label: labels, brNonZero: bool, size: wordSize, reg: xReg, length: brLength ref }
    
    and brLength = BrShort | BrExtended

    val nopCode  = 0wxD503201F
    and undefCode = 0wx00000000 (* Permanently undefined instruction. *)

    (* Add/subtract an optionally shifted 12-bit immediate (i.e. constant) to/from a register.
       The constant is zero-extended.  The versions that do not set the flags can use XSP as
       the destination; the versions that use the signs can use XZero as the destination i.e.
       they discard the result and act as a comparison. *)
    local
        fun addSubRegImmediate(sf, oper, s, xdOp) ({regN, regD, immed, shifted}) =
        let
            val () =
                if immed >= 0wx1000 then raise InternalError "addSubRegImmediate: immed > 12 bits" else ()
        in
            SimpleInstr(
                0wx11000000 orb (sf << 0w31) orb (oper << 0w30) orb (s << 0w29) orb
                (if shifted then 0wx400000 else 0w0) orb
                (wordToWord32 immed << 0w10) orb (word8ToWord32(xRegOrXSP regN) << 0w5) orb
                word8ToWord32(xdOp regD))
        end
    in
        val addImmediate = addSubRegImmediate(0w1, 0w0, 0w0, xRegOrXSP)
        and addSImmediate = addSubRegImmediate(0w1, 0w0, 0w1, xRegOrXZ)
        and subImmediate = addSubRegImmediate(0w1, 0w1, 0w0, xRegOrXSP)
        and subSImmediate = addSubRegImmediate(0w1, 0w1, 0w1, xRegOrXZ)
        and addImmediate32 = addSubRegImmediate(0w0, 0w0, 0w0, xRegOrXSP)
        and addSImmediate32 = addSubRegImmediate(0w0, 0w0, 0w1, xRegOrXZ)
        and subImmediate32 = addSubRegImmediate(0w0, 0w1, 0w0, xRegOrXSP)
        and subSImmediate32 = addSubRegImmediate(0w0, 0w1, 0w1, xRegOrXZ)
    end

    (* Add/subtract a shifted register, optionally setting the flags. *)
    local
        (* X31 is XZ here unlike the extended version.*)
        fun addSubtractShiftedReg (sf, oper, s) ({regM, regN, regD, shift}) =
        let
            val (shift, imm6) = shiftEncode shift
        in
            SimpleInstr(0wx0b000000 orb (sf << 0w31) orb (oper << 0w30) orb (s << 0w29) orb
                (shift << 0w22) orb (word8ToWord32(xRegOnly regM) << 0w16) orb
                (word8ToWord32 imm6 << 0w10) orb (word8ToWord32(xRegOrXZ regN) << 0w5) orb
                word8ToWord32(xRegOrXZ regD))
        end
    in
        val addShiftedReg = addSubtractShiftedReg(0w1, 0w0, 0w0)
        and addSShiftedReg = addSubtractShiftedReg(0w1, 0w0, 0w1)
        and subShiftedReg = addSubtractShiftedReg(0w1, 0w1, 0w0)
        and subSShiftedReg = addSubtractShiftedReg(0w1, 0w1, 0w1)
        and addShiftedReg32 = addSubtractShiftedReg(0w0, 0w0, 0w0)
        and addSShiftedReg32 = addSubtractShiftedReg(0w0, 0w0, 0w1)
        and subShiftedReg32 = addSubtractShiftedReg(0w0, 0w1, 0w0)
        and subSShiftedReg32 = addSubtractShiftedReg(0w0, 0w1, 0w1)
    end

    (* Add/subtract an extended register, optionally setting the flags. *)
    local
        (* SP can be used as Xn and also for Xd for the non-flags versions. *)
        fun addSubtractExtendedReg (sf, oper, s, opt, xD) ({regM, regN, regD, extend}) =
        let
            val (option, imm3) = extendArithEncode extend
        in
            SimpleInstr(0wx0b200000 orb (sf << 0w31) orb (oper << 0w30) orb (s << 0w29) orb
                (opt << 0w22) orb (word8ToWord32(xRegOnly regM) << 0w16) orb
                (option << 0w13) orb (word8ToWord32 imm3 << 0w10) orb
                (word8ToWord32(xRegOrXSP regN) << 0w5) orb
                word8ToWord32(xD regD))
        end
    in
        val addExtendedReg = addSubtractExtendedReg(0w1, 0w0, 0w0, 0w0, xRegOrXSP)
        and addSExtendedReg = addSubtractExtendedReg(0w1, 0w0, 0w1, 0w0, xRegOrXZ)
        and subExtendedReg = addSubtractExtendedReg(0w1, 0w1, 0w0, 0w0, xRegOrXSP)
        and subSExtendedReg = addSubtractExtendedReg(0w1, 0w1, 0w1, 0w0, xRegOrXZ)
    end

    (* Logical operations on a shifted register. *)
    local
        fun logicalShiftedReg (sf, oper, n) ({regM, regN, regD, shift}) =
        let
            val (shift, imm6) = shiftEncode shift
        in
            SimpleInstr(0wx0a000000 orb (sf << 0w31) orb (oper << 0w29) orb
                (shift << 0w22) orb (n << 0w21) orb (word8ToWord32(xRegOrXZ regM) << 0w16) orb
                (word8ToWord32 imm6 << 0w10) orb (word8ToWord32(xRegOrXZ regN) << 0w5) orb
                word8ToWord32(xRegOrXZ regD))
        end
    in
        val andShiftedReg = logicalShiftedReg(0w1, 0w0, 0w0)
        and orrShiftedReg = logicalShiftedReg(0w1, 0w1, 0w0)
        and eorShiftedReg = logicalShiftedReg(0w1, 0w2, 0w0)
        and andsShiftedReg = logicalShiftedReg(0w1, 0w3, 0w0)
        val andShiftedReg32 = logicalShiftedReg(0w0, 0w0, 0w0)
        and orrShiftedReg32 = logicalShiftedReg(0w0, 0w1, 0w0)
        and eorShiftedReg32 = logicalShiftedReg(0w0, 0w2, 0w0)
        and andsShiftedReg32 = logicalShiftedReg(0w0, 0w3, 0w0)
        (* There are also versions that operate with an inverted version
           of the argument. *)
    end

    (* Single-source operations. *)
    local
        fun oneSourceInstr (sf, s, opcode2, opcode) {regN, regD} =
            SimpleInstr(0wx5ac00000 orb (sf << 0w31) orb (s << 0w29) orb
                (opcode2 << 0w16) orb (opcode << 0w10) orb
                (word8ToWord32(xRegOnly regN) << 0w5) orb
                word8ToWord32(xRegOnly regD))
    in
        val countLeadingZeros   = oneSourceInstr(0w1, 0w0, 0w0, 0w4)
        and countLeadingZeros32 = oneSourceInstr(0w0, 0w0, 0w0, 0w4)
    end

    (* Two-source operations. *)
    local
        fun twoSourceInstr (sf, s, opcode) ({regM, regN, regD}) =
            SimpleInstr(0wx1ac00000 orb (sf << 0w31) orb (s << 0w29) orb
                (word8ToWord32(xRegOnly regM) << 0w16) orb (opcode << 0w10) orb
                (word8ToWord32(xRegOnly regN) << 0w5) orb
                word8ToWord32(xRegOnly regD))
    in
        (* Signed and unsigned division. *)
        val unsignedDivide   = twoSourceInstr(0w1, 0w0, 0wx2)
        and signedDivide     = twoSourceInstr(0w1, 0w0, 0wx3)
        and unsignedDivide32 = twoSourceInstr(0w0, 0w0, 0wx2)
        and signedDivide32   = twoSourceInstr(0w0, 0w0, 0wx3)
        (* Logical shift left Rd = Rn << (Rm mod 0w64) *)
        and logicalShiftLeftVariable = twoSourceInstr(0w1, 0w0, 0wx8)
        (* Logical shift right Rd = Rn >> (Rm mod 0w64) *)
        and logicalShiftRightVariable = twoSourceInstr(0w1, 0w0, 0wx9)
        (* Arithmetic shift right Rd = Rn ~>> (Rm mod 0w64) *)
        and arithmeticShiftRightVariable = twoSourceInstr(0w1, 0w0, 0wxa)
        and logicalShiftLeftVariable32 = twoSourceInstr(0w0, 0w0, 0wx8)
        and logicalShiftRightVariable32 = twoSourceInstr(0w0, 0w0, 0wx9)
        and arithmeticShiftRightVariable32 = twoSourceInstr(0w0, 0w0, 0wxa)
    end

    (* Three source operations.  These are all variations of multiply. *)
    local
        fun threeSourceInstr (sf, op54, op31, o0) ({regM, regA, regN, regD}) =
            SimpleInstr(0wx1b000000 orb (sf << 0w31) orb (op54 << 0w29) orb
                (op31 << 0w21) orb (word8ToWord32(xRegOnly regM) << 0w16) orb
                (o0 << 0w15) orb (word8ToWord32(xRegOrXZ regA) << 0w10) orb
                (word8ToWord32(xRegOnly regN) << 0w5) orb
                word8ToWord32(xRegOnly regD))
    in
        (* regD = regA + regN * regM *)
        val multiplyAndAdd = threeSourceInstr(0w1, 0w0, 0w0, 0w0)
        (* regD = regA - regN * regM *)
        and multiplyAndSub = threeSourceInstr(0w1, 0w0, 0w0, 0w1)
        and multiplyAndAdd32 = threeSourceInstr(0w0, 0w0, 0w0, 0w0)
        and multiplyAndSub32 = threeSourceInstr(0w0, 0w0, 0w0, 0w1)
        (* Multiply two 32-bit quantities and add/subtract a 64-bit quantity. *)
        and signedMultiplyAndAddLong = threeSourceInstr(0w1, 0w0, 0w1, 0w0)
        and signedMultiplyAndSubLong = threeSourceInstr(0w1, 0w0, 0w1, 0w1)
        (* Return the high-order part of a signed multiplication. *)
        fun signedMultiplyHigh({regM, regN, regD}) =
            threeSourceInstr(0w1, 0w0, 0w2, 0w0) { regM=regM, regN=regN, regD=regD, regA=XZero}
    end

    (* Loads: There are two versions of this on the ARM.  There is a version that
       takes a signed 9-bit byte offset and a version that takes an unsigned
       12-bit word offset. *)
    
    local
        fun loadStoreRegScaled (size, v, opc, xD) ({regT, regN, unitOffset}) =
        let
            val _ = (unitOffset >= 0 andalso unitOffset < 0x1000)
                orelse raise InternalError "loadStoreRegScaled: value out of range"
        in
            SimpleInstr(0wx39000000 orb (size << 0w30) orb (opc << 0w22) orb
                (v << 0w26) orb (Word32.fromInt unitOffset << 0w10) orb
                (word8ToWord32(xRegOrXSP regN) << 0w5) orb word8ToWord32(xD regT))
        end
    in
        val loadRegScaled = loadStoreRegScaled(0w3, 0w0, 0w1, xRegOrXZ)
        and storeRegScaled = loadStoreRegScaled(0w3, 0w0, 0w0, xRegOrXZ)
        (* (Unsigned) byte operations.  There are also signed versions. *)
        and loadRegScaledByte = loadStoreRegScaled (0w0, 0w0, 0w1, xRegOrXZ)
        and storeRegScaledByte = loadStoreRegScaled (0w0, 0w0, 0w0, xRegOrXZ)
        and loadRegScaled16 = loadStoreRegScaled (0w1, 0w0, 0w1, xRegOrXZ)
        and storeRegScaled16 = loadStoreRegScaled (0w1, 0w0, 0w0, xRegOrXZ)
        and loadRegScaled32 = loadStoreRegScaled (0w2, 0w0, 0w1, xRegOrXZ)
        and storeRegScaled32 = loadStoreRegScaled (0w2, 0w0, 0w0, xRegOrXZ)
        and loadRegScaledDouble = loadStoreRegScaled(0w3, 0w1, 0w1, vReg)
        and storeRegScaledDouble = loadStoreRegScaled(0w3, 0w1, 0w0, vReg)
        and loadRegScaledFloat = loadStoreRegScaled(0w2, 0w1, 0w1, vReg)
        and storeRegScaledFloat = loadStoreRegScaled(0w2, 0w1, 0w0, vReg)
    end    

    local
        (* Loads and stores with a signed byte offset.  This includes simple
           unscaled addresses, pre-indexing and post-indexing. *)
        fun loadStoreByteAddress (op4, xD) (size, v, opc) ({regT, regN, byteOffset}) =
        let
            val _ = (byteOffset >= ~256 andalso byteOffset < 256)
                orelse raise InternalError "loadStoreUnscaled: value out of range"
            val imm9 = Word32.fromInt byteOffset andb 0wx1ff
        in
            SimpleInstr(0wx38000000 orb (size << 0w30) orb (opc << 0w22) orb
                (v << 0w26) orb (imm9 << 0w12) orb (op4 << 0w10) orb
                (word8ToWord32(xRegOrXSP regN) << 0w5) orb word8ToWord32(xD regT))
        end
        
        val loadStoreUnscaled = loadStoreByteAddress (0w0, xRegOrXZ)
        and loadStoreUnscaledSIMD = loadStoreByteAddress (0w0, vReg)
        and loadStorePostIndex = loadStoreByteAddress (0w1, xRegOrXZ)
        and loadStorePreIndex = loadStoreByteAddress (0w3, xRegOrXZ)
    in
        val loadRegUnscaled = loadStoreUnscaled (0w3, 0w0, 0w1)
        and storeRegUnscaled = loadStoreUnscaled (0w3, 0w0, 0w0)
        (* (Unsigned) byte operations.  There are also signed versions. *)
        and loadRegUnscaledByte = loadStoreUnscaled (0w0, 0w0, 0w1)
        and loadRegUnscaledSignedByteTo64 = loadStoreUnscaled (0w0, 0w0, 0w2)
        and loadRegUnscaledSignedByteTo32 = loadStoreUnscaled (0w0, 0w0, 0w3)
        and storeRegUnscaledByte = loadStoreUnscaled (0w0, 0w0, 0w0)
        and loadRegUnscaled16 = loadStoreUnscaled (0w1, 0w0, 0w1)
        and loadRegUnscaledSigned16To64 = loadStoreUnscaled (0w1, 0w0, 0w2)
        and loadRegUnscaledSigned16To32 = loadStoreUnscaled (0w1, 0w0, 0w3)
        and storeRegUnscaled16 = loadStoreUnscaled (0w1, 0w0, 0w0)
        and loadRegUnscaled32 = loadStoreUnscaled (0w2, 0w0, 0w1)
        and loadRegUnscaledSigned32To64 = loadStoreUnscaled (0w2, 0w0, 0w2)
        and storeRegUnscaled32 = loadStoreUnscaled (0w2, 0w0, 0w0)
        and loadRegUnscaledFloat = loadStoreUnscaledSIMD (0w2, 0w1, 0w1)
        and storeRegUnscaledFloat = loadStoreUnscaledSIMD (0w2, 0w1, 0w0)
        and loadRegUnscaledDouble = loadStoreUnscaledSIMD (0w3, 0w1, 0w1)
        and storeRegUnscaledDouble = loadStoreUnscaledSIMD (0w3, 0w1, 0w0)

        val loadRegPostIndex = loadStorePostIndex (0w3, 0w0, 0w1)
        and storeRegPostIndex = loadStorePostIndex (0w3, 0w0, 0w0)
        and loadRegPostIndex32 = loadStorePostIndex (0w2, 0w0, 0w1)
        and storeRegPostIndex32 = loadStorePostIndex (0w2, 0w0, 0w0)
        and loadRegPostIndexByte = loadStorePostIndex (0w0, 0w0, 0w1)
        and storeRegPostIndexByte = loadStorePostIndex (0w0, 0w0, 0w0)

        val loadRegPreIndex = loadStorePreIndex (0w3, 0w0, 0w1)
        and storeRegPreIndex = loadStorePreIndex (0w3, 0w0, 0w0)
        and loadRegPreIndex32 = loadStorePreIndex (0w2, 0w0, 0w1)
        and storeRegPreIndex32 = loadStorePreIndex (0w2, 0w0, 0w0)
        and loadRegPreIndexByte = loadStorePreIndex (0w0, 0w0, 0w1)
        and storeRegPreIndexByte = loadStorePreIndex (0w0, 0w0, 0w0)
    end

    (* Load/store with a register offset i.e. an index register. *)
    local
        fun loadStoreRegRegisterOffset (size, v, opc, xD) ({regT, regN, regM, option}) =
        let
            val (opt, s) =
                case extendLSEncode option of
                    (opt, ScaleOrShift) => (opt, 0w1) | (opt, NoScale) => (opt, 0w0)
        in
            SimpleInstr(0wx38200800 orb (size << 0w30) orb (v << 0w26) orb (opc << 0w22) orb
                (word8ToWord32(xRegOnly regM) << 0w16) orb (opt << 0w13) orb (s << 0w12) orb
                (word8ToWord32(xRegOrXSP regN) << 0w5) orb word8ToWord32(xD regT))
        end
    in
        val loadRegIndexed = loadStoreRegRegisterOffset(0w3, 0w0, 0w1, xRegOrXZ)
        and storeRegIndexed = loadStoreRegRegisterOffset(0w3, 0w0, 0w0, xRegOrXZ)
        and loadRegIndexedByte = loadStoreRegRegisterOffset(0w0, 0w0, 0w1, xRegOrXZ)
        and storeRegIndexedByte = loadStoreRegRegisterOffset(0w0, 0w0, 0w0, xRegOrXZ)
        and loadRegIndexed16 = loadStoreRegRegisterOffset(0w1, 0w0, 0w1, xRegOrXZ)
        and storeRegIndexed16 = loadStoreRegRegisterOffset(0w1, 0w0, 0w0, xRegOrXZ)
        and loadRegIndexed32 = loadStoreRegRegisterOffset(0w2, 0w0, 0w1, xRegOrXZ)
        and storeRegIndexed32 = loadStoreRegRegisterOffset(0w2, 0w0, 0w0, xRegOrXZ)
        and loadRegIndexedFloat = loadStoreRegRegisterOffset(0w2, 0w1, 0w1, vReg)
        and storeRegIndexedFloat = loadStoreRegRegisterOffset(0w2, 0w1, 0w0, vReg)
        and loadRegIndexedDouble = loadStoreRegRegisterOffset(0w3, 0w1, 0w1, vReg)
        and storeRegIndexedDouble = loadStoreRegRegisterOffset(0w3, 0w1, 0w0, vReg)
    end

    local
        (* Loads and stores with special ordering. *)
        fun loadStoreExclusive(size, o2, l, o1, o0) {regS, regT2, regN, regT} =
            SimpleInstr(0wx08000000 orb (size << 0w30) orb (o2 << 0w23) orb (l << 0w22) orb
            
                (o1 << 0w21) orb (word8ToWord32(xRegOrXZ regS) << 0w16) orb (o0 << 0w15) orb
                (word8ToWord32(xRegOrXZ regT2) << 0w10) orb (word8ToWord32(xRegOrXSP regN) << 0w5) orb
                 word8ToWord32(xRegOrXZ regT))
    in
        fun loadAcquire{regN, regT} =
            loadStoreExclusive(0w3, 0w1, 0w1, 0w0, 0w1) {regS=XZero, regT2=XZero, regN=regN, regT=regT}
        and storeRelease{regN, regT} =
            loadStoreExclusive(0w3, 0w1, 0w0, 0w0, 0w1) {regS=XZero, regT2=XZero, regN=regN, regT=regT}
        and loadAcquire32{regN, regT} =
            loadStoreExclusive(0w2, 0w1, 0w1, 0w0, 0w1) {regS=XZero, regT2=XZero, regN=regN, regT=regT}
        and storeRelease32{regN, regT} =
            loadStoreExclusive(0w2, 0w1, 0w0, 0w0, 0w1) {regS=XZero, regT2=XZero, regN=regN, regT=regT}
        and loadAcquireByte{regN, regT} =
            loadStoreExclusive(0w0, 0w1, 0w1, 0w0, 0w1) {regS=XZero, regT2=XZero, regN=regN, regT=regT}
        and storeReleaseByte{regN, regT} =
            loadStoreExclusive(0w0, 0w1, 0w0, 0w0, 0w1) {regS=XZero, regT2=XZero, regN=regN, regT=regT}

        (* Acquire exclusive access to a memory location and load its current value *)
        and loadAcquireExclusiveRegister{regN, regT} =
            loadStoreExclusive(0w3, 0w0, 0w1, 0w0, 0w1) {regS=XZero, regT2=XZero, regN=regN, regT=regT}
        (* Release exclusive access and test whether it succeeded.  Sets regS to 0
           if successful otherwise 1, in which case we have to repeat the operation. *)
        and storeReleaseExclusiveRegister{regN, regS, regT} =
            loadStoreExclusive(0w3, 0w0, 0w0, 0w0, 0w1) {regS=regS, regT2=XZero, regN=regN, regT=regT}
    end

    local
        (* Load and store pairs.  The offsets are signed scaled values. *)
        fun loadStorePair op2 (opc, v, l, rT) {regT1, regT2, regN, unitOffset} =
        let
            val _ = (unitOffset >= ~64 andalso unitOffset < 64)
                orelse raise InternalError "loadStorePair: value out of range"
            val imm7 = Word32.fromInt unitOffset andb 0wx7f
        in
           SimpleInstr(0wx28000000 orb (opc << 0w30) orb (v << 0w26) orb (op2 << 0w23) orb
            (l << 0w22) orb (imm7 << 0w15) orb (word8ToWord32(rT regT2) << 0w10) orb
            (word8ToWord32(xRegOrXSP regN) << 0w5) orb word8ToWord32(rT regT1))
        end
        
        fun loadStorePairOffset args = loadStorePair 0w2 args
        and loadStorePairPostIndexed args = loadStorePair 0w1 args
        and loadStorePairPreIndexed args = loadStorePair 0w3 args
    in
        val storePairOffset = loadStorePairOffset(0w2, 0w0, 0w0, xRegOrXZ)
        and loadPairOffset =  loadStorePairOffset(0w2, 0w0, 0w1, xRegOrXZ)
        and storePairPostIndexed = loadStorePairPostIndexed(0w2, 0w0, 0w0, xRegOrXZ)
        and loadPairPostIndexed =  loadStorePairPostIndexed(0w2, 0w0, 0w1, xRegOrXZ)
        and storePairPreIndexed = loadStorePairPreIndexed(0w2, 0w0, 0w0, xRegOrXZ)
        and loadPairPreIndexed =  loadStorePairPreIndexed(0w2, 0w0, 0w1, xRegOrXZ)
        
        and storePairOffset32 = loadStorePairOffset(0w0, 0w0, 0w0, xRegOrXZ)
        and loadPairOffset32 =  loadStorePairOffset(0w0, 0w0, 0w1, xRegOrXZ)
        and storePairPostIndexed32 = loadStorePairPostIndexed(0w0, 0w0, 0w0, xRegOrXZ)
        and loadPairPostIndexed32 =  loadStorePairPostIndexed(0w0, 0w0, 0w1, xRegOrXZ)
        and storePairPreIndexed32 = loadStorePairPreIndexed(0w0, 0w0, 0w0, xRegOrXZ)
        and loadPairPreIndexed32 =  loadStorePairPreIndexed(0w0, 0w0, 0w1, xRegOrXZ)

        and storePairOffsetFloat = loadStorePairOffset(0w0, 0w1, 0w0, vReg)
        and loadPairOffsetFloat = loadStorePairOffset(0w0, 0w1, 0w1, vReg)
        and storePairPostIndexedFloat = loadStorePairPostIndexed(0w0, 0w1, 0w0, vReg)
        and loadPairPostIndexedFloat = loadStorePairPostIndexed(0w0, 0w1, 0w1, vReg)
        and storePairPreIndexedFloat = loadStorePairPreIndexed(0w0, 0w1, 0w0, vReg)
        and loadPairPreIndexedFloat = loadStorePairPreIndexed(0w0, 0w1, 0w1, vReg)

        and storePairOffsetDouble = loadStorePairOffset(0w0, 0w1, 0w0, vReg)
        and loadPairOffsetDouble = loadStorePairOffset(0w0, 0w1, 0w1, vReg)
        and storePairPostIndexedDouble = loadStorePairPostIndexed(0w1, 0w1, 0w0, vReg)
        and loadPairPostIndexedDouble = loadStorePairPostIndexed(0w1, 0w1, 0w1, vReg)
        and storePairPreIndexedDouble = loadStorePairPreIndexed(0w1, 0w1, 0w0, vReg)
        and loadPairPreIndexedDouble = loadStorePairPreIndexed(0w1, 0w1, 0w1, vReg)
    end

    (* Addresses must go in the constant area at the end of the code where they
       can be found by the GC. *)
    fun loadAddressConstant(xReg, valu) =
        LoadAddressLiteral{reg=xReg, value=valu, length=ref BrExtended}

    (* Non-address constants.  These may or may not be tagged values. *)
    fun loadNonAddressConstant(xReg, valu) =
        LoadNonAddressLiteral{reg=xReg, value=valu, length=ref BrExtended}

    (* Floating point constants.  TODO: We can use fmov dn,c for various  constant values. *)
    local
        (* Use the same instruction for both float and double. *)
        fun moviZero regD = SimpleInstr(0wx2F00E400 orb word8ToWord32(vReg regD))
    in
        fun loadFloatConstant(vReg, 0w0, _) = moviZero vReg
    
        |   loadFloatConstant(vReg, valu, work) =
                LoadFPLiteral{reg=vReg, value=valu, isDouble=false, length=ref BrExtended, work=work}

        and loadDoubleConstant(vReg, 0w0, _) = moviZero vReg

        |   loadDoubleConstant(vReg, valu, work) =
                LoadFPLiteral{reg=vReg, value=valu, isDouble=true, length=ref BrExtended, work=work}
    end

    local
        fun moveWideImmediate(sf, opc) {regD, immediate, shift} =
        let
            val hw =
                case (shift, sf) of
                    (0w0, _) => 0w0
                |   (0w16, _) => 0w1
                |   (0w24, 0w1) => 0w2
                |   (0w48, 0w1) => 0w3
                |   _ => raise InternalError "moveWideImmediate: invalid shift"
            val _ =
                immediate <= 0wxffff orelse raise InternalError "moveWideImmediate: immediate too large"
        in
            SimpleInstr(0wx12800000 orb (sf << 0w31) orb (opc << 0w29) orb
                (hw << 0w21) orb (wordToWord32 immediate << 0w5) orb word8ToWord32(xRegOnly regD))
        end
    in
        val moveNot32 = moveWideImmediate(0w0, 0w0)
        and moveZero32 = moveWideImmediate(0w0, 0w2)
        and moveKeep32 = moveWideImmediate(0w0, 0w3)
        and moveNot = moveWideImmediate(0w1, 0w0)
        and moveZero = moveWideImmediate(0w1, 0w2)
        and moveKeep = moveWideImmediate(0w1, 0w3)
    end

    (* Instructions involved in thread synchonisation. *)
    val yield = SimpleInstr 0wxD503203F (* Yield inside a spin-lock. *)
    and dmbIsh = SimpleInstr 0wxD5033BBF (* Memory barrier. *)
    
    (* Jump to the address in the register and put the address of the
       next instruction into X30. *)
    fun branchAndLinkReg(dest) =
        SimpleInstr(0wxD63F0000 orb (word8ToWord32(xRegOnly dest) << 0w5))

    (* Jump to the address in the register. *)
    fun branchRegister(dest) =
        SimpleInstr(0wxD61F0000 orb (word8ToWord32(xRegOnly dest) << 0w5))

    (* Jump to the address in the register and hint this is a return. *)
    fun returnRegister(dest) =
        SimpleInstr(0wxD65F0000 orb (word8ToWord32(xRegOnly dest) << 0w5))

    (* Put a label into the code. *)
    val setLabel = Label

    (* Create a label. *)
    fun createLabel () = ref [ref 0w0]

    (* A conditional or unconditional branch. *)
    and conditionalBranch(cond, label) = ConditionalBranch{label=label, jumpCondition=cond, length=ref BrExtended }
    and unconditionalBranch label = UnconditionalBranch{label=label, andLink=false}
    and branchAndLink label = UnconditionalBranch{label=label, andLink=true}
    (* Put the address of a label into a register - used for handlers and cases. *)
    and loadLabelAddress(reg, label) = LoadLabelAddress{label=label, reg=reg, length=ref BrExtended}
    (* Test a bit in a register and branch if zero/nonzero *)
    and testBitBranchZero(reg, bit, label) =
        TestBitBranch{label=label, bitNo=bit, brNonZero=false, reg=reg, length=ref BrExtended}
    and testBitBranchNonZero(reg, bit, label) =
        TestBitBranch{label=label, bitNo=bit, brNonZero=true, reg=reg, length=ref BrExtended}
    (* Compare a register with zero and branch if zero/nonzero *)
    and compareBranchZero(reg,  label) =
        CompareBranch{label=label, brNonZero=false, size=WordSize64, reg=reg, length=ref BrExtended}
    and compareBranchNonZero(reg, label) =
        CompareBranch{label=label, brNonZero=true, size=WordSize64, reg=reg, length=ref BrExtended}
    and compareBranchZero32(reg, label) =
        CompareBranch{label=label, brNonZero=false, size=WordSize32, reg=reg, length=ref BrExtended}
    and compareBranchNonZero32(reg, label) =
        CompareBranch{label=label, brNonZero=true, size=WordSize32, reg=reg, length=ref BrExtended}
    

    (* Set the destination register to the value of the first reg if the
       condition is true otherwise to a, possibly modified, version of
       the second argument.  There are variants that set it unmodified,
       incremented, inverted and negated. *)
    local
        fun conditionalSelect (sf, opc, op2) {regD, regFalse, regTrue, cond} =
            SimpleInstr(0wx1A800000 orb (sf << 0w31) orb (opc << 0w30) orb
                (word8ToWord32(xRegOrXZ regFalse) << 0w16) orb (cCode cond << 0w12) orb
                (op2 << 0w10) orb (word8ToWord32(xRegOrXZ regTrue) << 0w5) orb
                word8ToWord32(xRegOrXZ regD))
    in
        val conditionalSet = conditionalSelect(0w1, 0w0, 0w0)
        and conditionalSetIncrement = conditionalSelect(0w1, 0w0, 0w1)
        and conditionalSetInverted = conditionalSelect(0w1, 0w1, 0w0)
        and conditionalSetNegated = conditionalSelect(0w1, 0w1, 0w1)
        and conditionalSet32 = conditionalSelect(0w0, 0w0, 0w0)
        and conditionalSetIncrement32 = conditionalSelect(0w0, 0w0, 0w1)
        and conditionalSetInverted32 = conditionalSelect(0w0, 0w1, 0w0)
        and conditionalSetNegated32 = conditionalSelect(0w0, 0w1, 0w1)
    end

    (* This combines the effect of a left and right shift.  There are various
       derived forms of this depending on the relative values of immr and imms.
       if imms >= immr copies imms-immr-1 bits from bit position immr to the lsb
       bits of the destination.
       if imms < immr copies imms+1 bits from the lsb bit to bit position
       regsize-immr.
       How the remaining bits are affected depends on the instruction.
       BitField instructions do not affect other bits.
       UnsignedBitField instructions zero other bits.
       SignedBitField instructions set the high order bits to a copy of
       the high order bit copied and zero the low order bits. *)
    local
        fun bitfield (sf, opc, n) {immr, imms, regN, regD} =
            SimpleInstr(0wx13000000 orb (sf << 0w31) orb (opc << 0w29) orb (n << 0w22) orb
                (wordToWord32 immr << 0w16) orb (wordToWord32 imms << 0w10) orb (word8ToWord32(xRegOrXZ regN) << 0w5) orb
                word8ToWord32(xRegOrXZ regD))

    in
        val signedBitfieldMove32 = bitfield(0w0, 0w0, 0w0)
        and bitfieldMove32 = bitfield(0w0, 0w1, 0w0)
        and unsignedBitfieldMove32 = bitfield(0w0, 0w2, 0w0)
        and signedBitfieldMove64 = bitfield(0w1, 0w0, 0w1)
        and bitfieldMove64 = bitfield(0w1, 0w1, 0w1)
        and unsignedBitfieldMove64 = bitfield(0w1, 0w2, 0w1)

        (* Derived forms. *)
        fun logicalShiftLeft{shift, regN, regD} =
                unsignedBitfieldMove64{immr=Word.~ shift mod 0w64,
                    imms=0w64-0w1-shift, regN=regN, regD=regD}
        and logicalShiftLeft32{shift, regN, regD} =
                unsignedBitfieldMove32{immr=Word.~ shift mod 0w32,
                    imms=0w32-0w1-shift, regN=regN, regD=regD}

        and logicalShiftRight{shift, regN, regD} =
                unsignedBitfieldMove64{immr=shift, imms=0wx3f, regN=regN, regD=regD}
        and logicalShiftRight32{shift, regN, regD} =
                unsignedBitfieldMove32{immr=shift, imms=0wx1f, regN=regN, regD=regD}

        and unsignedBitfieldInsertinZeros{lsb, width, regN, regD} =
                unsignedBitfieldMove64{immr=Word.~ lsb mod 0w64,
                    imms=width-0w1, regN=regN, regD=regD}
        and unsignedBitfieldInsertinZeros32{lsb, width, regN, regD} =
                unsignedBitfieldMove32{immr=Word.~ lsb mod 0w32,
                    imms=width-0w1, regN=regN, regD=regD}

        and arithmeticShiftRight{shift, regN, regD} =
                signedBitfieldMove64{immr=shift, imms=0wx3f, regN=regN, regD=regD}
        and arithmeticShiftRight32{shift, regN, regD} =
                signedBitfieldMove32{immr=shift, imms=0wx1f, regN=regN, regD=regD}
        and signedBitfieldExtract{lsb, width, regN, regD} =
                signedBitfieldMove64{immr=lsb, imms=lsb+width-0w1, regN=regN, regD=regD}

        and bitfieldInsert{lsb, width, regN, regD} =
                bitfieldMove64{immr=Word.~ lsb mod 0w64, imms=width-0w1, regN=regN, regD=regD}
        and bitfieldInsert32{lsb, width, regN, regD} =
                bitfieldMove32{immr=Word.~ lsb mod 0w32, imms=width-0w1, regN=regN, regD=regD}
    end

    local
        (* Logical immediates.  AND, OR, XOR and ANDS.  Assumes that the immediate value
           has already been checked as valid.  The non-flags versions can use SP as the
           destination. *)
        fun logicalImmediate (s, opc, xD) {bits, regN, regD} =
        let
            val {n, imms, immr} = 
                case encodeBitPattern(bits, if s = 0w0 then WordSize32 else WordSize64) of
                    NONE => raise InternalError "testBitPattern: unable to encode bit pattern"
                |   SOME res => res
        in
            SimpleInstr(0wx12000000 orb (opc << 0w29) orb (s << 0w31) orb (n << 0w22) orb
                (wordToWord32 immr << 0w16) orb (wordToWord32 imms << 0w10) orb (word8ToWord32(xRegOrXZ regN) << 0w5) orb
                word8ToWord32(xD regD))
        end
    in
        val bitwiseAndImmediate = logicalImmediate (0w1, 0w0, xRegOrXSP)
        and bitwiseOrImmediate = logicalImmediate (0w1, 0w1, xRegOrXSP)
        and bitwiseXorImmediate = logicalImmediate (0w1, 0w2, xRegOrXSP)
        and bitwiseAndSImmediate = logicalImmediate (0w1, 0w3, xRegOrXZ)
        and bitwiseAndImmediate32 = logicalImmediate (0w0, 0w0, xRegOrXSP)
        and bitwiseOrImmediate32 = logicalImmediate (0w0, 0w1, xRegOrXSP)
        and bitwiseXorImmediate32 = logicalImmediate (0w0, 0w2, xRegOrXSP)
        and bitwiseAndSImmediate32 = logicalImmediate (0w0, 0w3, xRegOrXZ)
    end

    local
        (* Floating point operations - 2 source *)
        fun floatingPoint2Source (pt, opc) {regM, regN, regD} =
            SimpleInstr(0wx1E200800 orb (pt << 0w22) orb (word8ToWord32(vReg regM) << 0w16) orb
                (opc << 0w12) orb (word8ToWord32(vReg regN) << 0w5) orb word8ToWord32(vReg regD))
    in
        val multiplyFloat = floatingPoint2Source(0w0, 0wx0)
        and divideFloat = floatingPoint2Source(0w0, 0wx1)
        and addFloat = floatingPoint2Source(0w0, 0wx2)
        and subtractFloat = floatingPoint2Source(0w0, 0wx3)
        and multiplyDouble = floatingPoint2Source(0w1, 0wx0)
        and divideDouble = floatingPoint2Source(0w1, 0wx1)
        and addDouble = floatingPoint2Source(0w1, 0wx2)
        and subtractDouble = floatingPoint2Source(0w1, 0wx3)
    end

    local
        (* Move between a floating point and a general register with or without conversion. *)
        fun fmoveGeneral (sf, s, ptype, mode, opcode, rN, rD) {regN, regD} =
            SimpleInstr(0wx1E200000 orb (sf << 0w31) orb (s << 0w29) orb (ptype << 0w22) orb
                (mode << 0w19) orb (opcode << 0w16) orb
                (word8ToWord32(rN regN) << 0w5) orb word8ToWord32(rD regD))
        open IEEEReal
    in
        (* Moves without conversion *)
        val moveGeneralToFloat = fmoveGeneral(0w0, 0w0, 0w0, 0w0, 0w7, xRegOrXZ, vReg)
        and moveFloatToGeneral = fmoveGeneral(0w0, 0w0, 0w0, 0w0, 0w6, vReg, xRegOnly)
        and moveGeneralToDouble = fmoveGeneral(0w1, 0w0, 0w1, 0w0, 0w7, xRegOrXZ, vReg)
        and moveDoubleToGeneral = fmoveGeneral(0w1, 0w0, 0w1, 0w0, 0w6, vReg, xRegOnly)
        (* Moves with conversion - signed.  The argument is a 64-bit value. *)
        and convertIntToFloat = fmoveGeneral(0w1, 0w0, 0w0, 0w0, 0w2, xRegOrXZ, vReg)
        and convertIntToDouble = fmoveGeneral(0w1, 0w0, 0w1, 0w0, 0w2, xRegOrXZ, vReg)
        and convertInt32ToFloat = fmoveGeneral(0w0, 0w0, 0w0, 0w0, 0w2, xRegOrXZ, vReg)
        and convertInt32ToDouble = fmoveGeneral(0w0, 0w0, 0w1, 0w0, 0w2, xRegOrXZ, vReg)

        fun convertFloatToInt TO_NEAREST =
                fmoveGeneral(0w1, 0w0, 0w0, 0w0, 0w4, vReg, xRegOnly) (* fcvtas *)
        |   convertFloatToInt TO_NEGINF =
                fmoveGeneral(0w1, 0w0, 0w0, 0w2, 0w0, vReg, xRegOnly) (* fcvtms *)
        |   convertFloatToInt TO_POSINF =
                fmoveGeneral(0w1, 0w0, 0w0, 0w1, 0w0, vReg, xRegOnly) (* fcvtps *)
        |   convertFloatToInt TO_ZERO =
                fmoveGeneral(0w1, 0w0, 0w0, 0w3, 0w0, vReg, xRegOnly) (* fcvtzs *)

        and convertDoubleToInt TO_NEAREST =
                fmoveGeneral(0w1, 0w0, 0w1, 0w0, 0w4, vReg, xRegOnly) (* fcvtas *)
        |   convertDoubleToInt TO_NEGINF =
                fmoveGeneral(0w1, 0w0, 0w1, 0w2, 0w0, vReg, xRegOnly) (* fcvtms *)
        |   convertDoubleToInt TO_POSINF =
                fmoveGeneral(0w1, 0w0, 0w1, 0w1, 0w0, vReg, xRegOnly) (* fcvtps *)
        |   convertDoubleToInt TO_ZERO =
                fmoveGeneral(0w1, 0w0, 0w1, 0w3, 0w0, vReg, xRegOnly) (* fcvtzs *)

        and convertFloatToInt32 TO_NEAREST =
                fmoveGeneral(0w0, 0w0, 0w0, 0w0, 0w4, vReg, xRegOnly) (* fcvtas *)
        |   convertFloatToInt32 TO_NEGINF =
                fmoveGeneral(0w0, 0w0, 0w0, 0w2, 0w0, vReg, xRegOnly) (* fcvtms *)
        |   convertFloatToInt32 TO_POSINF =
                fmoveGeneral(0w0, 0w0, 0w0, 0w1, 0w0, vReg, xRegOnly) (* fcvtps *)
        |   convertFloatToInt32 TO_ZERO =
                fmoveGeneral(0w0, 0w0, 0w0, 0w3, 0w0, vReg, xRegOnly) (* fcvtzs *)

        and convertDoubleToInt32 TO_NEAREST =
                fmoveGeneral(0w0, 0w0, 0w1, 0w0, 0w4, vReg, xRegOnly) (* fcvtas *)
        |   convertDoubleToInt32 TO_NEGINF =
                fmoveGeneral(0w0, 0w0, 0w1, 0w2, 0w0, vReg, xRegOnly) (* fcvtms *)
        |   convertDoubleToInt32 TO_POSINF =
                fmoveGeneral(0w0, 0w0, 0w1, 0w1, 0w0, vReg, xRegOnly) (* fcvtps *)
        |   convertDoubleToInt32 TO_ZERO =
                fmoveGeneral(0w0, 0w0, 0w1, 0w3, 0w0, vReg, xRegOnly) (* fcvtzs *)
    end

    local
        fun floatingPtCompare(ptype, opc) {regM, regN} =
            SimpleInstr(0wx1E202000 orb (ptype << 0w22) orb
                (word8ToWord32(vReg regM) << 0w16) orb (word8ToWord32(vReg regN) << 0w5) orb
                (opc << 0w3))
    in
        val compareFloat = floatingPtCompare(0w0, 0w0) (* fcmp *)
        and compareDouble = floatingPtCompare(0w1, 0w0)
        (* It is also possible to compare a single register with zero using opc=1/3 *)
    end

    local
        (* Floating point single source. *)
        fun floatingPtSingle (ptype, opc) {regN, regD} =
            SimpleInstr(0wx1E204000 orb (ptype << 0w22) orb (opc << 0w15) orb
                (word8ToWord32(vReg regN) << 0w5) orb word8ToWord32(vReg regD))
    in
        val moveFloatToFloat = floatingPtSingle(0w0, 0wx0)
        and absFloat = floatingPtSingle(0w0, 0wx1)
        and negFloat = floatingPtSingle(0w0, 0wx2)
        and convertFloatToDouble = floatingPtSingle(0w0, 0wx5)
        and moveDoubleToDouble = floatingPtSingle(0w1, 0wx0)
        and absDouble = floatingPtSingle(0w1, 0wx1)
        and negDouble = floatingPtSingle(0w1, 0wx2)
        and convertDoubleToFloat = floatingPtSingle(0w1, 0wx4)
    end

    local
        fun atomicMemory (size, v, a, r, o3, opc) {regS, regN, regT} =
            SimpleInstr(0wx38200000 orb (size << 0w30) orb (v << 0w26) orb (a << 0w23) orb (r << 0w22) orb (o3 << 0w15) orb (opc << 0w12)
                orb (word8ToWord32(xRegOrXZ regS) << 0w16) orb (word8ToWord32(xRegOrXSP regN) << 0w5) orb word8ToWord32(xRegOrXZ regT))
    in
        val loadAddAL = atomicMemory(0w3, 0w0, 0w1, 0w1, 0w0, 0w0)
        and loadUMaxAL = atomicMemory(0w3, 0w0, 0w1, 0w1, 0w0, 0w6)
        and swapAL = atomicMemory(0w3, 0w0, 0w1, 0w1, 0w1, 0w0)
        and loadAddA = atomicMemory(0w3, 0w0, 0w1, 0w0, 0w0, 0w0)
        and loadUMaxA = atomicMemory(0w3, 0w0, 0w1, 0w0, 0w0, 0w6)
        and swapL = atomicMemory(0w3, 0w0, 0w0, 0w1, 0w1, 0w0)
    end


    (* This word is put in after a call to the RTS trap-handler.  All the registers
       are saved and restored across a call to the trap-handler; the register
       mask contains those that may contain an address and so need to be scanned and
       possibly updated if there is a GC. *)
    fun registerMask(regs) =
    let
        fun addToMask(r, mask) =
        let
            val rno = word8ToWord(xRegOnly r)
        in
            if rno > 0w24 (* In particular this can't be X30. *)
            then raise InternalError ("registerMask: invalid register "^Word.toString rno)
            else mask orb (0w1 << word8ToWord(xRegOnly r))
        end
        val maskWord = List.foldl addToMask 0w0 regs
    in
        SimpleInstr(0wx02000000 (* Reserved instr range. *) orb maskWord)
    end
    
    (* This is a bit of a hack but is the only way to get round the problem that when
       a callback (FFI closure) is called the code has none of the global registers.
       This isn't a problem in the native addressing version because we have
       absolute addresses but in 32-in-64 we need at least one absolute address to
       begin.  This embeds the global heap base pointer as a constant in the
       non-address constant area.  It requires the RTS to be able to find it and
       update it when the code is loaded.  We insert a nop followed by the
       pc-relative load.  This MUST be the first instruction in the code. *)
    local
        val getHeapBase: unit -> LargeWord.word = RunCall.rtsCallFull0 "PolyGetHeapBase"
    in
        fun loadGlobalHeapBaseInCallback reg =
			case archType of
				ArchC32 _ => [SimpleInstr nopCode, loadNonAddressConstant(reg, getHeapBase())]
            | 	ArchNative => raise InternalError "loadGlobalHeapBaseInCallback called with native addressing"
    end

    (* Size of each code word. *)
    fun codeSize (SimpleInstr _) = 1 (* Number of 32-bit words *)
    |   codeSize (LoadAddressLiteral{ length=ref BrShort, ...}) = 1
    |   codeSize (LoadAddressLiteral{ length=ref BrExtended, ...}) = 2
    |   codeSize (LoadNonAddressLiteral{ length=ref BrShort, ...}) = 1
    |   codeSize (LoadNonAddressLiteral{ length=ref BrExtended, ...}) = 2
    |   codeSize (LoadFPLiteral{ length=ref BrShort, ...}) = 1
    |   codeSize (LoadFPLiteral{ length=ref BrExtended, ...}) = 2
    |   codeSize (Label _) = 0
    |   codeSize (UnconditionalBranch _) = 1
    |   codeSize (LoadLabelAddress { length=ref BrShort, ...}) = 1
    |   codeSize (LoadLabelAddress { length=ref BrExtended, ...}) = 2
    |   codeSize (ConditionalBranch { length=ref BrShort, ...}) = 1
    |   codeSize (ConditionalBranch { length=ref BrExtended, ...}) = 2
    |   codeSize (TestBitBranch { length=ref BrShort, ...}) = 1
    |   codeSize (TestBitBranch { length=ref BrExtended, ...}) = 2
    |   codeSize (CompareBranch { length=ref BrShort, ...}) = 1
    |   codeSize (CompareBranch { length=ref BrExtended, ...}) = 2

    (* Store a 32-bit value in the code.  Always little-endian. *)
    fun writeInstr(value, wordAddr, seg) =
    let
        fun putBytes(value, a, seg, i) =
        if i = 0w4 then ()
        else
        (
            byteVecSet(seg, a+i, word32ToWord8(value andb 0wxff));
            putBytes(value >> 0w8, a, seg, i+0w1)
        )
    in
        putBytes(value, Word.<<(wordAddr, 0w2), seg, 0w0)
    end
    
    (* Store a 64-bit constant in the code area. *)
    fun write64Bit(value, word64Addr, seg) =
    let
        fun putBytes(value, a, seg, i) =
        if i = 0w8 then ()
        else
        (
            byteVecSet(seg,
                if not isBigEndian then a+i else a+0w8-i-0w1,
                Word8.fromLarge(Word64.toLarge value));
            putBytes(Word64.>>(value, 0w8), a, seg, i+0w1)
        )
    in
        putBytes(value, Word.<<(word64Addr, 0w3), seg, 0w0)
    end

    (* Set the sizes of branches depending on the distance to the destination. *)
    fun setLabelsAndSizes(ops, maxConstantSize) =
    let
        (* Set the labels and get the current size of the code. *)
        fun setLabels(Label(ref labs) :: ops, ic) = (List.app(fn d => d := ic) labs; setLabels(ops, ic))
        |   setLabels(oper :: ops, ic) = setLabels(ops, ic + Word.fromInt(codeSize oper))
        |   setLabels([], ic) = ic

        (* Set the labels and adjust the sizes, repeating until it never gets smaller *)
        fun setLabAndSize(ops, lastSize) =
        let
            (* See if we can shorten any branches.  The "addr" is the original address since
               that's what we've used to set the labels.  *)
            fun adjust([], _) = ()

            |   adjust(ConditionalBranch { length as ref BrExtended, label=ref labs, ...} :: instrs, addr) =
                let
                    val dest = !(hd labs)
                    val offset = Word.toInt dest - Word.toInt addr
                in
                    if willFitInRange(offset, 0w19) then length := BrShort else ();
                    adjust(instrs, addr + 0w2) (* N.B. Size BEFORE any adjustment *)
                end
            
            |   adjust(TestBitBranch { length as ref BrExtended, label=ref labs, ...} :: instrs, addr) =
                let
                    val dest = !(hd labs)
                    val offset = Word.toInt dest - Word.toInt addr
                in
                    if willFitInRange(offset, 0w14) then length := BrShort else ();
                    adjust(instrs, addr + 0w2) (* N.B. Size BEFORE any adjustment *)
                end

            |   adjust(CompareBranch { length as ref BrExtended, label=ref labs, ...} :: instrs, addr) =
                let
                    val dest = !(hd labs)
                    val offset = Word.toInt dest - Word.toInt addr
                in
                    if willFitInRange(offset, 0w19) then length := BrShort else ();
                    adjust(instrs, addr + 0w2) (* N.B. Size BEFORE any adjustment *)
                end

            |   adjust(LoadAddressLiteral { length as ref BrExtended, ...} :: instrs, addr) =
                let
                    val offset = Word.toInt (lastSize + maxConstantSize) - Word.toInt addr
                in
                    (* We can only shorten these in 32-in-64.  In native 64-bits we may need to move
                       the constant area *)
                    if (case archType of ArchC32 _ => true | ArchNative => false)
						 andalso willFitInRange(offset, 0w19)
					then length := BrShort else ();
                    adjust(instrs, addr + 0w2) (* N.B. Size BEFORE any adjustment *)
                end

            |   adjust(LoadNonAddressLiteral { length as ref BrExtended, ...} :: instrs, addr) =
                let
                    val offset = Word.toInt (lastSize + maxConstantSize) - Word.toInt addr
                in
                    if willFitInRange(offset, 0w19) then length := BrShort else ();
                    adjust(instrs, addr + 0w2) (* N.B. Size BEFORE any adjustment *)
                end

            |   adjust(LoadFPLiteral { length as ref BrExtended, ...} :: instrs, addr) =
                let
                    val offset = Word.toInt (lastSize + maxConstantSize) - Word.toInt addr
                in
                    if willFitInRange(offset, 0w19) then length := BrShort else ();
                    adjust(instrs, addr + 0w2) (* N.B. Size BEFORE any adjustment *)
                end

            |   adjust(LoadLabelAddress { length as ref BrExtended, label=ref labs, ...} :: instrs, addr) =
                let
                    val dest = !(hd labs)
                    val offset = Word.toInt dest - Word.toInt addr
                in
                    if willFitInRange(offset, 0w19) then length := BrShort else ();
                    adjust(instrs, addr + 0w2) (* N.B. Size BEFORE any adjustment *)
                end

            |   adjust(instr :: instrs, addr) = adjust(instrs, addr + Word.fromInt(codeSize instr))

            val () = adjust(ops, 0w0)

            val nextSize = setLabels(ops, 0w0)
        in
            if nextSize < lastSize then setLabAndSize(ops, nextSize)
            else if nextSize = lastSize then lastSize
            else raise InternalError "setLabAndSize - size increased"
        end
    in
        setLabAndSize(ops, setLabels(ops, 0w0))
    end

    fun genCode(ops, addressConsts, nonAddressConsts, addrConstMap, nonAddrConstMap) =
    let
        val numNonAddrConsts = Word.fromInt(List.length nonAddressConsts)
        and numAddrConsts = Word.fromInt(List.length addressConsts) (* 32-bit words. *)
        val constSizePlusExtras = (* Number of extra (poly)words needed. *)
            numNonAddrConsts * wordsPerNativeWord + numAddrConsts + 0w4 (* 4 extra words *)

        val codeSize (* Number of 32-bit instructions *) =
            setLabelsAndSizes(ops, constSizePlusExtras  * (Address.wordSize div 0w4) + 0w2 (*allow 2 UDFs*))

        val wordsOfCode = (codeSize + 0w2) div 0w2 (* Round up to 64-bits with the UDF marker(s) added. *)
        (* Put one or two UDF instructions at the end as markers. *)
        val endOfCodeWords =
            if Word.andb(codeSize, 0w1) = 0w0 then [SimpleInstr undefCode, SimpleInstr undefCode] else [SimpleInstr undefCode]
 
        (* Segment size in Poly words. *)
        val segSize = wordsOfCode*wordsPerNativeWord + constSizePlusExtras
        val codeVec = byteVecMake segSize
        
        fun testBit(bitNo, brNonZero, offset, reg) =
            0wx36000000 orb (if bitNo >= 0w32 then 0wx80000000 else 0w0) orb
                (if brNonZero then 0wx01000000 else 0w0) orb
                (word8ToWord32(Word8.andb(bitNo, 0wx3f)) << 0w19) orb
                ((offset andb 0wx3fff) << 0w5) orb word8ToWord32(xRegOnly reg)
        and compareBranch(size, brNonZero, offset, reg) =
            0wx34000000 orb (case size of WordSize64 => 0wx80000000 | WordSize32 => 0w0) orb
                (if brNonZero then 0wx01000000 else 0w0) orb
                ((offset andb 0wx7ffff) << 0w5) orb word8ToWord32(xRegOnly reg)

        fun genCodeWords([], _ , _, _) = ()

        |   genCodeWords(SimpleInstr code :: tail, wordNo, aConstNum, nonAConstNum) =
            (
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            )

        |   genCodeWords(LoadAddressLiteral{reg, length=ref BrExtended, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val code1 = 0wx90000000 orb word8ToWord32(xRegOnly reg)
                val code2 =
                    (case archType of ArchC32 _ => loadRegScaled32 | ArchNative => loadRegScaled)
                        {regT=reg, regN=reg, unitOffset=0}
            in
                writeInstr(code1, wordNo, codeVec);
                genCodeWords(code2 :: tail, wordNo+0w1, aConstNum+1, nonAConstNum)
            end

        |   genCodeWords(LoadAddressLiteral{reg, length=ref BrShort, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            (* Address literals can be shortened in 32-in-64 but are always 2 instrs in 64-bit.
               That allows for the constant area to be pulled out if necessary to make the
               code position-independent. *)
            let
                (* The offset is in 32-bit words.  The first of the constants is
                   at offset wordsOfCode+3.  Non-address constants are always 8 bytes but
                   address constants are 4 bytes in 32-in-64. *)
                val s = case archType of ArchC32 _ => 0w0 | ArchNative => 0w1
                    (* Load 64-bit word in 64-bit mode and 32-bits in 32-in-64. *)
                val constPos = Array.sub(addrConstMap, aConstNum)
                val offsetOfConstant =
                    (wordsOfCode+numNonAddrConsts)*0w2 + (0w3+constPos)*(Address.wordSize div 0w4) - wordNo
                val _ = willFitInRange(Word.toInt offsetOfConstant, 0w19) orelse raise InternalError "Offset to constant is too large"
                val code =
                    0wx18000000 orb (s << 0w30) orb (wordToWord32 offsetOfConstant << 0w5) orb word8ToWord32(xRegOnly reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum+1, nonAConstNum)
            end

        |   genCodeWords(LoadNonAddressLiteral{reg, length=ref BrExtended, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val code1 = 0wx90000000 orb word8ToWord32(xRegOnly reg)
                (* The load instruction is always 64-bits even in 32-in-64. *)
                val code2 = loadRegScaled{regT=reg, regN=reg, unitOffset=0}
            in
                writeInstr(code1, wordNo, codeVec);
                genCodeWords(code2 :: tail, wordNo+0w1, aConstNum, nonAConstNum+1)
            end

        |   genCodeWords(LoadNonAddressLiteral{reg, length=ref BrShort, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            (* These can be shortened since they're always part of the code. *)
            let
                (* The offset is in 32-bit words.  These are always 64-bits. *)
                val constPos = Array.sub(nonAddrConstMap, nonAConstNum)
                val offsetOfConstant = (wordsOfCode+constPos)*0w2 - wordNo
                val _ = willFitInRange(Word.toInt offsetOfConstant, 0w19) orelse raise InternalError "Offset to constant is too large"
                val code = 0wx58000000 orb (wordToWord32 offsetOfConstant << 0w5) orb word8ToWord32(xRegOnly reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum+1)
            end

        |   genCodeWords(LoadFPLiteral{reg, work, isDouble, length=ref BrExtended, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val code1 = 0wx90000000 orb word8ToWord32(xRegOnly work)
                val code2 = (if isDouble then loadRegScaledDouble else loadRegScaledFloat){regT=reg, regN=work, unitOffset=0}
            in
                writeInstr(code1, wordNo, codeVec);
                genCodeWords(code2 :: tail, wordNo+0w1, aConstNum, nonAConstNum+1)
            end

        |   genCodeWords(LoadFPLiteral{reg, isDouble, length=ref BrShort, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                (* The offset is in 32-bit words.  These are always 64-bits. *)
                val constPos = Array.sub(nonAddrConstMap, nonAConstNum)
                val offsetOfConstant = (wordsOfCode+constPos)*0w2 - wordNo
                val _ = willFitInRange(Word.toInt offsetOfConstant, 0w19) orelse raise InternalError "Offset to constant is too large"
                val code = (if isDouble then 0wx5c000000 else 0wx1c000000) orb
                                (wordToWord32 offsetOfConstant << 0w5) orb word8ToWord32(vReg reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum+1)
            end

        |   genCodeWords(Label _ :: tail, wordNo, aConstNum, nonAConstNum) = 
                genCodeWords(tail, wordNo, aConstNum, nonAConstNum) (* No code. *)

        |   genCodeWords(UnconditionalBranch{label=ref labs, andLink} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = willFitInRange(offset, 0w26) orelse raise InternalError "genCodeWords: branch too far"
                val linkBit = if andLink then 0wx80000000 else 0w0
            in
                writeInstr(0wx14000000 orb linkBit orb (Word32.fromInt offset andb 0wx03ffffff), wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(ConditionalBranch{ label=ref labs, jumpCondition=cond, length=ref BrShort }:: tail, wordNo,
                            aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = willFitInRange(offset, 0w19) orelse raise InternalError "genCodeWords: branch too far"
            in
                writeInstr(0wx54000000 orb ((Word32.fromInt offset andb 0wx07ffff) << 0w5)
                        orb cCode cond, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(ConditionalBranch{ label=ref labs, jumpCondition, length=ref BrExtended }:: tail, wordNo,
                            aConstNum, nonAConstNum) =
            let (* Long form - put a conditional branch with reversed sense round an unconditional branch. *)
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt (wordNo + 0w1) (* Next instruction. *)
                val _ = willFitInRange(offset, 0w26) orelse raise InternalError "genCodeWords: branch too far"
                val revCond = invertTest jumpCondition
            in
                writeInstr(0wx54000000 orb (0w2 << 0w5) orb cCode revCond, wordNo, codeVec);
                writeInstr(0wx14000000 orb (Word32.fromInt offset andb 0wx03ffffff), wordNo+0w1, codeVec);
                genCodeWords(tail, wordNo+0w2, aConstNum, nonAConstNum)
            end

        |   genCodeWords(LoadLabelAddress{reg, length=ref BrExtended, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val code1 = 0wx90000000 orb word8ToWord32(xRegOnly reg)
                val code2 = addImmediate{regN=reg, regD=reg, immed=0w0, shifted=false}
            in
                writeInstr(code1, wordNo, codeVec);
                genCodeWords(code2 :: tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(LoadLabelAddress{label=ref labs, reg, length=ref BrShort, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = willFitInRange(offset, 0w19) orelse raise InternalError "Offset to label address is too large"
                val code = 0wx10000000 orb ((Word32.fromInt offset andb 0wx7ffff) << 0w5) orb word8ToWord32(xRegOnly reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(TestBitBranch{label=ref labs, bitNo, brNonZero, reg, length=ref BrExtended} :: tail,
                    wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt (wordNo + 0w1) (* Next instruction *)
                val _ = willFitInRange(offset, 0w25) orelse raise InternalError "genCodeWords: branch too far"
                val _ = bitNo <= 0w63 orelse
                    raise InternalError "TestBitBranch: bit number > 63"
                val code = testBit(bitNo, (* Invert test *) not brNonZero, 0w2 (* Skip branch *), reg)
            in
                writeInstr(code, wordNo, codeVec);
                writeInstr(0wx14000000 orb (Word32.fromInt offset andb 0wx03ffffff), wordNo+0w1, codeVec);
                genCodeWords(tail, wordNo+0w2, aConstNum, nonAConstNum)
            end

        |   genCodeWords(TestBitBranch{label=ref labs, bitNo, brNonZero, reg, length=ref BrShort} :: tail,
                    wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = willFitInRange(offset, 0w14) orelse raise InternalError "TestBitBranch: Offset to label address is too large"
                val _ = bitNo <= 0w63 orelse
                    raise InternalError "TestBitBranch: bit number > 63"
                val code = testBit(bitNo, brNonZero, Word32.fromInt offset, reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end

        |   genCodeWords(CompareBranch{label=ref labs, brNonZero, size, reg, length=ref BrExtended} :: tail,
                    wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt (wordNo+0w1)
                val _ = willFitInRange(offset, 0w25) orelse raise InternalError "genCodeWords: branch too far"
                val code = compareBranch(size, (* Invert test *) not brNonZero, 0w2, reg)
            in
                writeInstr(code, wordNo, codeVec);
                writeInstr(0wx14000000 orb (Word32.fromInt offset andb 0wx03ffffff), wordNo+0w1, codeVec);
                genCodeWords(tail, wordNo+0w2, aConstNum, nonAConstNum)
            end

        |   genCodeWords(CompareBranch{label=ref labs, brNonZero, size, reg, length=ref BrShort} :: tail,
                    wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs)
                val offset = Word.toInt dest - Word.toInt wordNo
                val _ = willFitInRange(offset, 0w19) orelse raise InternalError "CompareBranch: Offset to label address is too large"
                val code = compareBranch(size, brNonZero, Word32.fromInt offset, reg)
            in
                writeInstr(code, wordNo, codeVec);
                genCodeWords(tail, wordNo+0w1, aConstNum, nonAConstNum)
            end
    in
        genCodeWords (ops @ endOfCodeWords, 0w0, 0, 0);
        (* Copy in the non-address constants. *)
        List.foldl(fn (cVal, addr) => (write64Bit(cVal, addr, codeVec); addr+0w1)) wordsOfCode nonAddressConsts;
        (codeVec (* Return the completed code. *), wordsOfCode+numNonAddrConsts (* And the size in 64-bit words. *))
    end

    (* Store a word, either 64-bit or 32-bit. *)
    fun setWord(value, wordNo, seg) =
    let
        val addrs = wordNo * Address.wordSize
        fun putBytes(value, a, seg, i) =
        if i = Address.wordSize then ()
        else
        (
            byteVecSet(seg,
                if not isBigEndian then a+i else a+wordSize-i-0w1,
                Word8.fromLarge value);
            putBytes(LargeWord.>>(value, 0w8), a, seg, i+0w1)
        )
    in
        putBytes(value, addrs, seg, 0w0)
    end
    
   
    (* Print the instructions in the code. *)
    fun printCode (codeVec, functionName, wordsOfCode, printStream) =
    let
        val numInstructions = wordsOfCode * (Address.wordSize div 0w4)
    
        fun printHex (v, n) =
        let
            val s = Word32.fmt StringCvt.HEX v
            val pad = CharVector.tabulate(Int.max(0, n-size s), fn _ => #"0")
        in
            printStream pad; printStream s
        end

        fun printCondition 0wx0 = printStream "eq"
        |   printCondition 0wx1 = printStream "ne"
        |   printCondition 0wx2 = printStream "cs"
        |   printCondition 0wx3 = printStream "cc"
        |   printCondition 0wx4 = printStream "mi"
        |   printCondition 0wx5 = printStream "pl"
        |   printCondition 0wx6 = printStream "vs"
        |   printCondition 0wx7 = printStream "vc"
        |   printCondition 0wx8 = printStream "hi"
        |   printCondition 0wx9 = printStream "ls"
        |   printCondition 0wxa = printStream "ge"
        |   printCondition 0wxb = printStream "lt"
        |   printCondition 0wxc = printStream "gt"
        |   printCondition 0wxd = printStream "le"
        |   printCondition 0wxe = printStream "al"
        |   printCondition _    = printStream "nv"

        (* Normal XReg with 31 being XZ *)
        fun prXReg 0w31 = printStream "xz"
        |   prXReg r = printStream("x" ^ Word32.fmt StringCvt.DEC r)

        (* XReg when 31 is SP *)
        fun prXRegOrSP 0w31 = printStream "sp"
        |   prXRegOrSP r = printStream("x" ^ Word32.fmt StringCvt.DEC r)

        (* Normal WReg with 31 being WZ *)
        fun prWReg 0w31 = printStream "wz"
        |   prWReg r = printStream("w" ^ Word32.fmt StringCvt.DEC r)

        (* WReg when 31 is WSP *)
        fun prWRegOrSP 0w31 = printStream "wsp"
        |   prWRegOrSP r = printStream("w" ^ Word32.fmt StringCvt.DEC r)

        (* Each instruction is 32-bytes. *)
        fun printWordAt wordNo =
        let
            val byteNo = Word.<<(wordNo, 0w2)
            val () = printHex(wordToWord32 byteNo, 6)  (* Address *)
            val () = printStream "\t"
            val wordValue =
                word8ToWord32 (codeVecGet (codeVec, byteNo)) orb
                (word8ToWord32 (codeVecGet (codeVec, byteNo+0w1)) << 0w8) orb
                (word8ToWord32 (codeVecGet (codeVec, byteNo+0w2)) << 0w16) orb
                (word8ToWord32 (codeVecGet (codeVec, byteNo+0w3)) << 0w24)
            val () = printHex(wordValue, 8) (* Instr as hex *)
            val () = printStream "\t"
        in
            if (wordValue andb 0wxfffffc1f) = 0wxD61F0000
            then
            let
                val rN = (wordValue andb 0wx3e0) >> 0w5
            in
                printStream "br\tx";
                printStream(Word32.fmt StringCvt.DEC rN)
            end

            else if (wordValue andb 0wxfffffc1f) = 0wxD63F0000
            then
            let
                val rN = (wordValue andb 0wx3e0) >> 0w5
            in
                printStream "blr\tx";
                printStream(Word32.fmt StringCvt.DEC rN)
            end

            else if (wordValue andb 0wxfffffc1f) = 0wxD65F0000
            then
            let
                val rN = (wordValue andb 0wx3e0) >> 0w5
            in
                printStream "ret\tx";
                printStream(Word32.fmt StringCvt.DEC rN)
            end

            else if wordValue = 0wxD503201F
            then printStream "nop"
            else if wordValue = 0wxD503203F
            then printStream "yield"
            else if wordValue = 0wxD5033BBF
            then printStream "dmb\tish"

            else if (wordValue andb 0wx1f800000) = 0wx12800000
            then (* Move of constants.  Includes movn and movk. *)
            let
                val rD = wordValue andb 0wx1f
                val imm16 = Word32.toInt((wordValue >> 0w5) andb 0wxffff)
                val isXReg = (wordValue andb 0wx80000000) <> 0w0
                val opc = (wordValue >> 0w29) andb 0w3
                val shift = (wordValue >> 0w21) andb 0w3
            in
                printStream (if opc = 0w3 then "movk\t" else "mov\t");
                printStream (if isXReg then "x" else "w");
                printStream(Word32.fmt StringCvt.DEC rD);
                printStream ",#";
                printStream(Int.toString(if opc = 0w0 then ~1 - imm16 else imm16));
                if shift = 0w0
                then ()
                else (printStream ",lsl #"; printStream(Word32.fmt StringCvt.DEC (shift*0w16)))
            end

            else if (wordValue andb 0wx3b000000) = 0wx39000000
            then (* Load/Store with unsigned, scaled offset. *)
            let
                (* The offset is in units of the size of the operand. *)
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm12 = (wordValue andb 0wx3ffc00) >> 0w10
                val (opcode, r, scale) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strb", "w", 0w0)
                    |   (0w0, 0w0, 0w1) => ("ldrb", "w", 0w0)
                    |   (0w1, 0w0, 0w0) => ("strh", "w", 0w2)
                    |   (0w1, 0w0, 0w1) => ("ldrh", "w", 0w2)
                    |   (0w2, 0w0, 0w0) => ("str", "w", 0w4)
                    |   (0w2, 0w0, 0w1) => ("ldr", "w", 0w4)
                    |   (0w3, 0w0, 0w0) => ("str", "x", 0w8)
                    |   (0w3, 0w0, 0w1) => ("ldr", "x", 0w8)
                    |   (0w2, 0w1, 0w0) => ("str", "s", 0w4)
                    |   (0w2, 0w1, 0w1) => ("ldr", "s", 0w4)
                    |   (0w3, 0w1, 0w0) => ("str", "d", 0w8)
                    |   (0w3, 0w1, 0w1) => ("ldr", "d", 0w8)
                    |   _ => ("??", "?", 0w1)
            in
                printStream opcode; printStream "\t"; printStream r; printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",["; prXRegOrSP rN;
                printStream ",#"; printStream(Word32.fmt StringCvt.DEC(imm12*scale));
                printStream "]"
            end

            else if (wordValue andb 0wx3b200c00) = 0wx38000000
            then (* Load/store unscaled immediate *)
            let
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm9 = (wordValue andb 0wx1ff000) >> 0w12
                val imm9Text =
                    if imm9 > 0wxff
                    then "-" ^ Word32.fmt StringCvt.DEC (0wx200 - imm9)
                    else Word32.fmt StringCvt.DEC imm9
                val (opcode, r) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("sturb", "w")
                    |   (0w0, 0w0, 0w1) => ("ldurb", "w")
                    |   (0w0, 0w0, 0w2) => ("ldursb", "w")
                    |   (0w0, 0w0, 0w3) => ("ldursb", "x")
                    |   (0w1, 0w0, 0w0) => ("sturh", "w")
                    |   (0w1, 0w0, 0w1) => ("ldurh", "w")
                    |   (0w1, 0w0, 0w2) => ("ldursh", "w")
                    |   (0w1, 0w0, 0w3) => ("ldursh", "x")
                    |   (0w2, 0w0, 0w0) => ("stur", "w")
                    |   (0w2, 0w0, 0w1) => ("ldur", "w")
                    |   (0w2, 0w0, 0w2) => ("ldursw", "x")
                    |   (0w3, 0w0, 0w0) => ("stur", "x")
                    |   (0w3, 0w0, 0w1) => ("ldur", "x")
                    |   (0w2, 0w1, 0w0) => ("stur", "s")
                    |   (0w2, 0w1, 0w1) => ("ldur", "s")
                    |   (0w3, 0w1, 0w0) => ("stur", "d")
                    |   (0w3, 0w1, 0w1) => ("ldur", "d")
                    |   _ => ("???", "?")
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",["; prXRegOrSP rN;
                printStream ",#"; printStream imm9Text; printStream "]"
            end

            else if (wordValue andb 0wx3b200c00) = 0wx38000400
            then (* Load/store immediate post-indexed *)
            let
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm9 = (wordValue andb 0wx1ff000) >> 0w12
                val imm9Text =
                    if imm9 > 0wxff
                    then "-" ^ Word32.fmt StringCvt.DEC (0wx200 - imm9)
                    else Word32.fmt StringCvt.DEC imm9
                val (opcode, r) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strb", "w")
                    |   (0w0, 0w0, 0w1) => ("ldrb", "w")
                    |   (0w2, 0w0, 0w0) => ("str", "w")
                    |   (0w2, 0w0, 0w1) => ("ldr", "w")
                    |   (0w3, 0w0, 0w0) => ("str", "x")
                    |   (0w3, 0w0, 0w1) => ("ldr", "x")
                    |   _ => ("???", "?")
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",["; prXRegOrSP rN;
                printStream "],#"; printStream imm9Text
            end

            else if (wordValue andb 0wx3b200c00) = 0wx38000c00
            then (* Load/store immediate pre-indexed *)
            let
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm9 = (wordValue andb 0wx1ff000) >> 0w12
                val imm9Text =
                    if imm9 > 0wxff
                    then "-" ^ Word32.fmt StringCvt.DEC (0wx200 - imm9)
                    else Word32.fmt StringCvt.DEC imm9
                val (opcode, r) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strb", "w")
                    |   (0w0, 0w0, 0w1) => ("ldrb", "w")
                    |   (0w2, 0w0, 0w0) => ("str", "w")
                    |   (0w2, 0w0, 0w1) => ("ldr", "w")
                    |   (0w3, 0w0, 0w0) => ("str", "x")
                    |   (0w3, 0w0, 0w1) => ("ldr", "x")
                    |   _ => ("???", "?")
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",["; prXRegOrSP rN;
                printStream ",#"; printStream imm9Text; printStream "]!"
            end

            else if (wordValue andb 0wx3b200c00) = 0wx38200800
            then (* Load/store with register offset i.e. an index register. *)
            let
                val size = wordValue >> 0w30
                and v = (wordValue >> 0w26) andb 0w1
                and opc = (wordValue >> 0w22) andb 0w3
                val rT = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                val option = (wordValue >> 0w13) andb 0w7
                val s = (wordValue andb 0wx1000) <> 0w0 
                val (opcode, r) =
                    case (size, v, opc) of
                        (0w0, 0w0, 0w0) => ("strb", "w")
                    |   (0w0, 0w0, 0w1) => ("ldrb", "w")
                    |   (0w1, 0w0, 0w0) => ("strh", "w")
                    |   (0w1, 0w0, 0w1) => ("ldrh", "w")
                    |   (0w2, 0w0, 0w0) => ("str", "w")
                    |   (0w2, 0w0, 0w1) => ("ldr", "w")
                    |   (0w3, 0w0, 0w0) => ("str", "x")
                    |   (0w3, 0w0, 0w1) => ("ldr", "x")
                    |   (0w2, 0w1, 0w0) => ("str", "s")
                    |   (0w2, 0w1, 0w1) => ("ldr", "s")
                    |   (0w3, 0w1, 0w0) => ("str", "d")
                    |   (0w3, 0w1, 0w1) => ("ldr", "d")
                    |   _ => ("???", "?")
                val (extend, xr) =
                    case option of
                        0w2 => (" uxtw", "w")
                    |   0w3 => if s then (" lsl", "x") else ("", "x")
                    |   0w6 => (" sxtw", "w")
                    |   0w7 => (" sxtx", "x")
                    |   _   => ("?", "?")
                val indexShift =
                    case (size, s) of
                        (0w0, true) => " #1"
                    |   (0w1, true) => " #1"
                    |   (0w2, true) => " #2"
                    |   (0w3, true) => " #3"
                    |   _ => ""
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",["; prXRegOrSP rN;
                printStream ","; printStream xr; printStream(Word32.fmt StringCvt.DEC rM);
                printStream extend; printStream indexShift;
                printStream "]"
            end

            else if (wordValue andb 0wx3f000000) = 0wx08000000
            then (* Loads and stores with special ordering. *)
            let
                val size = (wordValue >> 0w30) andb 0w3
                and o2 = (wordValue >> 0w23) andb 0w1
                and l = (wordValue >> 0w22) andb 0w1
                and o1 = (wordValue >> 0w21) andb 0w1
                and o0 = (wordValue >> 0w15) andb 0w1
                val rT = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rS = (wordValue >> 0w16) andb 0wx1f
                val (opcode, r) =
                    case (size, o2, l, o1, o0) of
                        (0w3, 0w1, 0w1, 0w0, 0w1) => ("ldar", "x")
                    |   (0w3, 0w1, 0w0, 0w0, 0w1) => ("stlr", "x")
                    |   (0w2, 0w1, 0w1, 0w0, 0w1) => ("ldar", "w")
                    |   (0w2, 0w1, 0w0, 0w0, 0w1) => ("stlr", "w")
                    |   (0w3, 0w0, 0w1, 0w0, 0w1) => ("ldaxr", "x")
                    |   (0w3, 0w0, 0w0, 0w0, 0w1) => ("stlxr", "x")
                    |   (0w0, 0w1, 0w1, 0w0, 0w1) => ("ldarb", "w")
                    |   (0w0, 0w1, 0w0, 0w0, 0w1) => ("stlrb", "w")
                    |   _ => ("??", "?")
            in
                printStream opcode; printStream "\t";
                if opcode = "stlxr"
                then (printStream "w"; printStream(Word32.fmt StringCvt.DEC rS); printStream ",")
                else ();
                printStream r;
                printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",["; prXRegOrSP rN; printStream "]"
            end

            else if (wordValue andb 0wx3a000000) = 0wx28000000
            then (* Load/store pairs of registers *)
            let
                val opc = (wordValue >> 0w30) andb 0w3
                and v = (wordValue >> 0w26) andb 0w1
                and op2 = (wordValue >> 0w23) andb 0w3
                and l = (wordValue >> 0w22) andb 0w1
                and imm7 = (wordValue >> 0w15) andb 0wx7f
                and rT2 = (wordValue >> 0w10) andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rT1 = wordValue andb 0wx1f
                val (opcode, r, scale) =
                    case (opc, v, l) of
                        (0w0, 0w0, 0w0) => ("stp", "w", 0w4)
                    |   (0w0, 0w0, 0w1) => ("ldp", "w", 0w4)
                    |   (0w2, 0w0, 0w0) => ("stp", "x", 0w8)
                    |   (0w2, 0w0, 0w1) => ("ldp", "x", 0w8)
                    |   (0w0, 0w1, 0w0) => ("stp", "s", 0w4)
                    |   (0w0, 0w1, 0w1) => ("ldp", "s", 0w4)
                    |   (0w1, 0w1, 0w0) => ("stp", "d", 0w8)
                    |   (0w1, 0w1, 0w1) => ("ldp", "d", 0w8)
                    |   _ => ("??", "?", 0w1)
                val imm7Text =
                    if imm7 > 0wx3f
                    then "-" ^ Word32.fmt StringCvt.DEC ((0wx80 - imm7) * scale)
                    else Word32.fmt StringCvt.DEC (imm7 * scale)
            in
                printStream opcode; printStream "\t"; printStream r;
                printStream(Word32.fmt StringCvt.DEC rT1); printStream ",";
                printStream r; printStream(Word32.fmt StringCvt.DEC rT2);
                printStream ",["; prXRegOrSP rN;
                case op2 of
                    0w1 => (* Post indexed *)
                        (printStream "],#"; printStream imm7Text)
                |   0w2 => (* Offset *)
                        (printStream ",#"; printStream imm7Text; printStream "]")
                |   0w3 => (* Pre indexed *)
                        (printStream ",#"; printStream imm7Text; printStream "]!")
                |   _ => printStream "??"
            end

            else if (wordValue andb 0wx1f800000) = 0wx11000000
            then
            let
                (* Add/Subtract a 12-bit immediate with possible shift. *)
                val sf = (wordValue >> 0w31) andb 0w1
                val rD = wordValue andb 0wx1f
                and rN = (wordValue andb 0wx3e0) >> 0w5
                and imm12 = (wordValue andb 0wx3ffc00) >> 0w10
                and shiftBit = wordValue andb 0wx400000
                val imm = if shiftBit <> 0w0 then imm12 << 0w12 else imm12
                val oper = (wordValue andb 0wx40000000) = 0w0
                val isS = (wordValue andb 0wx20000000) <> 0w0
                val prReg = if sf = 0w1 then prXRegOrSP else prWRegOrSP
            in
                if imm12 = 0w0 andalso (rN = 0w31 orelse rD = 0w31) andalso not isS
                then (printStream "mov\t"; prReg rD; printStream ","; prReg rN)
                else
                (
                    if isS andalso rD = 0w31
                    then printStream(if oper then "cmn\t" else "cmp\t")
                    else
                    (
                        printStream(if oper then "add" else "sub"); printStream(if isS then "s\t" else "\t");
                        prReg rD; printStream ","
                    );
                    prReg rN; printStream ",#"; printStream(Word32.fmt StringCvt.DEC imm)
                )
            end

            else if (wordValue andb 0wx7fe0ffe0) = 0wx2A0003E0
            then (* Move reg,reg.  This is a subset of ORR shifted register. *)
            let
                val reg = if (wordValue andb 0wx80000000) <> 0w0 then "x" else "w"
            in
                printStream "mov\t"; printStream reg;
                printStream(Word32.fmt StringCvt.DEC(wordValue andb 0wx1f));
                printStream ","; printStream reg;
                printStream(Word32.fmt StringCvt.DEC((wordValue >> 0w16) andb 0wx1f))
            end

            else if (wordValue andb 0wx1f000000) = 0wx0A000000
            then
            let
                (* Logical operations with shifted register. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                and imm6 = (wordValue >> 0w10) andb 0wx3f
                and shiftCode = (wordValue >> 0w22) andb 0wx3
                val opc = (wordValue >> 0w29) andb 0wx3
                val nBit = (wordValue >> 0w21) andb 0w1
                val reg = if (wordValue andb 0wx80000000) <> 0w0 then "x" else "w"
                val opcode =
                    case (opc, nBit) of
                        (0w0, 0w0) => "and"
                    |   (0w1, 0w0) => "orr"
                    |   (0w2, 0w0) => "eor"
                    |   (0w3, 0w0) => "ands"
                    |   _ => "??"
            in
                if rD = 0w31 andalso opc=0w3 andalso nBit = 0w0
                then printStream "tst\t"
                else
                (
                    printStream opcode; printStream"\t";
                    printStream reg;
                    printStream(Word32.fmt StringCvt.DEC rD); printStream ","
                );
                printStream reg; printStream(Word32.fmt StringCvt.DEC rN);
                printStream ","; printStream reg; printStream(Word32.fmt StringCvt.DEC rM);
                if imm6 <> 0w0
                then
                (
                    case shiftCode of
                        0w0 => printStream ",lsl #"
                    |   0w1 => printStream ",lsr #"
                    |   0w2 => printStream ",asr #"
                    |   _ => printStream ",?? #";
                    printStream(Word32.fmt StringCvt.DEC imm6)
                )
                else ()
            end

            else if (wordValue andb 0wx1f200000) = 0wx0B000000
            then
            let
                (* Add/subtract shifted register. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                and imm6 = (wordValue >> 0w10) andb 0wx3f
                and shiftCode = (wordValue >> 0w22) andb 0wx3
                val oper = (wordValue andb 0wx40000000) = 0w0
                val isS = (wordValue andb 0wx20000000) <> 0w0
                val pReg = if (wordValue andb 0wx80000000) <> 0w0 then prXReg else prWReg
            in
                if isS andalso rD = 0w31
                then printStream(if oper then "cmn\t" else "cmp\t")
                else
                (
                    printStream(if oper then "add" else "sub"); printStream(if isS then "s\t" else "\t");
                    pReg rD; printStream ","
                );
                pReg rN;
                printStream ","; pReg rM;
                if imm6 <> 0w0
                then
                (
                    case shiftCode of
                        0w0 => printStream ",lsl #"
                    |   0w1 => printStream ",lsr #"
                    |   0w2 => printStream ",asr #"
                    |   _ => printStream ",?? #";
                    printStream(Word32.fmt StringCvt.DEC imm6)
                )
                else ()
            end

            else if (wordValue andb 0wx1fe00000) = 0wx0b200000
            then
            let
                (* Add/subtract extended register. *)
                val rD = wordValue andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rM = (wordValue >> 0w16) andb 0wx1f
                and extend = (wordValue >> 0w13) andb 0w7
                and amount = (wordValue >> 0w10) andb 0w7
                and sf = (wordValue >> 0w31) andb 0w1
                and p = (wordValue >> 0w30) andb 0w1
                and s = (wordValue >> 0w29) andb 0w1
            in
                if s = 0w1 andalso rD = 0w31
                then printStream(if p = 0w0 then "cmn\t" else "cmp\t")
                else
                (
                    printStream(if p = 0w0 then "add" else "sub");
                    printStream(if s = 0w1 then "s\t" else "\t");
                    (if sf = 0w1 then prXRegOrSP else prWRegOrSP) rD; printStream ","
                );
                (if sf = 0w1 then prXRegOrSP else prWRegOrSP) rN;
                printStream ",";
                (if extend = 0w3 orelse extend = 0w7 then prXReg else prWReg) rM;
                case extend of
                    0w0 => printStream ",uxtb"
                |   0w1 => printStream ",uxth"
                |   0w2 => if amount = 0w0 andalso sf = 0w0 then () else printStream ",uxtw"
                |   0w3 => if amount = 0w0 andalso sf = 0w1 then () else printStream ",uxtx"
                |   0w4 => printStream ",sxtb"
                |   0w5 => printStream ",sxth"
                |   0w6 => printStream ",sxtw"
                |   0w7 => printStream ",sxtx"
                |   _ => printStream "?";
               
                if amount <> 0w0
                then printStream(" #" ^ Word32.fmt StringCvt.DEC amount)
                else ()
            end

            else if (wordValue andb 0wx3b000000) = 0wx18000000
            then
            let
                (* Load from a PC-relative address.  This may refer to the
                   address constant area or the non-address constant area. *)
                val rT = wordValue andb 0wx1f
                val opc = (wordValue >> 0w30) andb 0w3
                val v = (wordValue >> 0w26) andb 0w1
                (* The offset is in 32-bit words *)
                val byteAddr = word32ToWord(((wordValue andb 0wx00ffffe0) >> (0w5-0w2))) + byteNo
                (* We must NOT use codeVecGetWord if this is in the non-address
                   area.  It may well not be a tagged value. *)
                local
                    fun getConstant(cVal, 0w0) = cVal
                    |   getConstant(cVal, offset) =
                        let
                            val byteVal =
                                Word64.fromLarge(Word8.toLarge(codeVecGet (codeVec, byteAddr+offset-0w1)))
                        in
                            getConstant(Word64.orb(Word64.<<(cVal, 0w8), byteVal), offset-0w1)
                        end
                in
                    val constantValue = "0x" ^ Word64.toString(getConstant(0w0, 0w8)) (* It's a non-address constant *)
                end
                val reg =
                    case (opc, v) of
                        (0w0, 0w0) => "w"
                    |   (0w1, 0w0) => "x"
                    |   (0w0, 0w1) => "s"
                    |   (0w1, 0w1) => "d"
                    |   _ => "?"
            in
                printStream "ldr\t"; printStream reg;
                printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",0x"; printStream(Word.fmt StringCvt.HEX byteAddr);
                printStream "\t// "; printStream constantValue
            end

            else if (wordValue andb 0wxbf000000) = 0wx10000000
            then
            let
                (* Put a pc-relative address into a register. *)
                val rT = wordValue andb 0wx1f
                val byteOffset =
                    ((wordValue andb 0wx00ffffe0) << (Word.fromInt Word32.wordSize - 0w23) ~>>
                        (Word.fromInt Word32.wordSize - 0w20)) + ((wordValue >> 0w29) andb 0w3)
            in
                printStream "adr\tx"; printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",0x"; printStream(Word32.fmt StringCvt.HEX (wordToWord32 byteNo+byteOffset))
            end

            else if (wordValue andb 0wx9f000000) = 0wx90000000
            then (* ADRP *)
            let
                val rT = wordValue andb 0wx1f
                (* The value is a page offset *)
                val pageOffset = ((wordValue >> 0w29) andb 0w3) (* immlo *) orb
                    ((wordValue >> 0w3) andb 0wx1fffc)
            in
                printStream "adrp\tx"; printStream(Word32.fmt StringCvt.DEC rT);
                printStream ",0x"; printStream(Word32.fmt StringCvt.HEX (pageOffset*0w4096))
            end

            else if (wordValue andb 0wx7c000000) = 0wx14000000
            then (* Unconditional branch. *)
            let
                (* The offset is signed and the destination may be earlier. *)
                val byteOffset =
                    (wordValue andb 0wx03ffffff) << (Word.fromInt Word32.wordSize - 0w26) ~>>
                        (Word.fromInt Word32.wordSize - 0w28)
                val opc = if (wordValue andb 0wx80000000) = 0w0 then "b" else "bl"
            in
                printStream opc; printStream "\t0x";
                printStream(Word32.fmt StringCvt.HEX (wordToWord32 byteNo + byteOffset))
            end

            else if (wordValue andb 0wxff000000) = 0wx54000000
            then (* Conditional branch *)
            let
                val byteOffset =
                    (wordValue andb 0wx00ffffe0) << (Word.fromInt Word32.wordSize - 0w24) ~>>
                        (Word.fromInt Word32.wordSize - 0w21)
            in
                printStream "b.";
                printCondition(wordValue andb 0wxf);
                printStream "\t0x";
                printStream(Word32.fmt StringCvt.HEX (wordToWord32 byteNo+byteOffset))
            end

            else if (wordValue andb 0wx7e000000) = 0wx34000000
            then (* Compare and branch *)
            let
                val byteOffset =
                    (wordValue andb 0wx00ffffe0) << (Word.fromInt Word32.wordSize - 0w24) ~>>
                        (Word.fromInt Word32.wordSize - 0w21)
                val oper =
                    if (wordValue andb 0wx01000000) = 0w0
                    then "cbz" else "cbnz"
                val r = if (wordValue andb 0wx80000000) = 0w0 then "w" else "x"
            in
                printStream oper; printStream "\t";
                printStream r; printStream(Word32.fmt StringCvt.DEC (wordValue andb 0wx1f));
                printStream ",0x";
                printStream(Word32.fmt StringCvt.HEX (wordToWord32 byteNo+byteOffset))
            end

            else if (wordValue andb 0wx7e000000) = 0wx36000000
            then (* Test bit and branch *)
            let
                val byteOffset =
                    (wordValue andb 0wx000fffe0) << (Word.fromInt Word32.wordSize - 0w20) ~>>
                        (Word.fromInt Word32.wordSize - 0w17)
                val oper =
                    if (wordValue andb 0wx01000000) = 0w0
                    then "tbz" else "tbnz"
                val b40 = (wordValue >> 0w19) andb 0wx1f
                val bitNo = b40 orb ((wordValue >> 0w26) andb 0wx20)
                val r = if bitNo < 0w32 then "w" else "x"
            in
                printStream oper; printStream "\t";
                printStream r; printStream(Word32.fmt StringCvt.DEC (wordValue andb 0wx1f));
                printStream ",#"; printStream(Word32.fmt StringCvt.DEC bitNo); printStream ",0x";
                printStream(Word32.fmt StringCvt.HEX (wordToWord32 byteNo+byteOffset))
            end

            else if (wordValue andb 0wx3fe00000) = 0wx1A800000
            then
            let
                val sf = wordValue >> 0w31
                val opc = (wordValue >> 0w30) andb 0w1
                val op2 = (wordValue >> 0w10) andb 0w3
                val rT = wordValue andb 0wx1f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rM = (wordValue >> 0w16) andb 0wx1f
                val cond = (wordValue >> 0w12) andb 0wxf
                val opcode =
                    case (opc, op2) of
                        (0w0, 0w0) => "csel"
                    |   (0w0, 0w1) => "csinc"
                    |   (0w1, 0w0) => "csinv"
                    |   (0w1, 0w1) => "csneg"
                    |   _ => "??"
                val pReg = if sf = 0w0 then prWReg else prXReg
            in
                printStream opcode; printStream "\t";
                pReg rT; printStream ","; pReg rN; printStream ","; pReg rM;
                printStream ","; printCondition cond
            end

            else if (wordValue andb 0wx7f800000) = 0wx13000000
            then (* signed bitfield *)
            let
                val sf = wordValue >> 0w31
                (* N is always the same as sf. *)
                (*val nBit = (wordValue >> 0w22) andb 0w1*)
                val immr = (wordValue >> 0w16) andb 0wx3f
                val imms = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (r, wordSize) = if sf = 0w0 then ("w", 0w32) else if sf = 0w1 then ("x", 0w64) else raise InternalError "Neither"
            in
                if imms = wordSize - 0w1
                then printStream "asr\t"
                else printStream "sbfm\t";
                printStream r;
                printStream(Word32.fmt StringCvt.DEC rD);
                printStream ",";
                printStream r;
                printStream(Word32.fmt StringCvt.DEC rN);
                if imms = wordSize - 0w1
                then (printStream ",#0x"; printStream(Word32.toString immr))
                else
                (
                    printStream ",#0x"; printStream(Word32.toString immr);
                    printStream ",#0x"; printStream(Word32.toString imms)
                )
            end

            else if (wordValue andb 0wx7f800000) = 0wx33000000
            then (* bitfield move *)
            let
                val sf = wordValue >> 0w31
                (* N is always the same as sf. *)
                (*val nBit = (wordValue >> 0w22) andb 0w1*)
                val immr = (wordValue >> 0w16) andb 0wx3f
                val imms = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (r, wordSize) = if sf = 0w0 then ("w", 0w32) else ("x", 0w64)
            in
                if imms < immr
                then if rD = 0wx31 then printStream "bfc\t" else printStream "bfi\t"
                else printStream "bfxil\t";
                if imms >= immr orelse rD <> 0w31
                then
                (
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rD);
                    printStream ","
                )
                else ();
                printStream r;
                printStream(Word32.fmt StringCvt.DEC rN);
                (* Not certain that these are correct. *)
                if imms < immr
                then
                (
                    printStream ",#0x"; printStream(Word32.toString(wordSize - immr));
                    printStream ",#0x"; printStream(Word32.toString(imms+0w1))
                )
                else
                (
                    printStream ",#0x"; printStream(Word32.toString immr);
                    printStream ",#0x"; printStream(Word32.toString(imms+0w1-immr))
                )
            end

            else if (wordValue andb 0wx7f800000) = 0wx53000000
            then (* unsigned bitfield move *)
            let
                val sf = wordValue >> 0w31
                (* N is always the same as sf. *)
                (*val nBit = (wordValue >> 0w22) andb 0w1*)
                val immr = (wordValue >> 0w16) andb 0wx3f
                val imms = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (r, wordSize) = if sf = 0w0 then ("w", 0w32) else ("x", 0w64)
            in
                if imms + 0w1 = immr
                then
                (
                    printStream "lsl\t";
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rD);
                    printStream ",";
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rN);
                    printStream ",#0x"; printStream(Word32.toString(wordSize - immr))
                )
                else if imms = wordSize - 0w1
                then
                (
                    printStream "lsr\t";
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rD);
                    printStream ",";
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rN);
                    printStream ",#0x"; printStream(Word32.toString immr)
                )
                else if imms < immr
                then
                (
                    printStream "ubfiz\t";
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rD);
                    printStream ",";
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rN);
                    printStream ",#0x"; printStream(Word32.toString(wordSize - immr));
                    printStream ",#0x"; printStream(Word32.toString(imms+0w1))
                )
                else
                (
                    printStream "ubfm\t";
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rD);
                    printStream ",";
                    printStream r;
                    printStream(Word32.fmt StringCvt.DEC rN);
                    printStream ",#0x"; printStream(Word32.toString immr);
                    printStream ",#0x"; printStream(Word32.toString imms)
                )
            end

            else if (wordValue andb 0wx1f800000) = 0wx12000000
            then (* logical immediate *)
            let
                val sf = wordValue >> 0w31
                val opc = (wordValue >> 0w29) andb 0w3
                val nBit = (wordValue >> 0w22) andb 0w1
                val immr = (wordValue >> 0w16) andb 0wx3f
                val imms = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (opcode, r) =
                    case (sf, opc, nBit) of
                        (0w0, 0w0, 0w0) => ("and", "w")
                    |   (0w0, 0w1, 0w0) => ("orr", "w")
                    |   (0w0, 0w2, 0w0) => ("eor", "w")
                    |   (0w0, 0w3, 0w0) => ("ands", "w")
                    |   (0w1, 0w0, _) => ("and", "x")
                    |   (0w1, 0w1, _) => ("orr", "x")
                    |   (0w1, 0w2, _) => ("eor", "x")
                    |   (0w1, 0w3, _) => ("ands", "x")
                    |   _ => ("??", "?")
            in
                if rD = 0w31 andalso opc=0w3
                then printStream "tst\t"
                else
                (
                    printStream opcode;
                    printStream "\t";
                    printStream r; printStream(Word32.fmt StringCvt.DEC rD); printStream ","
                );
                printStream r; printStream(Word32.fmt StringCvt.DEC rN); printStream ",#0x";
                printStream(Word64.toString(decodeBitPattern{sf=sf, n=nBit, immr=immr, imms=imms}))
            end

            else if (wordValue andb 0wx5fe00000) = 0wx5ac00000
            then (* Single source instructions - currently just clz. *)
            let
                val sf = wordValue >> 0w31
                val s = (wordValue >> 0w29) andb 0w1
                val opcode2 = (wordValue >> 0w16) andb 0wx1f
                val opcode = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (oper, r) =
                    case (sf, s, opcode2, opcode) of
                        (0w1, 0w0, 0w0, 0w4) => ("clz", "x")
                    |   (0w0, 0w0, 0w0, 0w4) => ("clz", "w")
                    |   _ => ("??", "?")
            in
                printStream oper;
                printStream "\t";
                printStream r; printStream(Word32.fmt StringCvt.DEC rD); printStream ",";
                printStream r; printStream(Word32.fmt StringCvt.DEC rN)
            end

            else if (wordValue andb 0wx5fe00000) = 0wx1ac00000
            then (* Two source operations - shifts and divide. *)
            let
                val sf = wordValue >> 0w31
                val s = (wordValue >> 0w29) andb 0w1
                val rM = (wordValue >> 0w16) andb 0wx1f
                val opcode = (wordValue >> 0w10) andb 0wx3f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (oper, r) =
                    case (sf, s, opcode) of
                        (0w1, 0w0, 0wx2) => ("udiv", "x")
                    |   (0w1, 0w0, 0wx3) => ("sdiv", "x")
                    |   (0w0, 0w0, 0wx2) => ("udiv", "w")
                    |   (0w0, 0w0, 0wx3) => ("sdiv", "w")
                    |   (0w1, 0w0, 0wx8) => ("lsl", "x")
                    |   (0w0, 0w0, 0wx8) => ("lsl", "w")
                    |   (0w1, 0w0, 0wx9) => ("lsr", "x")
                    |   (0w0, 0w0, 0wx9) => ("lsr", "w")
                    |   (0w1, 0w0, 0wxa) => ("asr", "x")
                    |   (0w0, 0w0, 0wxa) => ("asr", "w")
                    |   _ => ("??", "?")
            in
                printStream oper;
                printStream "\t";
                printStream r; printStream(Word32.fmt StringCvt.DEC rD); printStream ",";
                printStream r; printStream(Word32.fmt StringCvt.DEC rN); printStream ",";
                printStream r; printStream(Word32.fmt StringCvt.DEC rM)
            end

            else if (wordValue andb 0wx1f000000) = 0wx1b000000
            then (* Three source operations - multiply add/subtract. *)
            let
                val sf = wordValue >> 0w31
                val op54 = (wordValue >> 0w29) andb 0w3
                val op31 = (wordValue >> 0w21) andb 0w7
                val o0 = (wordValue >> 0w15) andb 0w1
                val rM = (wordValue >> 0w16) andb 0wx1f
                val rA = (wordValue >> 0w10) andb 0wx1f
                val rN = (wordValue >> 0w5) andb 0wx1f
                val rD = wordValue andb 0wx1f
                val (oper, r1, r2) =
                    case (sf, op54, op31, o0, rA) of
                        (0w1, 0w0, 0w0, 0w0, 0w31) => ("mul", "x", "x")
                    |   (0w1, 0w0, 0w0, 0w0, _)    => ("madd", "x", "x")
                    |   (0w1, 0w0, 0w0, 0w1, 0w31) => ("mneg", "x", "x")
                    |   (0w1, 0w0, 0w0, 0w1, _)    => ("msub", "x", "x")
                    |   (0w0, 0w0, 0w0, 0w0, _)    => ("madd", "w", "w")
                    |   (0w0, 0w0, 0w0, 0w1, _)    => ("msub", "w", "w")
                    |   (0w1, 0w0, 0w2, 0w0, 0w31) => ("smulh", "x", "x")
                    |   (0w1, 0w0, 0w1, 0w0, 0w31) => ("smull", "x", "w")
                    |   (0w1, 0w0, 0w1, 0w0, _)    => ("smaddl", "x", "w")
                    |   (0w1, 0w0, 0w1, 0w1, _)    => ("smsubl", "x", "w")
                    |   _ => ("??", "?", "?")
            in
                printStream oper;
                printStream "\t";
                printStream r1; printStream(Word32.fmt StringCvt.DEC rD); printStream ",";
                printStream r2; printStream(Word32.fmt StringCvt.DEC rN); printStream ",";
                printStream r2; printStream(Word32.fmt StringCvt.DEC rM);
                if rA = 0w31 then ()
                else (printStream ","; printStream r1; printStream(Word32.fmt StringCvt.DEC rA))
            end

            else if (wordValue andb 0wx7f20fc00) = 0wx1E200000
            then (* Moves between floating point and general regs. *)
            let
                val sf = (wordValue >> 0w31) andb 0w1
                and s = (wordValue >> 0w29) andb 0w1
                and ptype = (wordValue >> 0w22) andb 0w3
                and mode = (wordValue >> 0w19) andb 0w3
                and opcode = (wordValue >> 0w16) andb 0w7
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rD = wordValue andb 0wx1f
                val (opc, dr, nr) =
                    case (sf, s, ptype, mode, opcode) of
                        (0w0, 0w0, 0w0, 0w0, 0w7) => ("fmov", "s", "w") (* w -> s *)
                    |   (0w0, 0w0, 0w0, 0w0, 0w6) => ("fmov", "w", "s") (* s -> w *)
                    |   (0w1, 0w0, 0w1, 0w0, 0w7) => ("fmov", "d", "x") (* d -> x *)
                    |   (0w1, 0w0, 0w1, 0w0, 0w6) => ("fmov", "x", "d") (* x -> d *)
                    |   (0w0, 0w0, 0w0, 0w0, 0w2) => ("scvtf", "w", "s")
                    |   (0w0, 0w0, 0w1, 0w0, 0w2) => ("scvtf", "w", "d")
                    |   (0w1, 0w0, 0w0, 0w0, 0w2) => ("scvtf", "x", "s")
                    |   (0w1, 0w0, 0w1, 0w0, 0w2) => ("scvtf", "x", "d")

                    |   (0w0, 0w0, 0w0, 0w0, 0w4) => ("fcvtas", "w", "s") (* s -> w *)
                    |   (0w0, 0w0, 0w0, 0w2, 0w0) => ("fcvtms", "w", "s") (* s -> w *)
                    |   (0w0, 0w0, 0w0, 0w1, 0w0) => ("fcvtps", "w", "s") (* s -> w *)
                    |   (0w0, 0w0, 0w0, 0w3, 0w0) => ("fcvtzs", "w", "s") (* s -> w *)
                    |   (0w0, 0w0, 0w1, 0w0, 0w4) => ("fcvtas", "w", "d") (* d -> w *)
                    |   (0w0, 0w0, 0w1, 0w2, 0w0) => ("fcvtms", "w", "d") (* d -> w *)
                    |   (0w0, 0w0, 0w1, 0w1, 0w0) => ("fcvtps", "w", "d") (* d -> w *)
                    |   (0w0, 0w0, 0w1, 0w3, 0w0) => ("fcvtzs", "w", "d") (* d -> w *)

                    |   (0w1, 0w0, 0w0, 0w0, 0w4) => ("fcvtas", "x", "s") (* s -> x *)
                    |   (0w1, 0w0, 0w0, 0w2, 0w0) => ("fcvtms", "x", "s") (* s -> x *)
                    |   (0w1, 0w0, 0w0, 0w1, 0w0) => ("fcvtps", "x", "s") (* s -> x *)
                    |   (0w1, 0w0, 0w0, 0w3, 0w0) => ("fcvtzs", "x", "s") (* s -> x *)
                    |   (0w1, 0w0, 0w1, 0w0, 0w4) => ("fcvtas", "x", "d") (* d -> x *)
                    |   (0w1, 0w0, 0w1, 0w2, 0w0) => ("fcvtms", "x", "d") (* d -> x *)
                    |   (0w1, 0w0, 0w1, 0w1, 0w0) => ("fcvtps", "x", "d") (* d -> x *)
                    |   (0w1, 0w0, 0w1, 0w3, 0w0) => ("fcvtzs", "x", "d") (* d -> x *)
                    |   _ => ("?", "?", "?")
            in
                printStream opc; printStream "\t";
                printStream dr; printStream(Word32.fmt StringCvt.DEC rD); printStream ",";
                printStream nr; printStream(Word32.fmt StringCvt.DEC rN)
            end
            
            else if (wordValue andb 0wxff200c00) = 0wx1E200800
            then (* Floating point two source operations. *)
            let
                val pt = (wordValue >> 0w22) andb 0w3
                and rM = (wordValue >> 0w16) andb 0wx1f
                and opc = (wordValue >> 0w12) andb 0wxf
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rT = wordValue andb 0wx1f
                val (opcode, r) =
                    case (pt, opc) of
                        (0w0, 0wx0) => ("fmul", "s")
                    |   (0w0, 0wx1) => ("fdiv", "s")
                    |   (0w0, 0wx2) => ("fadd", "s")
                    |   (0w0, 0wx3) => ("fsub", "s")
                    |   (0w1, 0wx0) => ("fmul", "d")
                    |   (0w1, 0wx1) => ("fdiv", "d")
                    |   (0w1, 0wx2) => ("fadd", "d")
                    |   (0w1, 0wx3) => ("fsub", "d")
                    |   _ => ("??", "?")
            in
                printStream opcode; printStream "\t";
                printStream r; printStream(Word32.fmt StringCvt.DEC rT); printStream ",";
                printStream r; printStream(Word32.fmt StringCvt.DEC rN); printStream ",";
                printStream r; printStream(Word32.fmt StringCvt.DEC rM)
            end

            else if (wordValue andb 0wxff207c00) = 0wx1E204000
            then (* Floating point single source. *)
            let
                val pt = (wordValue >> 0w22) andb 0w3
                and opc = (wordValue >> 0w15) andb 0wx3f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and rT = wordValue andb 0wx1f
                val (opcode, rS, rD) =
                    case (pt, opc) of
                        (0w0, 0wx0) => ("fmov", "s", "s")
                    |   (0w0, 0wx1) => ("fabs", "s", "s")
                    |   (0w0, 0wx2) => ("fneg", "s", "s")
                    |   (0w0, 0wx5) => ("fcvt", "s", "d")
                    |   (0w1, 0wx0) => ("fmov", "d", "d")
                    |   (0w1, 0wx1) => ("fabs", "d", "d")
                    |   (0w1, 0wx2) => ("fneg", "d", "d")
                    |   (0w1, 0wx4) => ("fcvt", "d", "s")
                    |   _ => ("??", "?", "?")
            in
                printStream opcode; printStream "\t";
                printStream rD; printStream(Word32.fmt StringCvt.DEC rT); printStream ",";
                printStream rS; printStream(Word32.fmt StringCvt.DEC rN)
            end

            else if (wordValue andb 0wxff20fc07) = 0wx1E202000
            then (* Floating point comparison *)
            let
                val pt = (wordValue >> 0w22) andb 0w3
                and rM = (wordValue >> 0w16) andb 0wx1f
                and rN = (wordValue >> 0w5) andb 0wx1f
                and opc = (wordValue >> 0w3) andb 0w3
                val (opcode, r) =
                    case (pt, opc) of
                        (0w0, 0wx0) => ("fcmp", "s")
                    |   (0w1, 0wx0) => ("fcmp", "d")
                    |   (0w0, 0wx2) => ("fcmpe", "s")
                    |   (0w1, 0wx2) => ("fcmpe", "d")
                    |   _ => ("??", "?")
            in
                printStream opcode; printStream "\t";
                printStream r; printStream(Word32.fmt StringCvt.DEC rN); printStream ",";
                printStream r; printStream(Word32.fmt StringCvt.DEC rM)
            end

            else if (wordValue andb 0wxffffffe0) = 0wx2F00E400
            then (* movi dn,#0 *)
            let
                val rD = wordValue andb 0wx1f
            in
                printStream "movi\td"; printStream(Word32.fmt StringCvt.DEC rD);
                printStream ",#0"
            end

            else if (wordValue andb 0wx1e000000) = 0wx02000000
            then (* This is an unallocated range.  We use it for the register mask. *)
            let
                fun printMask (0w25, _) = ()
                |   printMask (i, comma) =
                    if ((0w1 << i) andb wordValue) <> 0w0
                    then
                    (
                        if comma then printStream ", " else ();
                        printStream "x";
                        printStream(Word.fmt StringCvt.DEC i);
                        printMask(i+0w1, true)
                    )
                    else printMask(i+0w1, comma)
            in
                printStream "["; printMask(0w0, false); printStream "]"
            end

            else printStream "?"
            ;
            printStream "\n"
        end
        
        fun printAll i =
            if i = numInstructions then ()
            else (printWordAt i; printAll(i+0w1))
    in
        printStream functionName;
        printStream ":\n";
        printAll 0w0
    end

    (* Set the offsets of ADRP+LDR and ADRP+ADD instruction pairs.  The values in these instructions are,
       to some extent, absolute addresses so this needs to be done by the RTS. firstNonAddrConst and
       firstAddrConst are the offsets in bytes. *)
    fun setADRPAddresses(ops, codeVec, firstNonAddrConst, firstAddrConst, addrConstMap, nonAddrConstMap) =
    let
        fun setADRPAddrs([], _ , _, _) = ()

        |   setADRPAddrs(LoadAddressLiteral{length=ref BrExtended, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                (* Address constants are 32-bits in 32-in-64 and 64-bits in native 64-bits *)
                val constPos = Array.sub(addrConstMap, aConstNum)
                val addrOfConstant (* byte offset *) = firstAddrConst + constPos * Address.wordSize
            in
                codeVecPutConstant (codeVec, wordNo * 0w4, toMachineWord addrOfConstant,
                    case archType of ArchC32 _ => ConstArm64AdrpLdr32 | ArchNative => ConstArm64AdrpLdr64);
                setADRPAddrs(tail, wordNo+0w2, aConstNum+1, nonAConstNum)
            end

        |   setADRPAddrs(LoadNonAddressLiteral{length=ref BrExtended, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                (* The offset is in 32-bit words.  These are always 64-bits. *)
                val constPos = Array.sub(nonAddrConstMap, nonAConstNum)
                val offsetOfConstant (* byte offset *) = firstNonAddrConst+constPos*0w8
            in
                codeVecPutConstant (codeVec, wordNo * 0w4, toMachineWord offsetOfConstant, ConstArm64AdrpLdr64);
                setADRPAddrs(tail, wordNo+0w2, aConstNum, nonAConstNum+1)
            end

        |   setADRPAddrs(LoadFPLiteral{length=ref BrExtended, isDouble, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                (* The offset is in 32-bit words and the constants themselves are always 64-bits.  If
                   we're loading a 32-bit float we have to use 32-bit offsets. *)
                val constPos = Array.sub(nonAddrConstMap, nonAConstNum)
                val offsetOfConstant (* byte offset *) = firstNonAddrConst+constPos*0w8
            in
                codeVecPutConstant (codeVec, wordNo * 0w4, toMachineWord offsetOfConstant,
                                            if isDouble then ConstArm64AdrpLdr64 else ConstArm64AdrpLdr32);
                setADRPAddrs(tail, wordNo+0w2, aConstNum, nonAConstNum+1)
            end

        |   setADRPAddrs(LoadLabelAddress{label=ref labs, length=ref BrExtended, ...} :: tail, wordNo, aConstNum, nonAConstNum) =
            let
                val dest = !(hd labs) * 0w4
            in
                codeVecPutConstant (codeVec, wordNo * 0w4, toMachineWord dest, ConstArm64AdrpAdd);
                setADRPAddrs(tail, wordNo+0w2, aConstNum, nonAConstNum)
            end

       |    setADRPAddrs(instr :: tail, wordNo, aConstNum, nonAConstNum) =
                setADRPAddrs(tail, wordNo+Word.fromInt(codeSize instr), aConstNum, nonAConstNum)
    in
        setADRPAddrs (ops, 0w0, 0, 0)
    end

    (* Although this is used locally it must be defined at the top level
       otherwise a new RTS function will be compiled every time the
       containing function is called *)
    val sortFunction: (machineWord * int) array -> bool =
        RunCall.rtsCallFast1 "PolySortArrayOfAddresses"

    (* Adds the constants onto the code, and copies the code into a new segment *)
    fun generateCode {instrs, name=functionName, parameters, resultClosure, profileObject} =
    let
        val printStream = Pretty.getSimplePrinter(parameters, [])
        and printAssemblyCode = Debug.getParameter Debug.assemblyCodeTag parameters
        
        local
            (* Extract the constants. *)
            fun getConsts(LoadAddressLiteral {value, ...}, (addrs, nonAddrs, addrCount, nonAddrCount)) =
                    ((value, addrCount)::addrs, nonAddrs, addrCount+1, nonAddrCount)
            |   getConsts(LoadNonAddressLiteral {value, ...}, (addrs, nonAddrs, addrCount, nonAddrCount)) =
                    (addrs, (value, nonAddrCount)::nonAddrs, addrCount, nonAddrCount+1)
            |   getConsts(LoadFPLiteral {value, isDouble, ...}, (addrs, nonAddrs, addrCount, nonAddrCount)) =
                let
                    (* When loading a float we will only access the first 32-bits so if this is
                       big-endian we have to shift the value so it's there. *)
                    val shifted = if not isDouble andalso isBigEndian then LargeWord.<<(value, 0w32) else value
                in
                    (addrs, (shifted, nonAddrCount)::nonAddrs, addrCount, nonAddrCount+1)
                end
            |   getConsts(_, consts) = consts

            val (addressConstants, nonAddressConstants, addrConstCount, nonAddrConstCount) =
                List.foldl getConsts ([], [], 0, 0) instrs

            (* Sort the non-address constants to remove duplicates.  There don't seem to be
               many in practice.
               Since we're not actually interested in the order but only
               sorting to remove duplicates we can use a stripped-down Quicksort. *)
            fun sort([], out) = out
            |   sort((median, addr) :: tl, out) = partition(median, tl, [addr], [], [], out)

            and partition(median, [], addrs, less, greater, out) =
                    sort(less, sort(greater, (median, addrs) :: out))
            |   partition(median, (entry as (value, addr)) :: tl, addrs, less, greater, out) =
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
                    |   findDups((value, addr) :: tl, out) =
                        let
                            fun partition(e as (v, a), (eq, neq)) =
                                if PolyML.pointerEq(value, v)
                                then (a :: eq, neq)
                                else (eq, e :: neq)
                            val (eqAddr, neq) = List.foldl partition ([addr], []) tl
                        in
                            findDups(neq, (value, eqAddr) :: out)
                        end
                in
                    findDups(addressConstants, [])
                end
                else
                let
                    val arrayToSort = Array.fromList addressConstants
                    val _ = sortFunction arrayToSort
                    
                    fun makeList((v, a), []) = [(v, [a])]
                    |   makeList((v, a), l as (vv, aa) :: tl) =
                        if PolyML.pointerEq(v, vv)
                        then (vv, a :: aa) :: tl
                        else (v, [a]) :: l
                in
                    (Array.foldl makeList [] arrayToSort)
                end
        in
            val addressConsts = sortedConstants
            and nonAddressConsts = sort(nonAddressConstants, []) : (Word64.word * int list) list
            and addrConstCount = addrConstCount
            and nonAddrConstCount = nonAddrConstCount
        end

        (* Create maps that indicate for each constant where it is in the constant area. *)
        val addrConstMap = Array.array(addrConstCount, 0w0) and nonAddrConstMap = Array.array(nonAddrConstCount, 0w0)
        val _ = List.foldl(fn ((_, cnums), n) => (List.app(fn i => Array.update(addrConstMap, i, n)) cnums; n+0w1)) 0w0 addressConsts
        val _ = List.foldl(fn ((_, cnums), n) => (List.app(fn i => Array.update(nonAddrConstMap, i, n)) cnums; n+0w1)) 0w0 nonAddressConsts
        
        (* Generate the code and set the constant addresses at the same time. *)
        val (byteVec, nativeWordsOfCode) =
            genCode(instrs, List.map #1 addressConsts, List.map #1 nonAddressConsts, addrConstMap, nonAddrConstMap)

        val wordsOfCode = nativeWordsOfCode * wordsPerNativeWord

        (* +3 for profile count, function name and constants count *)
        val numOfConst = List.length addressConsts
        val segSize   = wordsOfCode + Word.fromInt numOfConst + 0w4
        val firstConstant = wordsOfCode + 0w3 (* Add 3 for no of consts, fn name and profile count. *)
    
        (* Put in the number of constants. This must go in before
           we actually put in any constants. *)
        local
            val lastWord = segSize - 0w1
        in
            val () = setWord(LargeWord.fromInt(numOfConst + 2), wordsOfCode, byteVec)
            (* Set the last word of the code to the (negative) byte offset of the start of the code area
               from the end of this word. *)
            val () = setWord(LargeWord.fromInt(numOfConst + 3) * ~(Word.toLarge Address.wordSize), lastWord, byteVec) 
        end

        (* Now we've filled in all the size info we need to convert the segment
           into a proper code segment before it's safe to put in any ML values. *)
        val codeVec = byteVecToCodeVec(byteVec, resultClosure)

        local
            val name     : string = functionName
            val nameWord : machineWord = toMachineWord name
        in
            val () = codeVecPutWord (codeVec, wordsOfCode+0w1, nameWord)
        end
        (* Profile ref.  A byte ref used by the profiler in the RTS. *)
        val () = codeVecPutWord (codeVec, wordsOfCode+0w2, profileObject)

        (* and then copy the constants from the constant list. *)
        local
            fun setConstant((value, _), num) =
            (
                codeVecPutWord (codeVec, firstConstant + num, value);
                num+0w1
            )
        in
            val _ = List.foldl setConstant 0w0 addressConsts
        end
        
        val () = setADRPAddresses(instrs, codeVec,
                    (nativeWordsOfCode-Word.fromInt(List.length nonAddressConsts)) * Address.nativeWordSize,
                    firstConstant * Address.wordSize, addrConstMap, nonAddrConstMap)
    in
        if printAssemblyCode
        then (* print out the code *)
            (printCode (codeVec, functionName, wordsOfCode, printStream); printStream"\n")
        else ();
        codeVecLock(codeVec, resultClosure)
    end (* copyCode *)


    structure Sharing =
    struct
        type closureRef = closureRef
        type instr = instr
        type xReg = xReg
        type vReg = vReg
        type labels = labels
        type condition = condition
        type shiftType = shiftType
        type wordSize = wordSize
        type 'a extend = 'a extend
        type scale = scale
    end
end;

