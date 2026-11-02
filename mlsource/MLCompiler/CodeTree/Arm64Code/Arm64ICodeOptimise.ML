(*
    Copyright David C. J. Matthews 2021-2, 2026

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

functor Arm64ICodeOptimise(
    structure Arm64ICode: ARM64ICODE
    structure IntSet: INTSET
    structure Identify: ARM64IDENTIFYREFERENCES
    structure Debug: DEBUG
    structure Pretty: PRETTY
    sharing Arm64ICode.Sharing = Identify.Sharing = IntSet = Arm64ICode
): ARM64ICODEOPTIMISE =
struct
    open Arm64ICode
    open IntSet
    open Identify

    datatype optimise =
        Changed of (preg, pregOrZero, preg) basicBlock vector * regProperty vector | Unchanged
    
    exception InternalError = Misc.InternalError
    
    (* Optimiser.
       This could incorporate optimisations done elsewhere.
       IdentifyReferences currently removes instructions that
       produce results in registers that are never used.
       AllocateRegisters deals generally with caching.

       The optimiser currently deals with booleans and conditions and with
       constants.
    *)
    
    (* This is a rewrite of the last instruction to set a boolean.
       This is almost always rewriting the next instruction.  The only
       possibility is that we have a ResetStackPtr in between. *)
    datatype boolRegRewrite =
        BRNone
        (* BRSetConditionToConstant - we have a comparison of two constant value.
           This will usually happen because we've duplicated a branch and
           set a register to a constant which we then compare. *)
    |   BRSetConditionToConstant of { srcCC: ccRef, test: condition -> bool }

    fun optimiseICode{ code, pregProps, ccCount=_, debugSwitches=_ } =
    let
        val hasChanged = ref false
        val pregPropSize = Vector.length pregProps
        val regCounter = ref pregPropSize (* Allocate new registers after the old. *)
        val regList = ref [] (* New properties are added in reverse order. *)
        fun newReg kind =
        (
            regList := kind :: ! regList;
            PReg (!regCounter)
        ) before regCounter := !regCounter + 1

        (* Constant values in registers. *)
        datatype regValue =
            SomeValue
        |   NonAddressConst of Word64.word
        |   AddressConstant of machineWord
        |   LargeBox of preg
        |   RealBox of preg * floatSize
        |   TaggedValue of preg * bool * opSize

        (* If this argument is a register and the register is mapped to a constant
           or another register replace the value.  Unlike the X86 version we
           don't map memory locations but we do map registers. *)
        (* TODO: This is potentially quadratic if we have a long piece
           of code with very many registers. *)
        fun getRegisterValue(preg as PReg pregNo, kill, regMap) =
        (
            case List.find(fn {dest, ... } => dest = preg) regMap of
                SOME { source, ...} =>
                (
                    source,
                    (* Filter it if it is the last reference. *)
                    if member(pregNo, kill)
                    then List.filter(fn {dest, ...} => dest <> preg) regMap
                    else regMap
                )
            |   NONE => (SomeValue, regMap)
        )

        fun optimiseBlock processed (block, flow, outCCState) =
        let
            fun optCode([], brCond, regMap, code, changed) = (code, brCond, regMap, changed)

            |   optCode({instr as AddSubImmediate{source, dest=ZeroReg, ccRef=SOME ccRefOut, immed, isAdd=false, length}, kill, ...} :: rest,
                        _, regMap, code, changed) =
                let
                    val (repArg1, memRefsOut) = getRegisterValue(source, kill, regMap)
                in
                    case repArg1 of
                        NonAddressConst test =>
                        (* AddSubImmediate is put in by CodetreeToIcode to test a boolean value.  It can also
                           arise as the result of pattern matching on booleans or even by tests such as = true.
                           If the source register is now a constant we want to propagate the constant
                           condition. *)
                        let
                            (* This comparison reduces to a constant.  *)
                            val _ = hasChanged := true
                            (* Signed comparison.  If this is a 32-bit operation the top word
                               could be zero so we need to convert this as Word32.
                               immediate values are always unsigned. *)
                            val testValue =
                                case length of
                                    OpSize64 => Word64.toLargeIntX test
                                |   OpSize32 => Word32.toLargeIntX(Word32.fromLarge test)
                            (* Put in a replacement so that if we were previously testing ccRefOut
                               we should instead test ccRef. *)
                            val signedCompare=LargeInt.compare(testValue, Word.toLargeInt immed)
                            and unsignedCompare=Word64.compare(test, Word64.fromLarge(Word.toLargeWord immed))
                            fun testResult condition =
                                case (condition, signedCompare, unsignedCompare) of
                                    (CondEqual,             EQUAL,  _)   => true
                                |   (CondEqual,             _,      _)   => false
                                |   (CondNotEqual,          EQUAL,  _)   => false
                                |   (CondNotEqual,          _,      _)   => true
                                |   (CondSignedLess,        LESS,   _)   => true
                                |   (CondSignedLess,        _,      _)   => false
                                |   (CondSignedGreater,     GREATER,_)   => true
                                |   (CondSignedGreater,     _,      _)   => false
                                |   (CondSignedLessEq,      GREATER,_)   => false
                                |   (CondSignedLessEq,      _,      _)   => true
                                |   (CondSignedGreaterEq,   LESS,   _)   => false
                                |   (CondSignedGreaterEq,   _,      _)   => true
                                |   (CondCarryClear,        _, LESS  )   => true
                                |   (CondCarryClear,        _,      _)   => false
                                |   (CondUnsignedHigher,    _,GREATER)   => true
                                |   (CondUnsignedHigher,    _,      _)   => false
                                |   (CondUnsignedLowOrEq,    _,GREATER)   => false
                                |   (CondUnsignedLowOrEq,    _,      _)   => true
                                |   (CondCarrySet,          _, LESS  )   => false
                                |   (CondCarrySet,          _,      _)   => true
                                    (* The overflow and parity checks should never occur. *)
                                |   _   => raise InternalError "getCondResult: comparison"

                            val repl = BRSetConditionToConstant{srcCC=ccRefOut, test=testResult}
                            val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                        in
                            optCode(rest, repl, memRefsOut, code, true)
                        end
                
                    |   _ => optCode(rest, BRNone, memRefsOut, instr :: code, changed)
                end

            |   optCode({instr as AddSubImmediate{source, dest=SomeReg dest, ccRef=NONE, immed, isAdd, length}, kill, ...} :: rest, _, regMap, code, changed) =
                (* This is frequently used to remove a tag from a value before an addition or subtraction.
                   If it's a constant we want to do that now. *)
                let
                    val (repArg1, newMap) = getRegisterValue(source, kill, regMap)
                in
                    case repArg1 of
                        NonAddressConst cVal =>
                        let
                            val addSub = if isAdd then cVal + Word.toLarge immed else cVal - Word.toLarge immed
                            (* Mask the result to 32-bits if this is a 32-bit operation. *)
                            val result =
                                case length of OpSize32 => Word64.andb(addSub, Word64.<<(0w1, 0w32) - 0w1) | OpSize64 => addSub
                        in
                            optCode(rest, BRNone,
                                {dest=dest, source=NonAddressConst result} :: newMap,
                                    LoadNonAddressConstant{dest=dest, source=result} :: code, true)
                        end

                    |   _ => optCode(rest, BRNone, newMap, instr ::code, changed)
                end

            |   optCode({instr as AddSubRegister{base, shifted, dest, ccRef, isAdd, length, shift}, kill, ...} :: rest,
                        _, regMap, code, changed) =
                let
                    (* If we have a constant as the second argument we can change this to the
                       immediate form. *)
                    val (repOp1, mapOp1) = getRegisterValue(base, kill, regMap)
                    val (repOp2, mapOp2) = getRegisterValue(shifted, kill, mapOp1)
                    val regAndImmed =
                        case (repOp1, repOp2, isAdd, shift) of
                            (_, NonAddressConst immed, _, ShiftNone) => if immed < 0w4096 then SOME(base, immed) else NONE
                            (* If this is an ADD we can also use the immediate if the first arg is a constant. *)
                        |   (NonAddressConst immed, _, true, ShiftNone) => if immed < 0w4096 then SOME(shifted, immed) else NONE
                        |   _ => NONE
                in
                    case regAndImmed of
                        SOME(srcReg, immediate) =>
                                optCode(rest, BRNone, mapOp2,
                                AddSubImmediate{source=srcReg, dest=dest, ccRef=ccRef,
                                    immed=Word.fromLargeWord(Word64.toLargeWord immediate), isAdd=isAdd, length=length} :: code, true)
                    |   NONE =>
                            optCode(rest, BRNone, mapOp2, instr :: code, changed)
                end

            |   optCode({instr as LogicalImmediate{source, dest=ZeroReg, ccRef=SOME ccRefOut, immed, logOp=LogAnd, ...}, kill, ...} :: rest,
                        _, regMap, code, changed) =
                let
                    val (repArg1, memRefsOut) = getRegisterValue(source, kill, regMap)
                in
                    case repArg1 of
                        NonAddressConst test =>
                        (* This was previously used to test a boolean but that has now changed.  It may also
                           occur as a result of pattern matching. *)
                        let
                            (* This comparison reduces to a constant.  *)
                            val _ = hasChanged := true
                            val result = LargeWord.andb(test, immed)
                            (* Put in a replacement so that if we were previously testing ccRefOut
                               we should instead test ccRef. *)
                            fun testResult CondEqual = result = 0w0
                            |   testResult CondNotEqual = result <> 0w0
                            |   testResult _ = raise InternalError "testResult: invalid condition"
                            val repl =
                                BRSetConditionToConstant{srcCC=ccRefOut, test=testResult}
                            val _ = isSome outCCState andalso raise InternalError "optCode: CC exported"
                        in
                            optCode(rest, repl, memRefsOut, code, true)
                        end
                
                    |   _ => optCode(rest, BRNone, memRefsOut, instr :: code, changed)
                end

            |   optCode({instr as LogicalRegister{base, shifted, dest, ccRef, logOp, length, shift}, kill, ...} :: rest,
                        _, regMap, code, changed) =
                let
                    (* If we have a constant as the second argument we can change this to the
                       immediate form. *)
                    val (repOp1, mapOp1) = getRegisterValue(base, kill, regMap)
                    val (repOp2, mapOp2) = getRegisterValue(shifted, kill, mapOp1)
                    val regAndImmed =
                        case (repOp1, repOp2, shift) of
                            (_, NonAddressConst immed, ShiftNone) =>
                                if isEncodableBitPattern (immed, length) then SOME(base, immed) else NONE
                        |   (NonAddressConst immed, _, ShiftNone) =>
                                if isEncodableBitPattern (immed, length) then SOME(shifted, immed) else NONE
                        |   _ => NONE
                in
                    case regAndImmed of
                        SOME(srcReg, immediate) =>
                                optCode(rest, BRNone, mapOp2,
                                LogicalImmediate{source=srcReg, dest=dest, ccRef=ccRef,
                                    immed=immediate, logOp=logOp, length=length} :: code, true)
                    |   NONE =>
                            optCode(rest, BRNone, mapOp2, instr :: code, changed)
                end

            |   optCode({instr as MoveRegister{dest, source}, kill, ...} :: rest, inCond, regMap, code, changed) =
                let
                    val (repSource, mapAfterReplace) = getRegisterValue(source, kill, regMap)
                    (* If the source is mapped to a constant we can set the destination to
                       the same constant. *)
                    val newMap =
                        case repSource of
                            SomeValue => mapAfterReplace
                        |   _ => {dest=dest, source=repSource} :: mapAfterReplace
                in
                    optCode(rest, inCond, newMap, instr :: code, changed)
                end

            |   optCode({instr as LoadNonAddressConstant{dest, source}, ...} :: rest, inCond, regMap, code, changed) =
                let
                    (* If we already have a register with this constant we would probably
                       be better off reusing it.  The map, though, needs to indicate that the
                       destination register contains the constant.
                       The X86 version always uses a load-constant here. *)
                    val newInstr =
                        case List.find(fn {source=NonAddressConst c, ... } => c = source | _ => false) regMap of
                            SOME{dest=cDest, ...} => MoveRegister{dest=dest, source=cDest}
                        |   NONE => instr
                in
                    optCode(rest, inCond, {dest=dest, source=NonAddressConst source} :: regMap, newInstr :: code, changed)
                end

            |   optCode({instr as LoadAddressConstant{dest, source}, ...} :: rest, inCond, regMap, code, changed) =
                    (* Address constant.  This is used in conjunction with UnboxValue *)
                    optCode(rest, inCond, {dest=dest, source=AddressConstant source} :: regMap, instr :: code, changed)

            |   optCode({instr as BoxLarge{ source, dest, ... }, ...} :: rest, inCond, regMap, code, changed) =
                (* Try to eliminate adjacent sequences of boxing and unboxing.  *)
                    optCode(rest, inCond, {dest=dest, source=LargeBox source} :: regMap, instr :: code, changed)

            |   optCode({instr as BoxTagFloat{ source, dest, floatSize, ... }, ...} :: rest, inCond, regMap, code, changed) =
                    optCode(rest, inCond, {dest=dest, source=RealBox(source, floatSize)} :: regMap, instr :: code, changed)

            |   optCode({instr as TagValue{ source, dest, isSigned, opSize}, ...} :: rest, inCond, regMap, code, changed) =
                    optCode(rest, inCond, {dest=dest, source=TaggedValue(source, isSigned, opSize)} :: regMap, instr :: code, changed)

            |   optCode({instr as UnboxLarge{ source, dest, ... }, kill, ...} :: rest,
                        inCond, regMap, code, changed) =
                let
                    val (repSource, mapAfterReplace) = getRegisterValue(source, kill, regMap)
                in
                    (* If we're unboxing a constant address we can unpack it now.  This is
                       intended to handle constant aruments to LargeWord operations. *)
                    case repSource of
                        AddressConstant cVal =>
                        let
                            (* Check this looks like a boxed large word. *)
                            open Address
                            val addr = toAddress cVal
                            val _ = isBytes addr andalso length addr = 0w8 div wordSize
                            val wordConst: LargeWord.word = RunCall.unsafeCast cVal
                            val newMap = {dest=dest, source=NonAddressConst wordConst} :: mapAfterReplace
                        in
                            optCode(rest, inCond, newMap, LoadNonAddressConstant{dest=dest, source=wordConst} :: code, true)
                        end

                        (* Or if it was recently boxed we can use the original and hopefully eliminate the box. *)
                    |   LargeBox original => optCode(rest, inCond, mapAfterReplace, MoveRegister{dest=dest, source=original} :: code, true)

                    |   _ => optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                end

            |   optCode({instr as UntagValue{ source, dest, isSigned, opSize }, kill, ...} :: rest, inCond, regMap, code, changed) =
                (* If we're untagging a constant we can produce the result now. *)
                let
                    val (repSource, mapAfterReplace) = getRegisterValue(source, kill, regMap)
                in
                    case repSource of
                        NonAddressConst cVal =>
                        let
                            (* The result depends on the kind of untagging.  Unsigned values
                               can just be left shifted but signed values need to be treated
                               differently for 32-bit and 64-bit operations. *)
                            val wordConst =
                                case (isSigned, opSize) of
                                    (false, _) => LargeWord.>>(cVal, 0w1)
                                |   (true, OpSize64) => LargeWord.~>>(cVal, 0w1)
                                |   (true, OpSize32) => Word32.toLarge(Word32.~>>(Word32.fromLarge cVal, 0w1))
                            val newMap = {dest=dest, source=NonAddressConst wordConst} :: mapAfterReplace
                        in
                            optCode(rest, inCond, newMap, LoadNonAddressConstant{dest=dest, source=wordConst} :: code, true)
                        end

                        (* Or if it was recently tagged we can eliminate the tagging and untagging.  This occurs, for
                           example, if we extract a character and then put it into a string.  Normally the lengths
                           will match but may use a 32-bit tagged value as a 64-bit value in the special case of
                           using a character to index into the array of single-character strings.  Mismatched
                           signed/unsigned could occur with conversions between Word.word and FixedInt.int
                           which are generally no-ops. *)
                    |   TaggedValue(original, wasSigned, oldOpSize) =>
                            if isSigned = wasSigned andalso (case (oldOpSize, opSize) of (OpSize64, OpSize32) => false | _ => true)
                            then optCode(rest, inCond, mapAfterReplace, MoveRegister{dest=dest, source=original} :: code, true)
                            else optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                    
                    |   _ => optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                end

            |   optCode({instr as UnboxTagFloat{ source, dest, floatSize=Float32 }, kill, ...} :: rest, inCond, regMap, code, changed) =
                let
                    val (repSource, mapAfterReplace) = getRegisterValue(source, kill, regMap)
                    val is32in64 = case archType of ArchC32 _ => true | ArchNative => false
                in
                    case repSource of
                        NonAddressConst cVal => (* Should only be Float32 in native 64-bits. *)
                        let
                            val _  = not is32in64 andalso LargeWord.andb(cVal, 0wxffffffff) = 0w1
                                        orelse raise InternalError "incorrect FP constant form"
                            val fpConstant = LargeWord.>>(cVal, 0w32)
                        in
                            optCode(rest, inCond, mapAfterReplace, LoadFPConstant{dest=dest, floatSize=Float32, source=fpConstant} :: code, true)
                        end

                    |   AddressConstant cVal =>
                        let
                            open Address
                            val addr = toAddress cVal
                            val _ = is32in64 andalso length addr = 0w1 andalso flags addr = F_bytes
                                orelse raise InternalError "incorrect FP constant form"
                            val fpConstant = RunCall.loadPolyWord(addr, 0w0)
                        in
                            optCode(rest, inCond, mapAfterReplace, LoadFPConstant{dest=dest, floatSize=Float32, source=fpConstant} :: code, true)
                        end

                    |   RealBox(original, fSize) =>
                        (
                            fSize = Float32 orelse raise InternalError "Mismatch float size";
                            optCode(rest, inCond, mapAfterReplace, UnaryFloatingPt{dest=dest, source=original, fpOp=MoveFloat} :: code, true)
                        )

                    |   _ => optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                end

            |   optCode({instr as UnboxTagFloat{ source, dest, floatSize=Double64 }, kill, ...} :: rest, inCond, regMap, code, changed) =
                let
                    val (repSource, mapAfterReplace) = getRegisterValue(source, kill, regMap)
                in
                    case repSource of
                        AddressConstant cVal =>
                        let
                            open Address
                            val addr = toAddress cVal
                            val _ = length addr = (0w8 div wordSize) andalso flags addr = F_bytes
                                orelse raise InternalError "incorrect FP constant form"
                            val fpConstant = RunCall.loadNativeWord(addr, 0w0)
                        in
                            optCode(rest, inCond, mapAfterReplace, LoadFPConstant{dest=dest, floatSize=Double64, source=fpConstant} :: code, true)
                        end

                    |   RealBox(original, fSize) =>
                        (
                            fSize = Double64 orelse raise InternalError "Mismatch float size";
                            optCode(rest, inCond, mapAfterReplace, UnaryFloatingPt{dest=dest, source=original, fpOp=MoveDouble} :: code, true)
                        )

                    |   _ => optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                end

                (* Some of these are specifically to reduce "ref" to its simplest form since it's generated
                   as a variable-length item. *)

            |   optCode({instr as AllocateMemoryVariable{ size, dest, saveRegs }, kill, ...} :: rest, inCond, regMap, code, changed) =
                (* Turn a variable size allocation into a fixed size allocation if the size is a constant. *)
                let
                    val (repSize, mapAfterReplace) = getRegisterValue(size, kill, regMap)
                in
                    case repSize of
                        NonAddressConst words =>
                        let
                            val wordsRequired =
                                case archType of
                                    ArchC32 shift =>
                                    let
                                        (* Have to round this up to the unit size. *)
                                        val unitSize = Word.toLarge(Word.<<(0w1, shift-0w1))
                                    in
                                        Word64.andb(words+unitSize(* +1 for length -1*), ~ unitSize)
                                    end
                                |   ArchNative => words+0w1
                            val bytesRequired = Word64.fromLarge(Word.toLarge Address.wordSize) * wordsRequired
                        in
                            optCode(rest, inCond, mapAfterReplace,
                                AllocateMemoryFixed{bytesRequired=bytesRequired, dest=dest, saveRegs=saveRegs} :: code, true)
                        end
                    |   _ => optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                end

            |   optCode({instr as InitialiseMem{size, addr, init}, kill, ...} :: rest, inCond, regMap, code, changed) =
                (* If we're initialising "a few" words we're better off unrolling the loop. *)
                let
                    val (repSize, mapAfterReplace) = getRegisterValue(size, kill, regMap)
                in
                    case repSize of
                        NonAddressConst words =>
                            if words <= 0w8
                            then
                            let
                                val nWords = LargeWord.toInt words
                                val loadType = case archType of ArchC32 _ => Load32 | ArchNative => Load64
                                fun unroll(n, l) =
                                    if n = nWords then l
                                    else unroll(n+1, StoreWithConstantOffset{ source=init, base=addr,
                                            byteOffset=n*Word.toInt Address.wordSize,
                                            loadType=loadType } :: l)
                            in
                                optCode(rest, inCond, mapAfterReplace, unroll(0, code), true)
                            end
                            else optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                    |   _ => optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                end

            |   optCode({instr as BitFieldShift{ source, dest, isSigned, length, immr, imms }, kill, ...} :: rest,
                            inCond, regMap, code, changed) =
                (* Bit shift.  Specifically this is used to shift the flags byte to construct a length word.  The flags are
                   frequently a constant.  Unlike BitFieldInsert this sets unused bits to either zero or the sign bit. *)
                let
                    val (repSource, mapAfterReplace) = getRegisterValue(source, kill, regMap)
                in
                    case repSource of
                        NonAddressConst cVal =>
                        let
                            val regSize = case length of OpSize32 => 0w32 | OpSize64 => 0w64
                        in
                            if not isSigned andalso imms + 0w1 = immr
                            then (* Simple left shift: ignore the other cases for the moment. *)
                            let
                                val wordConst64 = Word64.<<(cVal, regSize-immr)
                                val wordConst =
                                    case length of OpSize64 => wordConst64 | OpSize32 => Word64.andb(wordConst64, 0wxffffffff)
                                val newMap = {dest=dest, source=NonAddressConst wordConst} :: mapAfterReplace
                            in
                                optCode(rest, inCond, newMap, LoadNonAddressConstant{dest=dest, source=wordConst} :: code, true)
                            end
                            else optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                        end
                    |   _ => optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                end

            |   optCode({instr as BitFieldInsert{ source, dest, destAsSource, length=_, immr, imms }, kill, ...} :: rest,
                    inCond, regMap, code, changed) =
                (* Bit field insertion.  This is used to insert the length field into the register containing the
                   shifted flags value. *)
                let
                    val (repSource, mapAfterRepSrc) = getRegisterValue(source, kill, regMap)
                    val (repDestAs, mapAfterReplace) = getRegisterValue(destAsSource, kill, mapAfterRepSrc)
                in
                    case (repSource, repDestAs) of
                        (NonAddressConst srcVal, NonAddressConst dstVal) =>
                        if immr = 0w0
                        then (* Insert new bits without shifting. *)
                        let
                            (* We take imms-immr+1 bits from the source. *)
                            val maskSrc = Word64.>>(Word64.notb 0w0, 0w64-(imms+0w1))
                            val maskDst = Word64.notb maskSrc
                            val wordConst = Word64.orb(Word64.andb(dstVal, maskDst), Word64.andb(srcVal, maskSrc))
                            val newMap = {dest=dest, source=NonAddressConst wordConst} :: mapAfterReplace
                        in
                            optCode(rest, inCond, newMap, LoadNonAddressConstant{dest=dest, source=wordConst} :: code, true)
                        end
                        else optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                    |   _ => optCode(rest, inCond, mapAfterReplace, instr :: code, changed)
                end

                (* Clear the cache across a function call.  We would have to push these registers. *)
            |   optCode({instr as FunctionCall _, ...} :: rest, _, _, code, changed) =
                    optCode(rest, BRNone, [], instr::code, changed)                

            |   optCode({instr as BeginLoop, ...} :: rest, _, _, code, changed) =
                    (* Any register value from outside the loop is not valid inside. *)
                    optCode(rest, BRNone, [], instr::code, changed)                

            |   optCode({instr as JumpLoop _, ...} :: rest, _, _, code, changed) =
                    (* Likewise at the end of the loop.  Not sure if this is essential. *)
                    optCode(rest, BRNone, [], instr::code, changed)

                (* CompareByteVectors and BlockMove modify their arguments.  In particular
                   if we have the length as a constant in a register it won't still
                   have that value at the end.
                   TODO: This coult be refined. *)
            |   optCode({instr as CompareByteVectors _, ...} :: rest, _, _, code, changed) =
                    (* Likewise at the end of the loop.  Not sure if this is essential. *)
                    optCode(rest, BRNone, [], instr::code, changed)

            |   optCode({instr as BlockMove _, ...} :: rest, _, _, code, changed) =
                    (* Likewise at the end of the loop.  Not sure if this is essential. *)
                    optCode(rest, BRNone, [], instr::code, changed)

            |   optCode({instr, ...} :: rest, inCond, regMap, code, changed) =
                let
                    (* If this instruction affects the CC the cached SetToCondition will no longer be valid. *)
                    val afterCond =
                        case getInstructionCC instr of
                            CCUnchanged => inCond
                        |   _ => BRNone
                in
                    optCode(rest, afterCond, regMap, instr::code, changed)
                end

            val (blkCode, finalRepl, finalMap, blockChanged) = optCode(block, BRNone, [], processed, false)
            val _ = if blockChanged then hasChanged := true else ()
        in
            case (flow, finalRepl) of
                (* We have a Condition and a change to the condition. *)
                (flow as Conditional{ccRef, condition, trueJump, falseJump},
                 BRSetConditionToConstant({srcCC, test, ...})) =>
                    if srcCC = ccRef
                    then
                    let
                        val newFlow =
                            if test condition
                            then Unconditional trueJump
                            else Unconditional falseJump

                        val() = hasChanged := true
                    in
                        BasicBlock{flow=newFlow, block=List.rev blkCode}
                    end
                    else BasicBlock{flow=flow, block=List.rev blkCode}
              
            |   (flow as Unconditional jmp, _) =>
                let
                    val ExtendedBasicBlock{block=targetBlck, locals, exports, flow=targetFlow, outCCState=targetCC, ...} =
                        Vector.sub(code, jmp)
                    (* If the target is empty or is simply one or more Resets or a Return we're
                       better off merging this in rather than doing the jump.  We allow a single
                       Load  e.g. when loading a constant or moving a register.
                       If we have a CompareLiteral and we're comparing with a register in the map
                       that has been set to a constant we include that because the comparison will
                       then be reduced to a constant. *)
                    fun isSimple([], _, _) = true
                    |   isSimple ({instr=ResetStackPtr _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=ReturnResultFromFunction _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=RaiseExceptionPacket _, ...} :: instrs, moves, regMap) = isSimple(instrs, moves, regMap)
                    |   isSimple ({instr=MoveRegister{source, dest}, ...} :: instrs, moves, regMap) =
                        let
                            (* We frequently have a move of the original register into a new register before the test. *)
                            val newMap =
                                case List.find(fn {dest, ... } => dest = source) regMap of
                                    SOME {source, ...} => {dest=dest, source=source} :: regMap
                                |   NONE => regMap
                        in
                            moves = 0 andalso isSimple(instrs, moves+1, newMap)
                        end
                    |   isSimple ({instr=LoadNonAddressConstant _, ...} :: instrs, moves, regMap) =
                            moves = 0 andalso isSimple(instrs, moves+1, regMap)
                    |   isSimple ({instr=LoadAddressConstant _, ...} :: instrs, moves, regMap) =
                            moves = 0 andalso isSimple(instrs, moves+1, regMap)
                    |   isSimple ({instr=LogicalImmediate{source, dest=ZeroReg, logOp=LogAnd, ...}, ...} :: instrs, moves, regMap) =
                        let
                            val isReplace = List.find(fn {dest, ... } => dest = source) regMap
                        in
                            case isReplace of
                                SOME {source=NonAddressConst _, ...} => isSimple(instrs, moves, regMap)
                            |   _ => false
                        end
                    |   isSimple ({instr=AddSubImmediate{source, dest=ZeroReg, ...}, ...} :: instrs, moves, regMap) =
                        let
                            val isReplace = List.find(fn {dest, ... } => dest = source) regMap
                        in
                            case isReplace of
                                SOME {source=NonAddressConst _, ...} => isSimple(instrs, moves, regMap)
                            |   _ => false
                        end
                    |   isSimple _ = false
 
                in
                    (* Merge trivial blocks.  This previously also tried to merge non-trivial blocks if
                       they only had one reference but this ends up duplicating non-trivial code.  If we
                       have a trivial block that has multiple references but is the only reference to
                       a non-trivial block we can merge the non-trivial block into it.  That would
                       be fine except that at the same time we may merge this trivial block elsewhere. *)
                    (* The restriction that a block must only export "merge" registers is unfortunate
                       but necessary to avoid the situation where a non-merge register is defined at
                       multiple points and cannot be pushed to the stack.  This really isn't an issue
                       with blocks with unconditional branches but there are cases where we have
                       successive tests of the same condition and that results in local registers
                       being defined and then exported.  This occurs in, for example,
                       fun f x = if x > "abcde" then "yes" else "no"; *)
                    if isSimple(targetBlck, 0, finalMap) andalso
                            List.all (fn i => Vector.sub(pregProps, i) = RegPropMultiple) (setToList exports)
                    then
                    let
                        (* Copy the block, creating new registers for the locals. *)
                        val localMap = List.map (fn r => (PReg r, newReg(Vector.sub(pregProps, r)))) (setToList locals)
                        fun mapReg r = case List.find (fn (s, _) => r = s) localMap of SOME(_, s) => s | NONE => r
                        fun mapInstr(instr as ResetStackPtr _) = instr
                        |   mapInstr(ReturnResultFromFunction{results, fpResults, returnReg, numStackArgs}) =
                                ReturnResultFromFunction{results=List.map(fn(pr, r) => (mapReg pr, r))results,
                                                         fpResults=List.map(fn(pr, r) => (mapReg pr, r))fpResults,
                                                         returnReg=mapReg returnReg, numStackArgs=numStackArgs}
                        |   mapInstr(RaiseExceptionPacket{packetReg}) =
                                RaiseExceptionPacket{packetReg=mapReg packetReg}
                        |   mapInstr(MoveRegister{source, dest}) =
                                MoveRegister{source=mapReg source, dest=mapReg dest}
                        |   mapInstr(LoadNonAddressConstant{source, dest}) =
                                LoadNonAddressConstant{source=source, dest=mapReg dest}
                        |   mapInstr(LoadAddressConstant{source, dest}) =
                                LoadAddressConstant{source=source, dest=mapReg dest}
                        |   mapInstr(LogicalImmediate{source, dest=ZeroReg, immed, ccRef, logOp, length}) =
                                LogicalImmediate{source=mapReg source, dest=ZeroReg, immed=immed, ccRef=ccRef, logOp=logOp, length=length}
                        |   mapInstr(AddSubImmediate{source, dest=ZeroReg, immed, ccRef, isAdd, length}) =
                                AddSubImmediate{source=mapReg source, dest=ZeroReg, immed=immed, ccRef=ccRef, isAdd=isAdd, length=length}
                        |   mapInstr _ = raise InternalError "mapInstr: other instruction"
                        fun mapRegNo i = case(mapReg(PReg i)) of PReg r => r
                        (* Map the instructions and the sets although we only use the kill set. *)
                        fun mapCode{instr, current, active, kill} =
                            {instr=mapInstr instr, current=listToSet(map mapRegNo (setToList current)),
                             active=listToSet(map mapRegNo (setToList active)), kill=listToSet(map mapRegNo (setToList kill))}
                    in
                        hasChanged := true;
                        optimiseBlock blkCode(map mapCode targetBlck, targetFlow, targetCC)
                    end
                    else BasicBlock{flow=flow, block=List.rev blkCode}
                end

            |   (flow, _) => BasicBlock{flow=flow, block=List.rev blkCode}
        end
        
        fun optBlck(ExtendedBasicBlock{block, flow, outCCState, ...}) = optimiseBlock [] (block, flow, outCCState)
        val resVector = Vector.map optBlck code
    in
        if !hasChanged
        then
        let
            val extraRegs = List.rev(! regList)
            val props =
                if null extraRegs
                then pregProps
                else Vector.concat[pregProps, Vector.fromList extraRegs]
        in
            Changed(resVector, props)
        end
        else Unchanged
    end

    structure Sharing =
    struct
        type extendedBasicBlock = extendedBasicBlock
        and ('genReg, 'optGenReg, 'fpReg) basicBlock = ('genReg, 'optGenReg, 'fpReg) basicBlock
        and regProperty = regProperty
        and optimise = optimise
        and preg = preg
        and pregOrZero = pregOrZero
    end
end;
