(*
    Title:      Standard Basis Library: Real and Real32 structures.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2008, 2016-18, 2023

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

local
    open RealNumbersAsBits

    val floatMaxFiniteExp = 254
    and doubleMaxFiniteExp = 2046

    (* Functions common to Real and Real32 *)
    local
        open StringCvt
    in
        (* How many digits will the mantissa take?  It's possible
           to unroll this loop since the maximum for float is 9 and for
           double is 17.  We want to avoid long-format arbitrary precision
           arithmetic as much as possible so we stop at 10 digits which
           is the maximum short-format power of ten in 32-bit mode. *)
        fun ndigits (i: LargeInt.int) =
            if i >= 1000000000 then ndigits(i div 1000000000) + 9
            else if i >= 100000000 then 9
            else if i >= 10000000 then 8
            else if i >= 1000000 then 7
            else if i >= 100000 then 6
            else if i >= 10000 then 5
            else if i >= 1000 then 4
            else if i >= 100 then 3
            else if i >= 10 then 2
            else 1

        local
            (* PolyRealDoubleToDecimal returns an arbitrary precision number for
               the mantissa because it could be greater than 31 bits.  In 64-bit
               mode it will always be short and we want to use fixed int
               arithmetic if we can. *)
            fun fixedDigitList (0, r) = r
            |   fixedDigitList (n, r) =
                    fixedDigitList (Int.quot(n, 10), Int.rem(n, 10) :: r)
        in
            fun digitList(n, r) =
                if LibrarySupport.largeIntIsSmall n then fixedDigitList (Int.fromLarge n, r)
                else
                let
                    val (qu, rm) = IntInf.quotRem(n, 10)
                in
                    digitList (qu, (Int.fromLarge rm) :: r)
                end
        end

        (* Common functions to convert real/Real32.real to strings *)
        (* "ndigs" is the number of digits after the decimal point.
           For sciFmt that means that "ndigs+1" digits are produced.
           For fixFmt at least "ndigs+1" digits are produced.  If
           "ndigs" is 0 the value is printed as an integer without
           any ".0".
           In both cases ndigs > 0. *)
        fun exactFmt convert r =
        let
            val {sign, exponent, mantissa, class} = convert r
            open IEEEReal
        in
            case class of
                NAN => "nan"
            |   INF => if sign then "~inf" else "inf"
            |   _ (* NORMAL, ZERO, SUBNORMAL *) =>
                let
                    val s = if sign then "~" else ""
                    val exponent = exponent + ndigits mantissa
                    val (e, exp) =
                        if exponent = 0 then ("", "") else ("E", Int.toString exponent)
                in
                    String.concat[s, "0.", LargeInt.toString mantissa, e, exp]
                end
        end

        local
            val dble2str = RunCall.rtsCallFull3 "PolyRealDoubleToString" : real * char * int -> string
        in
            fun nonExactFmt (toDble, fmt, ndigs) r =
            let
                val dr = toDble r
                val exp = doubleExponent dr
            in
                if exp > doubleMaxFiniteExp
                then (* Non-finite *)
                (
                    if doubleMantissa dr <> 0
                    then "nan"
                    else if doubleSignBit dr then "~inf" else "inf"
                )
                else
                let
                    (* Use snprintf to do the conversion. *)
                    val str = dble2str(dr, fmt, ndigs)
                    (* G-format does not always put in a decimal point so
                       we may need to add .0 to the end to make it look like
                       an ML real. *)
                    fun hasDecOrE(_, true) = true
                    |   hasDecOrE(#".", _) = true
                    |   hasDecOrE(#"E", _) = true
                    |   hasDecOrE _ = false
                in
                    if fmt = #"G" andalso ndigs > 1 andalso not(CharVector.foldl hasDecOrE false str)
                    then str ^ ".0"
                    else str
                end
            end
        end

        (* Note: The definition says, reasonably, that negative values
           for the number of digits raises Size.  The tests also check
           for a very large value for the number of digits and seem to
           expect Size to be raised in that case.  Note that the exception
           is raised when fmt spec is evaluated and before it is applied
           to an actual real argument. *)
        fun fmtFunction (_, toDble) (SCI NONE) = nonExactFmt(toDble, #"E", 6)
        |   fmtFunction (_, toDble) (SCI (SOME d) ) =
                if d < 0 orelse d > 200 then raise General.Size
                else nonExactFmt(toDble, #"E", d)
        |   fmtFunction (_, toDble) (FIX NONE) = nonExactFmt(toDble, #"F", 6)
        |   fmtFunction (_, toDble) (FIX (SOME d) ) =
                if d < 0 orelse d > 200 then raise General.Size
                else nonExactFmt(toDble, #"F", d)
        |   fmtFunction (_, toDble) (GEN NONE) = nonExactFmt(toDble, #"G", 12)
        |   fmtFunction (_, toDble) (GEN (SOME d) ) =
                if d < 1 orelse d > 200 then raise General.Size
                else nonExactFmt(toDble, #"G", d)
        |   fmtFunction (toExact, _) EXACT = exactFmt toExact

    end
in

structure Real: REAL =
struct
    open IEEEReal
    val fromLargeInt: LargeInt.int -> real = Real.rtsCallFastI_R "PolyFloatArbitraryPrecision"
    
    val fromInt: int -> real =
        (* We have to select the appropriate conversion.  This will be
           reduced down to the appropriate function but has to be
           type-correct whether int is arbitrary precision or fixed
           precision.  Hence the "o Large/FixedInt.fromInt". *)
        if Bootstrap.intIsArbitraryPrecision
        then fromLargeInt o LargeInt.fromInt
        else Real.fromFixedInt o FixedInt.fromInt

    (* These are needed because we don't yet have conversion from string
       to real.  They are filtered out by the signature. *)
    val zero = fromInt 0 and one = fromInt 1 and four = fromInt 4
        
    type real = real (* Pick up from globals. *)

    structure Math: MATH =
    struct
        type real = real (* Pick up from globals. *)
        val sqrt  = Real.rtsCallFastR_R "PolyRealSqrt"
        and sin   = Real.rtsCallFastR_R "PolyRealSin"
        and cos   = Real.rtsCallFastR_R "PolyRealCos"
        and atan  = Real.rtsCallFastR_R "PolyRealArctan"
        and exp   = Real.rtsCallFastR_R "PolyRealExp"
        and ln    = Real.rtsCallFastR_R "PolyRealLog"
        and tan   = Real.rtsCallFastR_R "PolyRealTan"
        and asin  = Real.rtsCallFastR_R "PolyRealArcSin"
        and acos  = Real.rtsCallFastR_R "PolyRealArcCos"
        and log10 = Real.rtsCallFastR_R "PolyRealLog10"
        and sinh  = Real.rtsCallFastR_R "PolyRealSinh"
        and cosh  = Real.rtsCallFastR_R "PolyRealCosh"
        and tanh  = Real.rtsCallFastR_R "PolyRealTanh"

        val atan2 = Real.rtsCallFastRR_R "PolyRealAtan2"
        val pow = Real.rtsCallFastRR_R "PolyRealPow"

        (* Derived values. *)
        val e = exp one
        val pi = four * atan one
    
    end;

    infix 4 == != ?=;
    
    val op == = Real.==
    val op != : real * real -> bool = not o op ==
    
    val radix        = 2
    val precision    = 53
    val maxFinite    = doubleFromBinary{sign=false, exp=doubleMaxFiniteExp, mantissa = 0xFFFFFFFFFFFFF}
    val minNormalPos = doubleFromBinary{sign=false, exp=1, mantissa = 0}
    val minPos       = doubleFromBinary{sign=false, exp=0, mantissa = 1}

    val posInf : real = one/zero
    val negInf : real = ~one/zero
    
    (* Real is LargeReal. *)
    fun toLarge (x: real) : (*LargeReal.*)real =x
    fun fromLarge (_ : IEEEReal.rounding_mode) (x: (*LargeReal.*)real): real = x

    (* isNan can be defined in terms of unordered. *)
    fun isNan x = Real.unordered(x, x)
  
    fun isFinite x = doubleExponent x <= doubleMaxFiniteExp

    (* This could be implemented using signBit and doubleFromBinary *)
    val copySign : (real * real) -> real = Real.rtsCallFastRR_R "PolyRealCopySign"
    
    val signBit = doubleSignBit

    fun isNormal x =
    let val exp = doubleExponent x in exp > 0 andalso exp <= doubleMaxFiniteExp end

    fun class x =
    let
        val exp = doubleExponent x
    in
        if exp > doubleMaxFiniteExp
        then
        (
            if doubleMantissa x <> 0
            then NAN
            else INF
        )
        else if exp = 0
        then
        (
            if doubleMantissa x = 0
            then ZERO
            else SUBNORMAL
        )
        else NORMAL
    end

    fun sign x = 
        if isNan x then raise General.Domain
        else if x == zero then 0 else if x < zero then ~1 else 1

    fun sameSign (x, y) = signBit x = signBit y

    (* Returns the minimum.  In the case where one is a NaN it returns the
       other. In that case the comparison will be false. *)
    fun min (a: real, b: real): real = if a < b orelse isNan b then a else b
    (* Similarly for max. *)
    fun max (a: real, b: real): real = if a > b orelse isNan b then a else b

    fun checkFloat x =
        if isFinite x then x
        else if isNan x then raise General.Div else raise General.Overflow

    local
        val frExp: real -> int * real = RunCall.rtsCallFull1 "PolyRealFrexp"
        val fromManAndExp: real*int -> real = Real.rtsCallFastRI_R "PolyRealLdexp"
        
        open Real
    in
        fun toManExp r = 
            if not (isFinite r) orelse r == zero
            (* Nan, infinities and +/-0 all return r in the mantissa.
               We include 0 to preserve its sign. *)
            then {man=r, exp=0}
            else
            let
                val (exp, man) = frExp r
            in
                {man=man, exp=exp}
            end

        fun fromManExp {man, exp} = 
            if not (isFinite man) orelse man == zero
            (* Nan, infinities and +/-0 in the mantissa all return
               their argument. *)
            then man
            else if LibrarySupport.isShortInt exp
            then fromManAndExp(man, exp)
            else (* Long arbitrary precision *)
                copySign(if Int.>(exp, 0) then posInf else zero, man)
    end

    (* Convert to integer. *)
    local
        (* These are defined to raise Domain rather than Overflow on Nans. *)
        fun checkNan x = if isNan x then raise Domain else x
    in
        val realFloor = Real.rtsCallFastR_R "PolyRealFloor"
        and realCeil  = Real.rtsCallFastR_R "PolyRealCeil"
        and realTrunc  = Real.rtsCallFastR_R "PolyRealTrunc"
        and realRound  = Real.rtsCallFastR_R "PolyRealRound"

        local
            val doubleBias = 1023 (* This is the exponent value for 1.0 *)
            val doubleMantissaBits = precision - 1 (* One bit is implicit *)
            val doubleImplicitBit = IntInf.<<(1, Word.fromInt doubleMantissaBits)
        in
            (* Convert a real number to arbitrary precision.  It might be possible to
               include the rounding/truncation here. *)
            fun toArbitrary x =
            let
                open RealNumbersAsBits
                val ieeeExp = doubleExponent x
                and ieeeMant = doubleMantissa x
                and ieeeSign = doubleSignBit x
            in
                if ieeeExp = 2047
                then (* Non-finite *)
                    if ieeeMant <> 0 then raise General.Domain else raise General.Overflow
                else if ieeeExp < doubleBias
                then 0 (* less than 1 *)
                else
                let
                    (* Add the implicit bit to the mantissa and set the sign. *)
                    val m2a = ieeeMant + doubleImplicitBit
                    val m2s = if ieeeSign then ~m2a else m2a
                    val shift = ieeeExp - doubleBias - doubleMantissaBits
                in
                    if shift < 0
                    then IntInf.~>>(m2s, Word.fromInt(~shift))
                    else IntInf.<<(m2s, Word.fromInt shift)
               end
           end
        end

        fun toLargeInt IEEEReal.TO_NEGINF = toArbitrary o realFloor
         |  toLargeInt IEEEReal.TO_POSINF = toArbitrary o realCeil
         |  toLargeInt IEEEReal.TO_ZERO = toArbitrary o realTrunc
         |  toLargeInt IEEEReal.TO_NEAREST = toArbitrary o realRound

        (* Conversions to FixedInt are put in by the compiler.  If int is fixed we can
           use them otherwise we use the long versions.
           N.B.  FixedInt.toInt is a no-op but is needed so this is type-correct when
           int is arbitrary. *)
        val floor   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toArbitrary o realFloor else FixedInt.toInt o Real.floorFix o checkNan
        and ceil    =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toArbitrary o realCeil else FixedInt.toInt o Real.ceilFix o checkNan
        and trunc   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toArbitrary o realTrunc else FixedInt.toInt o Real.truncFix o checkNan
        and round   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toArbitrary o realRound else FixedInt.toInt o Real.roundFix o checkNan
        
        fun toInt IEEEReal.TO_NEGINF = floor
         |  toInt IEEEReal.TO_POSINF = ceil
         |  toInt IEEEReal.TO_ZERO = trunc
         |  toInt IEEEReal.TO_NEAREST = round
    end;

    local
        val posNan = abs(zero / zero)
        val negNan = ~posNan
    in
        fun fromDecimal { class = INF, sign=true, ...} = SOME negInf
          | fromDecimal { class = INF, sign=false, ...} = SOME posInf
          | fromDecimal { class = ZERO, sign=true, ...} = SOME (~ zero)
          | fromDecimal { class = ZERO, sign=false, ...} = SOME zero
             (* Generate signed Nans ignoring the digits and mantissa.  There
                was code here to set the mantissa but there's no reference to
                that in the current version of the Basis library.  *)
          | fromDecimal { class = NAN, sign=true, ... } = SOME negNan
          | fromDecimal { class = NAN, sign=false, ... } = SOME posNan

          | fromDecimal { class = _ (* NORMAL or SUBNORMAL *), sign, digits, exp} =
            (SOME(RealToDecimalConversion.decimalToDouble(sign, exp, digits)) handle General.Domain => NONE)
    end
        
    fun toDecimal r =
    let
        val {sign, exponent, mantissa, class} = RealToDecimalConversion.doubleToMinimal r
    in
        { class = class, sign = sign, exp = exponent + ndigits mantissa, digits = digitList(mantissa, []) }
    end

    val fmt = fmtFunction (RealToDecimalConversion.doubleToMinimal, toLarge)

    val toString = fmt (StringCvt.GEN NONE)

    (* Define these in terms of IEEEReal.scan since that deals with all the
       special cases. *)
    fun scan getc src =
        case IEEEReal.scan getc src of
            NONE => NONE
        |   SOME (ieer, rest) =>
            (
                case fromDecimal ieer of
                    NONE => NONE
                |   SOME r => SOME(r, rest)
            )

    val fromString = Option.composePartial (fromDecimal, IEEEReal.fromString)

    (* Converter to real values. This replaces the basic conversion
       function for reals installed in the bootstrap process.
       For more information see convInt in Int. *)
    local
        fun convReal (s: string) : real =
        let
            (* Conversion doesn't involve any real rounding so this should
               no longer be relevant. *)
            (* Set the rounding mode to TO_NEAREST whatever the current
               rounding mode.  Otherwise the result of compiling a piece of
               code with a literal constant could depend on what the rounding
               mode was set to.
               We should always support TO_NEAREST. *)
            val oldRounding = IEEEReal.getRoundingMode()
            val () = IEEEReal.setRoundingMode IEEEReal.TO_NEAREST
            val scanResult = StringCvt.scanString scan s
            val () = IEEEReal.setRoundingMode oldRounding
        in
            case scanResult of
                NONE => raise RunCall.Conversion "Invalid real constant"
              | SOME res => res
        end
    in
        (* Install this as a conversion function for real literals. *)
        val (): unit = RunCall.addOverload convReal "convReal"
    end

    open Real (* Get the other definitions. *)

    fun compare (r1, r2) =
        if r1 == r2 then General.EQUAL
        else if r1 < r2 then General.LESS
        else if r1 > r2 then General.GREATER
        else raise Unordered

    fun compareReal (r1, r2) =
        if r1 == r2 then EQUAL
        else if r1 < r2 then LESS
        else if r1 > r2 then GREATER
        else UNORDERED

    (* This seems to be similar to == except that where == always returns false
       if either argument is a NaN this returns true.  The implementation of ==
       treats the unordered case specially so it would be possible to implement
       this in the same way.  *)
    fun op ?= (x, y) = unordered(x, y) orelse x == y

    (* Although these may be built in in some architectures it's
       probably not worth treating them specially at the moment. *)
    fun *+ (x: real, y: real, z: real): real = x*y+z
    and *- (x: real, y: real, z: real): real = x*y-z

    val rem = Real.rtsCallFastRR_R "PolyRealRem"

    (* Split a real into whole and fractional parts. The fractional part must have
       the same sign as the number even if it is zero. *)
    fun split r =
    let
        val whole = realTrunc r
        val frac = r - whole
    in
        { whole = whole,
          frac =
            if not (isFinite r)
            then if isNan r then r else (* Infinity *) if r < zero then ~zero else zero
            else if frac == zero then if signBit r then ~zero else zero
            else frac }
    end

    (* Get the fractional part of a real. *)
    fun realMod r = #frac(split r)

    (* nextAfter: This was previously implemented in ML but, at the very least,
       needed to work with rounding to something other than TO_NEAREST. *)
    val nextAfter = Real.rtsCallFastRR_R "PolyRealNextAfter"

end (* Real *)


structure Real32: REAL where type real = Real32.real =
(* Real32 uses some definitions from the Real structure above. *)
struct
    open IEEEReal

    (* On both the X86 and ARM there is only a single conversion from
       double to float using the current rounding mode.  If we want
       a specific rounding mode we need to set the rounding. *)
    fun fromLarge mode value =
    let
        val current = getRoundingMode()
        val () = setRoundingMode mode
        val result = Real32.fromReal value
        val () = setRoundingMode current
    in
        result
    end
    
    val fromRealRound = fromLarge TO_NEAREST

    (* Defined to use the current rounding mode. *)
    val fromLargeInt = Real32.fromReal o Real.fromLargeInt

    val fromInt: int -> Real32.real =
        (* We have to select the appropriate conversion.  This will be
           reduced down to the appropriate function but has to be
           type-correct whether int is arbitrary precision or fixed
           precision.  Hence the "o Large/FixedInt.fromInt". *)
        if Bootstrap.intIsArbitraryPrecision
        then fromLargeInt o LargeInt.fromInt
        else Real32.fromFixedInt o FixedInt.fromInt
    
    val zero = fromInt 0 and one = fromInt 1 and four = fromInt 4

    val radix           = 2
    val precision       = 24
    val maxFinite       = floatFromBinary{sign=false, exp=floatMaxFiniteExp, mantissa = 0x7FFFFF}
    val minNormalPos    = floatFromBinary{sign=false, exp=1, mantissa = 0}
    val minPos          = floatFromBinary{sign=false, exp=0, mantissa = 1}

    local
        open Real32
    in
        val posInf : real = one/zero
        val negInf : real = ~one/zero

        val op != : real * real -> bool = not o op ==
    end

    infix 4 == != ?=;

    (* isNan can be defined in terms of unordered. *)
    fun isNan x = Real32.unordered(x, x)

    fun isFinite x = floatExponent x <= floatMaxFiniteExp

    local
        open Real32
    in
        val copySign : (real * real) -> real = rtsCallFastFF_F "PolyRealFCopySign"
    end
    
    val signBit = floatSignBit
    
    fun isNormal x =
    let val exp = floatExponent x in exp > 0 andalso exp <= floatMaxFiniteExp end

    fun class x =
    let
        val exp = floatExponent x
    in
        if exp > floatMaxFiniteExp
        then
        (
            if floatMantissa x <> 0
            then NAN
            else INF
        )
        else if exp = 0
        then
        (
            if floatMantissa x = 0
            then ZERO
            else SUBNORMAL
        )
        else NORMAL
    end

    local
        open Real32
    in
        fun sign x = 
            if isNan x then raise General.Domain
            else if x == zero then 0 else if x < zero then ~1 else 1
    end
        
    fun sameSign (x, y) = signBit x = signBit y

    local
 
        local
            val floatBias = 127 (* This is the exponent value for 1.0 *)
            val floatMantissaBits = precision - 1 (* One bit is implicit *)
            val floatImplicitBit = IntInf.<<(1, Word.fromInt floatMantissaBits)
        in
            (* Convert a real number to arbitrary precision.  It might be possible to
               include the rounding/truncation here. *)
            fun toArbitrary x =
            let
                open RealNumbersAsBits
                val ieeeExp = floatExponent x
                and ieeeMant = floatMantissa x
                and ieeeSign = floatSignBit x
            in
                if ieeeExp = 255
                then (* Non-finite *)
                    if ieeeMant <> 0 then raise General.Domain else raise General.Overflow
                else if ieeeExp < floatBias
                then 0 (* less than 1 *)
                else
                let
                    (* Add the implicit bit to the mantissa and set the sign. *)
                    val m2a = Int.toLarge ieeeMant + floatImplicitBit
                    val m2s = if ieeeSign then ~m2a else m2a
                    val shift = ieeeExp - floatBias - floatMantissaBits
                in
                    if shift < 0
                    then IntInf.~>>(m2s, Word.fromInt(~shift))
                    else IntInf.<<(m2s, Word.fromInt shift)
               end
           end
        end

        open Real32
    in
        (* Returns the minimum.  In the case where one is a NaN it returns the
           other. In that case the comparison will be false. *)
        fun min (a: real, b: real): real = if a < b orelse isNan b then a else b
        (* Similarly for max. *)
        fun max (a: real, b: real): real = if a > b orelse isNan b then a else b

        fun checkFloat x =
            if isFinite x then x
            else if isNan x then raise General.Div else raise General.Overflow

        (* On certain platforms e.g. mips, toLarge does not preserve
           the sign on nans.  We deal with the non-finite cases here. *)

        (* Use the Real versions for the moment. *)
        fun toManExp r =
            if not (isFinite r) orelse r == zero
                (* Nan, infinities and +/-0 all return r in the mantissa.
                   We include 0 to preserve its sign. *)
            then {man=r, exp=0}
            else
            let
                val {man, exp} = Real.toManExp(toLarge r)
            in
                {man = fromRealRound man, exp = exp }
            end

        and fromManExp {man, exp} =
            if not (isFinite man) orelse man == zero
            (* Nan, infinities and +/-0 in the mantissa all return
               their argument. *)
            then man
            else fromRealRound(Real.fromManExp{man=toLarge man, exp=exp})
    
        fun compare (r1, r2) =
            if r1 == r2 then General.EQUAL
            else if r1 < r2 then General.LESS
            else if r1 > r2 then General.GREATER
            else raise Unordered

        fun compareReal (r1, r2) =
            if r1 == r2 then EQUAL
            else if r1 < r2 then LESS
            else if r1 > r2 then GREATER
            else UNORDERED

        fun op ?= (x, y) = unordered(x, y) orelse x == y

        (* Although these may be built in in some architectures it's
           probably not worth treating them specially at the moment. *)
        fun *+ (x: real, y: real, z: real): real = x*y+z
        and *- (x: real, y: real, z: real): real = x*y-z

        val realFloor = rtsCallFastF_F "PolyRealFFloor"
        and realCeil  = rtsCallFastF_F "PolyRealFCeil"
        and realTrunc  = rtsCallFastF_F "PolyRealFTrunc"
        and realRound  = rtsCallFastF_F "PolyRealFRound"

        val rem = rtsCallFastFF_F "PolyRealFRem"

        (* Split a real into whole and fractional parts. The fractional part must have
           the same sign as the number even if it is zero. *)
        fun split r =
        let
            val whole = realTrunc r
            val frac = r - whole
        in
            { whole = whole,
              frac =
                if not (isFinite r)
                then if isNan r then r else (* Infinity *) if r < zero then ~zero else zero
                else if frac == zero then if signBit r then ~zero else zero
                else frac }
        end

        (* Get the fractional part of a real. *)
        fun realMod r = #frac(split r)
    
        val nextAfter = rtsCallFastFF_F "PolyRealFNextAfter"

        fun toLargeInt IEEEReal.TO_NEGINF = toArbitrary o realFloor
         |  toLargeInt IEEEReal.TO_POSINF = toArbitrary o realCeil
         |  toLargeInt IEEEReal.TO_ZERO = toArbitrary o realTrunc
         |  toLargeInt IEEEReal.TO_NEAREST = toArbitrary o realRound

        (* These are defined to raise Domain rather than Overflow on Nans. *)
        fun checkNan x = if isNan x then raise Domain else x
        (* If int is fixed we use the hardware conversions otherwise we use the real to arbitrary conversions. *)
        val floor   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toArbitrary o realFloor else FixedInt.toInt o floorFix o checkNan
        and ceil    =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toArbitrary o realCeil else FixedInt.toInt o ceilFix o checkNan
        and trunc   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toArbitrary o realTrunc else FixedInt.toInt o truncFix o checkNan
        and round   =
            if Bootstrap.intIsArbitraryPrecision
            then LargeInt.toInt o toArbitrary o realRound else FixedInt.toInt o roundFix o checkNan
    
        fun toInt IEEEReal.TO_NEGINF = floor
         |  toInt IEEEReal.TO_POSINF = ceil
         |  toInt IEEEReal.TO_ZERO = trunc
         |  toInt IEEEReal.TO_NEAREST = round
    end
   
    fun toDecimal r =
    let
        val {sign, exponent, mantissa, class} = RealToDecimalConversion.floatToMinimal r
    in
        { class = class, sign = sign, exp = exponent + ndigits mantissa, digits = digitList(mantissa, []) }
    end

    val fmt = fmtFunction (RealToDecimalConversion.floatToMinimal, Real32.toLarge)

    val toString = fmt (StringCvt.GEN NONE)
    
    open Real32 (* Inherit the type and the built-in functions. *)
        
    (* Convert from decimal.  This is defined to use TO_NEAREST.
       We need to handle NaNs specially because fromRealRound loses
       the sign on a NaN. *)
    local
        val posNan = abs(zero / zero)
        val negNan = ~posNan
    in
        fun fromDecimal { class = INF, sign=true, ...} = SOME negInf
        |   fromDecimal { class = INF, sign=false, ...} = SOME posInf
        |   fromDecimal { class = NAN, sign=true, ... } = SOME negNan
        |   fromDecimal { class = NAN, sign=false, ... } = SOME posNan
        |   fromDecimal { sign, exp, digits, ... } =
            (SOME(RealToDecimalConversion.decimalToFloat(sign, exp, digits)) handle General.Domain => NONE)
    end

    (* Define these in terms of IEEEReal.scan since that deals with all the special cases. *)
    fun scan getc src =
        case IEEEReal.scan getc src of
            NONE => NONE
        |   SOME (ieer, rest) =>
            (
                case fromDecimal ieer of
                    NONE => NONE
                |   SOME r => SOME(r, rest)
            )

    val fromString = Option.composePartial (fromDecimal, IEEEReal.fromString)


    structure Math =
    struct
        type real = real

        val sqrt  = rtsCallFastF_F "PolyRealFSqrt"
        and sin   = rtsCallFastF_F "PolyRealFSin"
        and cos   = rtsCallFastF_F "PolyRealFCos"
        and atan  = rtsCallFastF_F "PolyRealFArctan"
        and exp   = rtsCallFastF_F "PolyRealFExp"
        and ln    = rtsCallFastF_F "PolyRealFLog"
        and tan   = rtsCallFastF_F "PolyRealFTan"
        and asin  = rtsCallFastF_F "PolyRealFArcSin"
        and acos  = rtsCallFastF_F "PolyRealFArcCos"
        and log10 = rtsCallFastF_F "PolyRealFLog10"
        and sinh  = rtsCallFastF_F "PolyRealFSinh"
        and cosh  = rtsCallFastF_F "PolyRealFCosh"
        and tanh  = rtsCallFastF_F "PolyRealFTanh"

        val atan2 = rtsCallFastFF_F "PolyRealFAtan2"
        val pow = rtsCallFastFF_F "PolyRealFPow"

        (* Derived values. *)
        val e = exp one
        val pi = four * atan one
    end
    

    (* Converter for literal constants.  Copied from Real. *)
    local
        fun convReal (s: string) : real =
        let
            (* Conversion doesn't involve any real rounding so this should
               no longer be relevant. *)
            (* Set the rounding mode to TO_NEAREST whatever the current
               rounding mode.  Otherwise the result of compiling a piece of
               code with a literal constant could depend on what the rounding
               mode was set to. We should always support TO_NEAREST. *)
            val oldRounding = IEEEReal.getRoundingMode()
            val () = IEEEReal.setRoundingMode IEEEReal.TO_NEAREST
            val scanResult = StringCvt.scanString scan s
            val () = IEEEReal.setRoundingMode oldRounding
        in
            case scanResult of
                NONE => raise RunCall.Conversion "Invalid real constant"
              | SOME res => res
        end
    in
        (* Install this as a conversion function for real literals. *)
        val (): unit = RunCall.addOverload convReal "convReal"
    end
   
end (* Real32 *)

end;

structure Math = Real.Math;

structure LargeReal: REAL = Real;

(* Values available unqualified at the top-level. *)
val real : int -> real = Real.fromInt 
val trunc : real -> int = Real.trunc 
val floor : real -> int = Real.floor 
val ceil : real -> int = Real.ceil 
val round : real -> int =Real.round;

(* Overloads for Real32.real.  The overloads for real were added in InitialBasis. *)
val () = RunCall.addOverload Real32.>= ">="
and () = RunCall.addOverload Real32.<= "<="
and () = RunCall.addOverload Real32.>  ">"
and () = RunCall.addOverload Real32.<  "<"
and () = RunCall.addOverload Real32.+ "+"
and () = RunCall.addOverload Real32.- "-"
and () = RunCall.addOverload Real32.* "*"
and () = RunCall.addOverload Real32.~ "~"
and () = RunCall.addOverload Real32.abs "abs"
and () = RunCall.addOverload Real32./ "/";


(* Install print functions. *)
local
    fun print_real32 _ _ (r: Real32.real) =
        PolyML.PrettyString(Real32.fmt (StringCvt.GEN(SOME 7)) r)
in
    val () = PolyML.addPrettyPrinter print_real32
end;
local
    fun print_real _ _ (r: real) =
        PolyML.PrettyString(Real.fmt (StringCvt.GEN(SOME 10)) r)
in
    val () = PolyML.addPrettyPrinter print_real;
end;
