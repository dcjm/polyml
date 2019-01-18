(*
    Title:      Standard Basis Library: Real Signature and structure.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2008, 2016-18

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
    
    local
        (* The General call is now only used to get constants. *)
        val doRealReal : int*unit->real = RunCall.rtsCallFull2 "PolyRealGeneral"
        and doRealInt  : int*unit->int = RunCall.rtsCallFull2 "PolyRealGeneral"
        fun callReal n x = doRealReal(n, x)
        and callRealToInt n x = doRealInt(n, x)
    in
        val radix : int = callRealToInt 11 ()
        val precision : int = callRealToInt 12 ()
        val maxFinite : real = callReal 13 ()
        val minNormalPos : real = callReal 14 ()
        val minPos: real = callReal 15 ()
    end

    val posInf : real = one/zero;
    val negInf : real = ~one/zero;
    
    (* Real is LargeReal. *)
    fun toLarge (x: real) : (*LargeReal.*)real =x
    fun fromLarge (_ : IEEEReal.rounding_mode) (x: (*LargeReal.*)real): real = x

    local
        open Real
    in
        (* isNan can be defined in terms of unordered. *)
        fun isNan x = unordered(x, x)
        
        (* NAN values do not match and infinities when multiplied by 0 produce NAN. *)
        fun isFinite x = x * zero == zero
    
        val copySign : (real * real) -> real = Real.rtsCallFastRR_R "PolyRealCopySign"
        
        (* Get the sign bit by copying the sign onto a finite value and then
           testing.  This works for non-finite values and zeros. *)
        fun signBit r = copySign(one, r) < zero

        (* If we assume that all functions produce normalised results where
           possible, the only subnormal values will be those smaller than
           minNormalPos. *)
        fun isNormal x = isFinite x andalso abs x >= minNormalPos
    
        fun class x =
            if isFinite x then if x == zero then ZERO
               else if abs x >= minNormalPos then NORMAL
               else SUBNORMAL
            else if isNan x then NAN
               else (* not finite and not Nan *) INF
    
        fun sign x = 
            if isNan x then raise General.Domain
            else if x == zero then 0 else if x < zero then ~1 else 1
    end
        
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
        (* The RTS function converts to at most a 64-bit value (even on 
           32-bits).  That will convert all the bits of the mantissa
           but if the exponent is large we may have to multiply by
           some power of two. *)
        val realToInt: real -> LargeInt.int  = RunCall.rtsCallFull1 "PolyRealBoxedToLongInt"
        (* These are defined to raise Domain rather than Overflow on Nans. *)
        fun checkNan x = if isNan x then raise Domain else x
    in
        val realFloor = Real.rtsCallFastR_R "PolyRealFloor"
        and realCeil  = Real.rtsCallFastR_R "PolyRealCeil"
        and realTrunc  = Real.rtsCallFastR_R "PolyRealTrunc"
        and realRound  = Real.rtsCallFastR_R "PolyRealRound"

        fun toArbitrary x = 
            if isNan x then raise General.Domain
            else if not (isFinite x) then raise General.Overflow
            else
            let
                val { man, exp } = toManExp x
            in
                if exp <= precision
                then realToInt x
                else IntInf.<< (realToInt(fromManExp{man=man, exp=precision}), Word.fromInt(exp - precision))
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
        val realConv: string->real = RunCall.rtsCallFull1 "PolyRealBoxedFromString"

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
            (let
                fun toChar x =
                    if x < 0 orelse x > 9 then raise General.Domain
                    else Char.chr (x + Char.ord #"0")
                (* Turn the number into a string. *)
                val str = "0." ^ String.implode(List.map toChar digits) ^"E" ^
                    Int.toString exp
                (* Convert it to a real using the RTS conversion function.
                   Change any Conversion exceptions into Domain. *)
                val result = realConv str handle RunCall.Conversion _ => raise General.Domain
            in
                if sign then SOME (~result) else SOME result
            end 
                handle General.Domain => NONE
            )
    end
        
    local
        val dtoa: real*int*int -> string*int*int = RunCall.rtsCallFull3 "PolyRealBoxedToString"
        open StringCvt

        fun addZeros n =
            if n <= 0 then "" else "0" ^ addZeros (n-1)

        fun fixFmt ndigs r =
        if isNan r then "nan"
        else if not (isFinite r)
        then if r < zero then "~inf" else "inf"
        else
        let
            (* Try to get ndigs past the decimal point. *)
            val (str, exp, sign) = dtoa(r, 3, ndigs)
            val strLen = String.size str
            (* If the exponents is negative or zero we need to put a zero
               before the decimal point.  If the exponent is positive and
               less than the number of digits we can take that
               many characters off, otherwise we have to pad with zeros. *)
            val numb =
                if exp <= 0
                then (* Exponent is zero or negative - all significant digits are
                        after the decimal point.  Put in any zeros before
                        the significant digits, then the significant digits
                        and then any trailing zeros. *)
                    if ndigs = 0 then "0"
                    else "0." ^ addZeros(~exp) ^ str ^ addZeros(ndigs-strLen+exp)
                else if strLen <= exp
                then (* Exponent is not less than the length of the string -
                        all significant digits are before the decimal point. Add
                        any extra zeros before the decimal point then zeros after it. *)
                    str ^ addZeros(exp-strLen) ^ 
                        (if ndigs = 0 then "" else "." ^ addZeros ndigs)
                else (* Significant digits straddle the decimal point - insert the
                        decimal point and add any trailing zeros. *)
                    String.substring(str, 0, exp) ^ "." ^
                        String.substring(str, exp, strLen-exp) ^ 
                            addZeros(ndigs-strLen+exp)
        in
            if sign <> 0 then "~" ^ numb else numb
        end
        
        fun sciFmt ndigs r =
        if isNan r then "nan"
        else if not (isFinite r)
        then if r < zero then "~inf" else "inf"
        else
        let
            (* Try to get ndigs+1 digits.  1 before the decimal point and ndigs after. *)
            val (str, exp, sign) = dtoa(r, 2, ndigs+1)
            val strLen = String.size str
            fun addZeros n =
                if n <= 0 then "" else "0" ^ addZeros (n-1)
            val numb =
                if strLen = 0
                then "0" ^ (if ndigs = 0 then "" else "." ^ addZeros ndigs) ^ "E0"
                else 
                   (if strLen = 1
                    then str ^ (if ndigs = 0 then "" else "." ^ addZeros ndigs)
                    else String.substring(str, 0, 1) ^ "." ^
                            String.substring(str, 1, strLen-1) ^ addZeros (ndigs-strLen+1)
                    ) ^ "E" ^ Int.toString (exp-1)
        in
            if sign <> 0 then "~" ^ numb else numb
        end

        fun genFmt ndigs r =
        if isNan r then "nan"
        else if not (isFinite r)
        then if r < zero then "~inf" else "inf"
        else
        let
            (* Try to get ndigs digits. *)
            val (str, exp, sign) = dtoa(r, 2, ndigs)
            val strLen = String.size str
            val numb =
            (* Have to use scientific notation if exp > ndigs.  Also use it
               if the exponent is small (TODO: adjust this) *)
                if exp > ndigs orelse exp < ~5
                then (* Scientific format *)
                   (if strLen = 1 then str
                    else String.substring(str, 0, 1) ^ "." ^
                            String.substring(str, 1, strLen-1)
                    ) ^ "E" ^ Int.toString (exp-1)

                else (* Fixed format (N.B. no trailing zeros are added after the
                        decimal point apart from one if necessary) *)
                    if exp <= 0
                then (* Exponent is zero or negative - all significant digits are
                        after the decimal point.  Put in any zeros before
                        the significant digits, then the significant digits
                        and then any trailing zeros. *)
                    "0." ^ addZeros(~exp) ^ str
                else if strLen <= exp
                then (* Exponent is not less than the length of the string -
                        all significant digits are before the decimal point. Add
                        any extra zeros before the decimal point. Insert .0 at the
                        end to make it a valid real number. *)
                    str ^ addZeros(exp-strLen) ^ ".0"
                else (* Significant digits straddle the decimal point - insert the
                        decimal point. *)
                    String.substring(str, 0, exp) ^ "." ^
                        String.substring(str, exp, strLen-exp)
        in
            if sign <> 0 then "~" ^ numb else numb
        end

        fun strToDigitList str =
        let
            fun getDigs i l =
                if i < 0 then l
                else getDigs (i-1)
                        ((Char.ord(String.sub(str, i)) - Char.ord #"0") :: l)
        in
            getDigs (String.size str - 1) []
        end
    in
        fun toDecimal r =
        let
            val sign = signBit r
            val kind = class r
        in
            case kind of
                ZERO => { class = ZERO, sign = sign, digits=[], exp = 0 }
              | INF  => { class = INF, sign = sign, digits=[], exp = 0 }
              | NAN => { class = NAN, sign = sign, digits=[], exp = 0 }
              | _ => (* NORMAL/SUBNORMAL *)
                let
                    val (str, exp, sign) = dtoa(r, 0, 0)
                    val digits = strToDigitList str
                in
                    { class = kind, sign = sign <> 0, digits = digits, exp = exp }
                end
        end

        (* Note: The definition says, reasonably, that negative values
           for the number of digits raises Size.  The tests also check
           for a very large value for the number of digits and seem to
           expect Size to be raised in that case.  Note that the exception
           is raised when fmt spec is evaluated and before it is applied
           to an actual real argument.
           In all cases, even EXACT format, this should produce "nan" for a NaN
           and ignore the sign bit. *)
        fun fmt (SCI NONE) = sciFmt 6
          | fmt (SCI (SOME d) ) =
                if d < 0 orelse d > 200 then raise General.Size
                else sciFmt d
          | fmt (FIX NONE) = fixFmt 6
          | fmt (FIX (SOME d) ) =
                if d < 0 orelse d > 200 then raise General.Size
                else fixFmt d
          | fmt (GEN NONE) = genFmt 12
          | fmt (GEN (SOME d) ) =
                if d < 1 orelse d > 200 then raise General.Size
                else genFmt d
          | fmt EXACT = (fn r => if isNan r then "nan" else IEEEReal.toString(toDecimal r))
          
        val toString = fmt (GEN NONE)
    end


    fun scan getc src =
    let
        (* Return a list of digits. *)
        fun getdigits inp src =
            case getc src of
                NONE => (List.rev inp, src)
              | SOME(ch, src') =>
                    if ch >= #"0" andalso ch <= #"9"
                    then getdigits ((Char.ord ch - Char.ord #"0") :: inp) src'
                    else (List.rev inp, src)

        (* Read an unsigned integer.  Returns NONE if no digits have been read. *)
        fun getNumber sign digits acc src =
            case getc src of
                NONE => if digits = 0 then NONE else SOME(if sign then ~acc else acc, src)
              | SOME(ch, src') =>
                    if ch >= #"0" andalso ch <= #"9"
                    then getNumber sign (digits+1) (acc*10 + Char.ord ch - Char.ord #"0") src'
                    else if digits = 0 then NONE else SOME(if sign then ~acc else acc, src')

        (* Return the signed exponent. *)
        fun getExponent src =
            case getc src of
                NONE => NONE
              | SOME(ch, src') =>
                if ch = #"+"
                then getNumber false 0 0 src'
                else if ch = #"-" orelse ch = #"~"
                then getNumber true 0 0 src'
                else getNumber false 0 0 src

        fun read_number sign src =
            case getc src of
                NONE => NONE
              | SOME(ch, _) =>
                    if not (ch >= #"0" andalso ch <= #"9" orelse ch = #".")
                    then NONE (* Bad *)
                    else (* Digits or decimal. *)
                    let
                        (* Get the digits before the decimal point (if any) *)
                        val (intPart, srcAfterDigs) = getdigits [] src
                        (* Get the digits after the decimal point (if any).
                           If there is a decimal point we only accept it if
                           there is at least one digit after it. *)
                        val (decimals, srcAfterMant) =
                            case getc srcAfterDigs of
                                NONE => ([], srcAfterDigs)
                             |  SOME (#".", srcAfterDP) =>
                                    ( (* Check that the next character is a digit. *)
                                    case getc srcAfterDP of
                                        NONE => ([], srcAfterDigs)
                                      | SOME(ch, _) =>
                                            if ch >= #"0" andalso ch <= #"9"
                                            then getdigits [] srcAfterDP
                                            else ([], srcAfterDigs)
                                    )
                             |  SOME (_, _) => ([], srcAfterDigs)
                        (* The exponent is optional.  If it doesn't form a valid
                           exponent we return zero as the value and the
                           continuation is the beginning of the "exponent". *)
                        val (exponent, srcAfterExp) =
                            case getc srcAfterMant of
                                NONE => (0, srcAfterMant)
                             |  SOME (ch, src'''') =>
                                if ch = #"e" orelse ch = #"E"
                                then 
                                    (
                                    case getExponent src'''' of
                                        NONE => (0, srcAfterMant)
                                      | SOME x => x 
                                    )
                                else (0, srcAfterMant)
                        (* Generate a decimal representation ready for conversion.
                           We don't bother to strip off leading or trailing zeros. *)
                        val decimalRep = {class=NORMAL, sign=sign,
                                digits=List.@(intPart, decimals),
                                exp=exponent + List.length intPart}
                    in
                        case fromDecimal decimalRep of
                           SOME r => SOME(r, srcAfterExp)
                         | NONE => NONE
                    end
    in
        case getc src of
            NONE => NONE
         |  SOME(ch, src') =>
            if Char.isSpace ch (* Skip white space. *)
            then scan getc src' (* Recurse *)
            else if ch = #"+" (* Remove the + sign *)
            then read_number false src'
            else if ch = #"-" orelse ch = #"~"
            then read_number true src'
            else (* See if it's a valid digit. *)
                read_number false src
    end
    
    val fromString = StringCvt.scanString scan

    (* Converter to real values. This replaces the basic conversion
       function for reals installed in the bootstrap process.
       For more information see convInt in Int. *)
    local
        fun convReal (s: string) : real =
        let
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

end;

structure Math = Real.Math;

structure LargeReal: REAL = Real;

(* Values available unqualified at the top-level. *)
val real : int -> real = Real.fromInt 
val trunc : real -> int = Real.trunc 
val floor : real -> int = Real.floor 
val ceil : real -> int = Real.ceil 
val round : real -> int =Real.round;

(* Install print function. *)
local
    fun print_real _ _ (r: real) =
        PolyML.PrettyString(Real.fmt (StringCvt.GEN(SOME 10)) r)
in
    val () = PolyML.addPrettyPrinter print_real;
end;
