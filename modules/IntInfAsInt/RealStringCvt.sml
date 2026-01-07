(*
    Title:      Rebuild the basis library: Real and StringCvt
    Copyright   David C.J. Matthews 2016

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

(* Real *)
useBasis "IEEE_REAL.sml";
structure IEEEReal: IEEE_REAL =
struct
    open IEEEReal
    type decimal_approx =
        { class : float_class, sign : bool, digits : int list, exp : int }
    
    local
        fun toNewDA {class, sign, digits, exp } : decimal_approx =
            {class=class, sign=sign, digits = map FixedInt.toLarge digits, exp = FixedInt.toLarge exp }
        and fromNewDA ({class, sign, digits, exp } : decimal_approx) =
            {class=class, sign=sign, digits = map FixedInt.fromLarge digits, exp = FixedInt.fromLarge exp }
    in
        val toString = toString o fromNewDA 
        val scan = fn getc => fn src => Option.map(fn (v, c) => (toNewDA v, c)) (scan getc src)
        and fromString = (Option.map toNewDA) o fromString
    end
end;

(* There's a complication.  We need access to both the old and new versions of
   the StringCvt.realfmt datatype. *)
local
    structure OldStringCvt = StringCvt
in
    structure StringCvt: STRING_CVT =
    struct
        open StringCvt

        datatype realfmt
          = SCI of int option
          | FIX of int option
          | GEN of int option
          | EXACT

        val padRight = fn c => fn i => padRight c (FixedInt.fromInt i)
        and padLeft  = fn c => fn i => padLeft c (FixedInt.fromInt i)
    end;

    structure Real =
    struct
        open Real
        val radix = FixedInt.toLarge radix
        val precision = FixedInt.toLarge precision
        val sign = FixedInt.toLarge o sign
        val toManExp = fn r => let val {man, exp} = toManExp r in {man=man, exp= FixedInt.toLarge exp} end
        and fromManExp = fn {man, exp} => fromManExp{man=man, exp=FixedInt.fromLarge exp }
        val toInt = toLargeInt
        and fromInt = fromLargeInt

        val floor = toLargeInt IEEEReal.TO_NEGINF
        and ceil = toLargeInt IEEEReal.TO_POSINF
        and trunc = toLargeInt IEEEReal.TO_ZERO
        and round = toLargeInt IEEEReal.TO_NEAREST

        val toDecimal =
            fn r =>
                let
                    val {class, sign, digits, exp } = toDecimal r
                in
                    {class=class, sign=sign, digits = map FixedInt.toLarge digits, exp = FixedInt.toLarge exp }
                end
    
        val fromDecimal =
            fn {class, sign, digits, exp } =>
                fromDecimal {class=class, sign=sign, digits = map FixedInt.fromLarge digits, exp = FixedInt.fromLarge exp }

        local
            fun rfmt (StringCvt.SCI(SOME s)) r = fmt (OldStringCvt.SCI(SOME(FixedInt.fromLarge s))) r
            |   rfmt (StringCvt.SCI NONE) r = fmt (OldStringCvt.SCI NONE) r
            |   rfmt (StringCvt.FIX(SOME s)) r = fmt (OldStringCvt.FIX(SOME(FixedInt.fromLarge s))) r
            |   rfmt (StringCvt.FIX NONE) r = fmt (OldStringCvt.FIX NONE) r
            |   rfmt (StringCvt.GEN(SOME s)) r = fmt (OldStringCvt.GEN(SOME(FixedInt.fromLarge s))) r
            |   rfmt (StringCvt.GEN NONE) r = fmt (OldStringCvt.GEN NONE) r
            |   rfmt StringCvt.EXACT r = fmt OldStringCvt.EXACT r
        in
            val fmt = rfmt
        end
    end
end;

useBasis "REAL.sig"; (* This uses IEEEReal and the new StringCvt and decimal_approx *)
structure Real: REAL = Real;
structure LargeReal = Real;

val real : int -> real = Real.fromInt 
val trunc : real -> int = Real.trunc 
val floor : real -> int = Real.floor 
val ceil : real -> int = Real.ceil 
val round : real -> int =Real.round;
