(*
    Title:      Standard Basis Library: Real Signature
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2008, 2016

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

signature REAL =
sig
    type  real

    structure Math : MATH
        where type real = real

    val radix : int
    val precision : int

    val maxFinite : real
    val minPos : real
    val minNormalPos : real

    val posInf : real
    val negInf : real

    val + : (real * real) -> real
    val - : (real * real) -> real
    val * : (real * real) -> real
    val / : (real * real) -> real
    val *+ : real * real * real -> real
    val *- : real * real * real -> real

    val ~ : real -> real
    val abs : real -> real

    val min : (real * real) -> real
    val max : (real * real) -> real

    val sign : real -> int
    val signBit : real -> bool
    val sameSign : (real * real) -> bool
    val copySign : (real * real) -> real
    val compare : (real * real) -> General.order
    val compareReal : (real * real) -> IEEEReal.real_order

    val < : (real * real) -> bool
    val <= : (real * real) -> bool
    val > : (real * real) -> bool
    val >= : (real * real) -> bool

    val == : (real * real) -> bool
    val != : (real * real) -> bool

    val ?= : (real * real) -> bool

    val unordered : (real * real) -> bool

    val isFinite : real -> bool
    val isNan : real -> bool
    val isNormal : real -> bool
    val class : real -> IEEEReal.float_class

    val fmt : StringCvt.realfmt -> real -> string
    val toString : real -> string
    val fromString : string -> real option

    val scan : (char, 'a) StringCvt.reader -> (real, 'a) StringCvt.reader

    val toManExp : real -> {man : real, exp : int}
    val fromManExp : {man : real, exp : int} -> real

    val split : real -> {whole : real, frac : real}
    val realMod : real -> real

    val rem : (real * real) -> real

    val nextAfter : (real * real) -> real

    val checkFloat : real ->real

    val realFloor : real -> real
    val realCeil : real -> real
    val realTrunc : real -> real
    val realRound : real -> real

    val floor : real -> Int.int
    val ceil : real -> Int.int
    val trunc : real -> Int.int
    val round : real -> Int.int

    val toInt : IEEEReal.rounding_mode -> real -> int
    val toLargeInt : IEEEReal.rounding_mode -> real -> LargeInt.int

    val fromInt : int -> real
    val fromLargeInt : LargeInt.int -> real

    val toLarge : real -> LargeReal.real
    val fromLarge : IEEEReal.rounding_mode -> LargeReal.real -> real

    val toDecimal : real -> IEEEReal.decimal_approx
    val fromDecimal : IEEEReal.decimal_approx -> real option
end;
