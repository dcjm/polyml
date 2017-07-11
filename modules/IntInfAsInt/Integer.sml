(*
    Title:      Rebuild the basis library: integer
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


type int = IntInf.int;

(* Integer *)
(* Define this first.  It's explicitly referenced in the INTEGER signature. *)
structure Int = struct type int = IntInf.int end;

(* This uses Int.int so needs to be rebuilt. *)
useBasis "INTEGER";

signature INT_INF =
sig
    include INTEGER
    val divMod : int * int -> int * int
    val quotRem : int * int -> int * int
    val pow : int * Int.int -> int
    val log2 : int -> Int.int
    val orb : int * int -> int
    val xorb : int * int -> int
    val andb : int * int -> int
    val notb : int -> int
    val << : int * Word.word -> int
    val ~>> : int * Word.word -> int
end;

structure IntInf: INT_INF =
struct
    open IntInf
    val toInt = toLarge and fromInt = fromLarge
    val precision = Option.map FixedInt.toLarge precision
    val sign = FixedInt.toLarge o sign
    val log2 = FixedInt.toLarge o log2
    val pow = fn (i, j) => pow(i, FixedInt.fromLarge j)
end;

structure LargeInt: INTEGER = IntInf;

structure Int: INTEGER = LargeInt;

structure FixedInt: INTEGER =
struct
    open FixedInt
    val toInt = toLarge and fromInt = fromLarge
    val precision = Option.map toLarge precision
    val sign = FixedInt.toLarge o sign
end;

val () =
    case FixedInt.precision of SOME 31 => useBasis "Int31.sml" | SOME 63 => useBasis "Int63.sml" | _ => ();
