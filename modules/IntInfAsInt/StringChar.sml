(*
    Title:      Rebuild the basis library: String and char.
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

(* Char and String *)
useBasis "StringSignatures";

structure Char: CHAR =
struct
    open Char
    val maxOrd = FixedInt.toInt maxOrd
    val chr = chr o FixedInt.fromInt
    val ord = FixedInt.toInt o ord
end;

structure String: STRING =
struct
    open String
    val maxSize = FixedInt.toInt maxSize
    val size = FixedInt.toInt o size
    val sub = fn (s, i) => sub(s, FixedInt.fromInt i)
    val substring = fn (s, i, j) => substring(s, FixedInt.fromInt i, FixedInt.fromInt j)
    val extract = fn(s, i, j) => extract(s, FixedInt.fromInt i, Option.map FixedInt.fromInt j)
end;

structure Substring: SUBSTRING =
struct
    open Substring
    val base = fn s => let val (a, i, j) = base s in (a, FixedInt.toInt i, FixedInt.toInt j) end
    val size = FixedInt.toInt o size
    val sub = fn (s, i) => sub(s, FixedInt.fromInt i)
    val substring = fn (s, i, j) => substring(s, FixedInt.fromInt i, FixedInt.fromInt j)
    val extract = fn(s, i, j) => extract(s, FixedInt.fromInt i, Option.map FixedInt.fromInt j)
    val splitAt = fn (s, i) => splitAt(s, FixedInt.fromInt i)
    val slice = fn (s, i, j) => slice(s, FixedInt.fromInt i, Option.map FixedInt.fromInt j)
    val trimr = fn i => trimr(FixedInt.fromInt i)
    and triml = fn i => triml(FixedInt.fromInt i)
end;

val ord : char -> int = Char.ord 
val chr : int -> char = Char.chr 
val substring : string * int * int -> string = String.substring;
val size: string -> int = String.size;
