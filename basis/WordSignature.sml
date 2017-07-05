(*
    Title:      Standard Basis Library: WORD Signature
    Copyright   David Matthews 1999, 2005, 2012, 2016

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

(* This depends on there being suitable definitions of both Word and LargeWord
   structures.  We have basic definitions of those before we define this.  *)
signature WORD =
sig
    eqtype  word
    val wordSize : int
    val toLarge : word -> LargeWord.word
    val toLargeX : word -> LargeWord.word
    val toLargeWord : word -> LargeWord.word
    val toLargeWordX : word -> LargeWord.word
    val fromLarge : LargeWord.word -> word
    val fromLargeWord : LargeWord.word -> word
    val toLargeInt : word -> LargeInt.int
    val toLargeIntX : word -> LargeInt.int
    val fromLargeInt : LargeInt.int -> word
    val toInt : word -> Int.int
    val toIntX : word -> Int.int
    val fromInt : Int.int -> word
    val orb : (word * word) -> word
    val xorb : (word * word) -> word
    val andb : (word * word) -> word
    val notb : word -> word
    val << : (word * Word.word) -> word
    val >> : (word * Word.word) -> word
    val ~>> : (word * Word.word) -> word
    val + : (word * word) -> word
    val - : (word * word) -> word
    val * : (word * word) -> word
    val div : (word * word) -> word
    val mod : (word * word) -> word
    val ~ : word -> word
    val compare : (word * word) -> General.order
    val > : (word * word) -> bool
    val < : (word * word) -> bool
    val >= : (word * word) -> bool
    val <= : (word * word) -> bool
    val min : (word * word) -> word
    val max : (word * word) -> word
    val fmt : StringCvt.radix -> word -> string
    val toString : word -> string
    val fromString : string -> word option
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
end;
