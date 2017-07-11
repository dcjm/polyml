(*
    Title:      Rebuild the basis library: Word
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

(* Word, LargeWord etc. *)
useBasis "WordSignature.sml"; (* This depends on Word, Int, etc but the dependencies on Word don't affect this. *)

structure Word: WORD =
struct
    open Word
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;

structure LargeWord: WORD =
struct
    open LargeWord
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;

structure SysWord: WORD =
struct
    open SysWord
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;


structure Word32: WORD =
struct
    open Word32
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;

structure Word8: WORD =
struct
    open Word8
    val wordSize = FixedInt.toLarge wordSize
    val fromInt = fromLargeInt
    and toInt = toLargeInt
    and toIntX = toLargeIntX
end;

val () = if LargeWord.wordSize = 64 then useLocal "Word64.sml" else ();

