(*
    Title:      Standard Basis Library: PACK_WORD signature and structures
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* G&R 2004 status: updated, the structure names have changed. *)

signature PACK_WORD =
sig
    val bytesPerElem : int
    val isBigEndian : bool
    val subVec  : Word8Vector.vector * int -> LargeWord.word
    val subVecX : Word8Vector.vector * int -> LargeWord.word
    val subArr  : Word8Array.array * int -> LargeWord.word
    val subArrX : Word8Array.array * int -> LargeWord.word
    val update : Word8Array.array * int * LargeWord.word -> unit
end;

(* I'm not sure what use Pack8Big and Pack8Little are since
   they don't do any packing. *)
structure PackWord8Big : PACK_WORD =
struct
    val bytesPerElem = 1
    val isBigEndian = true
    val subVec = Word8.toLargeWord o Word8Vector.sub
    val subVecX = Word8.toLargeWordX o Word8Vector.sub
    val subArr = Word8.toLargeWord o Word8Array.sub
    val subArrX = Word8.toLargeWordX o Word8Array.sub
    fun update(a, i, v) = Word8Array.update(a, i, Word8.fromLargeWord v)
end;

structure PackWord8Little : PACK_WORD =
struct
(* Exactly the same as Pack8Big except for the value of isBigEndian *)
    open PackWord8Big
    val isBigEndian = false
end;

local
    infix << >>
    infix andb
    infix orb
    val op orb = LargeWord.orb
    and op << = LargeWord.<<
    and op >> = LargeWord.>>

    fun twoBytesToWord(hi: Word8.word, lo: Word8.word) =
        (Word8.toLargeWord hi << 0w8) orb Word8.toLargeWord lo

    fun twoBytesToWordX(hi: Word8.word, lo: Word8.word) =
        (Word8.toLargeWordX hi << 0w8) orb Word8.toLargeWord lo

    fun fourBytesToWord(highest, higher, lower, low) =
        (Word8.toLargeWord highest << 0w24) orb 
        (Word8.toLargeWord higher << 0w16) orb
        (Word8.toLargeWord lower << 0w8) orb 
        Word8.toLargeWord low

    fun fourBytesToWordX(highest, higher, lower, low) =
        (Word8.toLargeWordX highest << 0w24) orb 
        (Word8.toLargeWord higher << 0w16) orb
        (Word8.toLargeWord lower << 0w8) orb 
        Word8.toLargeWord low

in
    structure PackWord16Big : PACK_WORD =
    struct
        val bytesPerElem = 2
        val isBigEndian = true

        fun subVec(a, i) =
            twoBytesToWord(
                Word8Vector.sub(a, i*2), Word8Vector.sub(a, i*2+1))

        fun subVecX(a, i) =
            twoBytesToWordX(
                Word8Vector.sub(a, i*2), Word8Vector.sub(a, i*2+1))

        fun subArr(a, i) =
            twoBytesToWord(
                Word8Array.sub(a, i*2), Word8Array.sub(a, i*2+1))

        fun subArrX(a, i) =
            twoBytesToWordX(
                Word8Array.sub(a, i*2), Word8Array.sub(a, i*2+1))

        fun update(a, i, v) =
            (* Check the index before doing any update. *)
            if i < 0 orelse i*2+1 >= Word8Array.length a
            then raise Subscript
            else
            (Word8Array.update(a, i*2+1, Word8.fromLargeWord v);
             Word8Array.update(a, i*2, Word8.fromLargeWord(v >> 0w8))
            )
    end;

    structure PackWord16Little : PACK_WORD =
    struct
        val bytesPerElem = 2
        val isBigEndian = false

        fun subVec(a, i) =
            twoBytesToWord(
                Word8Vector.sub(a, i*2+1), Word8Vector.sub(a, i*2))

        fun subVecX(a, i) =
            twoBytesToWordX(
                Word8Vector.sub(a, i*2+1), Word8Vector.sub(a, i*2))

        fun subArr(a, i) =
            twoBytesToWord(
                Word8Array.sub(a, i*2+1), Word8Array.sub(a, i*2))

        fun subArrX(a, i) =
            twoBytesToWordX(
                Word8Array.sub(a, i*2+1), Word8Array.sub(a, i*2))

        fun update(a, i, v) =
            (* Check the index before doing any update. *)
            if i < 0 orelse i*2+1 >= Word8Array.length a
            then raise Subscript
            else
            (Word8Array.update(a, i*2, Word8.fromLargeWord v);
             Word8Array.update(a, i*2+1, Word8.fromLargeWord(v >> 0w8))
            )
    end;

    structure PackWord32Big : PACK_WORD =
    struct
        val bytesPerElem = 4
        val isBigEndian = true

        fun subVec(a, i) =
            fourBytesToWord(
                Word8Vector.sub(a, i*4), Word8Vector.sub(a, i*4+1),
                Word8Vector.sub(a, i*4+2), Word8Vector.sub(a, i*4+3))

        fun subVecX(a, i) =
            fourBytesToWordX(
                Word8Vector.sub(a, i*4), Word8Vector.sub(a, i*4+1),
                Word8Vector.sub(a, i*4+2), Word8Vector.sub(a, i*4+3))

        fun subArr(a, i) =
            fourBytesToWord(
                Word8Array.sub(a, i*4), Word8Array.sub(a, i*4+1),
                Word8Array.sub(a, i*4+2), Word8Array.sub(a, i*4+3))

        fun subArrX(a, i) =
            fourBytesToWordX(
                Word8Array.sub(a, i*4), Word8Array.sub(a, i*4+1),
                Word8Array.sub(a, i*4+2), Word8Array.sub(a, i*4+3))

        fun update(a, i, v) =
            (* Check the index before doing any update. *)
            if i < 0 orelse i*4+3 >= Word8Array.length a
            then raise Subscript
            else
            (Word8Array.update(a, i*4+3, Word8.fromLargeWord v);
             Word8Array.update(a, i*4+2, Word8.fromLargeWord(v >> 0w8));
             Word8Array.update(a, i*4+1, Word8.fromLargeWord(v >> 0w16));
             Word8Array.update(a, i*4, Word8.fromLargeWord(v >> 0w24))
            )
    end;

    structure PackWord32Little : PACK_WORD =
    struct
        val bytesPerElem = 4
        val isBigEndian = false

        fun subVec(a, i) =
            fourBytesToWord(
                Word8Vector.sub(a, i*4+3), Word8Vector.sub(a, i*4+2),
                Word8Vector.sub(a, i*4+1), Word8Vector.sub(a, i*4))

        fun subVecX(a, i) =
            fourBytesToWordX(
                Word8Vector.sub(a, i*4+3), Word8Vector.sub(a, i*4+2),
                Word8Vector.sub(a, i*4+1), Word8Vector.sub(a, i*4))

        fun subArr(a, i) =
            fourBytesToWord(
                Word8Array.sub(a, i*4+3), Word8Array.sub(a, i*4+2),
                Word8Array.sub(a, i*4+1), Word8Array.sub(a, i*4))

        fun subArrX(a, i) =
            fourBytesToWordX(
                Word8Array.sub(a, i*4+3), Word8Array.sub(a, i*4+2),
                Word8Array.sub(a, i*4+1), Word8Array.sub(a, i*4))

        fun update(a, i, v) =
            (* Check the index before doing any update. *)
            if i < 0 orelse i*4+3 >= Word8Array.length a
            then raise Subscript
            else
            (Word8Array.update(a, i*4, Word8.fromLargeWord v);
             Word8Array.update(a, i*4+1, Word8.fromLargeWord(v >> 0w8));
             Word8Array.update(a, i*4+2, Word8.fromLargeWord(v >> 0w16));
             Word8Array.update(a, i*4+3, Word8.fromLargeWord(v >> 0w24))
            )
    end

end;

