(*
    Title:      Standard Basis Library: Pack Real structures and signatures
    Author:     David Matthews
    Copyright   David Matthews 2000, 2015, 2021, 2023

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

signature PACK_REAL =
sig
    type real
    val bytesPerElem : int
    val isBigEndian : bool
    val toBytes   : real -> Word8Vector.vector
    val fromBytes : Word8Vector.vector -> real
    val subVec : Word8Vector.vector * int -> real
    val subArr : Word8Array.array * int -> real
    val update : Word8Array.array * int * real -> unit
end;

(* (Un)Pack a boxed real value.  This is used for double precision
   and also for single precision (Real32.real) on 32-bit platforms. *)

functor PackRealBoxed(
    type realType
    val isBigEndian: bool
    val realSize: word
    
) : PACK_REAL where type real = realType =

struct
    open LibrarySupport
    open LibrarySupport.Word8Array

    type real = realType
    
    val bytesPerElem: int = Word.toInt realSize
    
    val isBigEndian = isBigEndian

    local
        val System_move_bytes: address*address*word*word*word->unit = RunCall.moveBytes

        (* Move bytes, reversing the order. *)
        fun swapOrder(src: address, srcOff: word,
                      dest: address, destOff: word,
                      length: word) =
            if length = 0w0 then ()
            else
            (
                RunCall.storeByte(dest, destOff+length-0w1, RunCall.loadByte(src, srcOff));
                swapOrder(src, srcOff+0w1, dest, destOff, length-0w1)
            )
    in
        fun doMove(src: address, srcOff: word,
                   dest: address, destOff: word, wantBigEndian: bool) =
            if wantBigEndian = bigEndian (* Host byte order = required byte order *)
            then System_move_bytes(src, dest, srcOff, destOff, realSize)
            else (* Host byte order is reverse of required byte order. *)
                swapOrder(src, srcOff, dest, destOff, realSize)
    end

    fun toBytes r =
    let
        val v = allocString realSize
        (* r is actually represented by a pointer to a vector. *)
        val addr: address = RunCall.unsafeCast r
    in
        doMove(addr, 0w0, stringAsAddress v, wordSize, isBigEndian);
        RunCall.clearMutableBit v;
        w8vectorFromString v
    end

    fun fromBytes v =
    (* Raises an exception if the vector is too small and takes the first
       few elements if it's larger. *)
        if Word8Vector.length v < bytesPerElem
        then raise Subscript
        else
        let
            val r = allocBytes realSize
        in
            doMove(w8vectorAsAddress v, wordSize, r, 0w0, isBigEndian);
            RunCall.clearMutableBit r;
            (RunCall.unsafeCast r): real
        end

    fun subVec(v, i) =
    let
        val iW = unsignedShortOrRaiseSubscript i * realSize
    in
        if iW >= Word.fromInt(Word8Vector.length v)
        then raise Subscript (* This IS defined. *)
        else
        let
            val r = allocBytes realSize
        in
            doMove(w8vectorAsAddress v, wordSize + iW, r, 0w0, isBigEndian);
            RunCall.clearMutableBit r;
            (RunCall.unsafeCast r): real
        end
    end

    fun subArr(Array(l, v), i) =
    let
        val iW = unsignedShortOrRaiseSubscript i * realSize
    in
        if iW >= l
        then raise Subscript (* This IS defined. *)
        else
        let
            val r = allocBytes realSize
        in
            doMove(v, iW, r, 0w0, isBigEndian);
            RunCall.clearMutableBit r;
            (RunCall.unsafeCast r): real
        end
    end

    fun update(Array(l, v), i, r) =
    let
        val iW = unsignedShortOrRaiseSubscript i * realSize
    in
        if iW >= l
        then raise Subscript (* This IS defined. *)
        else
        let
            (* r is actually represented by a pointer to a vector. *)
            val addr: address = RunCall.unsafeCast r
        in
            doMove(addr, 0w0, v, iW, isBigEndian)
        end
    end
end;

structure PackRealBig: PACK_REAL =
    PackRealBoxed(type realType = real val isBigEndian = true val realSize = 0w8)
and PackRealLittle: PACK_REAL =
    PackRealBoxed(type realType = real val isBigEndian = false val realSize = 0w8);
