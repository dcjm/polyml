(*
    Title:      Standard Basis Library: IntInf structure and signature.
    Copyright   David Matthews 2000, 2016-17, 2023

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

structure IntInf : INT_INF =
struct
    type int = LargeInt.int
    
    val largeIntToWord: int -> word = RunCall.unsafeCast (* Masked out by signature *)

    val quotRem = LibrarySupport.quotRem

    fun divMod (x, y) =
    let
        val (q, r) = quotRem(x, y)
    in
        (* If the remainder is zero or the same sign as the
           divisor then the result is the same as quotRem.
           Otherwise round down the quotient and round up the remainder. *)
        if r = 0 orelse (r < 0) = (y < 0)
        then (q, r)
        else (q-1, r+y)
    end

    (* Return the position of the highest bit set in the value. *)
    local
        val log2Long: int -> Int.int = RunCall.rtsCallFast1 "PolyLog2Arbitrary";
    in
        fun log2 (i: int) : Int.int =
            if i <= 0 then raise Domain
            else if LibrarySupport.largeIntIsSmall i
            then Word.toInt(LibrarySupport.log2Word(largeIntToWord i))
            else log2Long i
    end

    local
    (* These are implemented in the RTS. *)
        val orbFn  : int * int -> int = RunCall.rtsCallFull2 "PolyOrArbitrary"
        and xorbFn : int * int -> int = RunCall.rtsCallFull2 "PolyXorArbitrary"
        and andbFn : int * int -> int = RunCall.rtsCallFull2 "PolyAndArbitrary"
        
        open LibrarySupport
    in
        (* Handle the short cases using the word operations.
           The special cases of or-ing with a short negative value
           and and-ing with a short positive also always produce short
           results but extracting the low-order word from the long-format
           argument involves an RTS call so it's not worthwhile. *)
        fun orb(i, j) =
            if largeIntIsSmall i andalso largeIntIsSmall j
            then Word.toLargeIntX(Word.orb(largeIntToWord i, largeIntToWord j))
            else orbFn(i, j)
        
        fun andb(i, j) =
            if largeIntIsSmall i andalso largeIntIsSmall j
            then Word.toLargeIntX(Word.andb(largeIntToWord i, largeIntToWord j))
            else andbFn(i, j)
        
        fun xorb(i, j) =
            if largeIntIsSmall i andalso largeIntIsSmall j
            then Word.toLargeIntX(Word.xorb(largeIntToWord i, largeIntToWord j))
            else xorbFn(i, j)
    end
    
    (* notb is defined as ~ (i+1) and there doesn't seem to be much advantage
       in implementing it any other way. *)
    fun notb i = ~(i + 1)
    
    local
        fun power(acc: LargeInt.int, _, 0w0) = acc
        |   power(acc, n, i) =
                power(
                    if Word.andb(i, 0w1) = 0w1
                    then acc * n
                    else acc,
                    n * n, Word.>>(i, 0w1)
                    )
    in
        fun pow(i: LargeInt.int, j: Int.int) =
            if j < 0
            then(* Various exceptional cases. *)
            (
                if i = 0 then raise Div
                else if i = 1 then 1
                else if i = ~1
                then if Int.rem(j, 2) = 0 then (*even*) 1 else (*odd*) ~1
                else 0
            )
            else if LibrarySupport.isShortInt j
            then power(1, i, Word.fromInt j)
            else
            (* Long: This is possible only if int is arbitrary precision.  If the
               value to be multiplied is anything other than 0 or 1
               we'll exceed the maximum size of a cell. *)
                if i = 0 then 0
            else if i = 1 then 1
            else raise Size
    end
    
    local
        val shiftR: LargeInt.int * word -> LargeInt.int = RunCall.rtsCallFull2 "PolyShiftRightArbitrary"
        and shiftL: LargeInt.int * word -> LargeInt.int = RunCall.rtsCallFull2 "PolyShiftLeftArbitrary"
        val maxShortShift = LibrarySupport.wordSize * 0w8 - 0w2
    in
        fun << (i: int, j: Word.word) =
            (* We can use a word shift provided the value is short and will not shift any bits out or
               into the sign bit.  This will only work for positive integers.  It would be possible
               to use this for negative integers by negating the argument and the result. *)
            if i = 0 orelse j = 0w0 then i
            else if LibrarySupport.largeIntIsSmall i andalso LibrarySupport.log2Word(largeIntToWord i) + j < maxShortShift
            then Word.toLargeIntX(Word.<<(largeIntToWord i, j))
            else shiftL(i, j)

        fun ~>> (i: int, j: Word.word) =
            if LibrarySupport.largeIntIsSmall i
            then Word.toLargeIntX(Word.~>>(largeIntToWord i, j))
            else shiftR(i, j)
    end

    open LargeInt (* Inherit everything from LargeInt.  Do this last because it overrides the overloaded functions. *)
end;
