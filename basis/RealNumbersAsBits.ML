(*
    Title:      Standard Basis Library: Real number support
    Author:     David Matthews
    Copyright   David Matthews 2023

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

(* Extract the components of Real.real (double) and Real32.real (float).
   The formats, particularly of Real32.real, differ between 32-bit and
   64-bit systems. *)

structure RealNumbersAsBits:
sig
    (* Floats.  The mantissa will fit in a short int in both 32- and 64-bits . *)
    val floatSignBit: Real32.real -> bool
    and floatExponent: Real32.real -> int
    and floatMantissa: Real32.real -> int
    and floatFromBinary: {sign: bool, exp: int, mantissa: int} -> Real32.real
    
    (* Doubles.  The mantissa is too large for a fixed int in 32-bit mode. *)
    val doubleSignBit: real -> bool
    and doubleExponent: real -> int
    and doubleMantissa: real -> LargeInt.int
    and doubleFromBinary: {sign: bool, exp: int, mantissa: LargeInt.int} -> real
end
=
struct

    local
        (* IEEE-754 format.
           Float: 1 sign bit, 8 exponent bits, 23 mantissa bits. *)
        (* In native 64-bit a float is implemented as a tagged value.  The top
           32 bits contain the float, the lower 32-bits are zeros except for the tag.
           Because word values are tagged this is equivalent to a shift of 31 bits in
           ML.
           In native 32-bit and 32-in-64 a float is implemented as a boxed value. *)
        open LibrarySupport
        val floatIsTagged = wordSize = 0w8
        val floatAsWord: Real32.real->word = RunCall.unsafeCast
        val floatAsInt: Real32.real->int = RunCall.unsafeCast
        val wordAsFloat: word->Real32.real = RunCall.unsafeCast
        fun byte(r: Real32.real, b: word): Word8.word =
            RunCall.loadByteFromImmutable(r, if bigEndian then b else 0w3-b)
        and setByte(r: Real32.real, b: word, v: Word8.word) =
            RunCall.storeByte(r, if bigEndian then b else 0w3-b, v)
    in
        fun floatSignBit (r: Real32.real): bool =
            if floatIsTagged then floatAsInt r < 0 else byte(r, 0w0) >= 0w128

        fun floatExponent (r: Real32.real): int =
        if floatIsTagged
        then Word.toInt(Word.andb(Word.>>(floatAsWord r, 0w23+0w31), 0wxff))
        else Word8.toInt(Word8.andb(byte(r, 0w0), 0wx7f)) * 2 +
                    Word8.toInt(Word8.>>(byte(r, 0w1), 0w7))

        fun floatMantissa (r: Real32.real): int =
        if floatIsTagged
        then Word.toInt(Word.andb(Word.>>(floatAsWord r, 0w31), 0wx7fffff))
        else (Word8.toInt(Word8.andb(byte(r, 0w1), 0wx7f)) * 256 +
                Word8.toInt(byte(r, 0w2))) * 256 +
                    Word8.toInt(byte(r, 0w3))

        fun floatFromBinary{sign: bool, exp: int, mantissa: int}: Real32.real =
        if floatIsTagged
        then
        let
            val signBit = if sign then Word.<<(0w1, 0w31+0w31) else 0w0
            val expo = Word.<<(Word.fromInt exp, 0w23+0w31)
            (* This assumes that the mantissa value is not too large. *)
            val mant = Word.<<(Word.fromInt mantissa, 0w31)
        in
            wordAsFloat(Word.orb(signBit, Word.orb(expo, mant)))
        end
        else
        let
            val r: Real32.real = RunCall.allocateByteMemory(0w1, 0wx41)
            val b0 = Word8.orb(if sign then 0wx80 else 0w0, Word8.fromInt(Int.toInt(Int.quot(exp, 2))))
            val () = setByte(r, 0w0, b0)
            (* The low order 24 bits will always fit in a word. *)
            val b = Word.orb(Word.<<(Word.fromInt exp, 0w23), Word.fromInt mantissa)
            fun w8fromW x = Word8.fromLarge(Word.toLarge x)
            val () = setByte(r, 0w1, w8fromW(Word.>>(b, 0w16)))
            val () = setByte(r, 0w2, w8fromW(Word.>>(b, 0w8)))
            val () = setByte(r, 0w3, w8fromW b)
            val () = RunCall.clearMutableBit r
        in
            r
        end
    end

    local
        (* IEEE-754 format.
           Double: 1 sign bit, 11 exponent bits, 52 mantissa bits. *)
        open LibrarySupport
        (* In native 64-bit and 32-in-64 Real.real and LargeWord.word are both boxed
           64-bit quantities. *)
        val realAsWord64: real -> LargeWord.word = RunCall.unsafeCast
        and word64AsReal: LargeWord.word -> real = RunCall.unsafeCast
        val realIsWord64 = sysWordSize = 0w8
        fun byte(r: real, b: word): Word8.word =
            RunCall.loadByteFromImmutable(r, if bigEndian then b else 0w7-b)
        and setByte(r: real, b: word, v: Word8.word) =
            RunCall.storeByte(r, if bigEndian then b else 0w7-b, v)
        (* We use this mask when LargeWord.word is 64-bits.  We don't write out
           the constant directly because it would cause an overflow when compiled in
           32-bit mode even though it's not used. *)
        val doubleMantissaMask = LargeWord.>>(LargeWord.fromInt ~1, 0w12)
    in
        fun doubleSignBit (r: real) : bool = byte(r, 0w0) >= 0w128

        fun doubleExponent (r: real): int =
            Int.fromInt(
                Word8.toInt(Word8.andb(byte(r, 0w0), 0wx7f)) * 16 +
                    Word8.toInt(Word8.>>(byte(r, 0w1), 0w4)))

        fun doubleMantissa (r: real): LargeInt.int =
        if realIsWord64
        then LargeWord.toLargeInt(LargeWord.andb(realAsWord64 r, doubleMantissaMask))
        else
            (((((Word8.toLargeInt(Word8.andb(byte(r, 0w1), 0wxf)) * 256 +
                Word8.toLargeInt(byte(r, 0w2))) * 256 +
                    Word8.toLargeInt(byte(r, 0w3))) * 256 +
                        Word8.toLargeInt(byte(r, 0w4))) * 256 +
                            Word8.toLargeInt(byte(r, 0w5))) * 256 + 
                                Word8.toLargeInt(byte(r, 0w6))) * 256 +
                                    Word8.toLargeInt(byte(r, 0w7))

        fun doubleFromBinary{sign: bool, exp: int, mantissa: LargeInt.int}: real =
        if realIsWord64
        then (* We can construct the value as a LargeWord.word and then cast it as a real. *)
        let
            val signBit = if sign then LargeWord.<<(0w1, 0w63) else 0w0
            val expo = LargeWord.<<(LargeWord.fromInt exp, 0w52)
            (* This assumes that the mantissa value is not too large. *)
            val mant = LargeWord.fromLargeInt mantissa
        in
            word64AsReal(LargeWord.orb(signBit, LargeWord.orb(expo, mant)))
        end
        else
        let
            val r: real = RunCall.allocateByteMemory(0w8 div wordSize, 0wx41)
            val b0 = Word8.orb(if sign then 0wx80 else 0w0, Word8.fromInt(Int.toInt(Int.quot(exp, 16))))
            val () = setByte(r, 0w0, b0)
            val b1 =
                Word8.orb(
                    Word8.<<(Word8.fromInt(Int.rem(exp, 16)), 0w4),
                        Word8.andb(Word8.fromLargeInt(IntInf.~>>(mantissa, 0w48)), 0wxf))
            val () = setByte(r, 0w1, b1)
            val () = setByte(r, 0w2, Word8.fromLargeInt(IntInf.~>>(mantissa, 0w40)))
            val () = setByte(r, 0w3, Word8.fromLargeInt(IntInf.~>>(mantissa, 0w32)))
            val () = setByte(r, 0w4, Word8.fromLargeInt(IntInf.~>>(mantissa, 0w24)))
            val () = setByte(r, 0w5, Word8.fromLargeInt(IntInf.~>>(mantissa, 0w16)))
            val () = setByte(r, 0w6, Word8.fromLargeInt(IntInf.~>>(mantissa, 0w8)))
            val () = setByte(r, 0w7, Word8.fromLargeInt mantissa)
            val () = RunCall.clearMutableBit r
        in
            r
        end
    end

end;