(*
    Title:      ASN1 support.
    Author:     David Matthews
    Copyright   David Matthews 2015-16, 2019, 2020

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

(**
These functions provide assistance in the encoding and decoding of ASN1
binary encoding.
**)

signature ASN1 =
sig
    datatype form = Primitive | Constructed

    datatype tagType =
        Universal of int * form
    |   Application of int * form
    |   Context of int * form
    |   Private of int * form

    val asn1Boolean: tagType  and asn1Integer: tagType
    and asn1BitString: tagType and asn1OctetString: tagType

    (* Parse the tag and length information to extract the first tag/value pair from the
       input.  Returns with the reader pointing at the start of the data. *)
    val readHeader: (Word8.word, 'a) StringCvt.reader -> ((tagType * int), 'a) StringCvt.reader

    (* Parse the tag and length information to extract the first tag/value pair from the
       input.  Returns the remainder of the input. *)
    val decodeItem: Word8VectorSlice.slice ->
        {tag: tagType, data: Word8VectorSlice.slice, remainder: Word8VectorSlice.slice} option

    val decodeInt: Word8VectorSlice.slice -> int
    and decodeLargeInt: Word8VectorSlice.slice -> LargeInt.int
    and decodeString: Word8VectorSlice.slice -> string
    and decodeBool: Word8VectorSlice.slice -> bool

    (* Encode a tag/value pair. *)
    val encodeItem: tagType * Word8Vector.vector list -> Word8Vector.vector list
    
    val encodeInt: int -> Word8Vector.vector
    and encodeString: string -> Word8Vector.vector
    and encodeBool: bool -> Word8Vector.vector
end;

structure Asn1: ASN1 =
struct
    datatype form = Primitive | Constructed

    datatype tagType =
        Universal of int * form
    |   Application of int * form
    |   Context of int * form
    |   Private of int * form

    (* A few standard tags *)
    val asn1Boolean = Universal(1, Primitive)
    and asn1Integer = Universal(2, Primitive)
    and asn1BitString = Universal(3, Primitive) (* Could also be constructed *)
    and asn1OctetString = Universal(4, Primitive) (* Could also be constructed *)

    open Word8VectorSlice
    (* Convert the length data.  The first byte is either the length itself, if it
       is less than 128 otherwise it is the number of bytes containing the length. *)

    fun getLength getNext p =
        case getNext p of
            SOME (n, t) =>
            if n < 0wx80 then SOME(Word8.toInt n, t)
            else
            let
                fun getL(0w0, m, l) = SOME(m, l)
                |   getL(n, m, t) =
                        case getNext t of
                            SOME (hd, tl) => getL(n-0w1, m * 256 + Word8.toInt hd, tl)
                        |   NONE => NONE
                val lengthOfLength = Word8.andb(n, 0wx7f)
            in
                if lengthOfLength = 0w0
                then raise Fail "Indefinite length is not implemented"
                else getL(lengthOfLength, 0, t)
            end
        |   NONE => NONE

    fun readHeader getNext input =
        case getNext input of
            SOME (code, t) =>
                let
                    (* The type is encoded in the top two bits of the first byte. *)
                    val tagType: int * form -> tagType =
                        case Word8.andb(code, 0wxc0) of
                            0wx00 => Universal
                        |   0wx40 => Application
                        |   0wx80 => Context
                        |   _     => Private

                    val sc = if Word8.andb(code, 0wx20) = 0w0 then Primitive else Constructed

                    (* The tag is the bottom five bits except that if it is 0x1f
                       the tag is encoded in subsequent bytes. *)
                    val tagRest =
                        case Word8.andb(code, 0w31) of
                            0w31 => (* This is a long-format tag *)
                                let
                                    fun decode (acc, seq) =
                                        case getNext seq of
                                            SOME(code, seq') =>
                                            let
                                                (* Keep accumulating the tags until we find a byte with the
                                                   top bit clear. *)
                                                val tag' = acc * 128 + Word8.toInt(Word8.andb(code, 0wx7f))
                                            in
                                                if Word8.andb(code, 0wx80) = 0w0
                                                then SOME(tag', seq')
                                                else decode(tag', seq')
                                            end
                                        |   NONE => NONE
                                in
                                    decode(0, t)
                                end
                        |   firstTag => SOME(Word8.toInt firstTag, t)

                in
                    case tagRest of
                        SOME(tag, rest) =>
                        (
                            case getLength getNext rest of
                                SOME(len, tail) => SOME((tagType(tag, sc), len), tail)
                            |   NONE => NONE
                        )
                    |   NONE => NONE
                end
        |   NONE => NONE

    (* Decode Word8VectorSlice.slice input. *)
    local
        fun getNext n =
            if length n = 0 then NONE
            else SOME(sub(n, 0), subslice(n, 1, NONE))
    in
        fun decodeItem input =
            case readHeader getNext input of
                SOME((tag, len), tail) =>
                    SOME{tag = tag,
                        data = Word8VectorSlice.subslice(tail, 0, SOME len),
                        remainder = Word8VectorSlice.subslice(tail, len, NONE)
                    }
            |   NONE => NONE

        fun decodeLargeInt p =
            case getNext p of
                NONE => 0
            |   SOME(h, tl) =>
                let
                    fun parseRest(n, p) =
                        case getNext p of
                            NONE => n
                        |   SOME (hd, tl) => parseRest(n * 256 + Word8.toLargeInt hd, tl)
                in
                    parseRest(Word8.toLargeIntX h, tl)
                end

        val decodeInt = LargeInt.toInt o decodeLargeInt
    end

    fun decodeString t = Byte.bytesToString(vector t)
    
    and decodeBool p = decodeInt p <> 0


    fun encodeItem (tag, value) =
    let
        open Word8Vector

        fun encodeTag(tagType, tagValue) =
            if tagValue < 31
            then [Word8.orb(tagType, Word8.fromInt tagValue)]
            else
            let
                (* Set the top bit on all bytes except the last. *)
                fun addToList(n, []) = [Word8.fromInt n]
                |   addToList(n, t) = Word8.fromInt(128 + n) :: t

                fun encode(n, t) =
                    if n < 128
                    then addToList(n, t)
                    else encode(n div 128, addToList(n mod 128, t))
            in
                Word8.orb(tagType, 0w31) :: encode(tagValue, [])
            end

        val tagCode =
            case tag of
                Universal (t, Primitive)       => encodeTag(0wx00, t)
            |   Universal (t, Constructed)     => encodeTag(0wx20, t)
            |   Application (t, Primitive)     => encodeTag(0wx40, t)
            |   Application (t, Constructed)   => encodeTag(0wx60, t)
            |   Context (t, Primitive)         => encodeTag(0wx80, t)
            |   Context (t, Constructed)       => encodeTag(0wxa0, t)
            |   Private (t, Primitive)         => encodeTag(0wxc0, t)
            |   Private (t, Constructed)       => encodeTag(0wxe0, t)

        (* Encode the length the argument. *)
        val length = List.foldl(fn (a, b) => length a + b) 0 value
        
        val lengthCode =
            if length < 128
            then [Word8.fromInt length]
            else
            let
                fun encodeLength (0, t) = t
                |   encodeLength (v, t) = encodeLength(v div 256, Word8.fromInt(v mod 256) :: t)

                val encodedLength = encodeLength(length, [])
            in
                Word8.orb(0wx80, Word8.fromInt(List.length encodedLength)) :: encodedLength
            end
    in
        fromList(tagCode @ lengthCode) :: value
    end

    fun encodeInt n =
    let
        fun encode (n, t) =
        let
            val lo = Word8.fromInt n (* Bottom byte *)
            val hi = n div 256
        in
            (* If the high byte is 0 or -1 and the sign bit is already
               correct we've finished. *)
            if hi = 0 andalso lo < 0w128 orelse hi = ~1 andalso lo >= 0w128
            then lo :: t
            else encode(hi, lo :: t)
        end
    in
        Word8Vector.fromList(encode(n, []))
    end

    val encodeString = Byte.stringToBytes
    
    fun encodeBool b = encodeInt(if b then 1 else 0)
end;
