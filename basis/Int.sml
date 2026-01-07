(*
    Title:      Standard Basis Library: LargeInt and FixedInt structures
    Copyright   David C.J. Matthews 1999, 2016, 2021

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


(* LargeInt is defined in INITIALISE. *)

signature INTEGER =
sig
    eqtype  int
    val toLarge : int -> LargeInt.int
    val fromLarge : LargeInt.int -> int
    val toInt : int -> Int.int
    val fromInt : Int.int -> int
    val precision : Int.int option

    val minInt : int option
    val maxInt : int option

    val ~ : int -> int
    val * : (int * int) -> int
    val div : (int * int) -> int
    val mod : (int * int) -> int
    val quot : (int * int) -> int
    val rem : (int * int) -> int
    val + : (int * int) -> int
    val - : (int * int) -> int
    val compare : (int * int) -> General.order

    val > : (int * int) -> bool
    val >= : (int * int) -> bool
    val < : (int * int) -> bool
    val <= : (int * int) -> bool

    val abs : int -> int
    val min : (int * int) -> int
    val max : (int * int) -> int
    val sign : int -> Int.int
    val sameSign : (int * int) -> bool
    val fmt : StringCvt.radix -> int -> string
    val toString : int -> string
    val fromString : string -> int option
    val scan : StringCvt.radix -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
end;

structure LargeInt: INTEGER =
struct
    (* Arbitrary precision int. *)
    type int = LargeInt.int

    fun toLarge i = i and fromLarge i = i
    
    (* Whether int is short or long we can just cast it here. *)
    val fromInt: Int.int -> int = RunCall.unsafeCast (* Just a cast. *)
 
    (* If int is fixed precision we have to check that the value will fit.  *)
    fun toInt(i: int): Int.int =
        if Bootstrap.intIsArbitraryPrecision orelse LibrarySupport.largeIntIsSmall i
        then RunCall.unsafeCast i
        else raise Overflow

    val precision = NONE (* Arbitrary precision. *)
    and minInt = NONE
    and maxInt = NONE

    val zero = fromInt 0 (* Avoids repeated use of fromInt. *)

    fun abs (i: int): int = if i >= zero then i else ~ i

    fun compare (i, j) =
        if i < j then General.LESS
        else if i > j then General.GREATER else General.EQUAL

    fun min (i, j) = if i < j then i else j
    and max (i, j) = if i > j then i else j
    
    fun sign i : Int.int = if i = zero then 0 else if i < zero then ~1 else 1
    
    fun sameSign(i, j) =
        if i = zero then j = zero
        else if i < zero then j < zero
        else (* i > 0 *) j > zero

    local
        val fixedIntAsWord: FixedInt.int -> word = RunCall.unsafeCast

        (* To reduce the need for arbitrary precision arithmetic we can try to
           process values in groups. *)
        (* Return the largest short value and the number of digits. *)
        fun maxShort(n, radix, acc) =
            if LibrarySupport.largeIntIsSmall(acc * radix)
            then maxShort(n+1, radix, acc*radix)
            else (acc, fixedIntAsWord n)
        val (maxB, lenB) = maxShort(0, fromInt 2, fromInt 1)
        and (maxO, lenO) = maxShort(0, fromInt 8, fromInt 1)
        and (maxD, lenD) = maxShort(0, fromInt 10, fromInt 1)
        and (maxH, lenH) = maxShort(0, fromInt 16, fromInt 1)
    in
        (* Local function *)
        fun baseOf StringCvt.BIN = (2,  maxB, lenB)
         |  baseOf StringCvt.OCT = (8,  maxO, lenO)
         |  baseOf StringCvt.DEC = (10, maxD, lenD)
         |  baseOf StringCvt.HEX = (16, maxH, lenH)
    end

    local
        open LibrarySupport

        (* Int.toChars turned out to be a major allocation hot-spot in some Isabelle
           examples.  The old code created a list of the characters and then concatenated
           them.  This cost 3 words for each character before the actual string was
           created.  This version avoids that problem.  This has also now been
           modified to reduce the arbitrary precision arithmetic required when the
           value is long.  Instead of reducing it by the radix each time we take off
           chunks of up to the maximum value that can be represented as a short precision
           value. *)

        fun toChar (digit: Int.int): char =
            if digit < 10 then Char.chr(Char.ord(#"0") + digit)
            else (* Hex *) Char.chr(Char.ord(#"A") + digit - 10)
    in
        fun fmt radix i =
        let
            val (base, maxShort, shortChars) = baseOf radix
            val negative = i < zero

            fun toChars(0, chars, continuation, pad) =
                (* Finished the group. *)
                if continuation = zero
                then
                (
                    (* Really finished.  Allocate the string. *)
                    if negative
                    then
                    let
                        val res = allocString(chars+0w1)
                    in
                        RunCall.storeByteInitialise(res, wordSize, #"~");
                        (res, wordSize+0w1)
                    end
                    else (* Positive *) (allocString chars, wordSize)
                )
                else (* Finished this group but have at least one more group. *)
                let
                    val (result, pos) = toCharGroup(continuation, chars + pad)
                    fun addZeros n =
                        if n = pad then ()
                        else (RunCall.storeByteInitialise(result, pos+n, #"0"); addZeros(n+0w1))
                in
                    addZeros 0w0;
                    (result, pos+pad)
                end

            |   toChars(i, chars, continuation, pad) =
                (* More to do in this group. *)
                let
                    (* TODO: We haven't defined Int.quot and Int.rem yet although they 
                       would be faster since we know this is short. *)
                    val ch = toChar (i mod base)
                    (* Get the string. *)
                    val (result, pos) =
                        toChars(i div base, chars+0w1, continuation, pad-0w1)
                in
                    RunCall.storeByteInitialise(result, pos, ch);
                    (result, pos+0w1)
                end

            (* Process a group of characters that will fit in a short
               precision number. *)
            and toCharGroup(i, chars) =
                if LibrarySupport.largeIntIsSmall i
                then toChars(toInt i, chars, zero, 0w0)
                else
                let
                    val (q, r) = quotRem(i, maxShort)
                in
                    toChars(toInt r, chars, q, shortChars)
                end
        in
            if i = zero
            then "0" (* This is the only case where we print a leading zero. *)
            else
            let
                val (result, _) = toCharGroup(abs i, 0w0)
            in
                RunCall.clearMutableBit result;
                result
            end
        end
    end
    
    val toString = fmt StringCvt.DEC
    
    fun scan radix getc src =
        let
        val (base, _, _) = baseOf radix
        val baseAsLarge = fromInt base
        val sixteen = fromInt 16
        
        (* Read the digits, accumulating the result in acc.  isOk is true
           once we have read a valid digit. *)
        fun read_digits src acc isOk =
            case getc src of
                NONE => if isOk then SOME(acc, src) else NONE
              | SOME(ch, src') =>
                if Char.ord ch >= Char.ord #"0"
                   andalso Char.ord ch < (Char.ord #"0" + base)
                then read_digits src'
                        (acc*baseAsLarge + fromInt(Char.ord ch - Char.ord #"0")) true
                else (* Invalid character - either end of number or bad no. *)
                    if isOk then SOME(acc, src) else NONE
                    
        fun read_hex_digits src acc isOk =
            case getc src of
                NONE => if isOk then SOME(acc, src) else NONE
              | SOME(ch, src') =>
                if Char.ord ch >= Char.ord #"0"
                   andalso Char.ord ch <= Char.ord #"9"
                then read_hex_digits src'
                        (acc*sixteen + fromInt(Char.ord ch - Char.ord #"0")) true
                else if Char.ord ch >= Char.ord #"A"
                   andalso Char.ord ch <= Char.ord #"F"
                then read_hex_digits src'
                        (acc*sixteen + fromInt(Char.ord ch - Char.ord #"A" + 10)) true
                else if Char.ord ch >= Char.ord #"a"
                   andalso Char.ord ch <= Char.ord #"f"
                then read_hex_digits src'
                        (acc*sixteen + fromInt(Char.ord ch - Char.ord #"a" + 10)) true
                else (* Invalid character - either end of number or bad no. *)
                    if isOk then SOME(acc, src) else NONE

        (*
           There is a special case with hex numbers.  A hex number MAY begin
           with 0x or 0X e.g. 0x1f0 but need not.  So "0x " and "0xg" are
           both valid and represent the value 0 with "x " and "xg" as the
           continuations of the input.
        *)
        fun read_number src =
            if base = 16
            then (* Hex. *)
                (
                case getc src of
                    NONE => NONE
                  | SOME(ch, src') =>
                        if ch <> #"0"
                        then read_hex_digits src zero false
                        else
                            (
                            case getc src' of
                                NONE => SOME(zero, src') (* Accept the 0 *)
                              | SOME(ch, src'') =>
                                    if ch = #"x" orelse ch = #"X"
                                    then
                                        (
                                        (*
                                           See if the characters after the 0x
                                           form a valid hex number.  If so return
                                           that, if not return the 0 and treat
                                           the rest of the string as starting
                                           with the x. 
                                        *)
                                        case read_hex_digits src'' zero false of
                                            NONE => SOME(zero, src') (* Accept the 0 *)
                                          | res => res
                                        )
                                    else (* Start from the 0. *)
                                        read_hex_digits src zero false
                            )
                )
            else (* Binary, octal and decimal *) read_digits src zero false
        in
        case getc src of
            NONE => NONE
         |  SOME(ch, src') =>
            if Char.isSpace ch (* Skip white space. *)
            then scan radix getc src' (* Recurse *)
            else if ch = #"+" (* Remove the + sign *)
            then read_number src'
            else if ch = #"-" orelse ch = #"~"
            then
                (
                case read_number src' of
                    NONE => NONE
                  | SOME(i, r) => SOME(~i, r)
                )
            else (* See if it's a valid digit. *)
                read_number src
        end
    
    (* TODO: Implement this directly? *)
    val fromString = StringCvt.scanString (scan StringCvt.DEC)

    (* Converter to LargeInt values. *)
    local
        (* The string may be either decimal or hex. *)
        fun convInt s =
            let
            val radix =
                if String.size s >= 3 andalso String.substring(s, 0, 2) = "0x"
                   orelse String.size s >= 4 andalso String.substring(s, 0, 3) = "~0x"
                then StringCvt.HEX else StringCvt.DEC
            in
                case StringCvt.scanString (scan radix) s of
                    NONE => raise RunCall.Conversion "Invalid integer constant"
                  | SOME res => res
            end
    in
        (* Add a conversion function. *)
        val () = RunCall.addOverload convInt "convInt"
    end

    open LargeInt (* Everything else. *)
end;

structure FixedInt: INTEGER =
struct
    (* This is now a fixed precision int.  Currently it is the same as the short
       form of an arbitrary precision int i.e. 31 bits on 32-bit machines and
       63 bits on 63-bits. *)
    type int = FixedInt.int (* Defined in the basis *)

    (* Whether int is fixed or arbitrary precision we can just cast it here. *)
    val toInt: int -> Int.int = RunCall.unsafeCast (* Just a cast. *)

    (* If int is arbitrary precision we have to check that the value will fit. *)
    fun fromInt(i: Int.int): int =
        if LibrarySupport.isShortInt i
        then RunCall.unsafeCast i
        else raise Overflow

    (* Conversion from fixed int to large is just a cast.  It will always fit. *)
    val toLarge: int -> LargeInt.int = RunCall.unsafeCast

    (* When converting from arbitrary precision we have to check. *)
    fun fromLarge(i: LargeInt.int): int =
        if LibrarySupport.largeIntIsSmall i
        then RunCall.unsafeCast i
        else raise Overflow

    local
        fun power2' n 0 : LargeInt.int = n
         |  power2' n i = power2' (2*n) (i-1)
        val power2 = power2' 1
        val bitsInWord: int = (RunCall.unsafeCast LibrarySupport.wordSize) * 8
        val wordSize = bitsInWord - 1 (* 31 or 63 bits *)
        val maxIntP1 = power2(wordSize-1)
    in
        val precision = SOME(toInt wordSize)
        val maxInt = SOME(fromLarge(maxIntP1-1))
        val smallestInt = fromLarge(~ maxIntP1)
        val minInt = SOME smallestInt
    end

    fun scan radix rdr src =
        case LargeInt.scan radix rdr src of
            NONE => NONE
        |   SOME(i, c) => SOME(fromLarge i, c)

    (* Converter to int values. This replaces the basic conversion
       function for ints installed in the bootstrap process. In
       particular this converter can handle hexadecimal. *)
    local
        fun convInt s =
        let
            val radix =
                if String.size s >= 3 andalso String.substring(s, 0, 2) = "0x"
                   orelse String.size s >= 4 andalso String.substring(s, 0, 3) = "~0x"
                then StringCvt.HEX else StringCvt.DEC
        in
            case StringCvt.scanString (scan radix) s of
                NONE => raise RunCall.Conversion "Invalid integer constant"
              | SOME res => res
        end
    in
        val () = RunCall.addOverload convInt "convInt"
    end 
    
    (* Can now open FixedInt. *)
    open FixedInt

    (* TODO: We should implement div and mod as built-ins because then they
       can access the remainder and quotient directly.
       Also, division by a power of two can be implemented as an
       arithmetic shift because this rounds towards negative infinity
       which is what we want. *)
 
    fun compare (i, j) =
        if i < j then General.LESS
        else if i > j then General.GREATER else General.EQUAL
    
    (*fun abs i = if i >= 0 then i else ~ i*)
    
    fun min (i, j) = if i < j then i else j
    and max (i, j) = if i > j then i else j
    
    fun sign i = if i = 0 then 0 else if i < 0 then ~1 else 1
    
    (* It might be possible to do something clever by xor-ing the 
       words together when both values are short. *)
    fun sameSign(i, j) =
        if i = 0 then j = 0
        else if i < 0 then j < 0
        else (* i > 0 *) j > 0

    fun fmt r n = LargeInt.fmt r (toLarge n)

    val fromString = StringCvt.scanString (scan StringCvt.DEC)
    and toString = LargeInt.toString o toLarge
    
    (* These are overloaded functions and are treated specially. *)
(*    val ~ : int->int = ~
    and op * : int*int->int = op *
    and op + : int*int->int = op +
    and op - : int*int->int = op -
    
    val op < : int*int->bool = op <
    and op > : int*int->bool = op >
    and op <= : int*int->bool = op <=
    and op >= : int*int->bool = op >=*)
end;

val () = RunCall.addOverload FixedInt.div "div"
and () = RunCall.addOverload FixedInt.mod "mod";

(* Add extra overloadings for arbitrary precision. *)
val () = RunCall.addOverload LargeInt.abs "abs"
and () = RunCall.addOverload LargeInt.div "div"
and () = RunCall.addOverload LargeInt.mod "mod";

local
    (* Install the pretty printer for int *)
    fun prettyFixed _ _ x = PolyML.PrettyString(FixedInt.toString x)
    fun prettyLarge _ _ x = PolyML.PrettyString(LargeInt.toString x)
in
    val () = PolyML.addPrettyPrinter prettyFixed
    and () = PolyML.addPrettyPrinter prettyLarge
end;

(* For the moment use arbitrary precision here. *)
structure Position = LargeInt;

(* The actual Int structure is defined depending on what int is. *)

