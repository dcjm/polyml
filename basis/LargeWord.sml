(*
    Title:      Standard Basis Library: Word and LargeWord Structure
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

(*
This file contains definitions of both LargeWord and Word.  SysWord is
defined to be LargeWord.
The only purpose of LargeWord is so that it can be used, as SysWord, to
hold the full machine word values for certain operating-system calls.
*)

(* This uses the global definition of type "word" made in the compiler.
   That type has special status as the default for literals of the form
   0wn in the absence of any other type information. *)
local
    open RuntimeCalls;
    type largeword = LargeWord.word
    and shortword = Word.word

    (* Extract a word value from a character stream. *)
    (* There's a complication here which is similar to that with 0x for
       Int.scan.  A word value may, optionally, be preceded by 0w or
       for hex values 0wx, 0wX, 0x or 0X.  Since this is optional it is
       possible for the value after the 0w to be anything, not just a
       valid number, in which case the result is the 0 and the continuation
       is w... *)
    fun scanWord radix getc src =
        let
        (* Some of this code duplicates code in Int.scan.  It would
           be better to avoid that if we could. The difficulty is that
           Int.scan allows the number to begin with a sign and also
           another 0x for hex values. *)
        val base: LargeInt.int =
            case radix of
                StringCvt.BIN => 2
              | StringCvt.OCT => 8
              | StringCvt.DEC => 10
              | StringCvt.HEX => 16
        
        (* Read the digits, accumulating the result in acc.  isOk is true
           once we have read a valid digit. *)
        fun read_digits src acc isOk =
            case getc src of
                NONE => if isOk then SOME(acc, src) else NONE
              | SOME(ch, src') =>
                if Char.ord ch >= Char.ord #"0"
                   andalso Char.ord ch < (Char.ord #"0" + LargeInt.toInt base)
                then read_digits src'
                        (acc*base + LargeInt.fromInt(Char.ord ch - Char.ord #"0")) true
                else (* Invalid character - either end of number or bad no. *)
                    if isOk then SOME(acc, src) else NONE

        fun read_hex_digits src acc isOk =
            case getc src of
                NONE => if isOk then SOME(acc, src) else NONE
              | SOME(ch, src') =>
                if Char.ord ch >= Char.ord #"0"
                   andalso Char.ord ch <= Char.ord #"9"
                then read_hex_digits src'
                        (acc*16 + LargeInt.fromInt(Char.ord ch - Char.ord #"0")) true
                else if Char.ord ch >= Char.ord #"A"
                   andalso Char.ord ch <= Char.ord #"F"
                then read_hex_digits src'
                        (acc*16 + LargeInt.fromInt(Char.ord ch - Char.ord #"A" + 10)) true
                else if Char.ord ch >= Char.ord #"a"
                   andalso Char.ord ch <= Char.ord #"f"
                then read_hex_digits src'
                        (acc*16 + LargeInt.fromInt(Char.ord ch - Char.ord #"a" + 10)) true
                else (* Invalid character - either end of number or bad no. *)
                    if isOk then SOME(acc, src) else NONE

        fun read_number src =
            case radix of
                StringCvt.HEX => read_hex_digits src 0 false
              | _ => (* Binary, octal and decimal *) read_digits src 0 false
        in
        case getc src of
            NONE => NONE
         |  SOME(#"0", src') =>
            let (* May be the start of the number or may be 0w, 0x etc. *)
                val after0 = 
                    case getc src' of
                        NONE => NONE
                      | SOME(ch, src'') =>
                        if ch = #"w"
                        then if radix = StringCvt.HEX
                        then (* Is it 0wx, 0wX ? *)
                            (
                            case getc src'' of
                                NONE => NONE
                              | SOME(ch, src''') =>
                                if ch = #"x" orelse ch = #"X"
                                then read_number src''' (* Skip the 0wx *)
                                else read_number src'' (* Skip the 0w *)
                            )
                        else read_number src'' (* Skip the 0w *)
                        else if (ch = #"x" orelse ch = #"X") andalso radix = StringCvt.HEX
                        then read_number src''
                        else read_number src (* Include the 0 in the input *)
            in
                (* If the string *)
                case after0 of
                    NONE => (* No valid number after it, return the zero .*)
                        SOME(0, src')
                  | res => res
            end

         |  SOME(ch, src') =>
                if Char.isSpace ch (* Skip white space. *)
                then scanWord radix getc src' (* Recurse *)
                else (* See if it's a valid digit. *)
                    read_number src
        end (* scanWord *)

    (* Conversion from integer may involve extracting the low-order word
       from a long-integer representation.  *)
    local
        (* Load the first word of a long form arbitrary precision
           number which is always little-endian, tag it and negate
           it if necessary. *)
        val getFirstWord: LargeInt.int -> word =
            RunCall.run_call1 POLY_SYS_get_first_long_word
        val isShortInt: LargeInt.int -> bool = RunCall.isShort
    in
        (* We previously had a single RTS function to do this.  I've
           replaced that by this code.  Since most of the time we're
           going to be converting short integers this will avoid
           making an RTS call.  getFirstWord can be implemented in
           the code-generator fairly easily on little-endian machines
           but it's too difficult to do it on the Sparc. *)
        fun wordFromLargeInt (i: LargeInt.int): word =
            if isShortInt i
            then RunCall.unsafeCast i
            else getFirstWord i
    end
    
    val wordFromInt: int -> word = RunCall.unsafeCast

    (* The maximum word is the largest tagged value.  The maximum large-word is
       the largest value that will fit in a machine word. *)
    local
        fun power2' n 0 : LargeInt.int = n
         |  power2' n i = power2' (2*n) (i-1)
        val power2 = power2' 1
        val bitsInWord: int = (RunCall.unsafeCast LibrarySupport.wordSize) * 8
    in
        val wordSize = bitsInWord - 1 (* 31 or 63 bits *)
        val maxWordP1: LargeInt.int = power2 wordSize (* One more than the maximum word *)
        val maxWord: LargeInt.int = maxWordP1 - 1
        val largeWordSize = bitsInWord
        val maxLargeWord = power2 largeWordSize - 1
        val largeWordTopBit: LargeInt.int = maxWordP1 (* The top bit of a large word *)
        val maxWordAsWord = wordFromLargeInt maxWord
    end

in
    structure Word :> WORD where type word = shortword =
    struct
        
        (* Word.word is represented using the short (tagged) integer format.
           It is, though, unsigned so large word values are represented in the
           same form as negative integers.  *)
        type word = word
        val fromInt = wordFromInt
        and wordSize = wordSize
        and fromLargeInt = wordFromLargeInt

        (* Conversion to signed integer is simple. *)
        val toIntX: word->int = RunCall.unsafeCast
        and toLargeIntX: word -> LargeInt.int = RunCall.unsafeCast
        
        (* Conversion to unsigned integer has to treat values with the sign bit
           set specially. *)
        fun toLargeInt x =
            let
                val signed = toLargeIntX x
            in
                if signed < 0 then maxWordP1 + signed else signed
            end

        fun toInt x = LargeInt.toInt(toLargeInt x)

        fun scan radix getc src =
            case scanWord radix getc src of
                NONE => NONE
            |   SOME(res, src') =>
                    if res > maxWord then raise General.Overflow
                    else SOME(fromLargeInt res, src')

        (* TODO: Implement this directly? *)
        val fromString = StringCvt.scanString (scan StringCvt.HEX)

        infix >> << ~>>
        
        (* We can format the result using the large integer format function. *)
        fun fmt radix i = LargeInt.fmt radix (toLargeInt i)
        val toString = fmt StringCvt.HEX
    
        fun compare (i, j) =
            if i < j then General.LESS
            else if i > j then General.GREATER else General.EQUAL
        
        fun min (i, j) = if i < j then i else j
        and max (i, j) = if i > j then i else j
        
        open Word (* Include all the initial definitions. *)

        fun notb x = xorb(maxWordAsWord, x)

    end (* Word *)

    (* LargeWord.word values have one more bit of precision than Word,word values and
       are always "boxed" i.e. held in a one word piece of memory with the "byte" bit set. *)
    structure LargeWord:> WORD where type word = largeword =
    struct
        type word = largeword
        val wordSize = largeWordSize

        (* As this is LargeWord we don't need to do anything here. *)
        fun toLargeWord x = x
        and toLargeWordX x = x
        and fromLargeWord x = x
        val toLarge = toLargeWord and toLargeX = toLargeWordX and fromLarge = fromLargeWord

        (* Logical operations.  Declare these first so they can be used in fromInt etc but
           leave the arithmetic operations until later. *)
        val orb : word*word->word = RunCall.run_call2 POLY_SYS_orb_longword
        and andb : word*word->word = RunCall.run_call2 POLY_SYS_andb_longword
        and xorb : word*word->word = RunCall.run_call2 POLY_SYS_xorb_longword
        and op >> : word*Word.word->word = RunCall.run_call2 POLY_SYS_shift_right_longword
        and op << : word*Word.word->word = RunCall.run_call2 POLY_SYS_shift_left_longword
        and op ~>> : word*Word.word->word = RunCall.run_call2 POLY_SYS_shift_right_arith_longword

        local
            val shortToWord: LargeInt.int -> largeword = RunCall.run_call1 POLY_SYS_signed_to_longword
            val getFirstWord: LargeInt.int -> Word.word = RunCall.run_call1 POLY_SYS_get_first_long_word
            val longToInt: largeword -> LargeInt.int = RunCall.run_call1 POLY_SYS_longword_to_tagged
            val zero: largeword = shortToWord 0
            val andbInt : LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.run_call2 POLY_SYS_anda

            infix << orb andb

            val topBitAsLargeWord: largeword =
                (* The top bit *) shortToWord 1 << Word.fromInt(largeWordSize - 1)

            fun topBitClear (x: largeword) : bool = (x andb topBitAsLargeWord) = zero 
            val isShortInt: LargeInt.int -> bool = RunCall.isShort
        in
            (* If it is short we just need to sign-extend it when storing it into
               memory.  If it is long it's more complicated.  getFirstWord returns
               the tagged value, possibly negated, but loses the high-order bit.
               That has to be put back. *)
            fun fromLargeInt x = 
                if isShortInt x
                then shortToWord x
                else
                let
                    val lo = Word.toLargeWord(getFirstWord x)
                in
                    (* TODO: Testing this bit will currently require an RTS call *)
                    if andbInt(x, largeWordTopBit) = 0
                    then lo
                    else topBitAsLargeWord orb lo
                end

            and toLargeInt x =
            let
                val asInt: LargeInt.int = longToInt x
            in
                (if asInt < 0 then maxWordP1 + asInt else asInt) +
                (if topBitClear x then 0 else largeWordTopBit)
            end
            and toLargeIntX x =
            let
                val asInt: LargeInt.int = longToInt x
            in
                (if asInt < 0 then maxWordP1 + asInt else asInt) -
                (if topBitClear x then 0 else largeWordTopBit)
            end
           
            val zero = zero
            val maxLargeWordAsLargeWord = fromLargeInt maxLargeWord
        end
        
        val fromInt = fromLargeInt o LargeInt.fromInt
        and toInt = LargeInt.toInt o toLargeInt
        and toIntX = LargeInt.toInt o toLargeIntX

        fun scan radix getc src =
            case scanWord radix getc src of
                NONE => NONE
            |   SOME(res, src') =>
                    if res > maxLargeWord then raise General.Overflow
                    else SOME(fromLargeInt res, src')

        val fromString = StringCvt.scanString (scan StringCvt.HEX)

        val op + : word*word->word = RunCall.run_call2 POLY_SYS_plus_longword
        and op - : word*word->word = RunCall.run_call2 POLY_SYS_minus_longword
        and op * : word*word->word = RunCall.run_call2 POLY_SYS_mul_longword
        
        local
            val d: word*word->word = RunCall.run_call2 POLY_SYS_div_longword
            and m: word*word->word = RunCall.run_call2 POLY_SYS_mod_longword
        in
            fun x div y = if x = zero then raise Div else d(x, y)
            and x mod y = if x = zero then raise Div else m(x, y)
        end

        fun ~ x = zero - x
        fun notb x = xorb(maxLargeWordAsLargeWord, x)

        val op > : word*word->bool = RunCall.run_call2 POLY_SYS_gt_longword
        and op < : word*word->bool = RunCall.run_call2 POLY_SYS_lt_longword
        and op >= : word*word->bool = RunCall.run_call2 POLY_SYS_geq_longword
        and op <= : word*word->bool = RunCall.run_call2 POLY_SYS_leq_longword
    
        fun compare (i, j) =
            if i < j then General.LESS
            else if i > j then General.GREATER else General.EQUAL
        
        fun min (i, j) = if i < j then i else j
        and max (i, j) = if i > j then i else j

        (* We can format the result using the large integer format function.
           Large unsigned values may be outside the short integer range. *)
        fun fmt radix i = LargeInt.fmt radix (toLargeInt i)
        val toString = fmt StringCvt.HEX
    end;
end;

local
    (* Install the pretty printer for Word.word *)
    fun prettyWord _ _ x =
        PolyML.PrettyString("0wx" ^ Word.toString x)
    and prettyLarge _ _ x =
        PolyML.PrettyString("0wx" ^ LargeWord.toString x)
in
    val () = PolyML.addPrettyPrinter prettyWord
    val () = PolyML.addPrettyPrinter prettyLarge
end;

(* Converter to word values.  These must be installed outside the structure
   because they depend on the type identifiers. *)
local

    (* The string may be either 0wnnn or 0wxXXX *)
    fun getRadix s =
        if String.size s > 2 andalso String.sub(s, 2) = #"x"
        then StringCvt.HEX else StringCvt.DEC

    fun convWord s =
        let
        val radix = getRadix s
        in
            case StringCvt.scanString (Word.scan radix) s of
                NONE => raise RunCall.Conversion "Invalid word constant"
              | SOME res => res
        end
    and convLarge s =
        let
        val radix = getRadix s
        in
            case StringCvt.scanString (LargeWord.scan radix) s of
                NONE => raise RunCall.Conversion "Invalid word constant"
              | SOME res => res
        end

in
    (* Install this as a conversion function for word literals.
       Unlike other overloaded functions there's no need to
       ensure that overloaded conversion functions are installed
       at the top-level.  The compiler has type "word" built in
       and will use this conversion function for literals of the
       form 0w... in preference to any other (e.g. for Word8.word)
       if unification does not give an explicit type.
       However, because LargeWord.word is abstract we have to
       install the convertor outside the structure. *)
    val () = RunCall.addOverload convWord "convWord"
    val () = RunCall.addOverload convLarge "convWord"
end;



(* Add the overloaded operators.  Do this outside the structure so
   that we can capture the inline code.  We've already done this for
   word (=Word.word) in the prelude. *)

val () = RunCall.addOverload LargeWord.~ "~";
val () = RunCall.addOverload LargeWord.+ "+";
val () = RunCall.addOverload LargeWord.- "-";
val () = RunCall.addOverload LargeWord.* "*";
val () = RunCall.addOverload LargeWord.div "div";
val () = RunCall.addOverload LargeWord.mod "mod";
val () = RunCall.addOverload LargeWord.< "<";
val () = RunCall.addOverload LargeWord.> ">";
val () = RunCall.addOverload LargeWord.<= "<=";
val () = RunCall.addOverload LargeWord.>= ">=";



