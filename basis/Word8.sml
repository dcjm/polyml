(*
    Title:      Standard Basis Library: Word8 Structure
    Author:     David Matthews
    Copyright   David Matthews 1999

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

(* G&R 2004 status: checked, no change *)

structure Word8 :> WORD =
struct
    (* This structure is largely derived from Word but we use opaque
       signature matching so that it is a different type. *)
    open Word
    
    (* Values of type Word8.word can be in the range 0..255 and
       are implemented using tagged integers.  *)
    (* There seem to be two ways to implement this, given that the underlying
       representation is a tagged 30/31 bit integer.  We could either ensure
       that the top 22/23 bits are always zero and mask them after every
       operation (or if the code-generator was clever, after several) or 
       we could allow these bits to be undefined and mask them before any
       operation which could expose them.  For the moment we choose the
       former.  *)
    (* N.B. The Byte structure contains functions which map between
       Word8.word values and Char.char.  It assumes that the underlying
       representation is the same. *)
    open RuntimeCalls

    val wordSize = 8
    val maxWord = 255
    val maxWordAsWord: word = RunCall.unsafeCast maxWord
    
    infix 8 << >> ~>>

    (* Comparison operations, min, max and compare, fmt, toString,
       orb, andb, xorb can be inherited directly from Word.
       Similarly div and mod since the results will always be no
       larger than the arguments. *)

    (* Not the same as Word.notb because it only affects the bottom 8 bits.  *)
    fun notb x = xorb(maxWordAsWord, x)

    (* Internal function to convert from Word.word. *)
    fun fromWord (w: Word.word) = andb(w, maxWordAsWord)

    (* Converting from LargeWord.word.  First convert to Word.word and
       then mask. *)
    val fromLargeWord = fromWord o Word.fromLargeWord
    and fromInt = fromWord o Word.fromInt
    and fromLargeInt = fromWord o Word.fromLargeInt
    
    val fromLarge = fromLargeWord

            (* Arithmetic shift - sign extends. *)
    (* TODO: Replace by built-in?  We need a separate function from
       arithmetic shift for Word because the sign bit is in a different
       place.  *)
    (* Shift the "sign" bit into the real sign bit position then
       shift right down again. *)
    local
        val toSignBit = Word.fromInt(Int.-(Word.wordSize,wordSize))
    in
        fun op ~>> (a: word, b: Word.word): word =
            fromWord(Word.~>>(Word.<<(a, toSignBit), Word.+(b, toSignBit)))
    end
    
    (* TODO: Replace with built-in?  We need to mask the result so
       that it remains in the range 0..255 *)
    fun op << (a: word, b: Word.word): word = andb(Word.<<(a,b), maxWordAsWord)

    (* Conversion to unsigned integer.  This is simpler than for Word
       because all Word8 values correspond to short integers. *)
    val toInt: word->int = RunCall.unsafeCast
    
    (* Conversion to signed integer. *)
    (* TODO: This could be implemented using shifts. i.e logical shift
       left by (Word.wordSize-Word8.wordSize) then arithmetic shift
       right by the same amount. *)
    fun toIntX (x: word) : int =
        let
            val intx = toInt x
            open Int (* We want integer arithmetic here. *)
        in
            if intx >= 128
            then intx-maxWord-1
            else intx
        end
    
    val toLargeInt = LargeInt.fromInt o toInt
    and toLargeIntX = LargeInt.fromInt o toIntX

    (* Convert to a large word by sign extending. *)
    fun toLargeWordX (w: word): LargeWord.word =
        LargeWord.fromInt(toIntX w);
    val toLargeX = toLargeWordX
    
    (* Use Word.scan but check that the result is in the range. *)
    val wordScan = scan;

    fun scan radix getc src =
        case wordScan radix getc src of
            NONE => NONE
         |  SOME(res, src') =>
                if res > maxWordAsWord
                then raise General.Overflow
                else SOME(res, src')

    val fromString = StringCvt.scanString (scan StringCvt.HEX)

    (* TODO: Replace by built-ins? *)
    fun op + (a, b) = fromWord(Word.+(a, b))
    and op - (a, b) = fromWord(Word.-(a, b))
    and op * (a, b) = fromWord(Word.*(a, b))
    
    fun ~ x = 0w0 - x

end;

(* Because we are using opaque signature matching we have to install
   type-dependent functions OUTSIDE the structure. *)
local
    (* The string may be either 0wnnn or 0wxXXX *)
    fun convWord s : Word8.word =
        let
        val radix =
            (* The word value must consist of at least 0w and a digit. *)
            if String.sub(s, 2) = #"x" then StringCvt.HEX else StringCvt.DEC
        in
            case StringCvt.scanString (Word8.scan radix) s of
                NONE => raise RunCall.Conversion "Invalid word8 constant"
              | SOME res => res
        end
        
    (* Install the pretty printer for Word8.word *)
    fun pretty _ _ x = PolyML.PrettyString("0wx" ^ Word8.toString x)
in
    val () = RunCall.addOverload convWord "convWord"
    val () = PolyML.addPrettyPrinter pretty
end;

(* Add the overloaded operators. *)
val () = RunCall.addOverload Word8.~ "~";
val () = RunCall.addOverload Word8.+ "+";
val () = RunCall.addOverload Word8.- "-";
val () = RunCall.addOverload Word8.* "*";
val () = RunCall.addOverload Word8.div "div";
val () = RunCall.addOverload Word8.mod "mod";
val () = RunCall.addOverload Word8.< "<";
val () = RunCall.addOverload Word8.> ">";
val () = RunCall.addOverload Word8.<= "<=";
val () = RunCall.addOverload Word8.>= ">=";
