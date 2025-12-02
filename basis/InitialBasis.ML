(*
    Copyright (c) 2000-2010, 2016-17, 2019 David C.J. Matthews

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

infix  7  * / div mod
infix  6  + - ^
infixr 5  :: @
infix  4  = <> > >= < <=
infix  3  := o
infix  0  before

(* Include this for the moment.  TODO: Check why Real, at any rate, requires
   the "redundant" structure binding in order for the built-ins to be
   properly inlined. *)
structure RunCall =
struct
    open RunCall
end;

(* Types and values from the initial Bool structure. *)
datatype bool = datatype Bool.bool
val not = Bool.not;


(* Types and values from the initial FixedInt structure.  *)
structure FixedInt =
struct
    open FixedInt (* Inherit built-in functions. *)
    
    fun ~ (x: int): int = 0 - x
    
    fun abs (i: int): int = if i >= 0 then i else ~ i
end;

val () = RunCall.addOverload FixedInt.>= ">="
and () = RunCall.addOverload FixedInt.<= "<="
and () = RunCall.addOverload FixedInt.>  ">"
and () = RunCall.addOverload FixedInt.<  "<"
and () = RunCall.addOverload FixedInt.+  "+"
and () = RunCall.addOverload FixedInt.-  "-"
and () = RunCall.addOverload FixedInt.*  "*"
and () = RunCall.addOverload FixedInt.~  "~"
and () = RunCall.addOverload FixedInt.abs  "abs";

structure LargeInt =
struct
    open LargeInt
    
    local
        val callAdd: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyAddArbitrary"
        and callSub: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolySubtractArbitrary"
        and callMult: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyMultiplyArbitrary"
        
        (* Comparison does not need to allocate memory so is a fast call. *)
        val callComp: LargeInt.int * LargeInt.int -> FixedInt.int = RunCall.rtsCallFast2 "PolyCompareArbitrary"
        
        exception Overflow  = RunCall.Overflow
    in
        val op + = fn (i, j) => add(i, j, callAdd)
        and op - = fn (i, j) => subtract(i, j, callSub)
        and op * = fn (i, j) => multiply(i, j, callMult)

        val op < = fn (i, j) => less(i, j, callComp)
        and op > = fn (i, j) => greater(i, j, callComp)
        and op <= = fn (i, j) => lessEq(i, j, callComp)
        and op >= = fn (i, j) => greaterEq(i, j, callComp)

        (* Negation.  Just use 0 - X.  *)
        fun ~ x = 0 - x
    end
    
    (* N.B.  div and mod are added on a bit further down. *)
end;

val () = RunCall.addOverload LargeInt.>= ">="
and () = RunCall.addOverload LargeInt.<= "<="
and () = RunCall.addOverload LargeInt.>  ">"
and () = RunCall.addOverload LargeInt.<  "<"
and () = RunCall.addOverload LargeInt.+  "+"
and () = RunCall.addOverload LargeInt.-  "-"
and () = RunCall.addOverload LargeInt.*  "*"
and () = RunCall.addOverload LargeInt.~  "~";
(*and () = RunCall.addOverload LargeInt.abs  "abs"*)


(* Now add div and mod. *)
local
    (* There's some duplication.  This is also in Int.sml. *)
    local
        fun power2' n 0 : LargeInt.int = n
         |  power2' n i = power2' (2*n) (i-1)
        val power2 = power2' 1
        val wordSize : word = RunCall.bytesPerWord
        val bitsInWord: int = (RunCall.unsafeCast wordSize) * 8
        val wordSize = bitsInWord - 1 (* 31 or 63 bits *)
    in
        val maxIntP1 = power2(wordSize-1)
    end
in
    structure FixedInt =
    struct
        open FixedInt

        local
            val fquot: FixedInt.int * FixedInt.int -> FixedInt.int = quot
            val frem: FixedInt.int * FixedInt.int -> FixedInt.int = rem
            val smallestInt = RunCall.unsafeCast(LargeInt.~ maxIntP1)
            infix 7 quot rem
            exception Overflow  = RunCall.Overflow
            and Div = RunCall.Div
        in
            fun op quot(_, 0) = raise RunCall.Div
            |   op quot(x, y) =
                if y = ~1 andalso x = smallestInt
                then raise Overflow
                else fquot(x,y)

            (* This should return zero when dividing minInt by ~1.  Since we
               are working with 31/63 bits this won't overflow and will return
               the correct answer. *)
            fun op rem(_, 0) = raise Div
            |   op rem(x, y) = frem (x, y)
        
            (* mod adjusts the result of rem to give the correcly signed result. *)
            fun x mod y =
                let
                    val remainder = x rem y
                in
                    if remainder = 0
                    then 0 (* If the remainder was zero the result is zero. *)
                    else if (remainder < 0) = (y < 0)
                    then remainder (* If the signs are the same there's no adjustment. *)
                    else remainder + y (* Have to add in the divisor. *)
                end

            (* div adjusts the result to round towards -infinity. *)
            fun x div y =
                let
                    val quotient = x quot y (* raises Div or Overflow as appropriate. *)
                    and remainder = x rem y
                in
                    if remainder = 0 orelse (remainder < 0) = (y < 0)
                    then quotient
                    else quotient-1
                end
        end

    end;

    structure LargeInt =
    struct
        open LargeInt
    
        local
            val isShort: LargeInt.int -> bool = RunCall.isShort
            val toShort: LargeInt.int -> FixedInt.int = RunCall.unsafeCast
            and fromShort: FixedInt.int -> LargeInt.int = RunCall.unsafeCast

            val callDiv: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyDivideArbitrary"
            and callRem: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyRemainderArbitrary"
            and callQuotRem: LargeInt.int * LargeInt.int -> LargeInt.int * LargeInt.int = RunCall.rtsCallFull2 "PolyQuotRemArbitraryPair"

            infix 7 quot rem

            exception Overflow  = RunCall.Overflow
            val smallestInt = ~ maxIntP1

            val zero = 0
        in
            val op quot =
                fn (_, 0) => raise RunCall.Div
                |  (i: int, j: int) =>
                    if isShort i andalso isShort j andalso not (j = ~1 andalso i = smallestInt)
                    then fromShort(FixedInt.quot(toShort i, toShort j))
                    else callDiv(i, j)

            (* We don't have to worry about overflow here because we will
               get the correct result if we divide the smallest int by -1 and
               because we're actually using 31/63 bits rather than true 32/64 bits
               we won't get a hardware trap. *)
            val op rem =
                fn  (_, 0) => raise RunCall.Div
                |   (i, j) =>
                    if isShort i andalso isShort j
                    then fromShort(FixedInt.rem(toShort i, toShort j))
                    else callRem(i, j)

            fun x mod y =
            let
                val r = x rem y
            in
                if r = zero orelse (y >= zero) = (r >= zero) then r else r + y
            end

            fun x div y =
            let
                (* If the signs differ the normal quot operation will give the wrong
                   answer. We have to round the result down by subtracting either y-1 or
                   y+1. This will round down because it will have the opposite sign to x *)
        
                (* ...
                val d = x - (if (y >= 0) = (x >= 0) then 0 else if y > 0 then y-1 else y+1)
                ... *)
                val xpos = x >= zero
                val ypos = y >= zero
        
                val d =
                    if xpos = ypos 
                    then x
                    else if ypos
                    then (x - (y - 1))
                    else (x - (y + 1))
            in
                d quot y (* may raise Div for divide-by-zero *)
            end
            
            (* This should end up in IntInf not LargeInt so it gets picked up by LibrarySupport. *)
            fun quotRem(i, j) =
                if isShort i andalso isShort j andalso not (j = ~1 andalso i = smallestInt)
                then (fromShort(FixedInt.quot(toShort i, toShort j)), fromShort(FixedInt.rem(toShort i, toShort j)))
                else callQuotRem(i, j)
        end
    end;
end;

val () = RunCall.addOverload FixedInt.div  "div"
and () = RunCall.addOverload FixedInt.mod  "mod"
and () = RunCall.addOverload LargeInt.div  "div"
and () = RunCall.addOverload LargeInt.mod  "mod";

structure Word =
struct
    open Word
    infix 8 << >> ~>> (* The shift operations are not infixed in the global basis. *)

    fun ~ x = 0w0 - x

    (* Redefine div and mod to include checks for zero. *)
    fun op div(_, 0w0) = raise RunCall.Div | op div(x, y) = Word.div(x, y)
    fun op mod(_, 0w0) = raise RunCall.Div | op mod(x, y) = Word.mod(x, y)

    local
        val maxBits = RunCall.bytesPerWord * 0w8 - 0w1
    in
        (* The X86 masks the shift value but ML defines a shift greater than the
           word length as returning zero except that a negative number with an
           arithmetic shift returns ~1.  The tests will all be optimised away
           if the shift is a constant. *)
        val op << = fn (a, b) => if b >= maxBits then 0w0 else a << b
        val op >> = fn (a, b) => if b >= maxBits then 0w0 else a >> b
        val op ~>> = fn (a, b) => a ~>> (if b > maxBits then maxBits else b)
    end

    val toLarge = toLargeWord and toLargeX = toLargeWordX and fromLarge = fromLargeWord
end;

val () = RunCall.addOverload Word.>= ">="
and () = RunCall.addOverload Word.<= "<="
and () = RunCall.addOverload Word.>  ">"
and () = RunCall.addOverload Word.<  "<"
and () = RunCall.addOverload Word.+  "+"
and () = RunCall.addOverload Word.-  "-"
and () = RunCall.addOverload Word.*  "*"
and () = RunCall.addOverload Word.~  "~"
and () = RunCall.addOverload Word.div  "div"
and () = RunCall.addOverload Word.mod  "mod";
(* N.B.  abs is not overloaded on word *)

structure LargeWord =
struct
    open LargeWord

    local
        infix 8 << >> ~>> (* The shift operations are not infixed in the global basis. *)
        val zero = Word.toLargeWord 0w0
        (* As with Word.word shifts we have to check that the shift does not exceed the
           word length.  N.B.  The shift amount is always a Word.word value. *)
        (* This is the same as wordSize in native 32-bit and 64-bit but different in 32-in-64. *)
        val sysWordSize = Word.*(RunCall.memoryCellLength zero, RunCall.bytesPerWord)
        val maxBits = Word.*(sysWordSize, 0w8) (* 32 or 64-bits. *)
    in
        val wordSize = maxBits
        val op << = fn (a, b) => if Word.>=(b, maxBits) then zero else a << b
        val op >> = fn (a, b) => if Word.>=(b, maxBits) then zero else a >> b
        val op ~>> = fn (a, b) => a ~>> (if Word.>(b, maxBits) then maxBits else b)
    end

    local
        val zero = Word.toLargeWord 0w0
    in
        fun x div y = if y = zero then raise RunCall.Div else LargeWord.div(x, y)
        and x mod y = if y = zero then raise RunCall.Div else LargeWord.mod(x, y)
    end
end;

(* We seem to need to have these apparently redundant structures to
   make sure the built-ins are inlined.  *)
structure Char =
struct
    open Char
end;

(* We want these overloads in String. *)
val () = RunCall.addOverload Char.>= ">="
and () = RunCall.addOverload Char.<= "<="
and () = RunCall.addOverload Char.>  ">"
and () = RunCall.addOverload Char.<  "<";

structure String =
struct
    open String
end;

(* Overloads for String are added in String.sml *)

structure Real =
struct
    open Real
end;

val () = RunCall.addOverload Real.>= ">="
and () = RunCall.addOverload Real.<= "<="
and () = RunCall.addOverload Real.>  ">"
and () = RunCall.addOverload Real.<  "<"
and () = RunCall.addOverload Real.+ "+"
and () = RunCall.addOverload Real.- "-"
and () = RunCall.addOverload Real.* "*"
and () = RunCall.addOverload Real.~ "~"
and () = RunCall.addOverload Real.abs "abs"
and () = RunCall.addOverload Real./ "/";

(* It seems this is sometimes required to ensure that a structure defined in
   Initialise is properly built.  It doesn't do any harm.  *)
structure Foreign =
struct
    open Foreign
end;

structure ForeignMemory =
struct
    open ForeignMemory
    
    (* Add wrappers to these functions so that they raise exceptions if they are called. *)
    val get64 =
        fn (s, i) =>
            if LargeWord.wordSize < 0w64
            then raise Foreign.Foreign "64-bit operations not available" else get64(s, i)
    and set64 =
        fn (s, i, v) =>
            if LargeWord.wordSize < 0w64
            then raise Foreign.Foreign "64-bit operations not available" else set64(s, i, v)
end;

(* This needs to be defined for StringSignatures but must not be defined in
   that file because that conflicts with building the IntAsIntInf module. *)
structure StringCvt = struct type  ('a, 'b) reader = 'b -> ('a * 'b) option end;

    (* We need to use the same identifier for this that we used when
       compiling the compiler, particularly "make". *)
    exception Fail = RunCall.Fail

(* A few useful functions which are in the top-level environment.
   Others are added later. *)

fun (var: 'a ref) := (v: 'a) : unit = RunCall.storeWord (var, 0w0, v)

(* The following version of "o" currently gets optimised better. *)
fun (f o g) = fn x => f (g x); (* functional composition *)

fun ! (ref x) = x;

fun length l =
    let
    (* Tail-recursive function. *)
    fun len [] i = i
     |  len (_::l) i = len l (i+1)
    in
    len l 0
    end

(* Print functions.  Some of these are replaced by functions in the Basis library and
   are installed here merely so that we can get useful output if we get a failure while
   compiling it. *)
local
    open PolyML

    fun print_bool _ _ (b: bool) =
        PrettyString(if b then "true" else "false")

    fun print_string _ _ (s: string) = PrettyString s (* Not escaped at the moment. *)

    fun print_char _ _ (c: char) =
        PrettyBlock (0, false, [], [PrettyString "#", PrettyString(RunCall.unsafeCast c)])

      fun nil @ y = y (* This is redefined later. *)
      |  (a::b) @ y = a :: (b @ y)

    fun print_list depth printEl (l: 'a list) =
        let
        (* Print the list as [<elem>, <elem>, etc ]. Replace the
           rest of the list by ... once the depth reaches zero. *)
          fun plist [] _ = []
           |  plist _ 0 = [PrettyString "..."]
           |  plist [h]    depth = [printEl (h, depth)]
           |  plist (h::t) depth =
                    printEl (h, depth) ::
                    PrettyString "," ::
                    PrettyBreak (1, 0) ::
                    plist t (depth - 1)
                    
        in
          PrettyBlock (1, false, [], (* Wrap this in a begin-end block to keep it together. *)
            PrettyString "[" ::
                ((if depth <= 0 then [PrettyString "..."] else plist l depth) @
                [PrettyString "]"]
                )
            )
        end

    fun print_int _ _ (i: int) =
    let
        fun pr (i: int) =
           if i < 0 then PrettyString "~" :: pr (~ i)
           else if i < 10 then [PrettyString(RunCall.unsafeCast(i + RunCall.unsafeCast #"0"))]
           else pr(i div 10) @ [PrettyString(RunCall.unsafeCast(i mod 10 + 48))]
    in
        PrettyBlock(1, false, [], pr i)
    end
in
    val () = addPrettyPrinter print_bool
    val () = addPrettyPrinter print_string
    val () = addPrettyPrinter print_char
    val () = addPrettyPrinter print_list
    val () = addPrettyPrinter print_int
end;
