(*
    Copyright (c) 2000-2010, 2016 David C.J. Matthews

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

(* Types and values from the initial Bool structure. *)
datatype bool = datatype Bool.bool
val not = Bool.not;


(* Types and values from the initial FixedInt structure.  *)
structure FixedInt =
struct
    open FixedInt
    (* TODO: These should be added by Initialise. *)
    
    val op + : int * int -> int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_add
    and op - : int * int -> int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_sub
    and op * : int * int -> int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_mul
    
    fun ~ (x: int): int = 0 - x
    
    fun abs (i: int): int = if i >= 0 then i else ~ i

    (* These are replaced later on.  We need some of these for e.g. String
       They neither check for overflow nor are they the correct ML definitions. *)
    local
        val q: FixedInt.int * FixedInt.int -> FixedInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_quot
        val r: FixedInt.int * FixedInt.int -> FixedInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_rem
    in
        fun op div(_, 0) = raise RunCall.Div | op div(x,y) = q(x, y)
        fun op mod(_, 0) = raise RunCall.Div | op mod(x,y) = r(x,y)
    end

end;

val () = RunCall.addOverload FixedInt.>= ">="
and () = RunCall.addOverload FixedInt.<= "<="
and () = RunCall.addOverload FixedInt.>  ">"
and () = RunCall.addOverload FixedInt.<  "<"
and () = RunCall.addOverload FixedInt.+  "+"
and () = RunCall.addOverload FixedInt.-  "-"
and () = RunCall.addOverload FixedInt.*  "*"
and () = RunCall.addOverload FixedInt.~  "~"
and () = RunCall.addOverload FixedInt.abs  "abs"
and () = RunCall.addOverload FixedInt.div  "div"
and () = RunCall.addOverload FixedInt.mod  "mod";

structure LargeInt =
struct
    open LargeInt
    
    local
        val isShort: LargeInt.int -> bool = RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short
        val toShort: LargeInt.int -> FixedInt.int = RunCall.unsafeCast
        and fromShort: FixedInt.int -> LargeInt.int = RunCall.unsafeCast

        val callAdd: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyAddArbitrary"
        and callSub: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolySubtractArbitrary"
        and callMult: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyMultiplyArbitrary"
        and callDiv: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyDivideArbitrary"
        and callRem: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.rtsCallFull2 "PolyRemainderArbitrary"
        
        (* Comparison does not need to allocate memory so is a fast call. *)
        val callComp: LargeInt.int * LargeInt.int -> FixedInt.int = RunCall.rtsCallFast2 "PolyCompareArbitrary"
        
        exception Overflow  = RunCall.Overflow
    in
        val op + =
            fn (i, j) =>
                if isShort i andalso isShort j
                then fromShort(toShort i + toShort j) handle Overflow => callAdd(i, j)
                else callAdd(i, j)
        and op - =
            fn (i, j) =>
                if isShort i andalso isShort j
                then fromShort(toShort i - toShort j) handle Overflow => callSub(i, j)
                else callSub(i, j)
        and op * =
            fn (i, j) =>
                if isShort i andalso isShort j
                then fromShort(toShort i * toShort j) handle Overflow => callMult(i, j)
                else callMult(i, j)

        (* Relational operations - no overflow.
           An alternative would be to consider the sign bit if one argument is short and
           the other long.  That would probably be better if one of the arguments was a
           short constant.
           e.g.
           val op < =
            fn (i, j) =>
                if isShort i then if isShort j then toShort i < toShort j else isLongPositive j
                else if isShort j then isLongNegative i
                else callComp(i, j) < 0 *)
        val op < =
            fn (i, j) =>
                if isShort i andalso isShort j
                then toShort i < toShort j
                else callComp(i, j) < 0
        and op > =
            fn (i, j) =>
                if isShort i andalso isShort j
                then toShort i > toShort j
                else callComp(i, j) > 0
        and op <= =
            fn (i, j) =>
                if isShort i andalso isShort j
                then toShort i <= toShort j
                else callComp(i, j) <= 0
        and op >= =
            fn (i, j) =>
                if isShort i andalso isShort j
                then toShort i >= toShort j
                else callComp(i, j) >= 0

        (* TODO: Negation. ?? *)
        val ~ : int -> int = RunCall.run_call1 RuntimeCalls.POLY_SYS_aneg

        (* TODO: Handle the short precision versions *)
        fun op div(_, 0) = raise RunCall.Div | op div(x,y) = callDiv(x, y)
        and op mod(_, 0) = raise RunCall.Div | op mod(x,y) = callRem(x,y)
    end
end;

val () = RunCall.addOverload LargeInt.>= ">="
and () = RunCall.addOverload LargeInt.<= "<="
and () = RunCall.addOverload LargeInt.>  ">"
and () = RunCall.addOverload LargeInt.<  "<"
and () = RunCall.addOverload LargeInt.+  "+"
and () = RunCall.addOverload LargeInt.-  "-"
and () = RunCall.addOverload LargeInt.*  "*"
and () = RunCall.addOverload LargeInt.~  "~"
(*and () = RunCall.addOverload LargeInt.abs  "abs"*)
and () = RunCall.addOverload LargeInt.div  "div"
and () = RunCall.addOverload LargeInt.mod  "mod";


structure Word =
struct
    open Word
    (* TODO: These should be added by Initialise. *)

    val op + : word * word -> word = RunCall.run_call2 RuntimeCalls.POLY_SYS_plus_word
    and op - : word * word -> word = RunCall.run_call2 RuntimeCalls.POLY_SYS_minus_word
    and op * : word * word -> word = RunCall.run_call2 RuntimeCalls.POLY_SYS_mul_word

    fun ~ x = 0w0 - x
    and abs x = if x >= 0w0 then x else ~ x

    local
        val d: word * word -> word = RunCall.run_call2 RuntimeCalls.POLY_SYS_div_word
        val m: word * word -> word = RunCall.run_call2 RuntimeCalls.POLY_SYS_mod_word
    in
        fun op div(_, 0w0) = raise RunCall.Div | op div(x, y) = d(x, y)
        fun op mod(_, 0w0) = raise RunCall.Div | op mod(x, y) = m(x, y)
    end
end;

val () = RunCall.addOverload Word.>= ">="
and () = RunCall.addOverload Word.<= "<="
and () = RunCall.addOverload Word.>  ">"
and () = RunCall.addOverload Word.<  "<"
and () = RunCall.addOverload Word.+  "+"
and () = RunCall.addOverload Word.-  "-"
and () = RunCall.addOverload Word.*  "*"
and () = RunCall.addOverload Word.~  "~"
and () = RunCall.addOverload Word.abs  "abs"
and () = RunCall.addOverload Word.div  "div"
and () = RunCall.addOverload Word.mod  "mod";

structure Char =
struct
    open Char
end;

val () = RunCall.addOverload Char.>= ">="
and () = RunCall.addOverload Char.<= "<="
and () = RunCall.addOverload Char.>  ">"
and () = RunCall.addOverload Char.<  "<";

structure String =
struct
    open String
    val op >= : string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrgeq
    and op <= : string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrleq
    and op >  : string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrgtr
    val op <  : string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrlss
end;

val () = RunCall.addOverload String.>= ">="
and () = RunCall.addOverload String.<= "<="
and () = RunCall.addOverload String.>  ">"
and () = RunCall.addOverload String.<  "<";

structure Real =
struct
    open Real
    val op >= : real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_geq
    and op <= : real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_leq
    and op >  : real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_gtr
    and op <  : real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_lss

    val op + : real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Add_real
    and op - : real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Sub_real
    and op * : real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Mul_real
    and op / : real * real -> real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Div_real

    val ~ : real->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_Neg_real
    val abs : real->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_Abs_real
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

(* Conversion functions.  At this stage we have installed a simple
 conversion function for strings and ints. *)
val it: string->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_conv_real;
RunCall.addOverload it "convReal";


    (* We need to use the same identifier for this that we used when
       compiling the compiler, particularly "make". *)
    exception Fail = RunCall.Fail

(* A few useful functions which are in the top-level environment.
   Others are added later. *)

local
    val System_setw: (* 'a ref*int*'a*) word*int*word ->unit  =
            RunCall.run_call3 RuntimeCalls.POLY_SYS_assign_word
in
    fun (var: 'a ref) := (v: 'a) : unit =
            (RunCall.unsafeCast System_setw (var, 0, v); ())
end;

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

local
    (* Temporary conversion function for characters. This is replaced in
       the Char structure. *)
    open RuntimeCalls;

    val isShortString   : string -> bool = RunCall.isShort

    fun convChar (s: string) : char =
    let
        val convS = Bootstrap.convString s
    in
        (* Should be exactly a single character i.e. a single short value. *)
        if isShortString convS then RunCall.unsafeCast convS
        else raise RunCall.Conversion "Bad character"
    end;

in
    val it = RunCall.addOverload convChar "convChar";
end;

(* Print functions.  Some of these are replaced by functions in the Basis library and
   are installed here merely so that we can get useful output if we get a failure while
   compiling it. *)
local
    open RuntimeCalls PolyML

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
