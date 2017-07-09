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

    (* These are replaced later on.  We need some of these for e.g. String
       They neither check for overflow nor are they the correct ML definitions. *)
    local
        val q: FixedInt.int * FixedInt.int -> FixedInt.int = quot
        val r: FixedInt.int * FixedInt.int -> FixedInt.int = rem
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
        val isShort: LargeInt.int -> bool = RunCall.isShort
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

        (* Negation.  Just use 0 - X.  *)
        fun ~ x = 0 - x

        (* Define div and mod as quot and rem.  They are redefined properly later. *)
        (* TODO: Handle the short precision cases *)
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
        (* As with Word.word shifts we have to check that the shift does not exceed the
           word length.  N.B.  The shift amount is always a Word.word value. *)
        val maxBits = Word.*(RunCall.bytesPerWord, 0w8) (* One bit more than Word.word. *)
        val zero = Word.toLargeWord 0w0
    in
        val op << = fn (a, b) => if Word.>=(b, maxBits) then zero else a << b
        val op >> = fn (a, b) => if Word.>=(b, maxBits) then zero else a >> b
        val op ~>> = fn (a, b) => a ~>> (if Word.>(b, maxBits) then maxBits else b)
    end

    local
        val zero = Word.toLargeWord 0w0
    in
        fun x div y = if x = zero then raise RunCall.Div else LargeWord.div(x, y)
        and x mod y = if x = zero then raise RunCall.Div else LargeWord.mod(x, y)
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

(* This is used to provide compatibility between 5.6.0 and 5.6.1. *)
structure Compat560 =
struct
    local
        (* These have to be defined outside the function.  The call compiles the code and
           we only want to do that once for each function. *)
        val setCodeConstantCall = RunCall.rtsCallFast4 "PolySetCodeConstant"
        and ffiGeneralCall = RunCall.rtsCallFull2 "PolyFFIGeneral"
        and oldForeignGeneralCall = RunCall.rtsCallFull2 "PolyForeignGeneral"
        and xWindowsGeneralCall = RunCall.rtsCallFull1 "PolyXWindowsGeneral"
        and threadGeneralCall = RunCall.rtsCallFull2 "PolyThreadGeneral"
        and createEntryPointObjectCall = RunCall.rtsCallFull1 "PolyCreateEntryPointObject"
        and osSpecificGeneralCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        and polySpecificGeneralCall = RunCall.rtsCallFull2 "PolySpecificGeneral"
        and timingGeneralCall = RunCall.rtsCallFull2 "PolyTimingGeneral"
        and processEnvGeneralCall = RunCall.rtsCallFull2 "PolyProcessEnvGeneral"
        and realGeneralCall = RunCall.rtsCallFull2 "PolyRealGeneral"
    in
        fun setCodeConstant(addr:'a , offset: word, cnstnt: 'b, flags: word): unit =
            RunCall.unsafeCast(setCodeConstantCall (RunCall.unsafeCast(addr, offset, cnstnt, flags)))
        and ffiGeneral(code: int, arg: 'a): 'b = RunCall.unsafeCast(ffiGeneralCall(RunCall.unsafeCast(code, arg)))
        and oldForeignGeneral(code: int, arg: 'a): 'b = RunCall.unsafeCast(oldForeignGeneralCall(RunCall.unsafeCast(code, arg)))
        and xWindowsGeneral (arg:'a):'b = RunCall.unsafeCast(xWindowsGeneralCall(RunCall.unsafeCast arg))
        and threadGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(threadGeneralCall(RunCall.unsafeCast(code, arg)))
        and createEntryPointObject(s: string): 'a = RunCall.unsafeCast(createEntryPointObjectCall s)
        and osSpecificGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(osSpecificGeneralCall(RunCall.unsafeCast(code, arg)))
        and polySpecificGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(polySpecificGeneralCall(RunCall.unsafeCast(code, arg)))
        and timingGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(timingGeneralCall(RunCall.unsafeCast(code, arg)))
        and processEnvGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(processEnvGeneralCall(RunCall.unsafeCast(code, arg)))
        and realGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(realGeneralCall(RunCall.unsafeCast(code, arg)))
        val isBigEndian: unit -> bool = RunCall.rtsCallFast1 "PolyIsBigEndian"
        and ffiSizeFloat: unit -> word = RunCall.rtsCallFast1 "PolySizeFloat"
        and ffiSizeDouble: unit -> word = RunCall.rtsCallFast1 "PolySizeDouble"
    end
end;
