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

(* Set up the overloading first so we can use it in the rest of the
   prelude. Setting up overloads is messy because of the need to capture
   the inline code for the functions. *)
 
  (* Conversion functions.  At this stage we have installed a simple
     conversion function for strings and ints. *)
  val it: string->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_conv_real;
  RunCall.addOverload it "convReal";

  (* Comparisons.  These are overloaded on strings, chars and word as well as int and real. *)
  
  (* N.B.  We have to bind these overloads in separate top-level declarations
     because of limitations in addOverload.  If we use a single local binding
     the code of the function will not be inlined.
     This has the unfortunate side-effect of leaving "it" bound to something
     but it's cleared out when we copy everything to the final top-level
     name space. *)
  val it: FixedInt.int*FixedInt.int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_geq;
  val () = RunCall.addOverload it ">=";
    local
        open RuntimeCalls
        type int = LargeInt.int
        val isShort: int -> bool = RunCall.run_call1 POLY_SYS_is_short

        val arbGeq: int*int->bool = RunCall.run_call2 POLY_SYS_int_geq
        val shortGeq: int*int->bool = RunCall.run_call2 POLY_SYS_fixed_geq
        fun geq(i, j) = if isShort i andalso isShort j then shortGeq(i, j) else arbGeq(i, j)
    in
        val it = geq
    end;
  val () = RunCall.addOverload it ">=";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrgeq;
  val () = RunCall.addOverload it ">=";
  val it: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_geq;
  val () = RunCall.addOverload it ">=";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_geq;
  val () = RunCall.addOverload it ">=";
  val it: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_geq;
  val () = RunCall.addOverload it ">=";
 

  val it: FixedInt.int*FixedInt.int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_leq;
  val () = RunCall.addOverload it "<=";
    local
        open RuntimeCalls
        type int = LargeInt.int
        val isShort: int -> bool = RunCall.run_call1 POLY_SYS_is_short

        val arbLeq: int*int->bool = RunCall.run_call2 POLY_SYS_int_leq
        val shortLeq: int*int->bool = RunCall.run_call2 POLY_SYS_fixed_leq
        fun leq(i, j) = if isShort i andalso isShort j then shortLeq(i, j) else arbLeq(i, j)
    in
        val it = leq
    end;
  val () = RunCall.addOverload it "<=";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrleq;
  val () = RunCall.addOverload it "<=";
  val it: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_leq;
  val () = RunCall.addOverload it "<=";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_leq;
  val () = RunCall.addOverload it "<=";
  val it: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_leq;
  val () = RunCall.addOverload it "<=";

  val it: FixedInt.int*FixedInt.int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_gtr;
  RunCall.addOverload it ">";
    local
        open RuntimeCalls
        type int = LargeInt.int
        val isShort: int -> bool = RunCall.run_call1 POLY_SYS_is_short

        val arbGtr: int*int->bool = RunCall.run_call2 POLY_SYS_int_gtr
        val shortGtr: int*int->bool = RunCall.run_call2 POLY_SYS_fixed_gtr
        fun gtr(i, j) = if isShort i andalso isShort j then shortGtr(i, j) else arbGtr(i, j)
    in
        val it = gtr
    end;
  RunCall.addOverload it ">";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrgtr;
  RunCall.addOverload it ">";
  val it: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_gtr;
  RunCall.addOverload it ">";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_gtr;
  RunCall.addOverload it ">";
  val it: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_gtr;
  RunCall.addOverload it ">";

  val it: FixedInt.int*FixedInt.int->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_lss;
  RunCall.addOverload it "<";
    local
        open RuntimeCalls
        type int = LargeInt.int
        val isShort: int -> bool = RunCall.run_call1 POLY_SYS_is_short

        val arbLss: int*int->bool = RunCall.run_call2 POLY_SYS_int_lss
        val shortLss: int*int->bool = RunCall.run_call2 POLY_SYS_fixed_lss
        fun lss(i, j) = if isShort i andalso isShort j then shortLss(i, j) else arbLss(i, j)
    in
        val it = lss
    end;
  RunCall.addOverload it "<";
  val it: string*string->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_teststrlss;
  RunCall.addOverload it "<";
  val it: real*real->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_lss;
  RunCall.addOverload it "<";
  val it: char*char->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_lss;
  RunCall.addOverload it "<";
  val it: word*word->bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_lss;
  RunCall.addOverload it "<";
  
  (* Binary operations. *)
  val it: FixedInt.int*FixedInt.int->FixedInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_add;
  RunCall.addOverload it "+";
  val it: LargeInt.int*LargeInt.int->LargeInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_aplus;
  RunCall.addOverload it "+";
  val it: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Add_real;
  RunCall.addOverload it "+";
  val it: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_plus_word;
  RunCall.addOverload it "+";

  val it: FixedInt.int*FixedInt.int->FixedInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_sub;
  RunCall.addOverload it "-";
  val it: LargeInt.int*LargeInt.int->LargeInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_aminus;
  RunCall.addOverload it "-";
  val it: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Sub_real;
  RunCall.addOverload it "-";
  val it: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_minus_word;
  RunCall.addOverload it "-";

  val it: FixedInt.int*FixedInt.int->FixedInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_mul;
  RunCall.addOverload it "*";
  val it: LargeInt.int*LargeInt.int->LargeInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_amul;
  RunCall.addOverload it "*";
  val it: real*real->real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Mul_real;
  RunCall.addOverload it "*";
  val it: word*word->word = RunCall.run_call2 RuntimeCalls.POLY_SYS_mul_word;
  RunCall.addOverload it "*";

  (* Unary operations. *)
  fun it (x: FixedInt.int) = 0 - x;
  RunCall.addOverload it "~";
  val it: LargeInt.int->LargeInt.int = RunCall.run_call1 RuntimeCalls.POLY_SYS_aneg;
  RunCall.addOverload it "~";
  val it: real->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_Neg_real;
  RunCall.addOverload it "~";
  local
    val zero: word = RunCall.unsafeCast 0
  in
    fun it x = zero - x
  end;
  RunCall.addOverload it "~";

  fun it (i: FixedInt.int): FixedInt.int = if i >= 0 then i else ~ i;
  RunCall.addOverload it "abs";
  val it: real->real = RunCall.run_call1 RuntimeCalls.POLY_SYS_Abs_real;
  RunCall.addOverload it "abs";
  local
    val zero: word = RunCall.unsafeCast 0
  in
    fun it x = if x >= zero then x else ~ x
  end;
  RunCall.addOverload it "abs";

  (* div, mod and / are overloaded in ML97.  *)
  val it: real * real -> real = RunCall.run_call2 RuntimeCalls.POLY_SYS_Div_real;
  RunCall.addOverload it "/";

    (* These are replaced later on.  We need some of these for e.g. String
       They neither check for overflow nor are they the correct ML definitions. *)
    local
        val q: FixedInt.int * FixedInt.int -> FixedInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_quot
    in
        fun it(_, 0) = raise RunCall.Div | it(x,y) = q(x, y)
    end;
    RunCall.addOverload it "div";
    local
        val r: FixedInt.int * FixedInt.int -> FixedInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_fixed_rem
    in
        fun it(_, 0) = raise RunCall.Div | it(x,y) = r(x,y)
    end;
    RunCall.addOverload it "mod";
    
    local
        val q: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_adiv
    in
        fun it(_, 0) = raise RunCall.Div | it(x,y) = q(x, y)
    end;
    RunCall.addOverload it "div";
    local
        val r: LargeInt.int * LargeInt.int -> LargeInt.int = RunCall.run_call2 RuntimeCalls.POLY_SYS_amod
    in
        fun it(_, 0) = raise RunCall.Div | it(x,y) = r(x,y)
    end;
    RunCall.addOverload it "mod";

    local
        val d: word * word -> word = RunCall.run_call2 RuntimeCalls.POLY_SYS_div_word
    in
        fun it(_, 0w0) = raise RunCall.Div | it(x, y) = d(x, y)
    end;
    RunCall.addOverload it "div";
    local
        val m: word * word -> word = RunCall.run_call2 RuntimeCalls.POLY_SYS_mod_word
    in
        fun it(_, 0w0) = raise RunCall.Div | it(x, y) = m(x, y)
    end;
    RunCall.addOverload it "mod";

    (* We need to use the same identifier for this that we used when
       compiling the compiler, particularly "make". *)
    exception Fail = RunCall.Fail

(* A few useful functions which are in the top-level environment.
   Others are added later. *)
val not: bool -> bool = RunCall.run_call1 RuntimeCalls.POLY_SYS_not_bool;

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

    val isShortString   : string -> bool = RunCall.run_call1 POLY_SYS_is_short

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

(* Add these in.  They are defined in Initialise in 5.6.1 but not in 5.6.0 *)
structure Word =
struct
    type word = word
end
and LargeWord =
struct
    type word = Bootstrap.word
end
and String =
struct
    type string = string
end
and Char =
struct
    type char = char
end;

structure StringCvt = struct type  ('a, 'b) reader = 'b -> ('a * 'b) option end;

(* Extend RunCall for compatibility with 5.6.1 *)
structure RunCall =
struct
    open RunCall
    fun isShort(a:'a): bool = RunCall.run_call1 RuntimeCalls.POLY_SYS_is_short a

    fun loadWord(a: 'a, i: word): 'b = run_call2 RuntimeCalls.POLY_SYS_load_word(a, i)
    and loadWordFromImmutable(a: 'a, i: word): 'b = run_call2 RuntimeCalls.POLY_SYS_load_word_immut(a, i)
    and storeWord(a: 'a, i: word, b: 'b): unit = run_call3 RuntimeCalls.POLY_SYS_assign_word(a, i, b)
 
    fun loadByte(a: 'a, i: word): 'b = run_call2 RuntimeCalls.POLY_SYS_load_byte(a, i)
    and loadByteFromImmutable(a: 'a, i: word): 'b = run_call2 RuntimeCalls.POLY_SYS_load_byte_immut(a, i)
    and storeByte(a: 'a, i: word, b: 'b): unit = run_call3 RuntimeCalls.POLY_SYS_assign_byte(a, i, b)
   
    fun memoryCellLength(a:'a): word = run_call1 RuntimeCalls.POLY_SYS_get_length a
    and memoryCellFlags(a:'a): word = run_call1 RuntimeCalls.POLY_SYS_get_flags a
    
    val bytesPerWord: word = run_call0 RuntimeCalls.POLY_SYS_bytes_per_word ()
    
    fun clearMutableBit(a: 'a): unit = run_call1 RuntimeCalls.POLY_SYS_lockseg a

    fun pointerEq(x: 'a ,y: 'a): bool = RunCall.run_call2 RuntimeCalls.POLY_SYS_word_eq (x,y)
    
    fun allocateByteMemory(len: word, flags: word): 'a =
        RunCall.run_call3 RuntimeCalls.POLY_SYS_alloc_store(len, flags, 0w0)
    
    fun allocateWordMemory(len: word, flags: word, init: 'a): 'b =
        RunCall.run_call3 RuntimeCalls.POLY_SYS_alloc_store(len, flags, init)

    (* N.B. moveBytes/word and POLY_SYS_move_bytes/words have slightly different argument ordering. *)
    fun moveBytes(src: 'a, dst: 'a, srcOffset: word, dstOffset: word, length: word) =
        RunCall.run_call5 RuntimeCalls.POLY_SYS_move_bytes(src, srcOffset, dst, dstOffset, length)

    fun moveWords(src: 'a, dst: 'a, srcOffset: word, dstOffset: word, length: word) =
        RunCall.run_call5 RuntimeCalls.POLY_SYS_move_words(src, srcOffset, dst, dstOffset, length)
end;

(* This is used to provide compatibility between 5.6.0 and 5.6.1. *)
structure Compat560 =
struct
    fun setCodeConstant(addr:'a , offset: word, cnstnt: 'b, flags: word): unit =
        RunCall.run_call4 RuntimeCalls.POLY_SYS_set_code_constant (addr, offset, cnstnt, flags)
    
    fun ffiGeneral(code: int, arg: 'a): 'b =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_ffi (code, arg)
        
    fun oldForeignGeneral(code: int, arg: 'a): 'b =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_foreign_dispatch (code, arg)
        
    fun xWindowsGeneral (arg:'a):'b =
        RunCall.run_call1 RuntimeCalls.POLY_SYS_XWindows arg
    
    fun threadGeneral(code: int, arg:'a):'b =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_thread_dispatch (code, arg)

    fun createEntryPointObject(s: string): 'a =
        RunCall.run_call1 RuntimeCalls.POLY_SYS_get_entry_point s
    
    fun osSpecificGeneral(code: int, arg:'a):'b =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_os_specific (code, arg)

    fun polySpecificGeneral(code: int, arg:'a):'b =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_poly_specific (code, arg)

    fun timingGeneral(code: int, arg:'a):'b =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_timing_dispatch (code, arg)

    fun processEnvGeneral(code: int, arg:'a):'b =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_process_env (code, arg)

    fun realGeneral(code: int, arg:'a):'b =
        RunCall.run_call2 RuntimeCalls.POLY_SYS_Real_Dispatch (code, arg)

    val isBigEndian: unit -> bool = RunCall.run_call0 RuntimeCalls.POLY_SYS_is_big_endian
end;
