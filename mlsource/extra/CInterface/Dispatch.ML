(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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


structure Dispatch: DispatchSig =
struct
   abstype machineWord = MachineWord of word
    with
        val toWord      : 'a -> machineWord   = RunCall.unsafeCast;
        val isShortInt  : machineWord -> bool = RunCall.isShort;
        val toShortInt  : machineWord -> int  = RunCall.unsafeCast; (* used for tags *)
        val unsafeCast  : machineWord -> 'a   = RunCall.unsafeCast; (* used for data values *)
    end

    local
        fun prettyWord _ _ (_: machineWord) = PolyML.PrettyString "?"
    in
        val () = PolyML.addPrettyPrinter prettyWord
    end

    structure Union : UnionSig =
    struct
    (***
     Defines datatypes used to communicate
     with new primitive: call_sym_and_convert.
    ***) 
      exception Never of string; (* Used for failed assertions. *)
      fun never s = raise Never s;
    
      (* Don't use datatypes because their representations might change.
        Tags were originally allocated in alphabetical order, but  ...
         datatype 'a union =
             Char    of string
           | Double  of real
           | Float   of real
           | Int     of int
           | Long    of int
           | Short   of int
           | String  of string;
           | Vol     of 'a
     ... *)
     
         (* "union" is the type of a raw value with the type information. *)
         (* we can't make this an abstype, because that might change the representation! *)
         type 'a union = machineWord * int;
         
         fun Char   (x:char)   : 'a union = (toWord x,1);
         fun Double (x:real)   : 'a union = (toWord x,2);
         fun Float  (x:real)   : 'a union = (toWord x,3);
         fun Int    (x:int)    : 'a union = (toWord x,4);
         fun Long   (x:int)    : 'a union = (toWord x,5);
         fun Short  (x:int)    : 'a union = (toWord x,6);
         fun String (x:string) : 'a union = (toWord x,7);
         fun Vol    (x:'a)     : 'a union = (toWord x,8);
         fun Uint   (x:int)    : 'a union = (toWord x,9);
         
         fun isChar   ((_,1) : 'a union) : bool   = true | isChar _   = false;
         fun isDouble ((_,2) : 'a union) : bool   = true | isDouble _ = false;
         fun isFloat  ((_,3) : 'a union) : bool   = true | isFloat _  = false;
         fun isInt    ((_,4) : 'a union) : bool   = true | isInt _    = false;
         fun isLong   ((_,5) : 'a union) : bool   = true | isLong _   = false;
         fun isShort  ((_,6) : 'a union) : bool   = true | isShort _  = false;
         fun isString ((_,7) : 'a union) : bool   = true | isString _ = false;
         fun isVol    ((_,8) : 'a union) : bool   = true | isVol _    = false;
         fun isUint   ((_,9) : 'a union) : bool   = true | isUint _   = false;
           
         fun deChar   ((x,1) : 'a union) : char   = unsafeCast x | deChar _   = never "deChar";
         fun deDouble ((x,2) : 'a union) : real   = unsafeCast x | deDouble _ = never "deDouble";
         fun deFloat  ((x,3) : 'a union) : real   = unsafeCast x | deFloat _  = never "deFloat";
         fun deInt    ((x,4) : 'a union) : int    = unsafeCast x | deInt _    = never "deInt";
         fun deLong   ((x,5) : 'a union) : int    = unsafeCast x | deLong _   = never "deLong";
         fun deShort  ((x,6) : 'a union) : int    = unsafeCast x | deShort _  = never "deShort";
         fun deString ((x,7) : 'a union) : string = unsafeCast x | deString _ = never "deString";
         fun deVol    ((x,8) : 'a union) : 'a     = unsafeCast x | deVol _    = never "deVol";
         fun deUint   ((x,9) : 'a union) : int    = unsafeCast x | deUint _   = never "deInt";
     
     
     (* ...
         datatype 'a unionChoice =
             chooseChar
           | chooseDouble
           | chooseFloat
           | chooseInt
           | chooseLong
           | chooseShort
           | chooseString;
           | chooseVol of 'a
     ... *)
     
         (* unionChoice is the type information used when we want to indicate the type of a result. *)
         type 'a unionChoice = machineWord;
    
         local
         (* DCJM 25/3/00.  This is a more complicated in ML97 than the
            original ML90. *)
             val w1: machineWord = toWord 1
             and w2: machineWord = toWord 2
             and w3: machineWord = toWord 3
             and w4: machineWord = toWord 4
             and w5: machineWord = toWord 5
             and w6: machineWord = toWord 6
             and w7: machineWord = toWord 7
             and w9: machineWord = toWord 9
         in
             val chooseChar   : 'a unionChoice = w1;
             val chooseDouble : 'a unionChoice = w2;
             val chooseFloat  : 'a unionChoice = w3;
             val chooseInt    : 'a unionChoice = w4;
             val chooseLong   : 'a unionChoice = w5;
             val chooseShort  : 'a unionChoice = w6;
             val chooseString : 'a unionChoice = w7;
             val chooseUint   : 'a unionChoice = w9;
         end;
    
         (* DCJM 8/10/99.  This originally said simply {1=x} presumably
           with the intention of creating a single boxed object. That
           doesn't work any longer.  Add an extra field to ensure that
           it's boxed.  (We may be able to get the effect we want using
           datatype 'a t = A | B of 'a ) *)
         val chooseVol    : 'a -> 'a unionChoice = fn x => toWord {1 = x, 2 = 99};
     
         fun isChooseChar   (x : 'a unionChoice) : bool = 
           isShortInt x andalso toShortInt x = 1;
     
         fun isChooseDouble (x : 'a unionChoice) : bool = 
           isShortInt x andalso toShortInt x = 2;
     
         fun isChooseFloat  (x : 'a unionChoice) : bool = 
           isShortInt x andalso toShortInt x = 3;
     
         fun isChooseInt    (x : 'a unionChoice) : bool = 
           isShortInt x andalso toShortInt x = 4;
     
         fun isChooseLong   (x : 'a unionChoice) : bool = 
           isShortInt x andalso toShortInt x = 5;
     
         fun isChooseShort  (x : 'a unionChoice) : bool = 
           isShortInt x andalso toShortInt x = 6;
     
         fun isChooseString (x : 'a unionChoice) : bool = 
           isShortInt x andalso toShortInt x = 7;
     
         fun isChooseUint   (x : 'a unionChoice) : bool = 
           isShortInt x andalso toShortInt x = 9;
     
         fun isChooseVol    (x : 'a unionChoice) : bool = 
           not (isShortInt x);
     
         (* needed? *)
         (* DCJM 8/10/99.  This original simply said { 1=y } but that no longer
            works.  Changed so that it definitely indirects. *)
         fun deChooseVol (x : 'a unionChoice) : 'a =
           if not (isShortInt x) then (case unsafeCast x of {1 = y, ...}:'a * 'a => y)
           else never "deChooseVol";
           
         (* directedArg is used to encode either an "in" parameter or an "out" parameter. *)
     
     (* ...
         datatype ('a, 'b) directedArg =
             In  of ('a * 'b) union
           | Out of 'a unionChoice;
     ... *)
     
        type ('a,'b) directedArg = machineWord * int;
        
        val In  : ('a * 'b) union -> ('a,'b) directedArg = fn x => (toWord x,1);
        val Out : 'a unionChoice  -> ('a,'b) directedArg = fn x => (toWord x,2);
             
        val isIn  : ('a,'b) directedArg -> bool = fn (_,1) => true | _ => false
        val isOut : ('a,'b) directedArg -> bool = fn (_,2) => true | _ => false
             
        val deIn  : ('a,'b) directedArg -> ('a * 'b) union = fn (x,1) => unsafeCast x | _ => never "deIn"
        val deOut : ('a,'b) directedArg -> 'a unionChoice  = fn (x,2) => unsafeCast x | _ => never "deOut"
    
    (* ...
      fun mapUnion f union =
          case union of
              Vol x       => Vol (f x)
            | Char x      => Char x
            | Double x    => Double x
            | Float x     => Float x
            | Int x       => Int x
            | Short x     => Short x
            | Long x      => Long x
            | String x    => String x
    ... *)
     fun mapUnion f (x : 'a union) =
       if isVol x then Vol (f (deVol x)) else x;
    
    (* ...
      fun mapUnionChoice f choice =
          case choice of
              chooseVol x   => chooseVol (f x)
            | chooseChar    => chooseChar
            | chooseDouble  => chooseDouble
            | chooseFloat   => chooseFloat
            | chooseInt     => chooseInt
            | chooseShort   => chooseShort
            | chooseLong    => chooseLong
            | chooseString  => chooseString
    ... *)
      fun mapUnionChoice f (x : 'a unionChoice) =
        if isChooseVol x then chooseVol (f (deChooseVol x)) else x;
    
    (* ...
      fun mapDirectedArg f g arg =
          case arg of
              In union   => In  (mapUnion (fn (ctype, vol) => (f ctype, g vol)) union)
            | Out choice => Out (mapUnionChoice f choice)
    ... *)          
    
      fun mapDirectedArg f g (x : ('a,'b) directedArg) =
        if isIn x 
        then In  (mapUnion (fn (ctype, vol) => (f ctype, g vol)) (deIn x))
        else Out (mapUnionChoice f (deOut x))
    end;

(* Don't use datatype because representation may change ...
    datatype RawCtype =
    Cchar | Cdouble | Cfloat | Cint | Clong | Cpointer | Cshort | Cstruct of int
... *)
    (* DCJM 23/3/04.  This provides the interface to foreign.c in the run-time system.
       The representation is shared with that code.  *)
    type RawCtype = machineWord;
    val Cchar     = toWord 1;
    val Cdouble   = toWord 2;
    val Cfloat    = toWord 3;
    val Cint      = toWord 4;
    val Clong     = toWord 5;
    val Cpointer  = toWord 6;
    val Cshort    = toWord 7;
    val Cuint     = toWord 8;
    (* The above are all tagged values.  A struct is represented by an untagged value
       which is a vector of types. *)
    fun Cstruct (elements: RawCtype list) = toWord(Vector.fromList elements)

    abstype rawvol = rawvol with end; (* Purely abstract type. *)

    local
        fun prettyVol _ _ (_: rawvol) = PolyML.PrettyString "?"
    in
        val () = PolyML.addPrettyPrinter prettyVol
    end

    local
        val oldForeignGeneralCall = RunCall.rtsCallFull2 "PolyForeignGeneral"
        fun oldForeignGeneral(code: int, arg: 'a): 'b =
            RunCall.unsafeCast(oldForeignGeneralCall(RunCall.unsafeCast(code, arg)))
    in
        fun foreign code args = oldForeignGeneral(code:int,args)
    end

    local
        val dispatch_index = ref 0;
    in
        fun next currier =
        let val f = currier (foreign (!dispatch_index))
        in  dispatch_index := 1 + !dispatch_index;
        f
        end
    end
    
    
    
    (* Curry functions *)    
    fun one f               = f         (* eta-reduced SPF 19/10/94 *)
    fun two f x y           = f (x,y)
    fun three f x y z       = f (x,y,z)
    
    (* The order of these declarations is critical - DO NOT CHANGE *)
    val get_foreign_debug   = next(one);
    val set_foreign_debug   = next(one);
    
    val load_lib            = next(one);
    val load_sym            = next(two);
    
    (*
    DCJM 7/4/04.  It took me a while to figure out what was going on since I didn't have
    any documentation.
    There were two versions of call_sym: call_sym_origCversion and call_sym_and_convert.
    
    The older version, call_sym_origCversion, took three parameters: sym,
    args and retChoice.  "sym" is the function to call, "args" is a list of pairs of
    type info (as a code) and values and "retChoice" indicates the type of the result.
    The result of this function is passed back as a vol.
    
    The newer version, call_sym_and_convert, also takes three parameters: sym,
    args and retChoice.  "sym" and "retChoice" are the same as the previous version
    but "args" is now a list of entries which may be either "in" parameters which
    are the same as before or "out" parameters where only the type is supplied (since
    an "out" parameter doesn't have a value before the function is called).  The result
    is now a pair where the first component is the function result as before and the
    second component the list of the values of the "out" parameters.
    The newer version does all the conversion of ML arguments into C values and of the
    result of the C function back into ML as part of a single RTS call whereas the
    older version required multiple RTS calls.
    *)
    
    val call_sym_and_convert  = next(three);
        
    val alloc               = next(one); (* Different name from that in foreign.c *)
    val address             = next(one);
    val deref               = next(one);
    val offset              = next(two);
    val assign              = next(three);
    val sizeof              = next(one); (* Different name from that in foreign.c *)
    val alignment           = next(one);
    
    val toCchar             = next(one);
    val fromCchar           = next(one);
    val toCdouble           = next(one);
    val fromCdouble         = next(one);
    val toCfloat            = next(one);
    val fromCfloat          = next(one);
    val toCint              = next(one);
    val fromCint            = next(one);
    val toClong             = next(one);
    val fromClong           = next(one);
    val toCshort            = next(one);
    val fromCshort          = next(one);
    
    val fillCstring         = next(two);
    val toCstring           = next(one);
    val fromCstring         = next(one);
    
    val toCuint             = next(one); (* Added DCJM 27/5/01 *)
    val fromCuint           = next(one); (* Added DCJM 27/5/01 *)
    
    val toCbytes            = next(one); (* Added DCJM 29/6/01 *)
    val fromCbytes          = next(one); (* Added DCJM 29/6/01 *)
    (* Note: fromCbytes takes a pair. *)
    
    val toCfunction         = next(three);(* Added DCJM 7/4/04 *)
    val toPascalfunction    = next(three);(* Added DCJM 7/4/04 *)
    
    val setFinal            = next(two);  (* Added DCJM 2/8/09. *)
    
    val null                = next(one) () (* Added DCJM 16/11/11. *)

end (* struct *)
