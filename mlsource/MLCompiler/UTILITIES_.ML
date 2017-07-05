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

(*
    Title:      Various Utilities.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1988
*)

functor UTILITIES_ ( 

(*****************************************************************************)
(*                  LEX                                                      *)
(*****************************************************************************)
structure LEX : LEXSIG

) :
   
(*****************************************************************************)
(*                  UTILITIES export signature                               *)
(*****************************************************************************)
sig
  type lexan;
  type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }

  val noDuplicates: (string * 'a * 'a -> unit) -> 
                       { apply: (string * 'a -> unit) -> unit,
                         enter:  string * 'a -> unit,
                         lookup: string -> 'a option };
    
  val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                            enter:  string * 'a -> unit,
                            lookup: string -> 'a option};
    
  val checkForDots: string * lexan * location -> unit;
    
  val mapTable: ('a * 'a -> bool) ->
                   {enter: 'a * 'b -> unit, lookup: 'a -> 'b option};
     
  val splitString: string -> { first:string,second:string }

    structure Sharing:
    sig
        type lexan = lexan
    end
  
end (* UTILITIES export signature *) =


(*****************************************************************************)
(*                  UTILITIES functor body                                   *)
(*****************************************************************************)
struct
  open LEX;
  
  (* A search list in which an identifier may only be entered once.
     Entering it a second time gives an error message (""fail"" is called) *)
   
  fun noDuplicates fail =
  let
    val variable = ref ([]);

    (* Works down the list from the first declaration to the last, applying
       a function to each element. *)

    fun apply f =
    let
      fun appl []                = ()
      |   appl ((name,value)::T) = (appl T; f (name, value)) 
    in    
      appl (!variable)
    end;
           
    (* Add entry to the list if it is not already there. If it is then  call 'fail'. *)

    fun enter (name : string, value) = 
    let
      fun look []         = variable := (name,value) :: !variable
      |   look ((n,old)::T) = if n = name then fail(name, old, value) else look T;
    in
      look (!variable)
    end;
   
    (* Look up name *)

    fun lookup (name : string) = 
    let
      fun look []         = NONE
      |   look ((n,v)::T) = if n = name then SOME v else look T;
    in
      look (!variable)
    end;
  in
    { apply = apply, enter = enter, lookup = lookup }
  end;

  fun searchList () =
  let
    val variable = ref [];
    
    (* Works down the list from the first declaration to the last, applying
       a function to each element. *)
    
    fun apply f =
    let
      fun appl []                = ()
      |   appl ((name,value)::T) = (appl T; f (name, value)) 
    in    
      appl (!variable)
    end (* apply *);
    
    (* Add to list. *)
    fun enter (name : string, value) = 
      variable := (name,value) :: !variable;
   
    (* Look up name *)
    fun lookup (name : string) = 
    let
      fun look []         = NONE
      |   look ((n,v)::T) = if n = name then SOME v else look T;
    in
      look (!variable)
    end;
  in
    { apply = apply, enter = enter, lookup = lookup }
  end; (* searchList *)

  fun checkForDots (name, lex, location) = 
  let
    fun check i =
      if i > size name
        then ()
      else if String.str(String.sub(name, i-1)) = "." 
    then errorMessage (lex, location,
           "qualified name " ^ name ^ " illegal here")
      else check (i + 1)
  in
    check 1
  end;

  (* Creates a table which will match values of one type and return
     corresponding results. *)
  
  fun mapTable (same : 'a * 'a -> bool) :
         {enter: 'a * 'b -> unit, lookup: 'a -> 'b option} =
  let
    val table : ('a * 'b) list ref = ref [];
     
    (* Don't allow duplicate keys in the list! *)
    local    
      fun member (_, [] : ('a * 'b) list) = false
      |   member (x, h::t) = same (#1 h, x) orelse member (x, t);
    
      fun delete (_, [] : ('a * 'b) list) = []
      |   delete (x, h::t) = if same (#1 h, x) then t else h :: delete (x, t);
    in  
      fun enter (entry as (a: 'a, _: 'b)) : unit =
      let
    val list    = !table;
    val newList = if member (a, list) then delete (a, list) else list;
      in
    table := entry :: newList
      end;
    end;

(* old ...         
    (* Add a newType entry to the table. *) 
    fun enter (a, b) = table := (a,b) :: !table;
... *)
    
    (* Get a value if it's there *)
    fun lookup (x : 'a) : 'b option = 
    let
      fun search []           = NONE
      |   search ((a, b) :: T) = if same (a, x) then SOME b else search T;
    in
      search (!table)
    end;
  in
    {enter = enter, lookup = lookup}
  end;

   fun splitString s =
   (* Divides a name into its components.
     Returns the string split at the last ".". The first part is empty
     if there are no dots in it. *)
   let
      val (first, second) = Substring.splitr(fn c => c <> #".") (Substring.full s)
   in
      if Substring.isEmpty first
      then {first  = "", second = s}
      else {first  = Substring.string(Substring.trimr 1 first),
            second = Substring.string second }
   end

    structure Sharing =
    struct
        type lexan = lexan
    end

end;
   
