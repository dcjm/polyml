(*
    Copyright (c) 2009, 2015-16 David C.J. Matthews

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

signature LEXSIG =
(*****************************************************************************)
(*                  LEX export signature                                     *)
(*****************************************************************************)
sig
    type lexan;
    type sys;
    type pretty;

    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }

    val insymbol: lexan -> unit;
     
    (* insymbol sets sy and id which are exported as "read-only" *)
     
    val sy:     lexan -> sys;
    val id:     lexan -> string;
    val location: lexan -> location;
    val pushBackSymbol: lexan * sys -> unit;
     
    val initial: (unit -> char option) * Universal.universal list -> lexan;

    (* Error handling *)
    val reportError:
        lexan ->
            { location: location, hard: bool, message: pretty, context: pretty option } -> unit
    (* Simple error message. *)
    val errorMessage: lexan * location * string -> unit
    (* Simple warning message. *)
    val warningMessage: lexan * location * string -> unit
     
    val errorOccurred: lexan -> bool;

    val nullLex: lexan; (* Used when no errors are expected - streams raise exceptions. *)
  
    (* To save passing an extra argument to many functions we include the
       debug/control parameters here. *)
    val debugParams: lexan -> Universal.universal list
    (* This is also not really part of the lexical analyser. *)
    val newBindingId: lexan -> FixedInt.int

    val errorDepth: lexan -> FixedInt.int

    (* Print error and warning messages. *)
    val errorMessageProcTag:
        ({ location: location, hard: bool, message: pretty, context: pretty option } -> unit) Universal.tag

    (* A null location *)
    val nullLocation: location

    (* Construct the location that starts at the start of the first location
       and ends at the end of the second.  Used to combine the locations of
       individual lexical units into a location for a larger syntactic unit. *)
    val locSpan: location * location -> location

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type pretty     = pretty
        and  lexan      = lexan
        and  sys        = sys
    end


end (* LEX export signature *);

