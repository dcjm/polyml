(*
    Copyright (c) 2009. 2015 David C.J. Matthews

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

(* Signature for the pretty printer. *)
signature PRETTY =
sig
    type context
    type pretty
    val ContextLocation:
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int, endLine: FixedInt.int, endPosition: FixedInt.int } -> context
    and ContextProperty: string * string (* User property. *) -> context

    val PrettyBlock: FixedInt.int * bool * context list * pretty list -> pretty
    and PrettyBreak: FixedInt.int * FixedInt.int -> pretty
    and PrettyString: string -> pretty
    
    val isPrettyBlock: pretty -> bool
    val isPrettyBreak: pretty -> bool
    val isPrettyString: pretty -> bool
    
    val projPrettyBlock: pretty -> int * bool * context list * pretty list
    val projPrettyBreak: pretty -> int * int
    val projPrettyString: pretty -> string

(*
    datatype context =
        ContextLocation of
            { file: string, startLine: int, startPosition: int, endLine: int, endPosition: int }
    |   ContextProperty of string * string (* User property. *)

    datatype pretty =
        PrettyBlock of int * bool * context list * pretty list
    |   PrettyBreak of int * int
    |   PrettyString of string
*)

    (* A simple "pretty printer" that just accumulates strings. *)
    val uglyPrint: pretty -> string

    (* A proper pretty printer. *)
    val prettyPrint: (string -> unit) * int -> pretty -> unit

    (* Tag for pretty printed out from PolyML.print. *)
    val printOutputTag : (pretty -> unit) Universal.tag
    (* Compiler output.  Used for timing information and compiler debug output. *)
    and compilerOutputTag: (pretty->unit) Universal.tag
    
    val getPrintOutput : Universal.universal list -> pretty -> unit
    and getCompilerOutput : Universal.universal list -> pretty -> unit
    (* getSimplePrinter prints strings through compilerOutput. *)
    and getSimplePrinter: Universal.universal list * int list -> string -> unit

    val tagPrettyBlock: word
    and tagPrettyBreak: word
    and tagPrettyString: word

    val maxPrettyTag: word

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type pretty     = pretty
        and  context    = context
    end
end;
