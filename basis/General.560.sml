(*
    Title:      Standard Basis Library: General Structure
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

(* G&R 2004 status: checked, no change. *)

signature GENERAL =
  sig
    eqtype  unit
    type  exn
    exception Bind
    exception Chr
    exception Div
    exception Domain
    exception Fail of string
    exception Match
    exception Overflow
    exception Size
    exception Span
    exception Subscript
    val exnName : exn -> string
    val exnMessage : exn -> string
    datatype order = LESS | EQUAL | GREATER
    val ! : 'a ref -> 'a
    val := : ('a ref * 'a) -> unit
    val o : (('b -> 'c) * ('a -> 'b)) -> 'a -> 'c
    val before : ('a * unit) -> 'a
    val ignore : 'a -> unit
  end;


(* We declare the values in the top-level environment and construct
   the structure afterwards rather than opening the structure.  The
   reason for this is that we would prefer that types unit and exn
   did not capture the General structure name. *)
local
    open RuntimeCalls (* for POLY_SYS and EXC numbers *)
    val System_loadw: exn*int->string  = RunCall.run_call2 POLY_SYS_load_word
in
    exception Bind      = RunCall.Bind
    and       Div       = RunCall.Div
    and       Match     = RunCall.Match
    and       Overflow  = RunCall.Overflow
    and       Subscript = RunCall.Subscript
    and       Size      = RunCall.Size

    exception Domain and Span and Chr

    (* Exception packets.  The first word is the code, a unique id; the second is
       the exception name and the third is the exception argument. *)
    fun exnName (ex: exn) = System_loadw(ex, 1)
    
    (* Since exception packets carry a printer function this is just PolyML.makestring. *)
    fun exnMessage (ex: exn) = PolyML.makestring ex
    
    datatype order = LESS | EQUAL | GREATER
    
    fun op before (a, _ : unit) = a
    fun ignore _ = ()
    
    structure General (*: GENERAL *) (* Don't use a signature here. *) =
        struct
        type unit = unit (* This has to be primitive because its value is given by () *)
        type exn = exn
        exception Bind = Bind and Div = Div and Match = Match and Chr = Chr
        exception Overflow = Overflow and Domain= Domain and Fail = Fail
        exception Span = Span and Subscript = Subscript and Size = Size

        val exnName = exnName
        and op := = op := and ! = ! and op o = op o
        and op before = op before and ignore = ignore

        val exnMessage = exnMessage
        
        datatype order = datatype order
    end
end

(* Although these are available unqualified we always use them
   qualified within this library so that dependencies are
   maintained. *)
