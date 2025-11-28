(*
    Copyright (c) David C.J. Matthews 2025

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

(* Export signature for SKIPS *)
signature SKIPS =
sig
    type sys
    type lexan
    type symset
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }

    val notfound: string * lexan -> unit
    val badsyms:  sys * lexan -> unit
    val getsym:   sys * lexan -> unit
    val skipon:   symset * symset * string * lexan -> unit
    val getid:    symset * symset * lexan -> string * location
    val getLabel: symset * lexan -> string * location
    val getList:  sys * symset * lexan -> ('b -> 'a * location * 'b) -> 'b -> 'a list * location * 'b
    
    structure Sharing:
    sig
        type sys        = sys
        and lexan       = lexan
        and symset      = symset
    end
end;
