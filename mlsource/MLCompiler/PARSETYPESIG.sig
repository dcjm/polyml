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

(* Export signature for PARSE_TYPE *)
signature PARSETYPESIG =
sig
    type symset
    type lexan
    type typeParsetree
    type parseTypeVar
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
     
    val parseType: symset * lexan * {lookupTvar:string -> parseTypeVar} -> typeParsetree * location
    
    structure Sharing:
    sig
        type symset         = symset
        type lexan          = lexan
        type typeParsetree  = typeParsetree
        type parseTypeVar   = parseTypeVar
    end
end;
