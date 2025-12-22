(*
    Copyright (c) 2013, 2025 David C.J. Matthews

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

signature PrintParsetreeSig =
sig
    type lexan
    type parsetree
    type matchtree
    type pretty
    type typeParsetree
    type types
    type parseTypeVar
    type typeConstrSet
    type typeId
    type structVals

    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    and printTypeEnv =
        { lookupType: string -> (typeConstrSet * (int->typeId) option) option,
          lookupStruct: string -> (structVals * (int->typeId) option) option}

    val displayParsetree: parsetree * FixedInt.int -> pretty
    val displayMatch: matchtree * FixedInt.int -> pretty
     (* A list of type variables. *)
    val displayTypeVariables: parseTypeVar list * FixedInt.int -> pretty list
    val displayTypeParse: typeParsetree * FixedInt.int * printTypeEnv -> pretty
   
    val typeFromTypeParse: typeParsetree -> types

    val errorNear: lexan * bool * parsetree * location * string -> unit
     
    structure Sharing:
    sig
        type lexan = lexan
        and  parsetree = parsetree
        and  matchtree = matchtree
        and  pretty = pretty
        and  typeParsetree = typeParsetree
        and  types = types
        and  parseTypeVar = parseTypeVar
        and  typeId = typeId
        and  structVals = structVals
        and  typeConstrSet = typeConstrSet
    end
end;
