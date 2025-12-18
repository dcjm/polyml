(*
    Copyright (c) 2009, 2015-16, 2025 David C. J. Matthews

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

signature TYPEIDCODESIG =
sig
    type typeId
    type codetree
    type codeBinding
    type types
    type typeConstrs
    type typeConstrSet
    type typeVar
    type level

    val codeId: typeId * level -> codetree
    val codeGenerativeId:
        { source: typeId, isEq: bool, isDatatype: bool, mkAddr: int->int, level: level} -> codetree
    
    val createDatatypeFunctions:
         {typeConstr: typeConstrSet, eqStatus: bool } list * (int->int) * level * bool -> codeBinding list
    
    val codeForUniqueId: unit->codetree

    (* Generate a function of the form t*int->pretty for values of type t. *)
    val printerForType: types * level -> codetree
    (* Generate a function of the form (t,t) -> bool. *)
    val equalityForType: types * level  -> codetree

    structure Sharing:
    sig
        type typeId     = typeId
        type codetree   = codetree
        type types      = types
        type typeConstrs= typeConstrs
        type typeConstrSet=typeConstrSet
        type typeVar=typeVar
        type codeBinding    = codeBinding
        type level = level
    end
end;
