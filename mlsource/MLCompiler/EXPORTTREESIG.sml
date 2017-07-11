(*
    Copyright (c) 2009, 2016 David C.J. Matthews

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

signature EXPORTTREESIG =
sig
    type pretty
    type types
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    type locationProp

    (* Export tree. *)
    type ptProperties
    type exportTree = location * ptProperties list

    val PTprint: (FixedInt.int -> pretty) -> ptProperties (* Print the tree *)
    val PTtype: types -> ptProperties (* Type of an expression *)
    val PTdeclaredAt: location -> ptProperties (* Declaration location for id. *)
    val PTopenedAt: location -> ptProperties (* When an identifier comes from an "open" the location of the open. *)
    val PTstructureAt: location -> ptProperties (* When an identifier comes from open S or S.a the declaration of S. *)
    val PTreferences: (bool * location list) -> ptProperties  (* The references to the ID.  The first is true if this is exported. *)
    val PTparent: (unit -> exportTree) -> ptProperties 
    val PTpreviousSibling: (unit -> exportTree) -> ptProperties 
    val PTnextSibling: (unit -> exportTree) -> ptProperties 
    val PTfirstChild: (unit -> exportTree) -> ptProperties
    val PTcompletions: string list -> ptProperties
    val PTbreakPoint: bool ref -> ptProperties (* Breakpoint associated with location. *)
    val PTdefId: FixedInt.int -> ptProperties (* Defining binding id *)
    val PTrefId: FixedInt.int -> ptProperties (* Reference binding id *)
    
    type navigation =
        {parent: (unit -> exportTree) option,
         next: (unit -> exportTree) option,
         previous: (unit -> exportTree) option}
    
    val exportList :
        (navigation * 'a -> exportTree)
            * (unit -> exportTree) option -> 'a list -> ptProperties list
            
    val exportNavigationProps: navigation -> ptProperties list

    val getStringAsTree: navigation * string * location * ptProperties list -> exportTree
    
    val rootTreeTag: navigation Universal.tag
    
    val mapLocationProps: locationProp list -> ptProperties list
    and definingLocationProps: locationProp list -> ptProperties list

    (* Types that can be shared. *)
    structure Sharing:
    sig
        type types          = types
        and  locationProp   = locationProp
        and  pretty         = pretty
        and  ptProperties   = ptProperties
    end
end;
