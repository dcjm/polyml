(*
    Title:      Source level debugger for Poly/ML
    Author:     David Matthews
    Copyright  (c)   David Matthews 2000, 2009, 2014-15, 2020, 2025

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

signature DEBUGGER =
sig
    type types
    type values
    type machineWord
    type fixStatus
    type structVals
    type typeConstrSet
    type signatures
    type functors
    type locationProp
    type typeId
    type level
    type lexan
    type codeBinding
    type codetree
    type environEntry
    type valueType
 
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }

    val envTypeId: typeId -> environEntry

    type breakPoint = bool ref

    (* Functions to make debug entries for various values, types etc. *)
    type debuggerStatus
    val initialDebuggerStatus: debuggerStatus

    val makeValDebugEntries:
        values list * debuggerStatus * level * lexan * (int -> int) -> codeBinding list * debuggerStatus
    val makeTypeConstrDebugEntries:  typeConstrSet list * debuggerStatus * level * lexan * (int -> int) -> codeBinding list * debuggerStatus
    val makeStructDebugEntries: structVals list * debuggerStatus * level * lexan * (int->int) ->
        codeBinding list * debuggerStatus
    val makeTypeIdDebugEntries: typeId list * debuggerStatus * level * lexan * (int->int) -> codeBinding list * debuggerStatus
    
    val updateDebugLocation: debuggerStatus * location * lexan -> codeBinding list * debuggerStatus

    (* Create a local break point and check the global and local break points. *)
    val breakPointCode: breakPoint option ref * location * level * lexan * (int->int) -> codeBinding list

    (* Add debugging calls on entry and exit to a function. *)
    val wrapFunctionInDebug:
        (debuggerStatus -> codetree) * string * codetree * types * types *
        location * debuggerStatus * level * lexan * (int -> int) -> codetree

    (* Exported functions that appear in PolyML.DebuggerInterface. *)
    type debugState (* The run-time state. *)

    val makeValue: debugState -> string * valueType * locationProp list * machineWord -> values
    and makeException: debugState -> string * valueType * bool * locationProp list * machineWord -> values
    and makeConstructor:
        debugState -> string * valueType * bool * int * locationProp list * machineWord -> values
    and makeAnonymousValue: debugState -> valueType * machineWord -> values
    
    val makeStructure: debugState -> string * signatures * locationProp list * machineWord -> structVals
    and makeTypeConstr: debugState -> typeConstrSet -> typeConstrSet
    
    val setOnEntry: (string * PolyML.location -> unit) option -> unit
    and setOnExit: (string * PolyML.location -> unit) option -> unit
    and setOnExitException: (string * PolyML.location -> exn -> unit) option -> unit
    and setOnBreakPoint: (PolyML.location * bool ref -> unit) option -> unit

    structure Sharing:
    sig
        type types          = types
        type values         = values
        type machineWord    = machineWord
        type fixStatus      = fixStatus
        type structVals     = structVals
        type typeConstrSet  = typeConstrSet
        type signatures     = signatures
        type functors       = functors
        type locationProp   = locationProp
        type environEntry   = environEntry
        type typeId         = typeId
        type level          = level
        type lexan          = lexan
        type codeBinding    = codeBinding
        type codetree       = codetree
        type debuggerStatus = debuggerStatus
        type valueType      = valueType
    end
end;
