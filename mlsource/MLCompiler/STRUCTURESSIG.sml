(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Modified David C. J. Matthews 2009.

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

(*****************************************************************************)
(*                  STRUCTURES export signature                              *)
(*****************************************************************************)
signature STRUCTURESSIG =
sig
    type structDec
    type structValue
    type structVals;
    type types;
    type parsetree;
    type lexan;
    type pretty;
    type values;
    type typeConstrSet
    type codetree;
    type signatures;
    type functors;
    type env;
    type sigBind and functorBind and structBind
    type machineWord
    type fixStatus
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    type topdec
    type program
    type ptProperties
    type exportTree = location * ptProperties list
    type navigation =
        {parent: (unit -> exportTree) option,
         next: (unit -> exportTree) option,
         previous: (unit -> exportTree) option}
    type typeParsetree
    type formalArgStruct
    type sigs

    val mkStructureDec:     structBind list * location -> structDec
    val mkStruct:           structDec list * location -> structValue
    val mkSignatureDec:     sigBind list * location -> topdec;
    val mkFunctorDec:       functorBind list * location -> topdec;
    val mkLocaldec:         structDec list * structDec list * location -> structDec;
    val mkLetdec:           structDec list * structValue * location -> structValue;
    val mkCoreLang:         parsetree * location -> structDec;
    val mkStructureBinding: (string * location) * (sigs * bool * location) option * structValue * location -> structBind
    val mkStructIdent:      string * location -> structValue;
    val mkSignatureBinding: (string * location) * sigs * location -> sigBind;
    val mkFunctorAppl:      string * structValue * location * location -> structValue;
    val mkFormalArg:        string * sigs -> formalArgStruct;
    val mkFunctorBinding:
        string * location * (sigs * bool * location) option * structValue * formalArgStruct * location -> functorBind;
    val mkSigConstraint:    structValue * sigs * bool * location -> structValue
    val mkTopDec:           structDec -> topdec
    val mkProgram:          topdec list * location -> program

    val pass2Structs:   program * lexan * env -> unit;

    val pass4Structs:
        codetree * program ->
            {
                fixes: (string * fixStatus) list, values: (string * values) list,
                structures: (string * structVals) list, signatures: (string * signatures) list,
                functors: (string * functors) list, types: (string* typeConstrSet) list
            };

    val gencodeStructs: program * lexan -> codetree * int

    val displayProgram: program * FixedInt.int -> pretty;

    val structsExportTree: navigation * program -> exportTree

    structure Sharing:
    sig
        type structDec      = structDec
        type structValue    = structValue
        type structVals     = structVals
        type types          = types
        type parsetree      = parsetree
        type lexan          = lexan
        type pretty         = pretty
        type values         = values
        type typeConstrSet  = typeConstrSet
        type codetree       = codetree
        type signatures     = signatures
        type functors       = functors
        type env            = env
        type sigBind        = sigBind
        and  functorBind    = functorBind
        and  structBind     = structBind
        type machineWord    = machineWord
        type fixStatus      = fixStatus
        type topdec         = topdec
        type program        = program
        type typeParsetree  = typeParsetree
        type formalArgStruct= formalArgStruct
        type ptProperties   = ptProperties
        type sigs           = sigs
    end
end;
