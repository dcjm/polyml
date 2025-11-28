(*
    Copyright David C. J. Matthews 2009

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

signature SIGNATURESSIG =
sig
    type sigs
    type structSigBind
    type parsetree
    type typeParsetree
    type parseTypeVar
    type pretty
    type ptProperties
    type env
    type signatures
    type lexan
    type typeId
    type specs
    type location =
        { file: string, startLine: FixedInt.int, startPosition: FixedInt.int,
          endLine: FixedInt.int, endPosition: FixedInt.int }
    type exportTree = location * ptProperties list
    type navigation =
        {parent: (unit -> exportTree) option,
         next: (unit -> exportTree) option,
         previous: (unit -> exportTree) option}

    val mkStructureSig:     structSigBind list * location -> specs
    val mkStructureSigBinding: (string * location) * (sigs * bool * location) * location -> structSigBind
    val mkValSig:           (string * location) * typeParsetree * location -> specs
    val mkExSig:            (string * location) * typeParsetree option * location -> specs
    val mkCoreType:         parsetree * location -> specs
    val mkSharing:          bool * (string * location) list * location -> specs
    val mkWhereType:        sigs * parseTypeVar list * string * typeParsetree * location -> sigs
    val mkInclude:          sigs list * location -> specs
    val mkSigIdent:         string * location -> sigs;
    val mkSig:              specs list * location -> sigs;

    val displaySigs: sigs * FixedInt.int -> pretty
    val sigExportTree: navigation * sigs -> exportTree 
    val sigVal: sigs * int * (int->typeId) * env * lexan * location -> signatures
    val undefinedSignature: signatures
    val isUndefinedSignature: signatures -> bool

    structure Sharing:
    sig
        type sigs           = sigs
        type structSigBind  = structSigBind
        type signatures     = signatures
        type parsetree      = parsetree
        type typeParsetree  = typeParsetree
        type parseTypeVar    = parseTypeVar
        type pretty         = pretty
        type ptProperties   = ptProperties
        type env            = env
        type lexan          = lexan
        type typeId         = typeId
        type specs          = specs
    end
end;
