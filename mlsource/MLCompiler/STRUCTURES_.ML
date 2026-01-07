(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Modified D.C.J. Matthews 2001-2016, 2020, 2025

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

(*
    Title:      Module Structure and Operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

functor STRUCTURES_ (

structure LEX : LEXSIG
structure CODETREE : CODETREE
structure STRUCTVALS : STRUCTVALSIG;
structure VALUEOPS : VALUEOPSSIG;
structure EXPORTTREE: EXPORTTREESIG
structure TYPETREE : TYPETREESIG
structure PARSETREE : PARSETREESIG
structure PRETTY : PRETTY
structure COPIER: COPIERSIG
structure TYPEIDCODE: TYPEIDCODESIG
structure SIGNATURES: SIGNATURESSIG
structure DEBUGGER : DEBUGGER

structure UTILITIES :
sig
  val noDuplicates: (string * 'a * 'a -> unit) -> 
         { apply: (string * 'a -> unit) -> unit,
           enter:  string * 'a -> unit,
           lookup: string -> 'a option };

  val searchList: unit -> { apply: (string * 'a -> unit) -> unit,
                            enter:  string * 'a -> unit,
                            lookup: string -> 'a option };
  val splitString: string -> { first:string,second:string }
end;

structure UNIVERSALTABLE:
sig
  type universal = Universal.universal
  type univTable
  type 'a tag = 'a Universal.tag
  
  val univEnter:  univTable * 'a tag * string * 'a -> unit;
  val univLookup: univTable * 'a tag * string -> 'a option;
  val univFold:   univTable * (string * universal * 'a -> 'a) * 'a -> 'a;
end;

structure DEBUG: DEBUG

sharing LEX.Sharing = VALUEOPS.Sharing = TYPETREE.Sharing = PARSETREE.Sharing
    = PRETTY.Sharing = EXPORTTREE.Sharing = STRUCTVALS.Sharing = COPIER.Sharing
    = CODETREE = UNIVERSALTABLE = TYPEIDCODE.Sharing = SIGNATURES.Sharing = DEBUGGER.Sharing

) : STRUCTURESSIG =

(*****************************************************************************)
(*                  STRUCTURES functor body                                  *)
(*****************************************************************************)
struct
  open Misc; 
  open PRETTY;

  open COPIER;
  open LEX;
  open CODETREE;
  open STRUCTVALS;
  open VALUEOPS;
  open TYPETREE;
  open PARSETREE;
  open UTILITIES;
  open DEBUG;
  open UNIVERSALTABLE;
  open Universal; (* for tag record selectors *)
  open EXPORTTREE;
  open TYPEIDCODE
  open SIGNATURES
  open DEBUGGER
  
    (* Transitional bindings.  Calls to these should be replaced by pattern matching. *)
    fun sigTab        (Signatures {tab,...})        = tab
    and sigMinTypes   (Signatures {firstBoundIndex,...})            = firstBoundIndex
    and sigMaxTypes   (Signatures {firstBoundIndex, boundIds,...})  = firstBoundIndex + List.length boundIds
    and sigTypeIdMap  (Signatures {typeIdMap, ...}) = typeIdMap
    and sigBoundIds   (Signatures {boundIds, ...})  = boundIds

    fun structName (Struct {name,...}) = name
    and structAccess (Struct {access,...}) = access
    and structLocations (Struct {locations,...}) = locations
    and structSignat (Struct {signat,...}) = signat

    (* Union of the various kinds of core language declaration.  Structures are included
        because they can be declared by opening a structure with substructures. *)
    datatype coreDeclaration =
        CoreValue       of values
    |   CoreType        of typeConstrSet
    |   CoreFix         of string*fixStatus (* Include the name because it isn't part of fixStatus. *)
    |   CoreStruct      of structVals

    (* Description of the actions to perform when a structure matches a signature. *)
    datatype valueMatching =
        ValueMatch of
            { sourceValue: values, targetType: types, coercion: valueCoercions }
    |   StructureMatch of
            { sourceStructure: structVals, contentsMatch: structureMatch}
    |   TypeIdMatch of
            { sourceIdNo: int, isEquality: bool }
 
    and valueCoercions = (* The coercions that may apply to a value. *)
        NoCoercion
    |   ExceptionToValue    
    |   ConstructorToValue

    withtype structureMatch = (int * valueMatching) list

    (* "structs" is the abstract syntax for the module language. *)
    datatype structValue =

        StructureIdent of (* A structure name *)
        {
            name:       string,         (* The name *)
            valRef:     structVals option ref, (* The variable found. *)
            location:   location
        } 

    |   StructDec      of (* struct ... end *)
        {
            alist: structDec list, (* List of items in it. *)
            location: location,
            matchToResult: structureMatch ref
        }

    |   FunctorAppl    of (* Application of a functor. *)
        {
            name:   string,
            arg:    structValue,
            valRef: functors option ref,      (* The functor looked up. *)
            nameLoc: location,      (* The location of the name itself. *)
            fullLoc: location,      (* The location of the full application. *)
            argIds:  { source: typeId, dest: typeId } list ref, (* The IDs that are required in the arguments. *)
            resIds:  { source: typeId, dest: typeId } list ref, (* Generative IDs in the result. *)
            matchToArgument: structureMatch ref
        }

    |   LetDec of (* let strdec in strexp. *)
        {
            decs:     structDec list,
            body:     structValue,
            line:     location
        }

    |   SigConstraint  of                    (* Constraint of str to match sig. *)
        {
            str: structValue,  (* Structure to constrain *)
            csig: sigs, (* Constraining signature *)
            opaque: bool,   (* True if opaque, false if transparent. *)
            sigLoc: location,
            opaqueIds: { source : typeId, dest: typeId } list ref,
            matchToConstraint: structureMatch ref
       }
  
    and structDec =
        StructureDec   of (* List of structure decs *)
        {
            bindings: structBind list,
            typeIdsForDebug: typeId list ref,
            line: location
        }   

    |   CoreLang      of (* Any other decln. *)
        {
            dec:   parsetree,           (* The value *)
            vars:  coreDeclaration list ref, (* The declarations *)
            location: location
        }
 
    |   Localdec of (* Local strdec in strdec. *)
        {
            decs:     structDec list,
            body:     structDec list,
            line:     location
        }
       

  withtype structBind =
      {
        name:      string,         (* The name of the structure *)
        nameLoc:   location,
        haveSig:   bool, (* Whether we moved an explicit signature to the value. *)
        value:     structValue,        (* And its value *)
        valRef:    structVals option ref, (* The structure variable declared. *)
        line:      location
      }
  
    fun mkStructIdent (name, location) =
        StructureIdent
        {
            name   = name,
            valRef = ref NONE,
            location = location
        }
  
  
    (* For struct...end, make a signature to accept the values. *)
    fun mkStruct(alist, location) =
      StructDec
        {
          alist = alist,
          location = location,
          matchToResult = ref []
        };
  
  
    fun mkCoreLang (dec, location) =
        CoreLang
        {
            dec   = dec,
            vars  = ref [],
            location = location
        };

    fun mkFunctorAppl (name, arg, nameLoc, fullLoc) =
        FunctorAppl
        {
            name   = name,
            arg    = arg,
            valRef = ref NONE,
            nameLoc = nameLoc,
            fullLoc = fullLoc,
            argIds  = ref nil,
            resIds  = ref nil,
            matchToArgument = ref []
        };
  
    fun mkFormalArg (name, signat) =
        {
            name      = name,
            sigStruct = signat,
            valRef    = ref NONE
        }

    fun mkLocaldec (decs, body, line) =
      Localdec 
         {
           decs     = decs,
           body     = body,
           line     = line
         };

    fun mkLetdec (decs, body, line) =
      LetDec 
         {
           decs     = decs,
           body     = body,
           line     = line
         };

    fun mkSigConstraint(str, csig, opaque, sigLoc) =
        SigConstraint
        {
            str=str, csig=csig, opaque=opaque, sigLoc=sigLoc,
            opaqueIds=ref nil, matchToConstraint = ref []
        }

    fun mkStructureDec(bindings, line) =
        StructureDec { bindings = bindings, typeIdsForDebug = ref [], line = line }
 
    fun mkStructureBinding ((name, nameLoc), signat, value, fullLoc): structBind =
    let
        (* If there's an explicit signature move that to a constraint. *)
        val value =
            case signat of
                NONE => value
            |   SOME (csig, opaque, sigLoc) =>
                    mkSigConstraint(value, csig, opaque, sigLoc)
    in
        { 
            name      = name,
            nameLoc   = nameLoc,
            haveSig   = isSome signat,
            value     = value,
            valRef    = ref NONE,
            line      = fullLoc
        }
    end;

    type formalArgStruct =
      {
        name:      string,
        sigStruct: sigs,
        valRef:    structVals option ref
      } (* The structure variable. *) 

    (* Top level declarations and program. *)
    datatype topdec =
        StrDec          of structDec * typeId list ref (* Structure decs and core lang. *)
    |   FunctorDec      of functorBind list * location      (* List of functor decs. *)
    |   SignatureDec    of sigBind list * location  (* List of signature decs *)

    withtype   (* Functor binding. *)
        functorBind =
        {
            name:      string,
            nameLoc:   location,
            haveSig:   bool, (* Whether we moved an explicit signature to the value. *)
            body:      structValue,
            arg:       formalArgStruct,
            valRef:    functors option ref,    (* The functor variable declared. *)
            resIds:    { source: typeId, dest: typeId } list ref,
            line:      location,
            matchToResult: structureMatch ref,
            (* If we are debugging we need these at code-gen time. *)
            debugArgVals: values list ref,
            debugArgStructs: structVals list ref,
            debugArgTypeConstrs: typeConstrSet list ref
        } 

    and sigBind =
        {
            name:      string, (* The name of the signature *)
            nameLoc:   location,
            sigStruct: sigs,(* Its value *)
            sigRef:    signatures ref, (* The "value" of the signature. *)
            line:      location
        }

    fun mkTopDec t = StrDec(t, ref nil)
    and mkFunctorDec s = FunctorDec s
    and mkSignatureDec s = SignatureDec s;
  
    fun mkFunctorBinding (name, nameLoc, signat, body, arg, line): functorBind =
    let
        (* If there's an explicit signature move that to a constraint. *)
        val body =
            case signat of
                NONE => body
            |   SOME (csig, opaque, sigLoc) =>
                    mkSigConstraint(body, csig, opaque, sigLoc)
    in
        {
          name      = name,
          nameLoc   = nameLoc,
          haveSig   = isSome signat,
          body      = body,
          arg       = arg,
          valRef    = ref NONE,
          resIds    = ref nil,
          line      = line,
          matchToResult = ref [],
          debugArgVals = ref [],
          debugArgStructs = ref [],
          debugArgTypeConstrs = ref []
        }
    end

    and mkSignatureBinding ((name, nameLoc), sg, ln) =
        { 
          name     = name,
          nameLoc  = nameLoc,
          sigStruct = sg,
          line      = ln,
          sigRef   = ref undefinedSignature
        }

    type program = topdec list * location
    fun mkProgram tl = tl

  (* Pretty printing *)

    fun displayList ([], _, _) _ = []
    
    |   displayList ([v], _, depth) dodisplay =
            if depth <= 0
            then [PrettyString "..."]
            else [dodisplay (v, depth)]
      
    |   displayList (v::vs, separator, depth) dodisplay =
            if depth <= 0
            then [PrettyString "..."]
            else
            let
                val brk = if separator = "," orelse separator = ";" then 0 else 1
            in
                PrettyBlock (0, false, [],
                    [
                        dodisplay (v, depth),
                        PrettyBreak (brk, 0),
                        PrettyString separator
                    ]
                ) ::
                PrettyBreak (1, 0) ::
                displayList (vs, separator, depth - 1) dodisplay
            end (* displayList *) 

    fun displayStruct (str, depth: FixedInt.int) =
        if depth <= 0 (* elide further text. *)
        then PrettyString "..."

        else
        case str of
            StructureDec { bindings = structList, ...} =>
            let
                fun displayStructBind (
                        {name, haveSig, value, ...}: structBind, depth) =
                let
                    (* If we desugared this before, return it to its original form. *)
                    val (sigStruct, value) =
                        case (haveSig, value) of
                            (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                (SOME(csig, opaque, sigLoc), str)
                        |   _ => (NONE, value)
                in
                    PrettyBlock (3, false, [],
                        PrettyString name ::
                        (
                            case sigStruct of (* Signature is optional *)
                                NONE => []
                            |   SOME (sigStruct, opaque, _) =>
                                [
                                    PrettyString (if opaque then " :>" else " :"),
                                    PrettyBreak (1, 0),
                                    displaySigs (sigStruct, depth - 1)
                                ]
                        ) @
                            [
                                PrettyString " =",
                                PrettyBreak (1, 0),
                                displayStructValue (value, depth - 1)
                            ]
                    )
                end
            in
                PrettyBlock (3, false, [],
                    PrettyString "structure" ::
                    PrettyBreak (1, 0) ::
                    displayList (structList, "and", depth) displayStructBind
                )
            end

          | Localdec {decs, body, ...} =>
                PrettyBlock (3, false, [],
                    PrettyString "local" ::
                    PrettyBreak (1, 0) ::
                    displayList (decs, ";", depth - 1) displayStruct @
                    [ PrettyBreak (1, 0), PrettyString "in", PrettyBreak (1, 0)] @
                    displayList (body, ";", depth - 1) displayStruct @
                    [ PrettyBreak (1, 0), PrettyString "end" ]
                )

          | CoreLang {dec, ...} =>
              displayParsetree (dec, depth - 1)

    and displayStructValue (str, depth) =
        if depth <= 0 (* elide further text. *)
        then PrettyString "..."

        else
        case str of
            StructureIdent {name, ...} =>
                PrettyString name

          | StructDec {alist, ...} =>
                PrettyBlock (1, true, [],
                    PrettyString "struct" ::
                    PrettyBreak (1, 0) ::
                    displayList (alist, "", depth) displayStruct @
                    [ PrettyBreak (1, 0), PrettyString "end"]
                )

          | FunctorAppl {name, arg, ...} =>
                PrettyBlock (1, false, [],
                    [
                        PrettyString (name ^ "("),
                        PrettyBreak (0, 0),
                        displayStructValue (arg, depth),
                        PrettyBreak (0, 0),
                        PrettyString ")"
                    ]
                )

          | LetDec {decs, body, ...} =>
                PrettyBlock (3, false, [],
                    PrettyString "let" ::
                    PrettyBreak (1, 0) ::
                    displayList (decs, ";", depth - 1) displayStruct @
                    [ PrettyBreak (1, 0), PrettyString "in", PrettyBreak (1, 0),
                      displayStructValue (body, depth - 1) ] @
                    [ PrettyBreak (1, 0), PrettyString "end" ]
                )

          | SigConstraint{str, csig, opaque, ...} =>
                PrettyBlock (0, false, [],
                    [
                        displayStructValue (str, depth - 1),
                        PrettyString (if opaque then " :>" else " :"),
                        PrettyBreak (1, 0),
                        displaySigs (csig, depth - 1)
                    ]
                )
    
    fun displayTopDec(top, depth) =
        if depth <= 0 (* elide further text. *)
        then PrettyString "..."

        else
        case top of
            StrDec(s, _) => displayStruct(s, depth)

        |   SignatureDec (structList : sigBind list, _) =>
            let
                fun displaySigBind ({name, sigStruct, ...}: sigBind, depth) =
                    PrettyBlock (3, false, [],
                        [
                            PrettyString (name ^ " ="),
                            PrettyBreak (1, 0),
                            displaySigs (sigStruct, depth - 1)
                        ]
                    )
            in 
                PrettyBlock (3, false, [],
                    PrettyString "signature" ::
                    PrettyBreak (1, 0) ::
                    displayList (structList, "and", depth) displaySigBind
                )
            end

        |   FunctorDec (structList : functorBind list, _) =>
            let
                fun displayFunctBind (
                        {name, arg={name=argName, sigStruct=argStruct, ...}, haveSig, body, ...}, depth) =
                let
                    val (sigStruct, body) =
                        case (haveSig, body) of
                            (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                (SOME(csig, opaque, sigLoc), str)
                        |   _ => (NONE, body)
                in
                    PrettyBlock (3, false, [],
                        PrettyString (name ^ "(") ::
                        PrettyBreak (1, 0) ::
                        PrettyBlock (1, false, [],
                            (
                                if argName = "" then []
                                else [ PrettyString (argName ^ " :"), PrettyBreak (1, 2)]
                            ) @
                            [displaySigs (argStruct, depth - 1)]
                        ) ::
                        PrettyString ")" ::
                        (
                            case sigStruct of
                                NONE  => [] (* Signature is optional *)
                            |   SOME (sigStruct, opaque, _) =>
                                [
                                    PrettyString(if opaque then " :>" else " :"),
                                    PrettyBreak (1, 0),
                                    displaySigs (sigStruct, depth - 1)
                                ]
                        ) @
                        [
                            PrettyBreak (1, 0),
                            PrettyString "=",
                            PrettyBreak (1, 0),
                            displayStructValue (body, depth - 1)
                        ]
                    )
                end
            in 
                PrettyBlock (3, false, [],
                    PrettyString "functor" ::
                    PrettyBreak (1, 0) ::
                    displayList (structList, "and", depth) displayFunctBind
                )
            end
        (* End displayTopDec *)
 
    fun displayProgram ((sl, _), d) =
        PrettyBlock(0, true, [],
            displayList (sl, "", d) displayTopDec
        )

    fun structExportTree(navigation, s: structDec) =
    let
         (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => displayStruct(s, d)) ::
            exportNavigationProps navigation

        fun asParent () = structExportTree(navigation, s)
    in
        case s of
            StructureDec{ bindings = sbl, line = location, ...} =>
            let
                fun exportSB(navigation, sb as {name, nameLoc, haveSig, value, line, valRef=ref structOpt, ...}) =
                    let
                        (* If we desugared this before, return it to its original form. *)
                        val (sigStruct, value) =
                            case (haveSig, value) of
                                (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                    (SOME(csig, opaque, sigLoc), str)
                            |   _ => (NONE, value)
                        fun exportThis () = exportSB(navigation, sb)
                        (* Three groups: name, signature and structures.
                           It's all complicated because the signature
                           may not be present. *)
                        val locProps =
                            case structOpt of
                                SOME(Struct{locations, ...}) => definingLocationProps locations
                            |   _ => []
                        fun getName () =
                        let
                            val next =
                                case sigStruct of
                                    SOME _ => getSigStruct
                                |   NONE => getValue
                        in
                            getStringAsTree({parent=SOME exportThis, previous=NONE, next=SOME next}, name, nameLoc, locProps)
                        end
                        
                        and getSigStruct () =
                        let
                            val next = SOME getValue
                            val (theSig, _, _) = valOf sigStruct
                        in
                            sigExportTree({parent=SOME exportThis, previous=SOME getName, next=next}, theSig)
                        end

                        and getValue () =
                        let
                            val previous =
                                case sigStruct of
                                    NONE => getName
                                |   SOME _ => getSigStruct
                        in
                            structValueExportTree({parent=SOME exportThis, previous=SOME previous, next=NONE}, value)
                        end
                    in
                        (line, PTfirstChild getName :: exportNavigationProps navigation)
                    end

                val expChild = exportList(exportSB, SOME asParent) sbl
            in
                (location, expChild @ commonProps)
            end

        |   CoreLang {dec, ...} => (* A value parse-tree entry. *)
                getExportTree(navigation, dec)

        |   Localdec {decs, body, line, ...} =>
                (line, exportList(structExportTree, SOME asParent) (decs @ body) @ commonProps)
 
    end

    and structValueExportTree(navigation, s: structValue) =
    let
         (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => displayStructValue(s, d)) ::
            exportNavigationProps navigation

        fun asParent () = structValueExportTree(navigation, s)
    in
        case s of

            StructureIdent { valRef = ref var, location, ... } =>
            let
                val locs =
                    case var of
                        SOME(Struct{locations, ...}) => locations
                    |   NONE => []
            in
                (* Get the location properties for the identifier. *)
                (location, mapLocationProps locs @ commonProps)
            end

        |   StructDec{ location, alist, ...} =>
                (location, exportList(structExportTree, SOME asParent) alist @ commonProps)

        |   FunctorAppl { valRef, name, nameLoc, fullLoc, arg, ... } =>
            let
                val locs =
                    case ! valRef of
                        SOME(Functor { locations, ...}) => locations
                    |   NONE => []
                (* Navigate between the functor name and the argument. *)
                (* The first position is the expression, the second the type *)
                fun getFunctorName () =
                    getStringAsTree({parent=SOME asParent, previous=NONE, next=SOME getFunctorArg},
                        name, nameLoc, mapLocationProps locs)
                and getFunctorArg () =
                    structValueExportTree({parent=SOME asParent, previous=SOME getFunctorName, next=NONE}, arg)
            in
                (fullLoc, PTfirstChild getFunctorName :: commonProps)
            end

        |   LetDec {decs, body, line, ...} =>
            let (* For simplicity just merge these as a single list. *)
                datatype allEntries = Value of structValue | Dec of structDec
                fun exportEntries(navigation, Value strval) = structValueExportTree(navigation, strval)
                |   exportEntries(navigation, Dec strdec) = structExportTree(navigation, strdec)
            in
                (line, exportList(exportEntries, SOME asParent) (List.map Dec decs @ [Value body]) @ commonProps)
            end

        |   SigConstraint { str, csig, sigLoc, ... } =>
            let
                (* Navigate between the functor name and the argument. *)
                (* The first position is the expression, the second the type *)
                fun getStructure () =
                    structValueExportTree({parent=SOME asParent, previous=NONE, next=SOME getSignature}, str)
                and getSignature () =
                    sigExportTree({parent=SOME asParent, previous=SOME getStructure, next=NONE}, csig)
            in
                (sigLoc, PTfirstChild getStructure :: commonProps)
            end
 
    end
    
    fun topDecExportTree(navigation, top: topdec) =
    let
         (* Common properties for navigation and printing. *)
        val commonProps =
            PTprint(fn d => displayTopDec(top, d)) ::
            exportNavigationProps navigation

        fun asParent () = topDecExportTree(navigation, top)
    in
        case top of
            StrDec(s, _) => structExportTree(navigation, s)

        |   SignatureDec(sigs, location) =>
            let
                fun exportSB(navigation,
                        sb as {name, nameLoc, sigStruct, line, sigRef=ref(Signatures{locations, ...}), ...}) =
                    let
                        fun exportThis () = exportSB(navigation, sb)
                        fun getName () =
                            getStringAsTree({parent=SOME exportThis, previous=NONE, next=SOME getSig}, name, nameLoc, 
                                definingLocationProps locations)
                        and getSig () =
                            sigExportTree({parent=SOME exportThis, previous=SOME getName, next=NONE}, sigStruct)
                    in
                        (line, PTfirstChild getName :: exportNavigationProps navigation)
                    end
            in
                (location, exportList(exportSB, SOME asParent) sigs @ commonProps)
            end

        |   FunctorDec(fbl, location) =>
            let
                fun exportFB(navigation,
                        fb as {name, nameLoc, haveSig, arg={sigStruct=argStruct, ...}, body, line, valRef=ref optFunc, ...}) =
                    let
                        val locations = case optFunc of SOME(Functor{locations, ...}) => locations | _ => []
                        val (sigStruct, body) =
                            case (haveSig, body) of
                                (true, SigConstraint{str, csig, opaque, sigLoc, ...}) =>
                                    (SOME(csig, opaque, sigLoc), str)
                            |   _ => (NONE, body)
                        val fbProps = exportNavigationProps navigation
                        fun exportThis () = exportFB(navigation, fb)
                        (* Because the signature is optional navigation on the arg and body depends on
                           whether there's a signature. *)
                        fun getName() =
                            getStringAsTree({parent=SOME exportThis, previous=NONE, next=SOME getArg},
                                name, nameLoc, definingLocationProps locations)
                        
                        and getArg() =
                        let
                            val next =
                                if isSome sigStruct then getSig else getBody
                        in
                            sigExportTree({parent=SOME exportThis, previous=SOME getName, next=SOME next},
                                        argStruct)
                        end
            
                        and getSig() =
                            sigExportTree({parent=SOME exportThis, previous=SOME getArg, next=SOME getBody},
                                        #1(valOf sigStruct))
            
                        and getBody() =
                        let
                            val previous = if isSome sigStruct then getSig else getArg
                        in
                            structValueExportTree({parent=SOME exportThis, previous=SOME previous, next=NONE}, body)
                        end
                    in
                        (line, PTfirstChild getName :: fbProps)
                    end

                val expChild = exportList(exportFB, SOME asParent) fbl
            in
                (location, expChild @ commonProps)
            end
    end

    (* Convert a "program" into a navigable tree. *)
    fun structsExportTree (parentTree, trees: program) =
    let
        val parentTreeNav = exportNavigationProps parentTree
        (* The top level is actually a list. *)
        fun exportTree(([], location)) = (location, parentTreeNav)
        |   exportTree(topdec as (sl, location)) =
        let
            fun getEntry(this as (s :: sl), getPrevious) (): exportTree =
                topDecExportTree(
                    {
                        parent = SOME(fn () => exportTree topdec), (* Parent is this. *)
                        previous = getPrevious,
                        (* If we have a successor then that is the entry and
                           its predecessor returns here. *)
                        next =
                        case sl of
                            [] => NONE
                        |   t  => SOME(getEntry(t, SOME(getEntry(this, getPrevious))))
                    },
                    s
                    )
            |   getEntry _ () = raise Empty
        in
            (location, parentTreeNav @ [PTfirstChild(getEntry(sl, NONE))])
        end
    in
        exportTree trees
    end

    (* Puts out an error message and then prints the piece of tree. *)
    fun errorMsgNear (lex, hard, near, lno, message) : unit =
    let
        val parameters = debugParams lex
        val errorDepth = getParameter errorDepthTag parameters
    in
        reportError lex
        {
            hard = hard, location = lno, message = message,
            context = SOME(near errorDepth)
        }
    end;

    fun idAccess (TypeId { access, ...}) = access 
    fun tcName       (TypeConstrs {name,...})       = name
    fun tcTypeVars   (TypeConstrs {typeVars,...})   = typeVars
    fun tcIdentifier (TypeConstrs {identifier,...}) = identifier
    fun tcLocations  (TypeConstrs {locations, ...}) = locations
    fun tcIsAbbreviation (TypeConstrs {identifier = TypeId{idKind = TypeFn _, ...},...}) = true
    |   tcIsAbbreviation _ = false

    val tcEquality = isEquality o tcIdentifier;

    fun tcArity(TypeConstrs {identifier=TypeId{idKind=TypeFn{tyVars, ...},...}, ...}) = length tyVars
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Bound{arity, ...},...}, ...}) = arity
    |   tcArity(TypeConstrs {identifier=TypeId{idKind=Free{arity, ...},...}, ...}) = arity

    fun tsConstructors(TypeConstrSet(_, tvs)) = tvs

    (* TODO: If the item being errored is in a substructure it currently doesn't report
       the name of the substructure. *)
    (* Report an error about signature-structure matching. *)
    fun sigStructMatchMsg (lex, near, lno, structName) (doDisplay: 'a -> pretty)
                (structValue: 'a, sigValue: 'a, reason) =
        let
            val message =
                PrettyBlock(3, true, [],
                    [
                        PrettyString
                            ("Structure does not match signature" ^
                                (if structName = "" then "." else " in sub-structure " ^ structName)),
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Signature:",
                                PrettyBreak(1, 0),
                                doDisplay sigValue
                            ]),
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Structure:",
                                PrettyBreak(1, 0),
                                doDisplay structValue
                            ]),
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Reason:",
                                PrettyBreak(1, 0),
                                reason
                            ])
                    ])
        in
            errorMsgNear(lex, true, near, lno, message)
        end

    fun sigStructMissingMsg (lex, near, lno, structName) (doDisplay: 'a -> pretty) (sigValue: 'a) =
        let
            val message =
                PrettyBlock(3, true, [],
                    [
                        PrettyString
                            ("Structure does not match signature" ^
                                (if structName = "" then "." else " in sub-structure " ^ structName)),
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Signature:",
                                PrettyBreak(1, 0),
                                doDisplay sigValue
                            ]),
                        PrettyBreak(1, 0),
                        PrettyBlock(3, false, [],
                            [
                                PrettyString "Structure:",
                                PrettyBreak(1, 0),
                                PrettyString "Not present"
                            ])
                    ])
        in
            errorMsgNear(lex, true, near, lno, message)
        end

    (* Older version: prints just a string message. *)
    fun errorNear(lex, hard, near, lno, message: string) =
        errorMsgNear (lex, hard, near, lno,
            PrettyBlock (0, false, [], [PrettyString message]))

    fun errorDepth lex =
    let
        open DEBUG
        val parameters = LEX.debugParams lex
    in
        getParameter errorDepthTag parameters
    end

    (* Error message routine for lookupType and lookupStructure. *)
    fun giveError (sVal : structValue, lno : LEX.location, lex : lexan) : string -> unit =
        fn (message : string) => errorNear (lex, true, fn n => displayStructValue(sVal, n), lno, message);

    (* Turn a result from matchTypes into a pretty structure so that it
       can be included in a message. *)
    (* TODO: When reporting type messages from inside the structure we should use
       the environment from within the structure and for type within the signature the signature env. *)
    fun matchErrorReport(lex, structTypeEnv, sigTypeEnv) =
        unifyTypesErrorReport(lex, structTypeEnv, sigTypeEnv, "match")

    datatype matchTypeResult =
        MatchError of matchResult
    |   MatchSuccess of types

    (* Check that two types match.  Returns either an error result or the set
       of polymorphic variables for the source and the target. *)
    fun matchTypes (candidate, target, targMap: int -> typeId option, _) =
    let
        fun copyId(TypeId{idKind=Bound{ offset, ...}, ...}) = targMap offset
        |   copyId _ = NONE
        fun copyATypeConstr tcon = copyTypeConstr(tcon, copyId, fn x => x, fn s => s)
        fun copyTarget t = (* Leave type variables. *)
            copyType (t, fn x => x, copyATypeConstr);
        val copiedTarget = copyTarget target
        (* Do the match to a version of the candidate with copies of the
           type variables so that we can instantiate them.  We could do
           this by passing in a mapping function but the problem is that
           if we have a type variable that gets unified to another variable
           we will not map it properly if it occurs again (we call "eventual"
           and get the second tv before calling the map function so we get a
           second copy and not the first copy). *)
        val (copiedCandidate : types, _) = generalise candidate;
    in
        case unifyTypes (copiedCandidate, copiedTarget) of
            NONE => (* Succeeded. Return the unified type.  Either will do. *)
                MatchSuccess copiedTarget
        |   SOME error => MatchError error
    end;

    (* Check that a matching has succeeded, and check the value
       constructors if they are datatypes. *)
    fun checkTypeConstrs (candidSet as TypeConstrSet(candid, candidConstrs),
                          targetSet as TypeConstrSet(target, targetConstrs),
                          targTypeMap: int -> typeId option, lex, near, lno, typeEnv, structPath) =
    let
        val candidName : string = tcName candid;
        val targetName : string = tcName target;
        val tvars = List.map TypeVar (tcTypeVars target); (* either will do *)
        (* If we get an error in the datatype itself print the full datatype. *)
        val printTypeEnv = { lookupType = fn _ => NONE, lookupStruct = fn _ => NONE }
        val errorInDatatype =
            sigStructMatchMsg(lex, near, lno, structPath)(fn t => displayTypeConstrs(t, errorDepth lex, printTypeEnv))
    in
        if tcArity candid <> tcArity target
        then () (* Have already given the error message. *)
        else (* Check the type constructors themselves first. This checks
                that the sharing constraints have been satisfied. *)
        case matchTypes (mkTypeConstruction (candidName, candid, tvars, []),
                         mkTypeConstruction (targetName, target, tvars, []), 
                         targTypeMap, lex) of
                MatchError error => (* Report the error. *)
                    errorInDatatype(candidSet, targetSet, matchErrorReport(lex, typeEnv, typeEnv) error)
            |   MatchSuccess _ =>
                (* We have already checked for matching a type in the structure to a datatype in the signature.
                   In ML97 we can't rebind an identifier in a signature so each constructor for this datatype
                   must be present in the signature i.e. it can't be hidden by a constructor for another datatype.
                   So we can check the types of the constructors when we check the values.  We still need to
                   check that if this has constructors that the candidate does not have more constructors. *)
                if null targetConstrs then () (* Target is just a type: this isn't a problem. *)
                else if List.length candidConstrs <= List.length targetConstrs
                then () (* If it's less then it will be picked up later. *)
                else
                let
                    fun checkConstrs(Value{name=candidConstrName, ...}) =
                        if List.exists(fn Value{name, ...} => name=candidConstrName) targetConstrs
                        then ()
                        else errorNear(lex, true, near, lno,
                                concat["Error while matching datatype ", candidName, ": constructor ", candidConstrName,
                                        " was present in the structure but not in the signature."]);
                in
                    List.app checkConstrs candidConstrs
                end
    end

    (* Check that a candidate signature (actually the environment part of
       a structure) matches a target signature. The direction is important
       because a candidate is allowed to have more components and more
       polymorphism than the target.  As part of the matching process we
       build up a map of typeIDs in the target to type IDs in the candidate
       and that is returned as the result.
       N.B. the map function takes an argument between minTarget and maxTarget. *)
    fun matchSigs(originalCandidate, originalTarget, near, lno, lex, typeIdEnv, typeEnv)
                        :(int -> typeId) * (int * valueMatching) list =
    let
        val candidate = (* The structure. *)
        let
            val Signatures { typeIdMap, firstBoundIndex, boundIds, ... } = originalCandidate
            val _ =
                case boundIds of
                    [] => ()
                |   _ => raise InternalError "Candidate structure with non-empty bound ID list"
        in
            if isUndefinedSignature originalCandidate
            then undefinedSignature
            else replaceMap(originalCandidate, typeIdMap, firstBoundIndex, [], typeIdEnv)
        end
        
        val target = (* The signature. *)
        let
            val Signatures { typeIdMap, firstBoundIndex, boundIds, ... } = originalTarget
            fun newMap n =
            if n < firstBoundIndex then typeIdEnv n
            else List.nth(boundIds, n-firstBoundIndex)
        in
            replaceMap(originalTarget, typeIdMap, firstBoundIndex, boundIds, newMap)
        end

        local
            val minTarget = sigMinTypes target
            and maxTarget = sigMaxTypes target
            (* All the Bound type IDs in the target are in this range.  We create an array
               to contain the matched IDs from the candidate. *)
            val matchArray = Array.array(maxTarget-minTarget, NONE)
        in
            (* These two functions are used during the match process. *)
            (* When looking up a Bound ID we return NONE if it is out of the range.
               Bound IDs below the minimum are treated as global at this level and so
               only match if they are the same in the target and candidate. *)
            fun lookupType n =
                if n < minTarget then NONE else Array.sub(matchArray, n-minTarget)
            and enterType (n, id) = 
                if n < minTarget then () else Array.update(matchArray, n-minTarget, SOME id)

            (* This is the result function.  If everything is right every entry in the
               array will be SOME but if we have had an error there may be entries that
               are still NONE.  To prevent an exception we return the undefined type in
               that case. *)
            fun resultType n = getOpt(Array.sub(matchArray, n-minTarget), tcIdentifier undefConstr)
        end

        (* Match typeIDs for types. This is slightly more
           complicated than simply assigning the stamps. *)
        fun matchNames (candidate, target, structPath) : unit =
        if isUndefinedSignature candidate
        then () (* Suppress unnecessary messages. *)
        else univFold (sigTab target,
            fn (dName, dVal, ()) =>
            if tagIs typeConstrVar dVal
            then
            let (* See if there is one with the same name. *)
                val targetSet as TypeConstrSet(target, targetConstrs) = tagProject typeConstrVar dVal;
                val printTypeEnv = { lookupType = fn _ => NONE, lookupStruct = fn _ => NONE }
                fun displayType t = displayTypeConstrs(t, errorDepth lex, printTypeEnv)
                val typeError = sigStructMatchMsg(lex, near, lno, structPath) displayType
                (* Condition for a candidate type to match an eqtype. *)
                fun permitsEquality(TypeConstrs {identifier=TypeId{idKind = TypeFn{equality=ref(TypeFnEq _), ...}, ...}, ...}) = true
                |   permitsEquality(TypeConstrs {identifier=TypeId{idKind = TypeFn{equality=ref TypeFnEqNever, ...}, ...}, ...}) = false
                |   permitsEquality(TypeConstrs{identifier, ...}) = isEquality identifier
            in (* Match up the types. This does certain checks but
                  does not check sharing. Equality is checked for. *)
                case univLookup (sigTab candidate, typeConstrVar, dName) of
                    SOME (candidSet as TypeConstrSet(candid, candidConstrs)) =>
                        if not (isUndefinedTypeConstr target) (* just in case *)
                        then
                        ( 
                            (* Check for arity and equality - value constructors 
                               are checked later. If the target is a bound identifier
                               in the range it can be matched by a candidate. *)
                            case tcIdentifier target of
                                TypeId{idKind=Bound { offset, ...}, ...} => enterType (offset, tcIdentifier candid)
                            |   _ => ();
                    
                            if tcArity target <> tcArity candid
                            then typeError(candidSet, targetSet,
                                        PrettyString "Types take different numbers of type arguments.")

                            (* Check that it's a datatype before checking for eqtype. *)
                            else if not (null targetConstrs) andalso null candidConstrs
                            then typeError(candidSet, targetSet, 
                                        PrettyString "Type in structure is not a datatype")

                            else if not(tcIsAbbreviation target) andalso tcEquality target
                                    andalso not (permitsEquality candid)
                            then typeError(candidSet, targetSet, 
                                        PrettyString "Type in structure is not an equality type")
                           
                            else () 
                        )
                        else ()
                |   NONE => sigStructMissingMsg(lex, near, lno, structPath) displayType targetSet
            end
             
            else if tagIs structVar dVal
            then
            let (* and sub-structures. *)
                val target = (tagProject structVar) dVal;
                (* For each target structure: find a candidate with the 
                   same name and recursively check them. *)
            in
                case univLookup (sigTab candidate, structVar, dName) of
                   SOME candid =>
                    matchNames (structSignat candid, structSignat target, structPath ^ dName ^ ".")
                |  NONE =>
                    let
                        fun displayStructure s =
                            PrettyBlock(0, false, [],
                                [PrettyString "structure" , PrettyBreak(1, 3), PrettyString(structName s)])
                    in
                        sigStructMissingMsg(lex, near, lno, structPath) displayStructure target
                    end
            end
            else (), (* not a type or structure *)
          ()  (* default value for fold *)
        ) (* matchNames *);
      
        val () = matchNames (candidate, target, "");
       
        (* Match the values and exceptions in the signatures.
           This actually does the checking of types. *)
        fun matchVals (candidate, target, structPath): (int * valueMatching) list =
        if isUndefinedSignature candidate
        then [] (* Suppress unnecessary messages. *)
        else (* Map the identifiers first, returning the originals if they are
           not in the map. *)
        let
            local
                fun matchStructures(dName, dVal, matches) =
                if tagIs typeConstrVar dVal
                then (* Types *)
                let (* For each type in the target ... *)
                    val target = tagProject typeConstrVar dVal
                in
                    (* Find a candidate with the same name. *)
                    case univLookup (sigTab candidate, typeConstrVar, dName) of
                        SOME candid =>
                            let
                                (* We don't actually check the value constructors here, just load them if they
                                   match.  Because of the no-redefinition rule value constructors in the signature
                                   must also be present in the value environment so we check them there. *)
                                fun matchConstructor(source as Value{typeOf, ...}, Value{access=Formal addr, ...}, matches) =
                                        (addr,
                                            ValueMatch { sourceValue = source, coercion = NoCoercion, targetType = typeOf }) :: matches
                                |   matchConstructor(_, _, matches) = matches
                            in
                                (* Now check that the types match. *)
                                checkTypeConstrs(candid, target, lookupType, lex, near, lno, typeEnv, structPath);
                                ListPair.foldl matchConstructor matches (tsConstructors candid, tsConstructors target)
                            end
                    |   NONE => matches (* If the lookup failed ignore
                                     the error - we've already reported it in matchNames *)
                end
       
                else if tagIs structVar dVal
                then
                let (* and each sub-structure *)
                    val target = tagProject structVar dVal
                in
                    (* For each target structure: find a candidate with the same
                       name and recursively check them. *)
                    case univLookup (sigTab candidate, structVar, dName) of
                        SOME candid =>
                        let
                            val substructMatch = matchVals (structSignat candid, structSignat target, structPath ^ dName ^ ".")
                        in
                            (* Produce the match instructions for the sub-structure. We only
                               include Formal entries here.  It's possible that there might be
                               Global entries in some circumstances. *)
                            case target of
                                Struct{access=Formal addr, ...} =>
                                    (addr,
                                        StructureMatch{ sourceStructure=candid,
                                                        contentsMatch = substructMatch}) :: matches
                            |   _ => matches
                        end
                    |   NONE => matches (* Ignore the error - we've already reported it in matchNames *)
                end
                else matches;
            in
                val structureMatches = univFold(sigTab target, matchStructures, [])
            end

            fun displayValue(value as Value {name, locations, typeOf, ...}) =
            let
                val decLocation =
                    case List.find (fn DeclaredAt _ => true | _ => false) locations of
                        SOME(DeclaredAt loc) => [ContextLocation loc]
                    |   _ => []
                val valName = PrettyBlock(0, false, decLocation, [PrettyString name])
                fun dispVal(kind, typeof) =
                    PrettyBlock(0, false, [],
                        [
                            PrettyString kind,
                            PrettyBreak(1, 3),
                            valName,
                            PrettyBreak(0, 0),
                            PrettyString(":"),
                            PrettyBreak(1, 0),
                            display (typeof, errorDepth lex, typeEnv)
                        ])
            in
                case value of
                    Value{class=Constructor _, ...} =>
                        (* When displaying the constructor show the function type.  We may have rebound
                           the constructor in the candidate structure so that it creates a different datatype. *)
                        dispVal("constructor", typeOf)
                |   Value{class=Exception, ...} =>
                        PrettyBlock(0, false, [],
                                PrettyString "exception" ::
                                PrettyBreak(1, 3) ::
                                valName ::
                            (
                                case getFnArgType typeOf of
                                   NONE => []
                                |  SOME excType =>
                                    [
                                        PrettyBreak (1, 1), PrettyString "of",
                                        PrettyBreak (1, 3), display (excType, errorDepth lex, typeEnv) ]
                            ))
                |   _ => dispVal("val", typeOf)    
            end

            local
                fun matchLocalValues(dName, dVal, matches) =
                if tagIs valueVar dVal
                then
                let
                    val destVal as Value { typeOf=destTypeOf, class=destClass, access=destAccess, ...} = tagProject valueVar dVal
                in
                    case univLookup (sigTab candidate, valueVar, dName) of
                        NONE => (sigStructMissingMsg(lex, near, lno, structPath) displayValue destVal; matches)
                    |   SOME (candid as Value { typeOf=sourceTypeOf, class=sourceClass, ...}) =>
                        let
                            (* If the target is a constructor or exception the candidate must be
                               similar.  If the candidate is a constructor or exception this will
                               match a value but requires some coercion. *)
                            datatype matchType = IsOK of valueCoercions | IsWrong of pretty
                            val matchKind =
                                case (destClass, sourceClass) of
                                    (Constructor _, Constructor _) => IsOK NoCoercion
                                |   (Constructor _, _) => IsWrong(PrettyString "Value is not a constructor")
                                |   (Exception, Exception) => IsOK NoCoercion
                                |   (Exception, _) => IsWrong(PrettyString "Value is not an exception")
                                |   (_, Exception) => IsOK ExceptionToValue
                                |   (_, Constructor _) => IsOK ConstructorToValue
                                |   _ => IsOK NoCoercion
                        in
                            case matchKind of
                                IsWrong error =>
                                    (
                                        sigStructMatchMsg(lex, near, lno, structPath)
                                            displayValue (candid, destVal, error);
                                        matches
                                    )
                            |   IsOK coercion =>
                                case matchTypes (sourceTypeOf, destTypeOf, lookupType, lex) of
                                    MatchSuccess instanceType =>
                                    (
                                        (* If it matches an entry in the signature it counts as being exported
                                           and therefore referenced. *)
                                        case candid of
                                            Value { references=SOME{exportedRef, ...}, ...} => exportedRef := true
                                        |   _ => ();
                                        (* Add the instance type to the instance types. *)
                                        case candid of
                                            Value{ instanceTypes=SOME instanceRef, ...} =>
                                                (* This has to be generalised before it is added here.
                                                   Unlike normal unification when matching to a signature
                                                   any polymorphic variables in the target will not have
                                                   been generalised. *)
                                                instanceRef := #1(generalise instanceType) :: !instanceRef
                                        |   _ => ();
                                        case destAccess of
                                            Formal destAddr =>
                                                (destAddr,
                                                    ValueMatch { sourceValue = candid, coercion = coercion,
                                                                 targetType = instanceType }) :: matches
                                        |   _ => matches (* This could be global. *)
                                    )
                                |   MatchError error =>
                                    (
                                        sigStructMatchMsg(lex, near, lno, structPath)
                                            displayValue (candid, destVal, matchErrorReport(lex, typeEnv, typeEnv) error);
                                        matches
                                    )
                        end
                end
                else matches
            in
                val matchedValues = univFold(sigTab target, matchLocalValues, structureMatches)
            end
        in
            matchedValues
        end (* matchVals *);
        
        val doMatch = matchVals (candidate, target, ""); (* Do the match. *)
    in
        (resultType, doMatch) (* Return the function to look up the results. *)
    end (* matchSigs *);


    val makeEnv = fn x => let val Env e = makeEnv x in e end;

    (* Any values in the signature are counted as exported.  This case applies if
       there was no result signature because if there was a signature the values
       would have been given their references and types in the signature matching. *)
    fun markValsAsExported resSig =
    let
        fun refVals(_, dVal, ()) =
        if tagIs valueVar dVal
        then
        let
            val valu = tagProject valueVar dVal
        in
            case valu of
                Value {references=SOME{exportedRef, ...}, ...} =>
                    exportedRef := true
            |   _ => ();
            (* If we have exported the value without a signature we use
               the most general type and discard any, possibly less general,
               references. *)
            case valu of
                Value{ typeOf, instanceTypes=SOME instanceRef, ...} =>
                    instanceRef := [#1(generalise typeOf)]
            |   _ => ()
        end
        else ()
    in
        univFold(sigTab resSig, refVals, ())
    end

    (* Construct a set of actions for matching a structure to itself.  This is only
       really needed to ensure that type IDs are passed through correctly but we
       don't actually do them here yet. *)
    fun makeCopyActions signat : (int * valueMatching) list =
    let
        fun matchEntry(_, dVal, matches) =
        if tagIs structVar dVal
        then
        let
            val str = tagProject structVar dVal
        in
            case str of
                Struct{access=Formal addr, ...} =>
                    (addr, StructureMatch{
                        sourceStructure=str, contentsMatch = makeCopyActions(structSignat str)}) :: matches
            |   _ => matches
        end
        else if tagIs valueVar dVal
        then
        let
            val v = tagProject valueVar dVal
        in
            case v of
                Value { access=Formal addr, typeOf, ...} =>
                    (addr, ValueMatch { sourceValue = v, coercion = NoCoercion, targetType = typeOf }) :: matches
            |   _ => matches
        end
        else if tagIs typeConstrVar dVal
        then
        let
            fun matchConstructor(v as Value{access=Formal addr, typeOf, ...}, matches) =
                    (addr, ValueMatch { sourceValue = v, coercion = NoCoercion, targetType = typeOf }) :: matches
            |   matchConstructor(_, matches) = matches
        in
            List.foldl matchConstructor matches (tsConstructors(tagProject typeConstrVar dVal))
        end
        else matches
    in
        univFold(sigTab signat, matchEntry, [])
    end

    (* Actions to copy the type Ids into the result signature. *)
    local
        fun matchTypeIds(_, []) = []
        |   matchTypeIds(n, (typeId as TypeId{ access = Formal addr, ...}) :: rest) =
                (addr, TypeIdMatch{ sourceIdNo=n, isEquality=isEquality typeId }) ::
                    matchTypeIds(n+1, rest)
        |   matchTypeIds(_, _) = raise InternalError "matchTypeIds: Not Formal"
    in
        fun makeMatchTypeIds destIds = matchTypeIds(0, destIds)
    end


  (* Second pass - identify names with values and type-check *)
 
      (* Process structure-returning expressions i.e. structure names,
         struct..end values and functor applications. *)
    fun structValue(str: structValue, newTypeId: (int*bool*bool*bool*typeIdDescription)->typeId, currentTypeCount,
                    newTypeIdEnv: unit -> int->typeId, Env env, lex, lno, structPath) =
    let
        val typeEnv =
        {
            lookupType =
                fn s => case #lookupType env s of NONE => NONE | SOME t => SOME(t, SOME(newTypeIdEnv())),
            lookupStruct =
                fn s => case #lookupStruct env s of NONE => NONE | SOME t => SOME(t, SOME(newTypeIdEnv()))
        }
    in
        case str of
            StructureIdent {name, valRef, location} =>
            let (* Look up the name and save the value. *)
                val result =
                    lookupStructure ("Structure", {lookupStruct = #lookupStruct env}, 
                               name, giveError (str, location, lex))
                val () = valRef := result
            in
                case result of
                    SOME(Struct{signat, ...}) => signat
                |   NONE => undefinedSignature
            end

        | FunctorAppl {name, arg, valRef, nameLoc, fullLoc, argIds, resIds, matchToArgument,  ... } =>
          (* The result structure must be copied to generate a new
             environment. This will make new types so that different
             applications of the functor yield different types. There may be 
             dependencies between the parameters and result signatures so
             copying may have to take that into account. *)
            (
                case #lookupFunct env name of
                    NONE =>
                    (
                        giveError (str, nameLoc, lex) ("Functor (" ^ name ^ ") has not been declared");
                        undefinedSignature
                    )
                |   SOME functr =>
                    let
                        val Functor { arg = Struct{signat=formalArgSig, ...}, result=functorResSig, ...} = functr
                        val () = valRef := SOME functr (* save it till later. *)
                        (* Apply a functor to an argument.  The result structure contains a mixture of IDs
                           from the argument structure and generative IDs from the result structure.
                           There are two parts to this process.
                           1.  We have to match the actual argument structure to the formal argument to
                               ensure that IDs are in the right place for the functor.
                           2.  We have to take the actual argument structure and the functor result structure
                               and produce a combination of this as a structure. *)
                        (* IDs:
                           argIDs: A list of pairs of IDs as Selected/Local/Global values and Formal values.
                              This contains the IDs that must be passed into the functor.
                           resIDs: A list of pairs of IDs as Local values and Formal values.  The Local value
                              is the location where a new generative ID is stored and the Formal offset is the
                              offset within the run-time vector returned by the signature where the source ID
                              for the generative ID is to be found. *)

                        (* This provides information about the arguments. *)
                        (* Get the actual parameter value. *)
                        val actualArgSig =
                            structValue(arg, newTypeId, currentTypeCount, newTypeIdEnv, Env env, lex, fullLoc, structPath);

                        local
                            (* Check that the actual arguments match formal arguments,
                               and instantiate the variables. *)
                            val (matchResults, matchActions) =
                                matchSigs (actualArgSig, formalArgSig,
                                    fn n => displayStructValue(str, n), fullLoc, lex, newTypeIdEnv(), typeEnv);
                            (* Record the code to match to this and include instructions to load the typeIDs. *)
                            val () = matchToArgument := matchActions @ makeMatchTypeIds(sigBoundIds formalArgSig)
                        in
                            val matchResults = matchResults
                        end
                    
                        (* Create a list of the type IDs that the argument must supply. *)
                        local
                            val maxT = sigMaxTypes formalArgSig and minT = sigMinTypes formalArgSig
                            val results = List.tabulate(maxT-minT, fn n => matchResults(n+minT))
                            val args = ListPair.mapEq(fn(s, d) => { source = s, dest = d })(results, sigBoundIds formalArgSig)
                        in
                            val () = argIds := args; (* Save for code-generation. *)
                        end

                        (* Now create the generative typeIDs.  These are IDs that are in the bound ID range of
                           the result signature.  Any type IDs inherited from the argument will have type ID
                           values less than sigMinTypes functorResSig. *)
                        local
                            fun makeNewTypeId(
                                oldId as TypeId{idKind=Bound{isDatatype, arity, ...}, description = { name=oldName, ...}, ...}) =
                            let
                                val description =
                                    { location = fullLoc, name = oldName, description = "Created from applying functor " ^ name }
                                val newId = newTypeId(arity, false, isEquality oldId, isDatatype, description)
                            in
                                { source = oldId, dest = newId }
                            end
                            |   makeNewTypeId _ = raise InternalError "Not Bound"
                            (* The resIds list tells the code-generator where to find the source of each
                               ID in the result structure and where to save the generative ID. *)
                            val sdList = List.map makeNewTypeId (sigBoundIds functorResSig)
                            val _ = resIds := sdList (* Save for code-generation. *)
                        in
                            (* This vector contains the resulting type IDs.  They all have Local access. *)
                            val resVector = Vector.fromList(List.map(fn { dest, ...} => dest) sdList)
                        end
                
                        (* Construct a result signature.  This will contain all the
                           IDs created here i.e. IDs in the argument and generative IDs at the start and then
                           all the values and structures returned from the functor.
                           When we come to code-generate we need to
                           1. Use loadOpaqueIds over the resIDs to create the opaque IDs.
                           2. Basically, do the same as StructDec to match to the result signature.
                           We don't need to do anything about type IDs from the argument.  Processing
                           the argument will ensure that type IDs created in the argument are declared
                           as Locals and if we pass localIDs to matchStructure we will load IDs from
                           both the argument and generative IDs created by loadOpaqueIds. *)
                        val minCopy = Int.min(sigMinTypes formalArgSig, sigMinTypes functorResSig)
                        val idEnv = newTypeIdEnv()
                        fun getCombinedTypeId n =
                            if n < minCopy then idEnv n
                            else if n >= sigMinTypes functorResSig
                            then Vector.sub(resVector, n - sigMinTypes functorResSig)
                            else if n >= sigMinTypes formalArgSig
                            then matchResults n
                            else sigTypeIdMap formalArgSig n

                        val resSig =
                            let
                                val Signatures { name, tab, locations, ... } = functorResSig
                            in
                                makeSignature(name, tab, currentTypeCount(), locations,
                                    composeMaps(sigTypeIdMap functorResSig, getCombinedTypeId), [])
                            end
                    in
                        resSig
                    end
            )
                           
        | StructDec {alist, location, matchToResult, ...} =>
          let
            (* Collection of declarations packaged into a structure
                or a collection of signatures. *)
            (* Some of the environment, the types and the value constructors,
               is generated during the first pass. Get the environment from
               the structure. *)
            val structTable = makeSignatureTable ()
            val structEnv = makeEnv structTable

            val makeLocalTypeId = newTypeId
            val makeLocalTypeIdEnv = newTypeIdEnv

            val newEnv =
            {
                enterType = #enterType structEnv,
                enterVal = #enterVal structEnv,
                enterStruct = #enterStruct structEnv,
                enterSig = fn _ => raise InternalError "Signature in Struct End",
                enterFunct = fn _ => raise InternalError "Functor in Struct End",
                lookupVal = lookupDefault (#lookupVal structEnv) (#lookupVal env),
                lookupType = lookupDefault (#lookupType structEnv) (#lookupType env),
                lookupStruct = lookupDefault (#lookupStruct structEnv) (#lookupStruct env),
                lookupSig    = #lookupSig   env, (* Global *)
                lookupFunct  = #lookupFunct env, (* Global *)
                lookupFix    = #lookupFix   env,
                (* Fixity declarations are dealt with in the parsing process.  They
                   are only processed again in this pass in order to get declarations
                   in the right order. *)
                enterFix     = fn _ => (),
                allValNames  = fn () => (#allValNames structEnv () @ #allValNames env ()),
                allStructNames = fn () => (#allStructNames structEnv () @ #allStructNames env ())
            }

            (* process body of structure *)
            val () =
                pass2Struct (alist, makeLocalTypeId, currentTypeCount, makeLocalTypeIdEnv, Env newEnv, lex, lno, structPath);

            (* We need to make a signature for the result in the form that can be used if there is no
               explicit signature, for example if this is used as the result of a functor.  That means
               creating Formal values for all the values and structures.  These Formal entries define
               the position in the run-time vector where each of the values and sub-structures are
               located.  We don't include typeIDs in this.  Any typeIDs that need to be included in
               the run-time vector are added by the functor declaration code. *)
            val finalTable = makeSignatureTable();
            val finalEnv = makeEnv finalTable

            (* Create the result signature and also build the match structure to match to it. *)
            fun enterItem(dName, dVal, (addrs, matches)) =
                if tagIs typeConstrVar dVal
                then
                let
                    val tConstr as TypeConstrSet(typConstr, valConstrs) = tagProject typeConstrVar dVal
                in
                    if null valConstrs
                    then (#enterType finalEnv (dName, tConstr); (addrs, matches))
                    else
                    let
                        (* If this is a datatype constructor convert the value constructors.
                           The "no redefinition" rule for signatures doesn't apply to a structure
                           so the signature we create here could have some constructors that have been
                           hidden by later declarations.  We still need the whole value environment in
                           case of datatype replication. *)
                        fun convertConstructor(
                                valVal as Value{class, typeOf, locations, references, name, instanceTypes, ...},
                                (otherConstrs, (addrs, matches))) =
                        let
                            val formalValue =
                                Value{class=class, name=name, typeOf=typeOf, access=Formal addrs,
                                      locations=locations, references=references, instanceTypes=instanceTypes}
                        in
                            (formalValue :: otherConstrs,
                                (addrs + 1,
                                    (addrs,
                                        ValueMatch { sourceValue = valVal, coercion = NoCoercion, targetType=typeOf}) :: matches))
                        end
                        val (newConstrs, newAddrMatch) = List.foldl convertConstructor ([], (addrs, matches)) valConstrs
                        fun makeTypeConstructor (name, typeVars, uid, locations) =
                            TypeConstrs
                            {
                                name       = name,
                                typeVars   = typeVars,
                                identifier = uid,
                                locations = locations
                            }
                        val newConstructor =
                            makeTypeConstructor(
                                tcName typConstr, tcTypeVars typConstr, tcIdentifier typConstr, tcLocations typConstr)
                    in
                        #enterType finalEnv (dName, TypeConstrSet(newConstructor, List.rev newConstrs));
                        newAddrMatch
                    end
                end

                else if tagIs structVar dVal
                then 
                let
                    val strVal = tagProject structVar dVal
                    val locations = structLocations strVal
                    val strSig = structSignat strVal
                    val matchSubStructure = makeCopyActions strSig
                in
                    #enterStruct finalEnv (dName, makeFormalStruct (dName, strSig, addrs, locations));
                    (addrs + 1,
                        (addrs, StructureMatch
                            { sourceStructure=strVal, contentsMatch = matchSubStructure}) :: matches)
                end

                else if tagIs valueVar dVal
                then
                let
                    val valVal = tagProject valueVar dVal
                in
                    (* If this is a type-dependent function such as PolyML.print we must put in the
                       original type-dependent version not the version which will have frozen
                       its type as 'a. *)
                    case valVal of
                        value as Value{access = Overloaded _, ...} =>
                        (
                            #enterVal finalEnv (dName, value);
                            (addrs, matches)
                        )
                    |   Value{class, typeOf, locations, references, instanceTypes, ...} =>
                        let
                            val formalValue =
                                Value{class=class, name=dName, typeOf=typeOf, access=Formal addrs,
                                      locations=locations, references=references,
                                      instanceTypes=instanceTypes}
                        in
                            #enterVal finalEnv (dName, formalValue);
                            (addrs + 1,
                                (addrs,
                                    ValueMatch { sourceValue = valVal, coercion = NoCoercion, targetType=typeOf}) :: matches)
                        end
                end
                else (addrs, matches)
            
            val () = matchToResult := #2(univFold(structTable, enterItem, (0, [])))
            val locations = [DeclaredAt location, SequenceNo (newBindingId lex)]
            val resSig =
                makeSignature("", finalTable, currentTypeCount(), locations, newTypeIdEnv(), [])
          in
            resSig
          end
                
        | LetDec {decs, body = localStr, line, ...} =>
          let (* let strdec in strexp end *)
            val newEnv = makeEnv (makeSignatureTable());
                   
            (* The environment for the local declarations. *)
            val localEnv =
             { 
              lookupVal     =
                lookupDefault (#lookupVal    newEnv) (#lookupVal    env),
              lookupType    =
                lookupDefault (#lookupType   newEnv) (#lookupType   env),
              lookupFix     = #lookupFix    newEnv,
              lookupStruct  =
                lookupDefault (#lookupStruct newEnv) (#lookupStruct env),
              lookupSig     = #lookupSig    env,
              lookupFunct   = #lookupFunct  env,  (* Sigs and functs are global *)
              enterVal      = #enterVal     newEnv,
              enterType     = #enterType    newEnv,
              (* Fixity declarations are dealt with in the parsing process.  At
                 this stage we simply need to make sure that local declarations
                 aren't entered into the global environment. *)
              enterFix      = fn _ => (),
              enterStruct   = #enterStruct  newEnv,
              enterSig      = #enterSig     newEnv,
              enterFunct    = #enterFunct   newEnv,
              allValNames  = fn () => (#allValNames newEnv () @ #allValNames env ()),
              allStructNames = fn () => (#allStructNames newEnv () @ #allStructNames env ())
             };
             
            (* Process the local declarations. *)
            val () =
              pass2Struct (decs, newTypeId, currentTypeCount, newTypeIdEnv, Env localEnv, lex, line, structPath);
                   
          in
            (* There should just be one entry in the "body" list. *)
            structValue(localStr, newTypeId, currentTypeCount, newTypeIdEnv, Env localEnv, lex, line, structPath)
          end
          
        | SigConstraint { str, csig, opaque, sigLoc, opaqueIds, matchToConstraint, ... } =>
            let
                val bodyIds = ref []
                val startTypes = currentTypeCount()
                val startTypeEnv = newTypeIdEnv()
                fun sconstraintMakeTypeId (arity, isVar, eq, isdt, desc) =
                let
                    val newId = newTypeId(arity, isVar, eq, isdt, desc)
                in
                    bodyIds := newId :: ! bodyIds;
                    newId
                end
                fun sconstraintTypeIdEnv () n =
                    if n < startTypes then startTypeEnv n
                    else valOf(
                        List.find(fn TypeId{idKind=Bound{offset, ...}, ...} => offset = n | _ => raise Subscript) (!bodyIds))

                val resSig =
                    structValue(str, sconstraintMakeTypeId, currentTypeCount, sconstraintTypeIdEnv, Env env, lex, lno, structPath);
                (* Get the explicit signature. *)
                val explicitSig = sigVal(csig, startTypes, startTypeEnv, Env env, lex, sigLoc)

                val minExplicitSig = sigMinTypes explicitSig and maxExplicitSig = sigMaxTypes explicitSig                 

                (* Match the signature.  This instantiates entries in typeMap. *)
                val (matchResults, matchActions) =
                    matchSigs (resSig, explicitSig, fn n => displayStructValue(str, n), sigLoc, lex, startTypeEnv, typeEnv);
                val () = matchToConstraint := matchActions

                val rSig =
                    if opaque
                    then
                    let
                        (* Construct new IDs for the generic IDs.  For each ID in the signature
                           we need to make a new Local ID. *)
                        fun makeNewId(oldId as TypeId{idKind=Bound{ isDatatype, arity, ...}, description = { name, ...}, ...}) =
                        let
                            val description =
                                { location = sigLoc, name = name, description = "Created from opaque signature" }
                        in
                            newTypeId(arity, false, isEquality oldId, isDatatype, description)
                        end
                        |   makeNewId _ = raise InternalError "Not Bound"

                        val sources = List.tabulate(maxExplicitSig-minExplicitSig, fn n => matchResults(n+minExplicitSig))
                        val dests = List.map makeNewId (sigBoundIds explicitSig)
                        (* Add the matching IDs to a list.  When we create the code for
                           the structure we need to create new run-time ID values using
                           the original equality code and a new ref to hold the printer. *)
                        val () = opaqueIds := ListPair.mapEq (fn (s, d) => { source=s, dest=d }) (sources, dests)
                        (* Create new IDs for all the bound IDs in the signature. *)
                        val v = Vector.fromList dests

                        (* And copy it to put in the names from the structure. *)
                        val currentEnv = newTypeIdEnv()
                        fun oldMap n =
                            if n < minExplicitSig
                            then currentEnv n
                            else Vector.sub (v, n - minExplicitSig)
                        val Signatures{locations, name, tab, typeIdMap, ...} = explicitSig
                    in
                        makeSignature(name, tab, currentTypeCount(),
                                        locations, composeMaps(typeIdMap, oldMap), [])
                    end
                    else (* Transparent: Use the IDs from the structure. *)
                    let
                        val newIdEnv = newTypeIdEnv ()
                        fun matchedIds n = if n < sigMinTypes explicitSig then newIdEnv n else matchResults n
                        val Signatures{locations, name, tab, typeIdMap, ...} = explicitSig
                    in
                        (* The result signature.  This needs to be able to enumerate the type IDs
                           including those we've added. *)
                        makeSignature(name, tab, currentTypeCount(),
                                        locations, composeMaps(typeIdMap, matchedIds), [])
                    end
            in
                rSig
            end
    end (* structValue *)

    and pass2Struct 
        (strs     : structDec list,
         makeLocalTypeId : (int * bool * bool * bool * typeIdDescription) -> typeId,
         makeCurrentTypeCount: unit -> int,
         makeTypeIdEnv: unit -> int -> typeId,
         Env env  : env,
         lex,
         lno      : LEX.location,
         structPath: string
         ) : unit =
    let
        fun pass2StructureDec (str : structDec, structList : structBind list, typeIdsForDebug) : unit =
        let (* Declaration of structures. *)
            (* The declarations must be made in parallel. i.e.
                structure A = struct ... end and B = A; binds B to the A
                in the PREVIOUS environment, not the A being declared. *)
            val sEnv =  (* The new names. *)
                noDuplicates 
                    (fn(name, _, _) => 
                      errorNear (lex, true, fn n => displayStruct(str, n), lno, 
                                 "Structure " ^ name ^ 
                                 " has already been bound in this declaration")
                    )

            (* Any new type Ids we create need to be added onto a list in case we need
               them for the debugger. *)
            fun captureIds args =
                let val id = makeLocalTypeId args in typeIdsForDebug := id :: ! typeIdsForDebug; id end

            (* Put the new names into this environment. *)
            fun pass2StructureBind ({name, value, valRef, line, nameLoc, ...}) : unit=
            let (* Each element in the list is a structure binding. *)
                val resSig =
                    structValue(value, captureIds, makeCurrentTypeCount, makeTypeIdEnv,
                                Env env, lex, line, structPath ^ name ^ ".");
                (* Any values in the signature are counted as exported. *)
                val () = markValsAsExported resSig;
                (* Now make a local structure variable using this signature. *)
                val locations = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]
                val var = makeLocalStruct (name, resSig, locations)
            in
                #enter sEnv (name, var);
                valRef := SOME var
            end
        in 
            List.app pass2StructureBind structList;
            (* Put them into the enclosing env. *)
            #apply sEnv (#enterStruct env)
        end; (* pass2StructureDec *)

        fun pass2Localdec (decs : structDec list, body : structDec list) : unit =
        let
         val newEnv = makeEnv (makeSignatureTable());
             
         (* The environment for the local declarations. *)
         val localEnv =
           {
            lookupVal     =
              lookupDefault (#lookupVal    newEnv) (#lookupVal    env),
            lookupType    =
              lookupDefault (#lookupType   newEnv) (#lookupType   env),
            lookupFix     = #lookupFix    newEnv,
            lookupStruct  =
              lookupDefault (#lookupStruct newEnv) (#lookupStruct env),
            lookupSig     = #lookupSig    env,
            lookupFunct   = #lookupFunct  env,
            enterVal      = #enterVal     newEnv,
            enterType     = #enterType    newEnv,
            enterFix      = fn _ => (),
            enterStruct   = #enterStruct  newEnv,
            enterSig      = #enterSig     newEnv,
            enterFunct    = #enterFunct   newEnv,
            allValNames  = fn () => (#allValNames newEnv () @ #allValNames env ()),
            allStructNames = fn () => (#allStructNames newEnv () @ #allStructNames env ())
           };
        
        (* Process the local declarations. *)
        val () =
            pass2Struct (decs, makeLocalTypeId, makeCurrentTypeCount, makeTypeIdEnv, Env localEnv, lex, lno, structPath);
             
        (* This is the environment used for the body of the declaration.
           Declarations are added both to the local environment and to
           the surrounding scope. *)
           
        (* Look-ups come from the local env *)
        val bodyEnv = 
          {
           lookupVal     = #lookupVal    localEnv,
           lookupType    = #lookupType   localEnv,
           lookupFix     = #lookupFix    localEnv,
           lookupStruct  = #lookupStruct localEnv,
           lookupSig     = #lookupSig    localEnv,
           lookupFunct   = #lookupFunct  localEnv,
           enterVal      =
             fn pair =>
               (
                #enterVal newEnv pair;
                #enterVal env    pair
               ),
           enterType     =
             fn pair =>
               (
                #enterType newEnv pair;
                #enterType env    pair
               ),
           enterFix      = #enterFix     localEnv,
           enterStruct   =
             fn pair =>
               (
                #enterStruct newEnv pair;
                #enterStruct env    pair
               ),
           enterSig      =
             fn pair =>
               (
                #enterSig newEnv pair;
                #enterSig env    pair
               ),
           enterFunct    = #enterFunct   localEnv,
           allValNames   = #allValNames  localEnv,
           allStructNames = #allStructNames localEnv
          };
      in 
        (* Now the body. *)
        pass2Struct (body, makeLocalTypeId, makeCurrentTypeCount, makeTypeIdEnv, Env bodyEnv, lex, lno, structPath)
      end; (* pass2Localdec *)
      
      fun pass2Singleton (dec : parsetree, vars) : unit =
      let (* Single declaration - may declare several names. *)
        (* As well as entering the declarations we must keep a list
            of the value and exception declarations. *)
         val newEnv = 
           {
             lookupVal     = #lookupVal    env,
             lookupType    = #lookupType   env,
             lookupFix     = #lookupFix    env,
             lookupStruct  = #lookupStruct env,
             lookupSig     = #lookupSig    env,
             lookupFunct   = #lookupFunct  env,
             (* Must add the entries onto the end in case a declaration
                with the same name is made. e.g.
                   local ... in val a=1; val a=2 end. *)
             enterVal      =
               fn (pair as (_,v)) =>
                 (
                   #enterVal env pair;
                   vars := !vars @ [CoreValue v]
                 ),
             enterType     =
               fn (pair as (_,t)) =>
                 (
                   #enterType env pair;
                   vars := !vars @ [CoreType t]
                 ),
             enterFix      =
               fn pair =>
                 (
                   #enterFix env pair;
                   vars := !vars @ [CoreFix pair]
                 ),
             (* This will only be used if we do `open A' where A
                contains sub-structures. *)
             enterStruct   =
               fn (pair as (_,v)) =>
                 (
                   #enterStruct env pair;
                   vars := !vars @ [CoreStruct v]
                 ), 
             enterSig      = #enterSig     env,
             enterFunct    = #enterFunct   env,
             allValNames   = #allValNames  env,
             allStructNames = #allStructNames env
           };

            (* Create a new type ID for each new datatype.  Add the structure path to the
               name. *)
            fun makeId (eq, isdt, (args, EmptyType), { location, name, description }) =
                    makeLocalTypeId(List.length args, true, eq, isdt,
                        { location = location, name = structPath ^ name, description = description })
            |   makeId (_, _, (typeVars, decType), { location, name, description }) =
                    makeTypeFunction(typeVars, decType,
                        { location = location, name = structPath ^ name, description = description })

            (* Process the body and discard the type. *)
            val _ : types = pass2 (dec, makeId, Env newEnv, lex, fn _ => false);
       in
         ()
       end (* pass2Singleton *)

        fun pass2Dec (str as StructureDec { bindings, typeIdsForDebug, ... }) =
                pass2StructureDec (str, bindings, typeIdsForDebug)

        |   pass2Dec(Localdec {decs, body, ...}) =
                pass2Localdec (decs, body)
        
        |   pass2Dec(CoreLang {dec, vars, ...}) =
                pass2Singleton (dec, vars)
    in        
        List.app pass2Dec strs (* Process all the top level entries. *)
    end (* pass2Struct *)


    fun pass2Structs ((strs, _): program, lex : lexan, Env globals : env) : unit =
    let
        (* Create a local environment to capture declarations.
           We don't want to add them to the global environment at this point. *)
        val newValEnv   = UTILITIES.searchList()
        and newTypeEnv  = UTILITIES.searchList()
        and newStrEnv   = UTILITIES.searchList()
        and newSigEnv   = UTILITIES.searchList()
        and newFuncEnv  = UTILITIES.searchList()

        val lookupVal =
            lookupDefault (#lookup newValEnv)  (#lookupVal globals)
        and lookupType =
            lookupDefault (#lookup newTypeEnv) (#lookupType globals)
        and lookupStruct =
            lookupDefault (#lookup newStrEnv)  (#lookupStruct globals)
        and lookupSig =
            lookupDefault (#lookup newSigEnv)  (#lookupSig globals)
        and lookupFunct =
            lookupDefault (#lookup newFuncEnv) (#lookupFunct globals)

        fun allValNames () =
        let
            val v = ref []
            val () = #apply newValEnv (fn (s, _) => v := s :: ! v)
        in
            !v @ #allValNames globals ()
        end

        fun allStructNames () =
        let
            val v = ref []
            val () = #apply newStrEnv (fn (s, _) => v := s :: ! v)
        in
            !v @ #allStructNames globals ()
        end

        val env = 
        {
            lookupVal     = lookupVal,
            lookupType    = lookupType,
            lookupFix     = #lookupFix globals,
            lookupStruct  = lookupStruct,
            lookupSig     = lookupSig,
            lookupFunct   = lookupFunct,
            enterVal      = #enter newValEnv,
            enterType     = #enter newTypeEnv,
            enterFix      = fn _ => (), (* ?? Already entered by the parser. *)
            enterStruct   = #enter newStrEnv,
            enterSig      = #enter newSigEnv,
            enterFunct    = #enter newFuncEnv,
            allValNames   = allValNames,
            allStructNames = allStructNames
        };
        (* Create the free identifiers.  These are initially Bound but are replaced
           by Free after the code has been executed and we have the values for the
           printer and equality functions.  They are only actually created in
           strdecs but functor or signature topdecs in the same program could
           refer to them. *)
        local
            val typeIds = ref []
        in
            fun topLevelId(arity, isVar, eq, isdt, description) =
            let
                val idNumber = topLevelIdNumber()
                val newId =
                    (if isVar then makeBoundIdWithEqUpdate else makeBoundId)
                        (arity, Local{addr = ref ~1, level = ref baseLevel}, idNumber, eq, isdt, description)
            in
                typeIds := newId :: ! typeIds;
                newId
            end

            and topLevelIdNumber() = List.length(!typeIds)

            and makeTopLevelIdEnv() =
                (* When we process a topdec we create a top-level ID environment which
                   matches any ID numbers we've already created in this "program" to the
                   actual ID.  Generally this will be empty. *)
                let
                    val typeVec = Vector.fromList(List.rev(!typeIds))
                in
                    fn n => Vector.sub(typeVec, n)
                end
        end

        (* We have to check that a type does not contain free variables and turn them into
           unique monotypes if they exist.  This is a bit messy.  We have to allow subsequent
           structure declarations to freeze the types (there's an example on p90 of the
           Definition) but we can't functors to get access to unfrozen free variables because
           that can break the type system.  *)
        fun checkValueForFreeTypeVariables(name: string, Value{typeOf, ...}) =
            checkForFreeTypeVariables(name, typeOf, lex, codeForUniqueId)

        (* Find all the values in the structure. *)
        fun checkStructSigForFreeTypeVariables(name: string, s: signatures) =
        let
            fun checkEntry(dName: string, dVal: universal, ()) =
                if tagIs structVar dVal
                then checkStructSigForFreeTypeVariables(name ^ dName ^ ".",
                        structSignat((tagProject structVar) dVal))
                else if tagIs valueVar dVal
                then checkValueForFreeTypeVariables(name ^ dName, (tagProject valueVar) dVal)
                else ()
        in
            univFold(sigTab s, checkEntry, ())
        end

        fun checkVariables (newValEnv, newStrEnv) =
        (
            #apply newValEnv
                (fn (s: string, v: values) => checkValueForFreeTypeVariables(s, v));
            #apply newStrEnv (
                fn (n: string, s: structVals) =>
                    checkStructSigForFreeTypeVariables(n^".", structSignat s))
        )
            
        fun pass2TopDec ([], envs) = List.app checkVariables envs

        |   pass2TopDec (StrDec(str, typeIds)::rest, envs) =
            let
                (* Remember the top-level Ids created in this strdec. *)
                fun makeId(arity, isVar, eq, isdt, desc) =
                let
                    val newId = topLevelId(arity, isVar, eq, isdt, desc)
                in
                    typeIds := newId :: ! typeIds;
                    newId
                end
            in
                (* strdec: structure or core-language topdec. *)
                pass2Struct([str], makeId, topLevelIdNumber, makeTopLevelIdEnv, Env env, lex, location lex, "");

                pass2TopDec(rest, if errorOccurred lex then [] else (newValEnv, newStrEnv) :: envs)
            end
   
        |   pass2TopDec((topdec as FunctorDec (structList : functorBind list, lno)) :: rest, envs) =
            let
                val () = List.app checkVariables envs (* Check previous variables. *)

                (* There is a restriction that the same name may not be bound twice.
                   As with other bindings functor bindings happen in parallel.
                   DCJM 6/1/00. *)
                val sEnv =  (* The new names. *)
                  noDuplicates 
                    (fn (name, _, _) =>
                        errorNear(lex, true, fn n => displayTopDec(topdec, n), lno,
                            "Functor " ^ name ^ " has already been bound in this declaration")
                    );

                val startTopLevelIDs = topLevelIdNumber()
                and topLevelEnv = makeTopLevelIdEnv()

                (* Put the new names into this environment. *)
                fun pass2FunctorBind
                    ({name, nameLoc,
                     arg = {name = argName, sigStruct = argSig, valRef = argVal},
                     body, valRef, resIds, line, matchToResult,
                     debugArgVals, debugArgStructs, debugArgTypeConstrs, ...}: functorBind) =
                let
                    (* When we apply a functor we share type IDs with the argument if they
                       have an ID less than sigMinTypes for the result signature and treat
                       other IDs as generative.  If we don't have an explicit result
                       signature or if we have a transparent signature the type IDs in the
                       result are those returned from the body.  To keep the argument IDs
                       separate from newly created IDs we start creating local IDs with
                       offsets after the args. *)
                    val typeStamps = ref startTopLevelIDs;
                    val localStamps = ref []

                    val argumentSignature =
                        sigVal (argSig, startTopLevelIDs, topLevelEnv, Env env, lex, line)

                    (* TODO: The location here is the location of the argument id if there is one.
                       nameLoc is the location of the name of the functor. *)
                    val locations = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]
                    val resArg = makeLocalStruct (argName, argumentSignature, locations)

                    (* sigVal will have numbered the bound IDs to start at startTopLevelIDs.  We
                       need to enter these bound IDs into the local environment but as Selected entries. *)
                    local
                        fun mkId(TypeId{idKind=Bound{ arity, eqType, isDatatype, offset, ...},
                                        description={ location, name, description}, access = Formal addr, ...}) =
                            TypeId{idKind=Bound { arity = arity, offset = offset, eqType = eqType, isDatatype = isDatatype },
                                    description =
                                    {
                                        location=location,
                                        (* Add the structure name to the argument type IDs. *)
                                        name=if argName = "" then name else argName^"."^name,
                                        description=description
                                    },
                                    access = makeSelected(addr, resArg)}
                        |   mkId _ = raise InternalError "mkId: Not Bound or not Formal"                            
                    in
                        (* argIDVector is part of the environment now. *)
                        val argIDVector = Vector.fromList(List.map mkId (sigBoundIds argumentSignature))
                        val () = typeStamps := !typeStamps + List.length(sigBoundIds argumentSignature) 
                    end

                    val startBodyIDs = ! typeStamps; (* After the arguments. *)

                    local
                        (* We also have to apply the above map to the signature.  Structures may not
                           have Formal entries for their type IDs so we must map them to the
                           Selected items.  Actually, there's a nasty sort of circularity here;
                           we'd like the Selected entries to use structArg as the base but we
                           can't create it until we have its signature...which uses structArg. *)
                        val argSigWithSelected =
                        let
                            val Signatures { tab, name, locations, typeIdMap, ...} = argumentSignature
                            fun mapToSelected n =
                                if n < startTopLevelIDs then topLevelEnv n
                                else Vector.sub(argIDVector, n-startTopLevelIDs)
                        in
                            makeSignature(name, tab, startBodyIDs, locations,
                                composeMaps(typeIdMap, mapToSelected), [])
                        end
                    in
                        val argEnv = makeEnv (makeSignatureTable()); (* Local name space. *)

                        (* We may either have a single named structure in which case that is the argument or
                           effectively a sig...end block in which case we have to open a dummy structure. *)
                        val () = 
                            if argName <> ""
                            then (* Named structure. *)
                            let
                                val structArg =
                                    Struct { name = argName, signat = argSigWithSelected, access = structAccess resArg,
                                             locations=structLocations resArg }
                            in
                                debugArgStructs := [structArg];
                                #enterStruct argEnv (argName, structArg)
                            end
                            else (* Open the dummy argument. Similar to "open" in treestruct. *)
                                COPIER.openSignature 
                                (argSigWithSelected,
                                {
                                  enterType   =
                                    fn (s,v) => (debugArgTypeConstrs := v :: ! debugArgTypeConstrs; #enterType argEnv (s, v)),
                                  enterStruct =
                                    fn (name, strVal) =>
                                    let
                                        val argStruct = makeSelectedStruct (strVal, resArg, [])
                                    in
                                        debugArgStructs := argStruct :: ! debugArgStructs;
                                        #enterStruct argEnv (name, argStruct)
                                    end,
                                  enterVal    =
                                    fn (name, value) =>
                                    let
                                        val argVal = mkSelectedVar (value, resArg, [])
                                    in
                                        debugArgVals := argVal :: ! debugArgVals;
                                        #enterVal argEnv (name, argVal)
                                    end
                                },
                                "")
                    end
             
                    val () = argVal := SOME resArg
             
                    (* Now process the body of the functor using the environment of
                           the arguments to the functor and the global environment. *)
                    val envWithArgs = 
                    {
                        lookupVal     =
                          lookupDefault (#lookupVal    argEnv) (#lookupVal    env),
                        lookupType    =
                          lookupDefault (#lookupType   argEnv) (#lookupType   env),
                        lookupFix     = #lookupFix    env,
                        lookupStruct  =
                          lookupDefault (#lookupStruct argEnv) (#lookupStruct env),
                        lookupSig     = #lookupSig    env,
                        lookupFunct   = #lookupFunct  env,
                        enterVal      = #enterVal     env,
                        enterType     = #enterType    env,
                        enterFix      = fn _ => (),
                        enterStruct   = #enterStruct  env,
                        enterSig      = #enterSig     env,
                        enterFunct    = #enterFunct   env,
                        allValNames  = fn () => (#allValNames argEnv () @ #allValNames env ()),
                        allStructNames = fn () => (#allStructNames argEnv () @ #allStructNames env ())
                    };

                    local
                        (* Create local IDs for any datatypes declared in the body or any generative
                           functor applications. *)
                        fun newTypeId(arity, isVar, eq, isdt, desc) =
                        let
                            val n = !typeStamps
                            val () = typeStamps := n + 1;
                            val newId =
                                (if isVar then makeBoundIdWithEqUpdate else makeBoundId)
                                    (arity, Local{addr = ref ~1, level = ref baseLevel}, n, eq, isdt, desc)
                        in
                            localStamps := newId :: !localStamps;
                            newId
                        end

                        fun typeIdEnv () =
                        let
                            val localIds = Vector.fromList(List.rev(! localStamps))
                        in
                            fn n =>
                                if n < startTopLevelIDs
                                then topLevelEnv n
                                else if n < startBodyIDs
                                then Vector.sub(argIDVector, n-sigMinTypes argumentSignature)
                                else Vector.sub(localIds, n-startBodyIDs)
                        end
                    in
                        val resSig =
                            structValue(body, newTypeId, fn () => !typeStamps, typeIdEnv,
                                        Env envWithArgs, lex, line, "")                        
                        val () =
                            if errorOccurred lex then ()
                            else checkStructSigForFreeTypeVariables(name^"().", resSig)
                        (* This counts as a reference. *)
                        val () = markValsAsExported resSig
                    end;

                    local
                        val startRunTimeOffsets = getNextRuntimeOffset resSig

                        fun convertId(n, id as TypeId{idKind=Bound { offset, isDatatype, arity, ...}, description, ...}) =
                                (* Either inherited from the argument or a new type ID. *)
                                makeBoundId (arity, Formal(n + startRunTimeOffsets), offset, isEquality id, isDatatype, description)
                        |   convertId (_, id) = id (* Free or TypeFunction. *)

                        val localVector = Vector.fromList(List.rev(!localStamps))
                        val bodyVec = Vector.mapi convertId localVector

                        val Signatures { name, tab, typeIdMap, locations, ...} = resSig
                        (* These local IDs are included in the bound ID range for the result
                           signature.  Since they were created in the functor new instances
                           will be generated when the functor is applied. *)
                        val newBoundIds = Vector.foldr (op ::) [] bodyVec

                        (* Record the ID map for code-generation. *)
                        val () =
                            resIds :=
                                Vector.foldri(fn (n, b, r) => { source=b, dest=Vector.sub(bodyVec, n)} :: r) [] localVector

                        fun resTypeMap n =
                            if n < startTopLevelIDs
                            then topLevelEnv n
                            else if n < startBodyIDs
                            then Vector.sub(argIDVector, n-sigMinTypes argumentSignature)
                            else Vector.sub(bodyVec, n-startBodyIDs)
                    in
                        val functorSig =
                            makeSignature(name, tab, startBodyIDs,
                                locations, composeMaps(typeIdMap, resTypeMap), newBoundIds)
                        val () = matchToResult := makeCopyActions functorSig @ makeMatchTypeIds newBoundIds
                    end

                     (* Now make a local functor variable and put it in the
                        name space. Because functors can only be declared at
                        the top level the only way it can be used is if we have 
                        functor F(..) = ... functor G() = ..F.. with no semicolon
                        between them. They will then be taken as a single
                        declaration and F will be picked up as a local. *)
                      (* Set the size of the type map. *)
                    val locations = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]
                    val var = makeFunctor (name, resArg, functorSig, makeLocal (), locations)
                in
                    #enter sEnv (name, var);
                    valRef := SOME var
                end
            in
                (* Each element in the list is a functor binding. *)
                List.app pass2FunctorBind structList;
                (* Put them into the enclosing env. *)
                #apply sEnv (#enterFunct env);
                pass2TopDec(rest, [])       
            end (* FunctorDec *)

        |   pass2TopDec((topdec as SignatureDec (structList : sigBind list, lno)) :: rest, envs) =
            let
                val () = List.app checkVariables envs (* Check previous variables. *)

                (* There is a restriction that the same name may not be bound twice.
                   As with other bindings functor bindings happen in parallel.
                   DCJM 6/1/00. *)
                val sEnv =  (* The new names. *)
                    noDuplicates 
                    (fn (name, _, _) => 
                      errorNear (lex, true, fn n => displayTopDec(topdec, n), lno, 
                                 "Signature " ^ name ^ " has already been bound in this declaration")
                    );

                val startTopLevelIDs = topLevelIdNumber()
                and topLevelEnv = makeTopLevelIdEnv()

                fun pass2SignatureBind ({name, nameLoc, sigStruct, line, sigRef, ...}) =
                let (* Each element in the list is a signature binding. *)
                    val Signatures { tab, typeIdMap, firstBoundIndex, boundIds, ...} =
                       sigVal (sigStruct, startTopLevelIDs, topLevelEnv, Env env, lex, line)
                    val locations = [DeclaredAt nameLoc, SequenceNo (newBindingId lex)]
                    val namedSig = (* Put in the signature name. *)
                        makeSignature(name, tab, firstBoundIndex, locations, typeIdMap, boundIds)
                in
                    sigRef := namedSig; (* Remember for pass4. *)
                    #enter sEnv (name, namedSig)
                end
            in
                List.app pass2SignatureBind structList;
                (* Put them into the enclosing env. *)
                #apply sEnv (#enterSig env) ;
                pass2TopDec(rest, [])
            end
    in 
        pass2TopDec(strs, []);
        (* Mark any exported values as referenced. *)
        #apply newValEnv
            (fn (s: string, valu: values) =>
                (
                    (* If we have exported the value we need to mark it as a
                       reference.  But if the identifier has been rebound we
                       only want to mark the last reference.  Looking the
                       identifier up will return only the last reference. *)
                    case #lookup newValEnv s of
                        SOME(Value { references=SOME{exportedRef, ...}, ...}) =>
                            exportedRef := true
                    |   _ => ();
                    (* Since it's been exported the instance type is the most general type.
                       We can discard any other instance type info since it cannot be
                       more general. *)
                    case valu of
                        Value{ typeOf, instanceTypes=SOME instanceRef, ...} =>
                            instanceRef := [#1(generalise typeOf)]
                    |   _ => ()
                )
            )
    end (*pass2Structs *);


  (*                            *
   *     Code-generation phase. *
   *                            *)

  (* Generate code from the expressions and arrange to return the results
      so that "pass4" can find them. *)
    fun gencodeStructs ((strs, _), lex) =
    let
        (* Before code-generation perform an extra pass through the tree to remove
           unnecessary polymorphism.  The type-checking computes a most general
           type for a value, typically a function, but it is frequently used in
           situations where a less general type would suffice. *)
        local
            fun leastGenStructDec(StructureDec { bindings, ... }) =
                    (* Declarations are independent so can be processed in order. *)
                    List.app (leastGenStructValue o #value) bindings

            |   leastGenStructDec(CoreLang{dec, ...}) = setLeastGeneralTypes(dec, lex)

            |   leastGenStructDec(Localdec{decs, body, ...}) =
                (
                    (* Process the body in reverse order then the declaration in reverse. *)
                    List.foldr (fn (d, ()) => leastGenStructDec d) () body;
                    List.foldr (fn (d, ()) => leastGenStructDec d) () decs
                )

            and leastGenStructValue(StructureIdent _) = ()

            |   leastGenStructValue(StructDec {alist, ...}) =
                    (* Declarations in reverse order. *)
                    List.foldr (fn (d, ()) => leastGenStructDec d) () alist

            |   leastGenStructValue(FunctorAppl {arg, ...}) = leastGenStructValue arg

            |   leastGenStructValue(LetDec {decs, body, ...}) =
                (
                    (* First the body then the declarations in reverse. *)
                    leastGenStructValue body;
                    List.foldr (fn (d, ()) => leastGenStructDec d) () decs
                )

            |   leastGenStructValue(SigConstraint {str, ...}) = leastGenStructValue str

            fun leastGenTopDec(StrDec(aStruct, _)) = leastGenStructDec aStruct
            |   leastGenTopDec(FunctorDec(fbinds, _)) = List.app(fn{body, ...} => leastGenStructValue body) fbinds
            |   leastGenTopDec(SignatureDec _) = ()
        in
            val () = (* These are independent so can be processed in order. *)
                List.app leastGenTopDec strs
        end
    
    (* Apply a function which returns a pair of codelists to a list of structs.
       This now threads the debugging environment through the functions so
       the name is no longer really appropriate.  DCJM 23/2/01. *)
    fun mapPair
        (_: 'a * debuggerStatus -> {code: codeBinding list, debug: debuggerStatus})
        [] debug =
          {
            code = [],
            debug = debug
          }
          
      | mapPair f (h::t) debug =
    let
      (* Process the list in order. In the case of a declaration sequence
         later entries in the list may refer to earlier ones. *)
        val this = f (h, debug);
        val rest = mapPair f t (#debug this);
     in  (* Return the combined code. *)
       { 
         code = #code this @ #code rest,
         debug = #debug rest
       }
     end;

    fun applyMatchActions (code : codetree, actions, sourceIds, mkAddr, level) =
    let
        (* Generate a new structure which will match the given signature.
           A structure is represented by a vector of entries, and its
           signature is a map which gives the offset in the vector of 
           each value. When we match a signature the candidate structure
           will in general not have its entries in the same positions as
           the target. We have to construct a new structure from it with
           the entries in the correct positions. In most cases the optimiser
           will simplify this code considerably so there is no harm in using
           a general mechanism.  Nevertheless, we check for the case when
           we are building a structure which is a direct copy of the original
           and use the original code if possible. *)
        fun matchSubStructure (code: codetree, actions: structureMatch): codetree * bool =
        let
            val decs = multipleUses (code, fn () => mkAddr 1, level)
            (* First sort by the address in the destination vector.  This previously
               used Misc.quickSort but that results in a lot of memory allocation for
               the partially sorted lists.  Since we should have exactly N items
               the range checking in "update" and the "valOf" provide additional
               checking that all the items are present. *)
            val a = Array.array(List.length actions, NONE)
            val () = List.app(fn (i, action) => Array.update(a, i, SOME action)) actions
            val sortedActions = Array.foldri (fn (n, a, l) => (n, valOf a) :: l) [] a

            fun applyAction ((destAddr, StructureMatch { sourceStructure, contentsMatch }), (otherCode, allSame)) =
                let
                    val access = structAccess sourceStructure;
                    (* Since these have come from a signature we might expect all
                       the entries to be "formal". However if the structure is
                       global the entries in the signature may be global, and if
                       the structure is in a "struct .. end" it may be local. *)
                    val (code, equalDest) =
                        case access of
                            Formal sourceAddr => (mkInd (sourceAddr, #load decs level), sourceAddr=destAddr)
                        |   _ => (codeStruct (sourceStructure, level), false)
                    val (resCode, isSame) = matchSubStructure (code, contentsMatch: structureMatch)
                in
                    (resCode::otherCode, allSame andalso equalDest andalso isSame)
                end

            |   applyAction
                ((destAddr, ValueMatch { sourceValue as Value{typeOf=sourceTypeOf, name, ...}, coercion, targetType }),
                 (otherCode, allSame)) =
                let
                    (* Set up a new type variable environment in case this needs to create type
                       values to match a polymorphic source value to a monomorphic context. *)
                    val typeVarMap = TypeVarMap.defaultTypeVarMap(mkAddr, level)
                    (* If the entry is from a signature select from the code.  Apply any coercion from
                       constructors to values. *)
                    fun loadCode localLevel =
                        case sourceValue of
                            Value{access=Formal svAddr, ...} =>
                            (
                                case coercion of
                                    NoCoercion => mkInd (svAddr, #load decs localLevel)

                                |   ExceptionToValue =>
                                    let
                                        fun loadEx l = mkInd (svAddr, #load decs l)
                                    in
                                        case getFnArgType sourceTypeOf of
                                            NONE => mkTuple [loadEx localLevel, mkStr name,
                                                     CodeZero, codeLocation nullLocation]
                                        |   SOME _ =>
                                            let
                                                val nLevel = newLevel level
                                            in
                                                mkProc 
                                                    (mkTuple[loadEx nLevel, mkStr name,
                                                        mkLoadArgument 0, codeLocation nullLocation],
                                                    1, "", getClosure nLevel, 0)
                                            end
                                    end

                                |   ConstructorToValue =>
                                        (* Extract the injection function/nullary value. *)
                                        ValueConstructor.extractInjection(mkInd (svAddr, #load decs localLevel))
                           )

                        | _ =>
                            (
                                case coercion of
                                    NoCoercion =>
                                        codeVal (sourceValue, localLevel, typeVarMap, [], lex, location nullLex)
                                |   ExceptionToValue =>
                                        codeExFunction(sourceValue, localLevel, typeVarMap, [], lex, location nullLex)
                                |   ConstructorToValue =>
                                        mkInd(1, codeVal (sourceValue, localLevel, typeVarMap, [], lex, location nullLex))
                            )

                    local
                        val (copiedCandidate, sourceVars) = generalise sourceTypeOf
                        val sourceVars =
                            List.filter (fn {equality, ...} => not justForEqualityTypes orelse equality) sourceVars
                        val () = 
                            case unifyTypes(copiedCandidate, targetType) of
                                NONE => ()
                            |   SOME report => (print(name ^ ":\n"); PolyML.print report; raise InternalError "unifyTypes failed in pass 3")
                        val filterTypeVars = List.filter (fn tv => not justForEqualityTypes orelse tvEquality tv)
                        val destVars = filterTypeVars (getPolyTypeVars(targetType, fn _ => NONE))

                        (* If we have the same polymorphic variables in the source
                           and destination we don't need to apply a coercion.
                           N.B. We may have the same number of polymorphic variables
                           but still have to apply it if we have, for example,
                           fun f x => x   matching  val f: 'a list -> 'a list. *)
                        fun equalEntry({value=source, ...}, destTv) =
                            case eventual source of
                                TypeVar sourceTv => sameTv(sourceTv, destTv)
                            |   _ => false
                    in
                        val (polyCode, justCopy) =
                            if ListPair.allEq equalEntry (sourceVars, destVars)
                            then
                                (loadCode(level) (* Nothing to do. *),
                                     (* We're just copying if this is the same address. *)
                                     case sourceValue of
                                        Value{access=Formal sourceAddr, ...} => destAddr=sourceAddr
                                     |  _ => false)

                            else if null destVars (* Destination is monomorphic. *)
                            then (applyToInstance(sourceVars, level, typeVarMap, loadCode), false)

                            else
                            let
                                open TypeVarMap
                                val destPolymorphism = List.length destVars
                                val localLevel = newLevel level
                                val argAddrs =
                                    List.tabulate(destPolymorphism, fn n => fn l => mkLoadParam(n, l, localLevel))
                                val argMap = ListPair.zipEq(destVars, argAddrs)
                                val addrs = ref 0
                                fun mkAddrs n = ! addrs before (addrs := !addrs+n)
                                val newTypeVarMap = extendTypeVarMap(argMap, mkAddrs, localLevel, typeVarMap)
                                (* Apply the source to the parameters provided by the destination.  In almost
                                   all cases we will be removing polymorphism here but it is possible to
                                   add polymorphism through type definitions of the form type 'a t = int. *)
                                val application =
                                    applyToInstance(sourceVars, localLevel, newTypeVarMap, loadCode)
                            in
                                (mkProc(
                                    mkEnv(getCachedTypeValues newTypeVarMap, application),
                                    destPolymorphism, name ^ "(P)", getClosure localLevel, !addrs), false)
                            end
                    end
                in
                    (mkEnv(TypeVarMap.getCachedTypeValues typeVarMap, polyCode) :: otherCode,
                     (* We can use the original structure if nothing else has changed, the offset in
                        the destination structure is the same as the offset in the source and
                        we don't have any coercion. *)
                     allSame andalso justCopy andalso (case coercion of NoCoercion => true | _ => false))
                end

            |   applyAction ((_, TypeIdMatch { sourceIdNo, isEquality }), (otherCode, _)) =
                     (* Get the corresponding source ID. *)
                    (codeAccess(sourceIds(sourceIdNo, isEquality), level) :: otherCode, false)

            val (codeList, isAllEq) = List.foldr applyAction ([], true) sortedActions
        in
            if isAllEq then (code, true) else (mkEnv (#dec decs, mkTuple codeList), false)
        end
    in
        #1 (matchSubStructure (code, actions))
    end (* applyMatchActions *)

    fun vaLocal (Local x) = x | vaLocal _ = raise InternalError "not local"

    (* If we are declaring a structure with an opaque signature we need to create
       the run-time IDs for newly generated IDs. *)
    fun loadOpaqueIds (opaqueIds, mkAddr, level) =
    let
        fun decId { dest as TypeId{idKind=dKind, ...}, source } =
        let
            val { addr=idAddr, level=idLevel } = vaLocal(idAccess dest)
            val addr = mkAddr 1;
            val () = idAddr := addr and () = idLevel := level
            val isDatatype = case dKind of Bound{isDatatype, ...} => isDatatype | _ => false
            val idCode =
                codeGenerativeId{source=source, isEq=isEquality dest, isDatatype=isDatatype, mkAddr=mkAddr, level=level}
        in
            mkDec(addr, idCode)
        end
    in
        List.map decId opaqueIds
    end

    (* Code-generate a structure value. *)
    fun structureCode (str, strName, debugEnv, mkAddr, level: level):
                { code: codeBinding list, load: codetree } =
    case str of
        FunctorAppl {arg, valRef = ref functs, argIds=ref argIds, resIds=ref resIds,
                     matchToArgument=ref matchToArgument, ...} =>
        let
            val {code = argCodeSource, load = argLoadSource, ...} =
                structureCode (arg, strName, debugEnv, mkAddr, level)
            (* Match the actual argument to the required arguments. *)
            fun getMatchedId(n, isEq) =
                case #source(List.nth (argIds, n)) of
                    id as TypeId{idKind=TypeFn _, ...} => (* Have to generate a function here. *)
                        Global(codeGenerativeId{source=id, isEq=isEq, isDatatype=false(*??*), mkAddr=mkAddr, level=level})
                |   id => idAccess id

            val argCode = applyMatchActions(argLoadSource, matchToArgument, getMatchedId, mkAddr, level)

            (* To produce the generative type IDs we need to save the result vector returned by the
               functor application and then generate the new type IDs from the IDs in it.  To make valid
               source IDs we have to turn the Formal entries in the signature into Selected entries. *)
            val resAddr = mkAddr 1
            local 
                val dummyResStruct = makeLocalStruct("", undefinedSignature, []) (* Dummy structure. *)
                val resl = vaLocal (structAccess dummyResStruct);
                val () = #addr  resl := resAddr; 
                val () = #level resl := level
                fun mkSelected {
                        source = TypeId{idKind, access = Formal addr, description}, dest } =
                    { source = TypeId{idKind=idKind, access = makeSelected(addr, dummyResStruct), description = description },
                      dest = dest }
                |   mkSelected _ = raise InternalError "makeSelected: Not Bound or not Formal"
                val resultIds = List.map mkSelected resIds
            in
                val loadIds = loadOpaqueIds(resultIds, mkAddr, level)
            end
            
            val functorCode =
                case functs of
                    SOME(Functor{access=functorAccess, ...}) => codeAccess (functorAccess, level)
                |   NONE => raise InternalError "FunctorAppl: undefined"
        in
            (* Evaluate the functor. *)
            {
                code =
                    argCodeSource @
                    (mkDec(resAddr, mkEval (functorCode, [argCode])) ::
                     loadIds),
                load = mkLoadLocal resAddr
            }
        end

    |   StructureIdent {valRef = ref v, ...} => { code = [], load = codeStruct (valOf v, level) }

    |   LetDec {decs, body = localStr, ...} =>
        let (* let strdec in strexp end *)
            (* Generate the declarations but throw away the loads. *)
            val typeVarMap = TypeVarMap.defaultTypeVarMap(mkAddr, level)
            (* TODO: Get the debug environment correct here. *)
            fun processBody(decs, _, debugEnv, _, _, _) = (decs, debugEnv)
            val (code, debug) =
                codeStrdecs(strName, decs, debugEnv, mkAddr, level, typeVarMap, [], processBody)
            val {code = bodyCode, load = bodyLoad } =
                structureCode (localStr, strName, debug, mkAddr, level)
        in
            {
                code = TypeVarMap.getCachedTypeValues typeVarMap @ code @ bodyCode,
                load = bodyLoad
            }
        end

      | StructDec {alist, matchToResult=ref matchToResult, ...} =>
        let
            val typeVarMap = TypeVarMap.defaultTypeVarMap(mkAddr, level)
            fun processBody(decs, _, debugEnv, _, _, _) = (decs: codeBinding list, debugEnv)
            val (coded, _(*debugEnv*)) = codeStrdecs(strName, alist, debugEnv, mkAddr, level, typeVarMap, [], processBody)
            (* We match to the dummy signature here.  If there is a signature outside
               we will match again.  This results in double copying but that should
               all be sorted out by the optimiser. *)
            val loads = List.rev(List.foldl(fn (s, l) => codeLoadStrdecs(s, level) @ l) [] alist)
        in 
          (* The result is a block containing the declarations and
             code to load the results. *)
            {
                code = TypeVarMap.getCachedTypeValues typeVarMap @ coded,
                load = applyMatchActions (mkTuple loads, matchToResult, fn _ => raise Subscript, mkAddr, level)
            }
        end

    |   SigConstraint { str, opaqueIds=ref opaqueIds, matchToConstraint = ref matchToConstraint,... } =>
        let
            val {code = strCode, load = strLoad, ...} = structureCode (str, strName, debugEnv, mkAddr, level)
            val tempDecs = multipleUses (strLoad, fn () => mkAddr 1, level);
            val ids = loadOpaqueIds(opaqueIds, mkAddr, level)
        in
            {
                code = strCode @ #dec tempDecs @ ids,
                load = applyMatchActions (#load tempDecs level, matchToConstraint, fn _ => raise Subscript, mkAddr, level)
            }
        end
        (* structureCode *)

    (* We need to generate code for the declaration and then code to load
       the results into a tuple. *)
    and codeStrdecs (strName, [], debugEnv, mkAddr, level, typeVarMap, leadingDecs, processBody) =
            processBody(leadingDecs: codeBinding list, strName, debugEnv, mkAddr, level, typeVarMap) (* Do the continuation. *)
        
    |   codeStrdecs (strName, (StructureDec { bindings = structList, typeIdsForDebug = ref debugIds, ... }) :: sTail,
                     debugEnv, mkAddr, level, _(*typeVarMap*), leadingDecs, processBody) =
        let
            fun codeStructureBind ({name, value, valRef, ...}: structBind, debug) =
            let
                val structureVal = valOf(! valRef)
                val sName = strName ^ name ^ "."
                val {code = strCode, load = strLoad, ...} = structureCode (value, sName, debug, mkAddr, level)
                val addr = mkAddr 1
                val var  = vaLocal (structAccess structureVal)
                val () = #addr var  := addr; 
                val () = #level var := level;
                val (debugDecs, newDebug) = makeStructDebugEntries([structureVal], debugEnv, level, lex, mkAddr)
            in (* Get the code and save the result in the variable. *)
                {
                    code = strCode @ [mkDec (addr, strLoad)] @ debugDecs : codeBinding list,
                    debug = newDebug
                }
            end
            
            val { code: codeBinding list, debug = strDebug } = (* Code-generate each declaration. *)
                mapPair codeStructureBind structList debugEnv

            val (debugIdDecs, idDebug) = makeTypeIdDebugEntries(debugIds, strDebug, level, lex, mkAddr)

            (* A structure binding may introduce new type IDs either directly or by way of
               opaque signatures or functor application.  Ideally we'd add these using something like
               markTypeConstructors but for now just start a new environment. *)
            (* TODO: Check this.  It looks as though TypeVarMap.getCachedTypeValues newTypeVarMap
               always returns the empty list. *)
            val newTypeVarMap = TypeVarMap.defaultTypeVarMap(mkAddr, level)
            val (codeRest, debugRest) =
                codeStrdecs (strName, sTail, idDebug, mkAddr, level, newTypeVarMap, [], processBody)
        in
            (leadingDecs @ code @ debugIdDecs @ TypeVarMap.getCachedTypeValues newTypeVarMap @ codeRest, debugRest)
        end

    |   codeStrdecs (strName, (Localdec {decs, body, ...}) :: sTail,
                     debugEnv, mkAddr, level, typeVarMap, leadingDecs, processBody) =
        let
            fun processTail(previousDecs, newStrName, newDebugEnv, newMkAddr, newLevel, newTypeVarMap) =
            let
                (* TODO: Get the debug environment right here. *)
            in
                codeStrdecs (newStrName, sTail, newDebugEnv, newMkAddr, newLevel, newTypeVarMap,
                             previousDecs, processBody)
            end
            
            fun processBody(previousDecs, newStrName, newDebugEnv, newMkAddr, newLevel, newTypeVarMap) =
            let
                (* TODO: Get the debug environment right here. *)
            in
                codeStrdecs (newStrName, body, newDebugEnv, newMkAddr, newLevel, newTypeVarMap,
                             previousDecs, processTail)
            end
        in
            (* Process the declarations then the body and then the tail. *)
            codeStrdecs (strName, decs, debugEnv, mkAddr, level, typeVarMap, leadingDecs, processBody)
        end
 
      | codeStrdecs (strName, (CoreLang {dec, ...}) :: sTail,
                     debugEnv, mkAddr, level, typeVarMap, leadingDecs, processBody) =
        let
            fun processTail(newCode, newDebugEnv, newTypeVarMap) =
                codeStrdecs (strName, sTail, newDebugEnv, mkAddr, level, newTypeVarMap,
                              newCode, processBody)
            val (code, debug) =
                gencode (dec, lex, debugEnv, level, mkAddr, typeVarMap, strName, processTail)
        in
            (leadingDecs @ code, debug)
        end
    (* end codeStrdecs *)

    (* Generate a list of load instructions to build the result tuple. *)
    and codeLoadStrdecs(StructureDec { bindings, ... }, _) =
        let
            fun loadStructureBind ({valRef = ref v, ...}, loads) =
            let
                val { addr=ref addr, ...}  = vaLocal (structAccess(valOf v))
            in
                mkLoadLocal addr :: loads
            end
        in
            (* Code-generate each declaration. *)
            List.foldl loadStructureBind [] bindings
        end

    |   codeLoadStrdecs(Localdec {body, ...}, level) =
            List.foldl (fn(s, l) => codeLoadStrdecs(s, level) @ l) [] body          
 
    |   codeLoadStrdecs(CoreLang {vars=ref vars, ...}, level) =
        let
          (* Load each variable, exception and type ID (i.e. equality & print function)
             that has been declared.
             Since value declarations may be mutually recursive we have
             to code-generate the declarations first then return the values. *)
            val typeVarMap = TypeVarMap.defaultTypeVarMap(fn _ => raise InternalError "typeVarMap", level)
            fun loadVals (CoreValue v, rest)  = codeVal (v, level, typeVarMap, [], nullLex, location nullLex) :: rest
            |   loadVals (CoreStruct s, rest) = codeStruct (s, level) :: rest
            |   loadVals (CoreType (TypeConstrSet(_, tcConstructors)), rest) =
                    (* Type IDs are handled separately but we need to load the value constructors if
                       this is a datatype.  This is really only because of datatype replication where
                       we need to be able to get the value constructors from the datatype. *)
                    List.rev(List.map( fn v => codeVal (v, level, typeVarMap, [], nullLex, location nullLex)) tcConstructors)
                        @ rest
            |   loadVals (_, rest) = rest
        in
            List.foldl loadVals [] vars
        end
 
        fun codeTopdecs (StrDec(str, _), debugEnv, mkAddr) =
            let
                open TypeVarMap
                val level = baseLevel
                val typeVarMap = TypeVarMap.defaultTypeVarMap(mkAddr, level)
                val (code, debug) =
                    codeStrdecs("", [str], debugEnv, mkAddr, level, typeVarMap, [],
                        fn(decs, _, debugEnv, _, _, _) => (decs, debugEnv))
            in
                { code = TypeVarMap.getCachedTypeValues typeVarMap @ code, debug = debug }
            end

        |   codeTopdecs (FunctorDec (structList : functorBind list, _), debugEnv, mkOuterAddr) =
            let
               fun codeFunctorBind ({name, arg = {valRef=argValRef, ...}, body, valRef, resIds=ref resIds,
                                      matchToResult=ref matchToResult, debugArgVals, debugArgStructs, debugArgTypeConstrs, ...}: functorBind, debugEnv) =
                let
                    val argVal = valOf(! argValRef)
                    local (* Separate context for each functor binding. *)
                        val address = ref 1
                    in
                        fun mkAddr n = !address before (address := ! address+n)
                        val level = newLevel baseLevel (* Inside the functor *)
                    end
                    val arg = vaLocal (structAccess argVal)
                    (* Create a local binding for the argument.  This allows the new variable to be a local. *)
                    val localAddr = mkAddr 1
                    val () = #addr  arg := localAddr 
                    val () = #level arg := level
                    val func = valOf(!valRef)

                    local
                        (* These are the entries for the functor arguments. *)
                        val (typeIdDebugDecs, typeIdDebugEnv) =
                            makeTypeIdDebugEntries(sigBoundIds (structSignat argVal), debugEnv, level, lex, mkAddr)
                        val (structDebugDecs, structDebugEnv) =
                            makeStructDebugEntries(! debugArgStructs, typeIdDebugEnv, level, lex, mkAddr)
                        val typeVarMap = TypeVarMap.defaultTypeVarMap(mkAddr, level) (* ???Check??? *)
                        val (valDebugDecs, valDebugEnv) =
                            makeValDebugEntries(! debugArgVals, structDebugEnv, level, lex, mkAddr, typeVarMap)
                        val (typeDebugDecs, typeDebugEnv) =
                            makeTypeConstrDebugEntries(! debugArgTypeConstrs, valDebugEnv, level, lex, mkAddr)
                    in
                        val fBindDebugDecs = typeIdDebugDecs @ structDebugDecs @ valDebugDecs @ typeDebugDecs
                        val fBindDebugEnv = typeDebugEnv
                    end

                    (* Process the body and make a function out of it. *)
                    local
                        val {code = strCode, load = strLoad, ...} =
                            structureCode (body, name ^ "().", fBindDebugEnv, mkAddr, level)
                        fun getIds(n, isEq) =
                            case #source(List.nth(resIds, n)) of
                                id as TypeId{idKind=TypeFn _, ...} => (* Have to generate a function here. *)
                                    Global(codeGenerativeId{source=id, isEq=isEq, isDatatype=false (*??*), mkAddr=mkAddr, level=level})
                            |   id => idAccess id

                        val matchedCode = applyMatchActions(strLoad, matchToResult, getIds, mkAddr, level)
                    in
                        val functorCode = (* The function that implements the functor. *)
                            (if getParameter inlineFunctorsTag (debugParams lex) then mkMacroProc else mkProc)
                                (mkEnv(mkDec(localAddr, mkLoadArgument 0) :: (fBindDebugDecs @ strCode), matchedCode),
                                1, name, getClosure level, mkAddr 0)
                    end

                    (* Set the address of this variable. Because functors can only
                       be declared at the top level the only way it can be used is
                       if we have 
                        functor F(..) = ... functor G() = ..F..
                       with no semicolon between them. They will then be taken as
                       a single declaration and F will be picked up as a local. *)
                    val addr = mkOuterAddr 1
                    val Functor { access, ...} = func
                    val var = vaLocal access
                    val ()  = #addr  var := addr;
                    val ()  = #level var := baseLevel(* Top level *);
                in
                    {
                        code = [mkDec (addr, functorCode)],
                        debug = debugEnv
                    }
                end
            in
              mapPair codeFunctorBind structList debugEnv
            end

        |   codeTopdecs(SignatureDec _, debugEnv, _) = { code = [], debug = debugEnv }

        and loadTopdecs (StrDec(str, ref typeIds)) =
            let
                val level = baseLevel
                val load = codeLoadStrdecs(str, level)
                (* Load all the IDs created in this topdec even if they're not directly referenced. *)
                fun loadIds id = codeId(id, level)
            in
                load @ List.rev(List.map loadIds typeIds)
            end

        |   loadTopdecs (FunctorDec (structList, _)) =
            let
                fun loadFunctorBind ({valRef, ...}) =
                let
                    val Functor{access, ...} = valOf(! valRef)
                    val {addr = ref addr, ...} = vaLocal access
                in
                    mkLoadLocal addr
                end
            in
                List.rev(List.map loadFunctorBind structList)
            end

        |   loadTopdecs(SignatureDec _) = []

        local (* Separate context for each top level item. *)
            val address = ref 0
        in
            fun mkAddr n = !address before (address := ! address+n)
        end
   
        val coded = (* Process top level list. *)
            mapPair (fn (str, debug) => codeTopdecs (str, debug, mkAddr))
                strs initialDebuggerStatus
        val loaded = List.foldl (fn (s, l) => loadTopdecs s @ l) [] strs
    in 
        (* The result is code for a vector containing the results of the
           declarations which pass4 can use to pull out the values after
           the code has been run. *)
        (mkEnv (#code coded, mkTuple(List.rev loaded)), mkAddr 0)
    end (* gencodeStructs *);

  (* Once the code has been executed the declarations must be added to
     the global scope. The type and infix status environments have already
     been processed so they can be dumped into the global environment
     unchanged. The values and exceptions, however, have to be picked out
     the compiled code. Note: The value constructors are actually produced
     at the same time as their types but are dumped out by enterGlobals. *)
  (* This previously only processed declarations which required some code-generation and
     evaluation (structures, values and functors).  It now includes types, signatures and
     fixity so that all declarations can be printed in the order of declaration.  DCJM 6/6/02. *)
    fun pass4Structs (results, (strs: topdec list, _)) =
    let
        fun extractStruct(str, mapTypeIds, args as (addr, { fixes, values, structures, signatures, functors, types } )) =
        case str of
            StructureDec { bindings, ... } =>
            let
                fun extractStructureBind ({name, valRef, line, ...}: structBind, (addr, structures)) =
                let
                    val structCode = mkInd (addr, results)
                    (* We need to replace type IDs with their Global versions. *)
                    
                    local
                        val Struct{signat=Signatures { name, locations, typeIdMap, tab, ...}, ...} = valOf(!valRef)
                    in
                        val resultSig =
                            makeSignature(name, tab, 0, locations, composeMaps(typeIdMap, mapTypeIds), [])
                    end
                in
                    (* Make a global structure. *)
                    (addr + 1, (name, makeGlobalStruct (name, resultSig, structCode, [DeclaredAt line])) :: structures)
                end

                val (newAddr, newstructures) = List.foldl extractStructureBind (addr, structures) bindings
            in
                (newAddr, { structures=newstructures, functors=functors, signatures=signatures,
                      fixes=fixes, values=values, types=types })
            end
 
      | Localdec {body, ...} =>
            List.foldl (fn(s, a) => extractStruct(s, mapTypeIds, a))args body          
 
      (* Value, exception or type declaration at the top level. *)
      | CoreLang {vars=ref vars, ...} =>
        let (* Enter the values and exceptions. *)
            (* Copy the types to replace the type IDs with the versions with Global access. *)
            fun replaceTypes t =
            let
                fun copyId(TypeId{idKind=Bound{ offset, ...}, ...}) = SOME(mapTypeIds offset)
                |   copyId _ = NONE
                fun replaceTypeConstrs tcon = copyTypeConstr (tcon, copyId, fn x => x, fn s => s)
            in
                copyType(t, fn tv=>tv, replaceTypeConstrs)
            end

            fun makeDecs (CoreValue(Value{class, name, typeOf, locations, access, ...}),
                          (addr, { fixes, values, structures, signatures, functors, types } )) =
                let
                    (* Extract the value from the result vector except if we have a type-dependent
                       function e.g. PolyML.print where we must use the type-dependent version. *)
                    val newAccess =
                        case access of
                            Overloaded _ => access
                        |   _ => Global(mkInd (addr, results))
                    (* Replace the typeIDs. *)
                    val newVal =
                        Value{class=class, name=name, typeOf=replaceTypes typeOf, access=newAccess,
                              locations=locations, references = NONE, instanceTypes=NONE}
                in
                    (addr+1, { fixes=fixes, values=(name, newVal) :: values, structures=structures,
                               signatures=signatures, functors=functors, types=types } )
                end

            |   makeDecs (CoreStruct dec, (addr, {fixes, values, structures, signatures, functors, types})) =
                (* If we open a structure we've created in the same "program" we may have a non-global
                   substructure.  We have to process any structures and also map any type IDs. *)
                let
                    local
                        val Signatures { name, locations, typeIdMap, tab, ...} = structSignat dec
                    in
                        val resultSig =
                            makeSignature(name, tab, 0, locations, composeMaps(typeIdMap, mapTypeIds), [])
                    end
                    val strName = structName dec
                    val newStruct =
                        Struct { name = strName, signat = resultSig,
                                 access = Global(mkInd (addr, results)), locations = structLocations dec }
                in
                 (addr+1, { fixes=fixes, values=values, structures=(strName, newStruct) :: structures,
                          signatures=signatures, functors=functors, types=types } )
                end

            |   makeDecs (CoreFix pair, (addr, {fixes, values, structures, signatures, functors, types})) =
                 (addr, { fixes=pair :: fixes, values=values, structures=structures,
                          signatures=signatures, functors=functors, types=types } )

            |   makeDecs (CoreType (TypeConstrSet(tc, constrs)),
                                (addr, {fixes, values, structures, signatures, functors, types})) =
                let
                    fun loadConstr(Value{class, name, typeOf, locations, ...}, (addr, others)) =
                    let
                        val newAccess = Global(mkInd (addr, results))
                        (* Don't bother replacing the type ID here.  fullCopyDatatype will do it. *)
                        val newConstr =
                            Value{class=class, name=name, typeOf=typeOf, access=newAccess,
                                  locations=locations, references = NONE, instanceTypes=NONE}
                    in
                        (addr+1, others @ [newConstr])
                    end
                    val (nextAddr, newConstrs) = List.foldl loadConstr (addr, []) constrs
                    val copiedTC = fullCopyDatatype(TypeConstrSet(tc, newConstrs), mapTypeIds, "")
                    val newName = #second(splitString(tcName tc))
                in
                    (nextAddr, { fixes=fixes, values=values, structures=structures,
                             signatures=signatures, functors=functors, types=(newName, copiedTC) :: types } )
                end
        in
            List.foldl makeDecs args vars
        end
    
    fun extractTopDec(str, (addr, env as { fixes, values, structures, signatures, functors, types }, nIds, mapPrevTypIds)) =
    case str of
        StrDec(str, ref typeIds) =>
        let
            (* Create new Free IDs for top-level IDs. *)            
            fun loadId(TypeId{idKind=Bound{eqType, arity, ...}, description, ...}, (n, ids)) =
            let
                val newId = makeFreeId(arity, Global(mkInd(n, results)), pling eqType, description)
            in
                (n+1, newId :: ids)
            end
            |   loadId _ = raise InternalError "Not Bound"

            (* Construct the IDs and reverse the list so the first ID is first*)
            val (newAddr, mappedIds) = List.foldl loadId (addr, []) typeIds
            val idMap = Vector.fromList mappedIds
            fun mapTypeIds n =
                if n < nIds then mapPrevTypIds n else Vector.sub(idMap, n-nIds)
            val (resAddr, resEnv) = extractStruct (str, mapTypeIds, (newAddr, env))
        in
            (resAddr, resEnv, nIds + Vector.length idMap, mapTypeIds)
        end

    |   FunctorDec (structList : functorBind list, _) =>
        let
            (* Get the functor values. *)
            fun extractFunctorBind ({name, valRef, ...}: functorBind, (addr, funcs)) =
            let
                val code = mkInd (addr, results)
                val func = valOf(!valRef)
                (* We need to convert any references to typeIDs created in strdecs in the
                   same "program". *)
                (* The result signature shares with the argument so we only copy IDs less than
                   the min for the argument signature. *)
                val Functor {result=fnResult, name=functorName, locations=functorLocations,
                             arg=fnArg as Struct{name = fnArgName, signat=fnArgSig, ...}, ...} = func
                
                local
                    val Signatures { name, tab, typeIdMap, boundIds, firstBoundIndex, locations, ... } = fnArgSig
                    fun newMap n =
                    if n < firstBoundIndex
                    then mapPrevTypIds n
                    else List.nth(boundIds, n-firstBoundIndex)
                in
                    val functorArgSig =
                        makeSignature(name, tab, firstBoundIndex, locations, composeMaps(typeIdMap, newMap), boundIds)
                    val copiedArg =
                        Struct{name=fnArgName, signat=functorArgSig,
                               access=structAccess fnArg, locations=structLocations fnArg}
                end
                local
                    val Signatures { name, tab, typeIdMap, boundIds, firstBoundIndex, locations, ... } = fnResult
                    val Signatures { boundIds=argBoundIds, firstBoundIndex=argMinTypes, ...} = functorArgSig
                    fun newMap n =
                    if n >= firstBoundIndex
                    then List.nth(boundIds, n-firstBoundIndex)
                    else if n >= argMinTypes
                    then case List.nth(argBoundIds, n-argMinTypes) of
                        (* Add the argument structure name onto the name of type IDs in the argument. *)
                        TypeId{ access, idKind, description={location, name, description}} =>
                            TypeId { access=access, idKind=idKind,
                                    description=
                                        {
                                            location=location, description=description,
                                            name=if fnArgName = "" then name else fnArgName^"."^name
                                        }
                                   }
                    else mapPrevTypIds n
                in
                    val functorSigResult =
                        makeSignature(name, tab, firstBoundIndex, locations, composeMaps(typeIdMap, newMap), boundIds)
                end
                val funcTree = 
                    makeFunctor(functorName, copiedArg, functorSigResult, Global code, functorLocations)
            in
                (addr + 1, (name, funcTree) :: funcs)
            end
            val (newAddr, newfunctors ) = List.foldl extractFunctorBind (addr, functors) structList
        in
            (newAddr, { functors=newfunctors, fixes=fixes, values=values,
                      signatures=signatures, structures=structures, types=types }, nIds, mapPrevTypIds)
        end

     | SignatureDec (structList : sigBind list, _) =>
        let
            (* We need to convert any references to typeIDs created in strdecs in the same "program". *)
            fun copySignature fnSig =
            let
                val Signatures { name, tab, typeIdMap, firstBoundIndex, boundIds, locations, ... } = fnSig
                fun mapIDs n =
                    if n < firstBoundIndex
                    then mapPrevTypIds n
                    else List.nth(boundIds, n-firstBoundIndex)
            in   
                makeSignature(name, tab, firstBoundIndex, locations, composeMaps(typeIdMap, mapIDs), boundIds)
            end
            val newSigs = List.map (fn ({sigRef=ref s, name, ...}: sigBind) => (name, copySignature s)) structList
        in
           (addr, { fixes=fixes, values=values, structures=structures,
                    signatures=newSigs @ signatures, functors=functors, types=types }, nIds, mapPrevTypIds)
        end
    
    val empty = { fixes=[], values=[], structures=[], functors=[], types=[], signatures=[] }

    val (_, result, _, _) = List.foldl extractTopDec (0, empty, 0, fn _ => raise Subscript) strs;
    (* The entries in "result" are in reverse order of declaration and may contain duplicates.
       We need to reverse and filter the lists so that we end up with the lists in order
       and with duplicates removed. *)
    fun revFilter result [] = result
     |  revFilter result ((nameValue as (name, _)) ::rest) =
    let
        (* Remove any entries further down the list if they have the same name. *)
        val filtered = List.filter (fn (n,_) => name <> n) rest
    in
        revFilter (nameValue :: result) filtered
    end
  in
    { fixes=revFilter [] (#fixes result), values=revFilter [] (#values result), structures=revFilter [] (#structures result),
      functors=revFilter [] (#functors result), types=revFilter [] (#types result), signatures=revFilter [] (#signatures result) }
  end (* pass4Structs *)

    structure Sharing =
    struct
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
        type structSigBind  = structSigBind
        type typeVarForm    = typeVarForm
        type sigs           = sigs
    end
end;
