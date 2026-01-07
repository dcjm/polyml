(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008, 2015, 2020.

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
    Title:      Body of ML Compiler.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


functor COMPILER_BODY (

structure SYMSET :
sig
  type sys
  type symset
  
  val inside: sys * symset -> bool;
  val ++ :    symset * symset -> symset
  val abortParse:   symset
  val semicolon:    symset
end;

structure LEX : LEXSIG
structure CODETREE : CODETREE
structure STRUCTVALS : STRUCTVALSIG; 
structure VALUEOPS : VALUEOPSSIG;
structure EXPORTTREE: EXPORTTREESIG
structure STRUCTURES : STRUCTURESSIG
structure PARSEDEC : PARSEDECSIG
structure DEBUG: DEBUG

structure UTILITIES :
sig
  val searchList: unit ->
    { apply: (string * 'a -> unit) -> unit,
      enter:  string * 'a -> unit,
      lookup: string -> 'a  option}
end;

structure PRETTY : PRETTY

structure MISC :
sig
  exception InternalError of string
  val lookupDefault : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option
  val quickSort : ('a -> 'a -> bool) -> 'a list -> 'a list
end;

sharing STRUCTVALS.Sharing = VALUEOPS.Sharing = PRETTY.Sharing = STRUCTURES.Sharing
      = LEX.Sharing = EXPORTTREE.Sharing = SYMSET = PARSEDEC = CODETREE

 ) : COMPILERBODYSIG

=

(*****************************************************************************)
(*                  COMPILERBODY functor body                                *)
(*****************************************************************************)
struct
    open MISC;
    open STRUCTVALS;
    open PRETTY;
    open LEX;

    open STRUCTURES
    open EXPORTTREE

    type nameSpace =
      { 
        lookupVal:    string -> values option,
        lookupType:   string -> typeConstrSet option,
        lookupFix:    string -> fixStatus option,
        lookupStruct: string -> structVals option,
        lookupSig:    string -> signatures option,
        lookupFunct:  string -> functors option,

        enterVal:     string * values      -> unit,
        enterType:    string * typeConstrSet -> unit,
        enterFix:     string * fixStatus   -> unit,
        enterStruct:  string * structVals  -> unit,
        enterSig:     string * signatures  -> unit,
        enterFunct:   string * functors    -> unit,

        allVal:       unit -> (string*values) list,
        allType:      unit -> (string*typeConstrSet) list,
        allFix:       unit -> (string*fixStatus) list,
        allStruct:    unit -> (string*structVals) list,
        allSig:       unit -> (string*signatures) list,
        allFunct:     unit -> (string*functors) list
      }

    val stopSyms = let open SYMSET in op ++ (abortParse, semicolon) end

    fun baseCompiler (lex : lexan, nameSpace: nameSpace, debugSwitches) :
       exportTree option * (unit ->
           { fixes: (string * fixStatus) list, values: (string * values) list,
             structures: (string * structVals) list, signatures: (string * signatures) list,
             functors: (string * functors) list, types: (string * typeConstrSet) list }) option =
    let (* let1 *)

        val compilerOutput = getCompilerOutput debugSwitches
        val printParsetree = DEBUG.getParameter DEBUG.parsetreeTag debugSwitches

        val globals =
        { 
            lookupFix    = #lookupFix nameSpace,
            lookupVal    = #lookupVal nameSpace,
            lookupType   = #lookupType nameSpace,
            lookupSig    = #lookupSig nameSpace,
            lookupStruct = #lookupStruct nameSpace,
            lookupFunct  = #lookupFunct nameSpace,
            enterFix     = #enterFix nameSpace,
            enterVal     = #enterVal nameSpace,
            enterType    = #enterType nameSpace,
            enterStruct  = #enterStruct nameSpace,
            enterSig     = #enterSig nameSpace,
            enterFunct   = #enterFunct nameSpace,
            allValNames  = fn () => map #1 (#allVal nameSpace ()),
            allStructNames = fn () => map #1 (#allStruct nameSpace ())
        }

        val startLocn = location lex

        val () =
            if SYMSET.inside (sy lex, SYMSET.semicolon)
            then insymbol lex
            else ()
      
        val parentTreeNav =
            case List.find (Universal.tagIs rootTreeTag) debugSwitches of
                SOME opt => Universal.tagProject rootTreeTag opt
            |   NONE => { parent = NONE, next = NONE, previous = NONE }

    in
        (* An empty declaration (or end of file!) *)
        if SYMSET.inside (sy lex, stopSyms)
        then if errorOccurred lex (* We could have, for example, an unterminated comment. *)
        then (NONE, NONE)
        else (SOME(locSpan(startLocn, location lex), []), 
                 SOME (fn () => { fixes=[], values=[], structures=[], functors=[], types=[], signatures=[] })
            ) (* Do nothing *)
        else
        let
            (* create a "throw away" compiling environment for this topdec *)
            val newFixEnv = UTILITIES.searchList ()
            val enterFix  = #enter newFixEnv
            val lookupFix = lookupDefault (#lookup newFixEnv) (#lookupFix globals)
   
            (* parse a program: a sequence of topdecs ending with a semicolon. *)
            val parseTree : STRUCTURES.program = 
                let
                    val fixes : PARSEDEC.fixes = {enterFix  = enterFix, lookupFix = lookupFix}
                    val params : PARSEDEC.params =
                        {recordPunning = DEBUG.getParameter DEBUG.languageExtensionsTag debugSwitches}
                in
                    PARSEDEC.parseDec (stopSyms, lex, fixes, params)
                end

            val () = 
                if printParsetree
                then compilerOutput (STRUCTURES.displayProgram (parseTree, 10000))
                else ()
        in
            if errorOccurred lex
            then (NONE, NONE) (* Error: No result and the parse tree won't be useful. *)
            else
            let
                (* If no errors then do second pass to match identifiers
                   and declarations and return type of expression. *)
                val () = STRUCTURES.pass2Structs (parseTree, lex, Env globals)
          
            in
                if errorOccurred lex
                then (SOME(structsExportTree(parentTreeNav, parseTree)), NONE)
                else
                let
                    (* Only code-generate if there were no errors and it's not a directive. *)
                    val (structCode, nLocals) = STRUCTURES.gencodeStructs (parseTree, lex)
                in
                    if errorOccurred lex
                    then (* Errors can be produced during the code-generation phase. *)
                        (SOME(structsExportTree(parentTreeNav, parseTree)), NONE) (* Error: No result. *)
                    else
                    let                      
                        val resultCode = CODETREE.genCode(structCode, debugSwitches, nLocals)
                        (* This is the function that is returned as the result of
                           the compilation. *)
                        fun executeCode() = STRUCTURES.pass4Structs (resultCode (), parseTree)
                    in
                        (SOME(structsExportTree (parentTreeNav, parseTree)), SOME executeCode)
                    end
                end
            end
        end
    end

    fun compiler (nameSpace: nameSpace, getChar: unit->char option, parameters: Universal.universal list) : 
       exportTree option * (unit ->
           { fixes: (string * fixStatus) list, values: (string * values) list,
             structures: (string * structVals) list, signatures: (string * signatures) list,
             functors: (string * functors) list, types: (string * typeConstrSet) list }) option =
    let
        val debugSwitches = parameters 
        val lex = LEX.initial(getChar, debugSwitches)
        val compilerOutput = getCompilerOutput parameters
        fun printReport s = compilerOutput(PrettyString s)
    in
        baseCompiler(lex, nameSpace, debugSwitches)
      handle
        SML90.Interrupt =>
        (
            printReport "Compilation interrupted\n";
            raise SML90.Interrupt
        )
         
      | InternalError s =>
        let
            val s' =
                "Exception- InternalError: " ^ String.toString s ^ " raised while compiling"
        in
            printReport (s' ^ "\n");
            raise Fail s'
        end
       
      | exn =>
        let
            val s' =
              "Exception- " ^ General.exnName exn ^ " unexpectedly raised while compiling"
        in
            printReport (s' ^ "\n");
            raise Fail s'
        end
    end

    structure Sharing =
    struct
        type values = values
        and  typeConstrSet = typeConstrSet
        and  fixStatus = fixStatus
        and  structVals = structVals
        and  signatures = signatures
        and  functors = functors
        and  ptProperties = ptProperties
    end

end; (* struct *)
