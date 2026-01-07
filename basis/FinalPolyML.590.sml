(*
    Title:      Nearly final version of the PolyML structure
    Author:     David Matthews
    Copyright   David Matthews 2008-9, 2014, 2015-17, 2019-21, 2025

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
Based on:

    Title:      Poly Make Program.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


(*
This is the version of the PolyML structure that can be compiled after we
have the rest of the basis library.  In particular it binds in TextIO.stdIn
and TextIO.stdOut.

This contains the top-level read-eval-print loop as well as "use" and
Poly/ML's "make".

The rootFunction has now been pulled out into a separate file and is added on
after this. 
*)
local
    (* A hash table with a mutex that protects against multiple threads
       rehashing the table by entering values at the same time. *)
    structure ProtectedTable :>
    sig
        type 'a ptable
        val create: unit -> 'a ptable
        val lookup: 'a ptable -> string -> 'a option
        val enter: 'a ptable -> string * 'a -> unit
        val all: 'a ptable -> unit -> (string * 'a) list
        val delete: 'a ptable -> string -> unit
    end
    =
    struct
        open HashArray Thread.Mutex LibraryIOSupport
        type 'a ptable = 'a hash * mutex

        fun create () = (hash 10, mutex())
        and lookup(tab, mutx) = protect mutx (fn s => sub(tab, s))
        and enter(tab, mutx) = protect mutx (fn (s, v) => update(tab, s, v))
        and all(tab, mutx) = protect mutx (fn () => fold (fn (s, v, l) => ((s, v) :: l)) [] tab)
        and delete(tab, mutx) = protect mutx (fn s => HashArray.delete (tab, s))
    end

    fun quickSort _                      ([]:'a list)      = []
    |   quickSort _                      ([h]:'a list)     = [h]
    |   quickSort (leq:'a -> 'a -> bool) ((h::t) :'a list) =
    let
        val (after, befor) = List.partition (leq h) t
    in
        quickSort leq befor @ (h :: quickSort leq after)
    end

    open PolyML.NameSpace
 
    local
        open ProtectedTable
        val fixTable = create() and sigTable = create() and valTable = create()
        and typTable = create() and fncTable = create() and strTable = create()
    in
        val globalNameSpace: PolyML.NameSpace.nameSpace =
        {
            lookupFix    = lookup fixTable,
            lookupSig    = lookup sigTable,
            lookupVal    = lookup valTable,
            lookupType   = lookup typTable,
            lookupFunct  = lookup fncTable,
            lookupStruct = lookup strTable,
            enterFix     = enter fixTable,
            enterSig     = enter sigTable,
            enterVal     = enter valTable,
            enterType    = enter typTable,
            enterFunct   = enter fncTable,
            enterStruct  = enter strTable,
            allFix       = all fixTable,
            allSig       = all sigTable,
            allVal       = all valTable,
            allType      = all typTable,
            allFunct     = all fncTable,
            allStruct    = all strTable
        }

        val forgetFix    = delete fixTable
        and forgetSig    = delete sigTable
        and forgetVal    = delete valTable
        and forgetType   = delete typTable
        and forgetFunct  = delete fncTable
        and forgetStruct = delete strTable
    end

    local
        open PolyML (* For prettyprint datatype *)

        (* Install a pretty printer for parsetree properties.  This isn't done in
           the compiler. *)
        fun prettyProps depth _ l =
            if depth <= 0 then PrettyString "..."
            else prettyProp(l, depth-1)
        
        (* Use prettyRepresentation to print most of the arguments *)
        and prettyProp(PTbreakPoint b, d) =     blockArg("PTbreakPoint", prettyRepresentation(b, d))
        |   prettyProp(PTcompletions s, d) =    blockArg("PTcompletions", prettyRepresentation(s, d))
        |   prettyProp(PTdeclaredAt l, d) =     blockArg("PTdeclaredAt", prettyRepresentation(l, d))
        |   prettyProp(PTdefId i, d) =          blockArg("PTdefId", prettyRepresentation(i, d))
        |   prettyProp(PTfirstChild _, _) =     blockArg("PTfirstChild", PrettyString "fn")
        |   prettyProp(PTnextSibling _, _) =    blockArg("PTnextSibling", PrettyString "fn")
        |   prettyProp(PTopenedAt f, d) =       blockArg("PTopenedAt", prettyRepresentation(f, d))
        |   prettyProp(PTparent _, _) =         blockArg("PTparent", PrettyString "fn")
        |   prettyProp(PTpreviousSibling _, _)= blockArg("PTpreviousSibling", PrettyString "fn")
        |   prettyProp(PTprint _, _) =          blockArg("PTprint", PrettyString "fn")
        |   prettyProp(PTreferences f, d) =     blockArg("PTreferences", prettyRepresentation(f, d))
        |   prettyProp(PTrefId f, d) =          blockArg("PTrefId", prettyRepresentation(f, d))
        |   prettyProp(PTstructureAt f, d) =    blockArg("PTstructureAt", prettyRepresentation(f, d))
        |   prettyProp(PTtype f, d) =           blockArg("PTtype", prettyRepresentation(f, d))
        
        and blockArg (s, arg) =
            PrettyBlock(3, true, [], [PrettyString s, PrettyBreak(1, 1), parenthesise arg])
        
        and parenthesise(p as PrettyBlock(_, _, _, PrettyString "(" :: _)) = p
        |   parenthesise(p as PrettyBlock(_, _, _, PrettyString "{" :: _)) = p
        |   parenthesise(p as PrettyBlock(_, _, _, PrettyString "[" :: _)) = p
        |   parenthesise(p as PrettyBlock(_, _, _, _ :: _)) =
                PrettyBlock(3, true, [], [ PrettyString "(", PrettyBreak(0, 0), p, PrettyBreak(0, 0), PrettyString ")" ])
        |   parenthesise p = p

    in
        val () = addPrettyPrinter prettyProps
    end

    (* PolyML.compiler takes a list of these parameter values.  They all
       default so it's possible to pass only those that are actually
       needed. *)
    datatype compilerParameters =
        CPOutStream of string->unit
        (* Output stream for debugging and other output from the compiler.
           Provides a default stream for other output.  Default: TextIO.print *)
    |   CPNameSpace of PolyML.NameSpace.nameSpace
        (* Name space to look up and enter results.  Default: globalNameSpace *)
    |   CPErrorMessageProc of
            { message: PolyML.pretty, hard: bool, location: PolyML.location, context: PolyML.pretty option } -> unit
        (* Called by the compiler to generate error messages.
           Arguments (message, isHard, lineNo, context).  message is the message.
           isHard is true if this is an error, false if a warning. 
           location is the file-name, line number and position.  context is an
           optional extra piece of information showing the part of the parse tree
           where the error was detected.
           Default: print this to CPOutStream value using CPLineNo and CPFileName. *)
    |   CPLineNo of unit -> int
        (* Called by the compiler to get the current "line number".  This is passed
           to CPErrorMessageProc and the debugger.  It may actually be a more general
           location than a source line.  Default: fn () => 0 i.e. no line numbering. *)
    |   CPLineOffset of unit -> int
        (* Called by the compiler to get the current "offset".  This is passed
           to CPErrorMessageProc and the debugger.  This may either be an offset on
           the current file, a byte offset or simply zero.
           Default: fn () => 0 i.e. no line offset. *)
    |   CPFileName of string
        (* The current file being compiled.  This is used by the default CPErrorMessageProc
           and the debugger.  Default: "" i.e. interactive stream. *)
    |   CPPrintInAlphabeticalOrder of bool
        (* Whether to sort the results by alphabetical order before printing them.  Applies
           only to the default CPResultFun.  Default value of printInAlphabeticalOrder. *)
    |   CPResultFun of {
            fixes: (string * Infixes.fixity) list, values: (string * Values.value) list,
            structures: (string * Structures.structureVal) list, signatures: (string * Signatures.signatureVal) list,
            functors: (string * Functors.functorVal) list, types: (string * TypeConstrs.typeConstr) list} -> unit
        (* Function to apply to the result of compiling and running the code.
           Default: print and enter the values into CPNameSpace. *)
    |   CPCompilerResultFun of
            PolyML.parseTree option *
            ( unit -> {
                fixes: (string * Infixes.fixity) list, values: (string * Values.value) list,
                structures: (string * Structures.structureVal) list, signatures: (string * Signatures.signatureVal) list,
                functors: (string * Functors.functorVal) list, types: (string * TypeConstrs.typeConstr) list}) option -> unit -> unit
        (* Function to process the result of compilation.  This can be used to capture the
           parse tree even if type-checking fails.
           Default: Execute the code and call the result function if the compilation
           succeeds.  Raise an exception if the compilation failed. *)
    |   CPProfiling of int
        (* Deprecated: No longer used. *)
    |   CPTiming of bool
        (* Deprecated: No longer used.  *)
    |   CPDebug of bool
        (* Control whether calls to the debugger should be inserted into the compiled
           code.  This allows breakpoints to be set, values to be examined and printed
           and functions to be traced at the cost of extra run-time overhead.
           Default: value of PolyML.Compiler.debug *)
    |   CPPrintDepth of unit->int
        (* This controls the depth of printing if the default CPResultFun is used.  It
           is also bound into any use of PolyML.print in the compiled code and will
           be called to get the print depth whenever that code is executed.
           Default: Get the current value of PolyML.print_depth. *)
    |   CPPrintStream of string->unit
        (* This is bound into any occurrence of PolyML.print and is used to produce
           the outut.  Default: CPOutStream. *)
    |   CPErrorDepth of int
        (* Controls the depth of context to produce in error messages.
           Default : value of PolyML.error_depth. *)
    |   CPLineLength of int
        (* Bound into any occurrences of PolyML.print.  This is the length of a line
           used in the pretty printer.  Default: value of PolyML.line_length. *)
    |   CPRootTree of
        {
            parent: (unit -> PolyML.parseTree) option,
            next: (unit -> PolyML.parseTree) option,
            previous: (unit -> PolyML.parseTree) option
        }
        (* This can be used to provide a parent for parse trees created by the
           compiler.  This appears as a PTparent property in the tree.
           The default is NONE which does not to provide a parent.  *)
    |   CPAllocationProfiling of int
        (* Controls whether to add profiling information to each allocation.  Currently
           zero means no profiling and one means add the allocating function. *)

    |   CPDebuggerFunction of int * Values.value * int * string * string * nameSpace -> unit
        (* Deprecated: No longer used. *)

    |   CPBindingSeq of unit -> int
        (* Used to create a sequence no for PTdefId properties.  This can be used in an IDE
           to allocate a unique Id for an identifier.  Default fn _ => 0. *)

    (* References for control and debugging. *)
    val timing = ref false
    and printDepth: int ref = ref 0
    and errorDepth: int ref = ref 6
    and lineLength: int ref = ref 77
    and allocationProfiling = ref false
    
    val assemblyCode = ref false
    and codetree = ref false
    and codetreeAfterOpt = ref false
    and icode = ref false
    and parsetree = ref false
    and compilerDebug = ref 0
    and reportUnreferencedIds = ref false
    and reportExhaustiveHandlers = ref false
    and narrowOverloadFlexRecord = ref false
    and createPrintFunctions = ref true
    and reportDiscardFunction = ref true
    and reportDiscardNonUnit = ref false
    val lowlevelOptimise = ref true
    
    val debug = ref false
    val inlineFunctors = ref true
    val maxInlineSize: int ref = ref 80
    val printInAlphabeticalOrder = ref true
    val traceCompiler = ref false (* No longer used. *)
    
    fun prettyPrintWithIDEMarkup(stream : string -> unit, lineWidth : int): PolyML.pretty -> unit =
    let
        open PolyML
        val openDeclaration = "\u001bD"
        val closeDeclaration = "\u001bd"
        val separator = "\u001b,"
        val finalSeparator = "\u001b;"
        
        fun beginMarkup context =
            case List.find (fn ContextLocation _ => true | _ => false) context of
                SOME (ContextLocation{file,startLine,startPosition,endPosition, ...}) =>
                let
                    (* In the unlikely event there's an escape character in the
                       file name convert it to ESC-ESC. *)
                    fun escapeEscapes #"\u001b" = "\u001b\u001b"
                    |   escapeEscapes c = str c
                in
                    stream openDeclaration;
                    stream(String.translate escapeEscapes file);
                    stream separator;
                    stream(FixedInt.toString startLine);
                    stream separator;
                    stream(FixedInt.toString startPosition);
                    stream separator;
                    stream(FixedInt.toString endPosition);
                    stream finalSeparator
                end
            |   _ => ()
            
        fun endMarkup context =
            List.app (fn ContextLocation _ => stream closeDeclaration | _ => ()) context
    in
        prettyMarkup (beginMarkup, endMarkup) (stream, lineWidth)
    end;

    (* useMarkupInOutput is set according to the setting of *)
    val useMarkupInOutput = ref false
    fun prettyPrintWithOptionalMarkup(stream, lineWidth) =
        if ! useMarkupInOutput then prettyPrintWithIDEMarkup(stream, lineWidth)
        else PolyML.prettyPrint(stream, lineWidth)

    (* Top-level prompts. *)
    val prompt1 = ref "> " and prompt2 = ref "# ";

    fun printOut s =
        TextIO.print s
        (* If we get an exception while writing to stdOut we've got
           a big problem and can't continue.  It could happen if
           we have closed stdOut.  Try reporting the error through
           stdErr and exit. *)
        handle Thread.Thread.Interrupt => raise Thread.Thread.Interrupt
        |     exn =>
            (
                (
                    TextIO.output(TextIO.stdErr,
                        concat["Exception ", exnName exn,
                               " raised while writing to stdOut.\n"]);
                    TextIO.flushOut TextIO.stdErr (* probably unnecessary. *)
                ) handle _ => ();
                (* Get out without trying to do anything else. *)
                OS.Process.terminate OS.Process.failure
            )

    (* Default function to print and enter a value. *)
    fun printAndEnter (inOrder: bool, space: PolyML.NameSpace.nameSpace,
                       stream: string->unit, depth: int)
        { fixes: (string * Infixes.fixity) list, values: (string * Values.value) list,
          structures: (string * Structures.structureVal) list, signatures: (string * Signatures.signatureVal) list,
          functors: (string * Functors.functorVal) list, types: (string * TypeConstrs.typeConstr) list}: unit =
    let
        (* We need to merge the lists to sort them alphabetically. *)
        datatype decKind =
            FixStatusKind of Infixes.fixity
        |   TypeConstrKind of TypeConstrs.typeConstr
        |   SignatureKind of Signatures.signatureVal
        |   StructureKind of Structures.structureVal
        |   FunctorKind of Functors.functorVal
        |   ValueKind of Values.value

        val decList =
            map (fn (s, f) => (s, FixStatusKind f)) fixes @
            map (fn (s, f) => (s, TypeConstrKind f)) types @
            map (fn (s, f) => (s, SignatureKind f)) signatures @
            map (fn (s, f) => (s, StructureKind f)) structures @
            map (fn (s, f) => (s, FunctorKind f)) functors @
            map (fn (s, f) => (s, ValueKind f)) values

        fun kindToInt(FixStatusKind _) = 0
        |   kindToInt(TypeConstrKind _) = 1
        |   kindToInt(SignatureKind _) = 2
        |   kindToInt(StructureKind _) = 3
        |   kindToInt(FunctorKind _) = 4
        |   kindToInt(ValueKind _) = 5

        fun order (s1: string, k1) (s2, k2) =
                if s1 = s2 then kindToInt k1 <= kindToInt k2
                else s1 <= s2

        (* Don't sort the declarations if we want them in declaration order. *)
        val sortedDecs =
            if inOrder then quickSort order decList else decList

        fun enterDec(n, FixStatusKind f) = #enterFix space (n,f)
        |   enterDec(n, TypeConstrKind t) = #enterType space (n,t)
        |   enterDec(n, SignatureKind s) = #enterSig space (n,s)
        |   enterDec(n, StructureKind s) = #enterStruct space (n,s)
        |   enterDec(n, FunctorKind f) = #enterFunct space (n,f)
        |   enterDec(n, ValueKind v) = #enterVal space (n,v)

        fun printDec(_, FixStatusKind f) =
                prettyPrintWithOptionalMarkup (stream, !lineLength) (Infixes.print f)

        |   printDec(_, TypeConstrKind t) =
                prettyPrintWithOptionalMarkup (stream, !lineLength) (TypeConstrs.print(t, FixedInt.fromInt depth, SOME space))

        |   printDec(_, SignatureKind s) =
                prettyPrintWithOptionalMarkup (stream, !lineLength) (Signatures.print(s, FixedInt.fromInt depth, SOME space))

        |   printDec(_, StructureKind s) =
                prettyPrintWithOptionalMarkup (stream, !lineLength) (Structures.print(s, FixedInt.fromInt depth, SOME space))

        |   printDec(_, FunctorKind f) =
                prettyPrintWithOptionalMarkup (stream, !lineLength) (Functors.print(f, FixedInt.fromInt depth, SOME space))

        |   printDec(_, ValueKind v) =
                if Values.isConstructor v andalso not (Values.isException v)
                then () (* Value constructors are printed with the datatype. *)
                else prettyPrintWithOptionalMarkup (stream, !lineLength) (Values.printWithType(v, FixedInt.fromInt depth, SOME space))

    in
        (* First add the declarations to the name space and then print them.  Doing it this way
           improves the printing of types since these require look-ups in the name space.  For
           instance the constructors of a datatype from an opened structure should not include
           the structure name but that will only work once the datatype itself is in the global
           name-space. *)
        List.app enterDec sortedDecs;
        if depth > 0 then List.app printDec sortedDecs else ()
    end

    local
        open Bootstrap Bootstrap.Universal
        (* To allow for the possibility of changing the representation we don't make Universal
           be the same as Bootstrap.Universal. *)

        (* Default error message function. *)
        fun defaultErrorProc printString
            {message: PolyML.pretty, hard: bool,
             location={startLine, startPosition, endPosition, file, ...}: PolyML.location,
             context: PolyML.pretty option} =
        let
            open PolyML
            val fullMessage =
                case context of
                    NONE => message
                |   SOME ctxt =>
                        PrettyBlock(0, true, [],
                            [ message, PrettyBreak(1, 0),
                                PrettyBlock(2, false, [], [PrettyString "Found near", PrettyBreak(1, 0), ctxt])
                            ])
        in
            if ! useMarkupInOutput
            then (* IDE mark-up of error messages.  This is actually the same as within the IDE. *)
            let
                val openError = "\u001bE"
                val closeError = "\u001be"
                val separator = "\u001b,"
                val finalSeparator = "\u001b;"
            in
                printString(
                    concat
                        [
                            openError,
                            if hard then "E" else "W", separator,
                            file, (* TODO double any escapes. *) separator,
                            FixedInt.toString startLine, separator,
                            FixedInt.toString startPosition, separator,
                            FixedInt.toString endPosition, finalSeparator
                         ]
                    );
                prettyPrintWithIDEMarkup(printString, !lineLength) fullMessage;
                printString closeError
            end
            else (* Plain text form. *)
            (
                printString(concat
                   ( (if file = "" then ["poly: "] else [file, ":"]) @
                     (if startLine = 0 then [] else [FixedInt.toString startLine]) @
                     (if startPosition = 0 then [": "] else [".", FixedInt.toString startPosition, "-", FixedInt.toString endPosition, ": "]) @
                     (if hard then ["error: "] else ["warning: "]) ));
(*                   ( (if hard then ["Error-"] else ["Warning-"]) @
                     (if file = "" then [] else [" in '", file, "',"]) @
                     (if startLine = 0 then [] else [" line ", Int.toString startLine]) @
                     (if startLine = 0 andalso file = "" then [] else [".\n"]))); *)
                PolyML.prettyPrint(printString, !lineLength) fullMessage
            )
        end
    in
        (* This function ends up as PolyML.compiler.  *)
        fun polyCompiler (getChar: unit->char option, parameters: compilerParameters list) =
        let
            (* Find the first item that matches or return the default. *)
            fun find _ def [] = def
              | find f def (hd::tl) =
                  case f hd of
                      SOME s => s
                  |   NONE => find f def tl
        
            val outstream = find (fn CPOutStream s => SOME s | _ => NONE) TextIO.print parameters
            val nameSpace = find (fn CPNameSpace n => SOME n | _ => NONE) globalNameSpace parameters
            val lineNo = find (fn CPLineNo l => SOME l | _ => NONE) (fn () => 0) parameters
            val lineOffset = find (fn CPLineOffset l => SOME l | _ => NONE) (fn () => 0) parameters
            val fileName = find (fn CPFileName s => SOME s | _ => NONE) "" parameters
            val printInOrder = find (fn CPPrintInAlphabeticalOrder t => SOME t | _ => NONE)
                                (! printInAlphabeticalOrder) parameters
            val printDepth = find (fn CPPrintDepth f => SOME f | _ => NONE) (fn () => !printDepth) parameters
            val resultFun = find (fn CPResultFun f => SOME f | _ => NONE)
               (printAndEnter(printInOrder, nameSpace, outstream, printDepth())) parameters
            val printString = find (fn CPPrintStream s => SOME s | _ => NONE) outstream parameters
            val errorProc =  find (fn CPErrorMessageProc f => SOME f | _ => NONE) (defaultErrorProc printString) parameters
            val debugging = find (fn CPDebug t => SOME t | _ => NONE) (! debug) parameters
            val allocProfiling = find(fn CPAllocationProfiling l  => SOME l | _ => NONE) (if !allocationProfiling then 1 else 0) parameters
            val bindingSeq = find(fn CPBindingSeq l  => SOME l | _ => NONE) (fn () => 0) parameters
            local
                (* Default is to filter the parse tree argument. *)
                fun defaultCompilerResultFun (_, NONE) = raise Fail "Static Errors"
                |   defaultCompilerResultFun (_, SOME code) = fn () => resultFun(code()) 
            in
                val compilerResultFun = find (fn CPCompilerResultFun f => SOME f | _ => NONE)
                    defaultCompilerResultFun parameters
            end

            (* TODO: Make this available as a parameter. *)
            val prettyOut = prettyPrintWithOptionalMarkup(printString, !lineLength)
            
            val compilerOut = prettyPrintWithOptionalMarkup(outstream, !lineLength)

            (* Parent tree defaults to empty. *)
            val parentTree =
                find (fn CPRootTree f => SOME f | _ => NONE)
                    { parent = NONE, next = NONE, previous = NONE } parameters

            (* Pass all the settings.  Some of these aren't included in the parameters datatype (yet?). *)
            val treeAndCode =
                PolyML.compiler(nameSpace, getChar,
                    [
                    tagInject errorMessageProcTag errorProc,
                    tagInject compilerOutputTag compilerOut,
                    tagInject lineNumberTag (FixedInt.fromInt o lineNo),
                    tagInject offsetTag (FixedInt.fromInt o lineOffset),
                    tagInject fileNameTag fileName,
                    tagInject bindingCounterTag (FixedInt.fromInt o bindingSeq),
                    tagInject inlineFunctorsTag (! inlineFunctors),
                    tagInject maxInlineSizeTag (FixedInt.fromInt(! maxInlineSize)),
                    tagInject parsetreeTag (! parsetree),
                    tagInject codetreeTag (! codetree),
                    tagInject icodeTag (! icode),
                    tagInject lowlevelOptimiseTag (! lowlevelOptimise),
                    tagInject assemblyCodeTag (! assemblyCode),
                    tagInject codetreeAfterOptTag (! codetreeAfterOpt),
                    tagInject compilerDebugTag (! compilerDebug),
                    tagInject profileAllocationTag (FixedInt.fromInt allocProfiling),
                    tagInject errorDepthTag (FixedInt.fromInt(! errorDepth)),
                    tagInject printDepthFunTag (FixedInt.fromInt o printDepth),
                    tagInject lineLengthTag (FixedInt.fromInt(! lineLength)),
                    tagInject debugTag debugging,
                    tagInject printOutputTag prettyOut,
                    tagInject rootTreeTag parentTree,
                    tagInject reportUnreferencedIdsTag (! reportUnreferencedIds),
                    tagInject reportExhaustiveHandlersTag (! reportExhaustiveHandlers),
                    tagInject narrowOverloadFlexRecordTag (! narrowOverloadFlexRecord),
                    tagInject createPrintFunctionsTag (! createPrintFunctions),
                    tagInject reportDiscardedValuesTag
                        (if ! reportDiscardNonUnit then 2 else if ! reportDiscardFunction then 1 else 0)
                    ])
        in
            compilerResultFun treeAndCode
        end

        (* Top-level read-eval-print loop.  This is the normal top-level loop and is
           also used for the debugger. *)
        fun topLevel {isDebug, nameSpace, exitLoop, exitOnError, isInteractive, startExec, endExec } =
        let
            (* This is used as the main read-eval-print loop.  It is also invoked
               by running code that has been compiled with the debug option on
               when it stops at a breakpoint.  In that case debugEnv contains an
               environment formed from the local variables.  This is placed in front
               of the normal top-level environment. *)
           
            (* Don't use the end_of_stream because it may have been set by typing
               EOT to the command we were running. *)
            val endOfFile    = ref false;
            val realDataRead = ref false;
            val lastWasEol   = ref true;
    
            (* Each character typed is fed into the compiler but leading
               blank lines result in the prompt remaining as firstPrompt until
               significant characters are typed. *)
            fun readin () : char option =
            let
                val () =
                    if isInteractive andalso !lastWasEol (* Start of line *)
                    then if !realDataRead
                    then printOut (if isDebug then "debug " ^ !prompt2 else !prompt2)
                    else printOut (if isDebug then "debug " ^ !prompt1 else !prompt1)
                    else ();
             in
                case TextIO.input1 TextIO.stdIn of
                    NONE => (endOfFile := true; NONE)
                |   SOME #"\n" => ( lastWasEol := true; SOME #"\n" )
                |   SOME ch =>
                       (
                           lastWasEol := false;
                           if ch <> #" "
                           then realDataRead := true
                           else ();
                           SOME ch
                       )
            end; (* readin *)
    
            (* Remove all buffered but unread input. *)
            fun flushInput () =
                case TextIO.canInput(TextIO.stdIn, 1) of
                    SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
                |   _ => (* No input waiting or we're at EOF. *) ()
    
            fun readEvalPrint () : unit =
            let
            in
                realDataRead := false;
                (* Compile and then run the code. *)
                let
                    val startCompile = Timer.startCPUTimer()

                    (* Compile a top-level declaration/expression. *)
                    val code =
                        polyCompiler (readin, [CPNameSpace nameSpace, CPOutStream printOut])
                            (* Don't print any times if this raises an exception. *)
                        handle exn as Fail s =>
                        (
                            printOut(s ^ "\n");
                            flushInput();
                            lastWasEol := true;
                            PolyML.Exception.reraise exn
                        )
                        
                    val endCompile = Timer.checkCPUTimer startCompile

                    (* Run the code *)
                    val startRun = Timer.startCPUTimer()
                    val () = startExec() (* Enable any debugging *)
                    (* Run the code and capture any exception (temporarily). *)
                    val finalResult = (code(); NONE) handle exn => SOME exn
                    val () = endExec() (* Turn off debugging *)
                    (* Print the times if required. *)
                    val endRun = Timer.checkCPUTimer startRun
                    val () =
                        if !timing
                        then printOut(
                                concat["Timing - compile: ", Time.fmt 1 (#usr endCompile + #sys endCompile),
                                       " run: ", Time.fmt 1 (#usr endRun + #sys endRun), "\n"])
                        else ()
                in
                    case finalResult of
                        NONE => () (* No exceptions raised. *)
                    |   SOME exn => (* Report exceptions in running code. *)
                        let
                            open PolyML PolyML.Exception
                            val exLoc =
                                case exceptionLocation exn of
                                    NONE => []
                                |   SOME loc => [ContextLocation loc]
                        in
                            prettyPrintWithOptionalMarkup(TextIO.print, ! lineLength)
                                (PrettyBlock(0, false, [],
                                    [
                                        PrettyBlock(0, false, exLoc, [PrettyString "Exception-"]),
                                        PrettyBreak(1, 3),
                                        prettyRepresentation(exn, FixedInt.fromInt(! printDepth)),
                                        PrettyBreak(1, 3),
                                        PrettyString "raised"
                                    ]));
                            PolyML.Exception.reraise exn
                        end
                end
            end; (* readEvalPrint *)
            
            fun handledLoop () : unit =
            (
                (* Process a single top-level command. *)
                readEvalPrint()
                    handle Thread.Thread.Interrupt =>
                        (* Allow ^C to terminate the debugger and raise Interrupt in
                           the called program. *)
                        if exitOnError then OS.Process.exit OS.Process.failure
                        else if isDebug then (flushInput(); raise Thread.Thread.Interrupt)
                        else ()
                    |   _ =>
                        if exitOnError
                        then OS.Process.exit OS.Process.failure
                        else ();
                (* Exit if we've seen end-of-file or we're in the debugger
                   and we've run "continue". *)
                if !endOfFile orelse exitLoop() then ()
                else handledLoop ()
            )
        in
            handledLoop ()  
        end
    end

    val suffixes = ref ["", ".ML", ".sml", ".sig"]
 

    (*****************************************************************************)
    (*                  "use": compile from a file.                              *)
    (*****************************************************************************)
   
    val useFileTag: string option Universal.tag = Universal.tag()
    fun getUseFileName(): string option = Option.join (Thread.Thread.getLocal useFileTag)

    fun use (originalName: string): unit =
    let
        (* use "f" first tries to open "f" but if that fails it tries "f.ML", "f.sml" etc. *)
        (* We use the functional layer and a reference here rather than TextIO.input1 because
           that requires locking round every read to make it thread-safe.  We know there's
           only one thread accessing the stream so we don't need it here. *)
        fun trySuffixes [] =
            (* Not found - attempt to open the original and pass back the
               exception. *)
            (TextIO.getInstream(TextIO.openIn originalName), originalName)
         |  trySuffixes (s::l) =
            (TextIO.getInstream(TextIO.openIn (originalName ^ s)), originalName ^ s)
                handle IO.Io _ => trySuffixes l
        (* First in list is the name with no suffix. *)
        val (inStream, fileName) = trySuffixes("" :: ! suffixes)
        val stream = ref inStream
        (* Record the file name.  This allows nested calls to "use" to set the
           correct path. *)
        val oldName = getUseFileName()
        val () = Thread.Thread.setLocal(useFileTag, SOME fileName)

        val lineNo   = ref 1;
        fun getChar () : char option =
            case TextIO.StreamIO.input1 (! stream) of
                NONE => NONE
            |   SOME (eoln as #"\n", strm) =>
                (
                    lineNo := !lineNo + 1;
                    stream := strm;
                    SOME eoln
                )
            |   SOME(c, strm) => (stream := strm; SOME c)
    in
        while not (TextIO.StreamIO.endOfStream(!stream)) do
        let
            val code = polyCompiler(getChar, [CPFileName fileName, CPLineNo(fn () => !lineNo)])
                handle exn =>
                    ( TextIO.StreamIO.closeIn(!stream); PolyML.Exception.reraise exn )
        in
            code() handle exn =>
            (
                (* Report exceptions in running code. *)
                TextIO.print ("Exception- " ^ exnMessage exn ^ " raised\n");
                TextIO.StreamIO.closeIn (! stream);
                Thread.Thread.setLocal(useFileTag, oldName);
                PolyML.Exception.reraise exn
            )
        end;
        (* Normal termination: close the stream. *)
        TextIO.StreamIO.closeIn (! stream);
        Thread.Thread.setLocal(useFileTag, oldName)

    end (* use *)
 
    local
        open Time
    in
        fun maxTime (x : time, y : time): time = 
            if x < y then y else x
    end

    exception ObjNotFile;
    
    type 'a tag = 'a Universal.tag;
  
    fun splitFilename (name: string) : string * string =
    let
         val {dir, file } = OS.Path.splitDirFile name
    in
         (dir, file)
    end

    (* Make *)
    (* There are three possible states - The object may have been checked,
     it may be currently being compiled, or it may not have been
     processed yet. *)
    datatype compileState = NotProcessed | Searching | Checked;

    fun longName (directory, file) = OS.Path.joinDirFile{dir=directory, file = file}
    
    local
        (* Test whether a file really exists rather than a case-insensitive version. *)
        fun reallyexists fileName =
        let
            open OS.FileSys
            val {dir, file} = OS.Path.splitDirFile fileName
            val dirName = if dir = "" then OS.Path.currentArc else dir
            fun readAll () =
                let
                    val direc = openDir dirName
                    fun read () =
                        case readDir direc of
                            NONE => false
                        |   SOME f => f = file orelse read ()
                in
                    read () before closeDir direc
                end
        in
            (OS.Path.isRoot dir orelse dir = "" orelse reallyexists dir) andalso readAll()
        end

        fun findFileTuple _                   [] = NONE
        |   findFileTuple (directory, object) (suffix :: suffixes) =
        let
            val fileName  = object ^ suffix
            val fileTuple = (directory, fileName)
        in
            if reallyexists(longName fileTuple)
            then SOME fileTuple
            else findFileTuple (directory, object) suffixes
        end
    
    in
        fun filePresent (directory : string, kind: string option, object : string) =
        let
            (* Construct suffixes with the architecture and version number in so
               we can compile architecture- and version-specific code. *)
            val archSuffix = "." ^ String.map Char.toLower (PolyML.architecture())
            val versionSuffix = "." ^ Int.toString Bootstrap.compilerVersionNumber
            val extraSuffixes =
                case kind of
                    NONE => [archSuffix, versionSuffix, ""]
                |   SOME k => ["." ^ k ^ archSuffix, "." ^ k ^ versionSuffix, "." ^ k, archSuffix, versionSuffix, ""]
            val standardSuffixes =
                case kind of
                    SOME "signature" => ".sig" :: ! suffixes
                |   _ => !suffixes
            val addedSuffixes =
                List.foldr(fn (i, l) => (List.map (fn s => s ^ i) extraSuffixes) @ l) [] standardSuffixes
        in
            (* For each of the suffixes in the list try it. *)
            findFileTuple (directory, object) addedSuffixes
        end

        (* See if the corresponding file is there and if it is a directory. *)
        fun testForDirectory (name: string) : bool =
            if reallyexists name
            then OS.FileSys.isDir name handle OS.SysErr _ => false (* No such file. *)
            else false
    end

    (* Time stamps. *)
    type timeStamp = Time.time;
    val firstTimeStamp : timeStamp = Time.zeroTime;
    
    local
        open ProtectedTable
        (* Global tables to hold information about entities that have been made using "make". *)
        val timeStampTable: timeStamp ptable = create()
        and dependencyTable: string list ptable = create()
    in
        (* When was the entity last built?  Returns zeroTime if it hasn't. *)
        fun lastMade (objectName : string) : timeStamp =
            getOpt(lookup timeStampTable objectName, firstTimeStamp)

        (* Get the dependencies as an option type. *)
        val getMakeDependencies = lookup dependencyTable

        (* Set the time stamp and dependencies. *)
        fun updateMakeData(objectName, times, depends) =
        (
            enter timeStampTable (objectName, times);
            enter dependencyTable (objectName, depends)
        )
    end

    (* Main make function *)
    fun make (targetName: string) : unit =
    let
        local
            val sourceDateEpochEnv : string option = OS.Process.getEnv "SOURCE_DATE_EPOCH";
        in
            val sourceDateEpoch : timeStamp option =
                case sourceDateEpochEnv of
                     NONE => NONE
                   | SOME s =>
                       (case LargeInt.fromString s of
                             NONE => NONE
                           | SOME t => SOME(Time.fromSeconds t) handle Time.Time => NONE)
        end;

        (* Get the current time. *)
        val newTimeStamp : unit -> timeStamp = case sourceDateEpoch of
                                                    NONE => Time.now
                                                  | SOME t => fn _ => t;
        (* Get the date of a file. *)
        val fileTimeStamp : string -> timeStamp = case sourceDateEpoch of
                                                    NONE => OS.FileSys.modTime
                                                  | SOME t => fn _ => t;

        (* This serves two purposes. It provides a list of objects which have been
           re-made to prevent them being made more than once, and it also prevents
           circular dependencies from causing infinite loops (e.g. let x = f(x)) *)
            local
                open HashArray;
                val htab : compileState hash = hash 10;
            in
                fun lookupStatus (name: string) : compileState =
                    getOpt(sub (htab, name), NotProcessed);
                  
                fun setStatus (name: string, cs: compileState) : unit =
                    update (htab, name, cs)
            end;

        (* Remove leading directory names to get the name of the object itself.
           e.g. "../compiler/parsetree/gencode" yields simply "gencode". *)
        val (dirName,objectName) = splitFilename targetName;
 
        (* Looks to see if the file is in the current directory. If it is and
           the file is newer than the corresponding object then it must be
           remade. If it is a directory then we attempt to remake the
           directory by compiling the "bind" file. This will only actually be
           executed if it involves some identifier which is newer than the
           result object. *)
        fun remakeObj (objName: string, kind: string option, findDirectory: string option -> string -> string) =
        let
        (* Find a directory that contains this object. An exception will be
             raised if it is not there. *)
            val directory = findDirectory kind objName
            val fullName  =
                if directory = "" (* Work around for bug. *)
                then objName
                else OS.Path.joinDirFile{dir=directory, file=objName}

            val objIsDir  = testForDirectory fullName
            val here      = fullName
      
            (* Look to see if the file exists, possibly with an extension,
               and get the extended version. *)
            val fileTuple =
                let
                    (* If the object is a directory the source is in the bind file. *)
                    val (dir : string, file : string) =
                        if objIsDir
                        then (here,"ml_bind")
                        else (directory, objName);
                in
                    case filePresent (dir, kind, file) of
                        SOME res' => res'
                    |   NONE      => raise Fail ("No such file or directory ("^file^","^dir^")")
                end ;
            
            val fileName = longName fileTuple;

            val newFindDirectory : string option -> string -> string =
                if objIsDir
                then
                let
                    (* Look in this directory then in the ones above. *)
                    fun findDirectoryHere kind (name: string) : string =
                        case filePresent (here, kind, name) of
                          NONE => findDirectory kind name (* not in this directory *)
                        | _    => here;
                in
                    findDirectoryHere
                end
                else findDirectory
    
            (* Compiles a file. *)
            fun remakeCurrentObj () =
            let
                val () = print ("Making " ^ objName ^ "\n");
                local
                    (* Keep a list of the dependencies. *)
                    val deps : bool HashArray.hash = HashArray.hash 10;
                    
                    fun addDep name =
                        if getOpt(HashArray.sub (deps, name), true)
                        then HashArray.update(deps, name, true)
                        else ();
                    
                    (* Called by the compiler to look-up a global identifier. *)
                    fun lookupMakeEnv (globalLook, kind: string option) (name: string) : 'a option =
                    let
                        (* Have we re-declared it ? *)
                        val res = lookupStatus name;
                    in
                        case res of
                            NotProcessed  =>
                            (
                                (* Compile the dependency. *)
                                remakeObj (name, kind, newFindDirectory);
                                (* Add this to the dependencies. *)
                                addDep name
                            )

                        |  Searching => (* In the process of making it *)
                           print("Circular dependency: " ^ name ^  " depends on itself\n")

                        | Checked => addDep name; (* Add this to the dependencies. *)

                        (* There was previously a comment about returning NONE here if
                           we had a problem remaking a dependency. *)
                        globalLook name
                    end (* lookupMakeEnv *)

                    (* Enter the declared value in the table. Usually this will be the
                        target we are making. Also set the state to "Checked". The
                        state is set to checked when we finish making the object but
                        setting it now suppresses messages about circular dependencies
                        if we use the identifier within the file. *)
                    fun enterMakeEnv (kind : string, enterGlobal) (name: string, v: 'a) : unit =
                    (
                        (* Put in the value. *)
                        enterGlobal (name, v);
                        print ("Created " ^ kind ^ " " ^ name ^ "\n");
                        
                        (* The name we're declaring may appear to be a dependency
                           but isn't, so don't include it in the list. *)
                        HashArray.update (deps, name, false);
                        
                        if name = objName
                        then
                        let
                            (* Put in the dependencies i.e. those names set to true in the table. *)
                            val depends =
                                HashArray.fold (fn (s, v, l) => if v then s :: l else l) [] deps;
                            
                            (* Put in a time stamp for the new object.  We need to make
                               sure that it is no older than the newest object it depends
                               on.  In theory that should not be a problem but clocks on
                               different machines can get out of step leading to objects
                               made later having earlier time stamps. *)
                            val newest =
                                List.foldl (fn (s: string, t: timeStamp) =>
                                    maxTime (lastMade s, t)) (fileTimeStamp fileName) depends;
                            
                            val timeStamp = maxTime(newest, newTimeStamp());
                        in         
                            setStatus (name, Checked);
                            updateMakeData(name, timeStamp, depends)
                        end
                        else ()
                    ) (* enterMakeEnv *);
     
                in
                    val makeEnv =
                        { 
                            lookupFix    = #lookupFix globalNameSpace,
                            lookupVal    = #lookupVal globalNameSpace,
                            lookupType   = #lookupType globalNameSpace,
                            lookupSig    = lookupMakeEnv (#lookupSig globalNameSpace, SOME "signature"),
                            lookupStruct = lookupMakeEnv (#lookupStruct globalNameSpace, SOME "structure"),
                            lookupFunct  = lookupMakeEnv (#lookupFunct globalNameSpace, SOME "functor"),
                            enterFix     = #enterFix globalNameSpace,
                            enterVal     = #enterVal globalNameSpace,
                            enterType    = #enterType globalNameSpace,
                            enterStruct  = enterMakeEnv ("structure", #enterStruct globalNameSpace),
                            enterSig     = enterMakeEnv ("signature", #enterSig globalNameSpace),
                            enterFunct   = enterMakeEnv ("functor", #enterFunct globalNameSpace),
                            allFix       = #allFix globalNameSpace,
                            allVal       = #allVal globalNameSpace,
                            allType      = #allType globalNameSpace,
                            allSig       = #allSig globalNameSpace,
                            allStruct    = #allStruct globalNameSpace,
                            allFunct     = #allFunct globalNameSpace
                        };
                end; (* local for makeEnv *)

                val inputFile = OS.Path.joinDirFile{dir= #1 fileTuple, file= #2 fileTuple}
    
                val inStream = TextIO.openIn inputFile;
    
                val () =
                let (* scope of exception handler to close inStream *)
                    val endOfStream = ref false;
                    val lineNo     = ref 1;
        
                    fun getChar () : char option =
                        case TextIO.input1 inStream of
                            NONE => (endOfStream := true; NONE) (* End of file *)
                        |   eoln as SOME #"\n" => (lineNo := !lineNo + 1; eoln)
                        |   c => c
                 in
                    while not (!endOfStream) do
                    let
                        val code = polyCompiler(getChar,
                            [CPNameSpace makeEnv, CPFileName fileName, CPLineNo(fn () => !lineNo)])
                    in
                        code ()
                            handle exn as Fail _ => PolyML.Exception.reraise exn
                            |  exn =>
                            (
                                print ("Exception- " ^ exnMessage exn ^ " raised\n");
                                PolyML.Exception.reraise exn
                            )
                    end
                end (* body of scope of inStream *)
                    handle exn => (* close inStream if an error occurs *)
                    (
                        TextIO.closeIn inStream;
                        PolyML.Exception.reraise exn
                    )
            in (* remake normal termination *)
                TextIO.closeIn inStream 
            end (* remakeCurrentObj *)
            
        in (* body of remakeObj *)
            setStatus (objName, Searching);
         
             (* If the file is newer than the object then we definitely must remake it.
               Otherwise we look at the dependency list and check those. If the result
               of that check is that one of the dependencies is newer than the object
               (probably because it has just been recompiled) we have to recompile
               the file. Compiling a file also checks the dependencies and recompiles
               them, generating a new dependency list. That is why we don't check the
               dependency list if the object is out of date with the file. Also if the
               file has been changed it may no longer depend on the things it used to
               depend on. *)
 
            let
                val objDate = lastMade objName
       
                fun maybeRemake (s:string) : unit =
                case lookupStatus s of
                    NotProcessed => (* see if it's a file. *)
                        (* Compile the dependency. *)
                        remakeObj(s, kind, newFindDirectory)
                    
                    | Searching => (* In the process of making it *)
                        print ("Circular dependency: " ^ s ^ " depends on itself\n")
                    
                    |  Checked => () (* do nothing *)

                open Time
                (* Process each entry and return true if
                   any is newer than the target. *)
                val processChildren =
                    List.foldl
                    (fn (child:string, parentNeedsMake:bool) =>
                        (
                            maybeRemake child;
                            (* Find its date and see if it is newer. *)
                            parentNeedsMake orelse lastMade child > objDate
                        )
                    )
                    false;
            in
                if objDate < fileTimeStamp fileName orelse
                    (
                        (* Get the dependency list. There may not be one if
                           this object has not been compiled with "make". *) 
                        case getMakeDependencies objName of
                            SOME d => processChildren d
                        |   NONE => true (* No dependency list - must use "make" on it. *)
                    )       
                then remakeCurrentObj ()
                else ()
            end;

            (* Mark it as having been checked. *)
            setStatus (objName, Checked)
        end (* body of remakeObj *)
  
        (* If the object is not a file mark it is checked. It may be a
           pervasive or it may be missing. In either case mark it as checked
           to save searching for it again. *)
        handle
                ObjNotFile => setStatus (objName, Checked)
            
            |   exn => (* Compilation (or execution) error. *)
                (
                    (* Mark as checked to prevent spurious messages. *)
                    setStatus (objName, Checked);
                    raise exn
                )
    in (*  body of make *)
        (* Check that the target exists. *)
        case filePresent (dirName, NONE, objectName) of
            NONE =>
            let
                val dir =
                    if dirName = "" then ""
                    else " (directory "^dirName^")";
                val s = "File "^objectName^" not found" ^ dir
            in
                print (s ^ "\n");
                raise Fail s
            end
        
        | _ =>
        let
            val targetIsDir = testForDirectory targetName;
            
            (* If the target we are making is a directory all the objects
               must be in the directory. If it is a file we allow references
               to other objects in the same directory. Objects not found must
               be pervasive. *)
            fun findDirectory kind (s: string) : string =
                if (not targetIsDir orelse s = objectName) andalso
                    isSome(filePresent(dirName, kind, s))
                then dirName
                else raise ObjNotFile;
        in
            remakeObj (objectName, NONE, findDirectory)
                handle exn  => 
                (
                    print (targetName ^ " was not declared\n");
                    PolyML.Exception.reraise exn
                )
        end
    end (* make *)

in
    structure PolyML =
    struct
        open PolyML
        (* We must not have a signature on the result otherwise print and makestring
           will be given polymorphic types and will only produce "?" *)

        val globalNameSpace = globalNameSpace

        val use = use and make = make
        val suffixes = suffixes and getUseFileName = getUseFileName
        val compiler = polyCompiler

        val prettyPrintWithIDEMarkup = prettyPrintWithIDEMarkup

        structure Compiler =
        struct
            datatype compilerParameters = datatype compilerParameters

            val compilerVersion = Bootstrap.compilerVersion
            val compilerVersionNumber = Bootstrap.compilerVersionNumber

            val forgetSignature: string -> unit = forgetSig
            and forgetStructure: string -> unit = forgetStruct
            and forgetFunctor: string -> unit = forgetFunct
            and forgetValue: string -> unit = forgetVal
            and forgetType: string -> unit = forgetType
            and forgetFixity: string -> unit = forgetFix

            fun signatureNames (): string list = #1(ListPair.unzip (#allSig globalNameSpace ()))
            and structureNames (): string list = #1(ListPair.unzip (#allStruct globalNameSpace ()))
            and functorNames (): string list = #1(ListPair.unzip (#allFunct globalNameSpace ()))
            and valueNames (): string list = #1(ListPair.unzip (#allVal globalNameSpace ()))
            and typeNames (): string list = #1(ListPair.unzip (#allType globalNameSpace ()))
            and fixityNames (): string list = #1(ListPair.unzip (#allFix globalNameSpace ()))

            val prompt1 = prompt1 and prompt2 = prompt2
            and timing = timing and printDepth = printDepth
            and errorDepth = errorDepth and lineLength = lineLength
            and allocationProfiling = allocationProfiling
            
            val assemblyCode = assemblyCode and codetree = codetree
            and codetreeAfterOpt = codetreeAfterOpt and icode = icode
            and parsetree = parsetree and reportUnreferencedIds = reportUnreferencedIds
            and lowlevelOptimise = lowlevelOptimise and reportExhaustiveHandlers = reportExhaustiveHandlers
            and narrowOverloadFlexRecord = narrowOverloadFlexRecord and compilerDebug = compilerDebug
            and createPrintFunctions = createPrintFunctions
            and reportDiscardFunction = reportDiscardFunction
            and reportDiscardNonUnit = reportDiscardNonUnit
            
            val debug = debug
            val inlineFunctors = inlineFunctors
            val maxInlineSize = maxInlineSize
            val printInAlphabeticalOrder = printInAlphabeticalOrder
            val traceCompiler = traceCompiler
        end
        
        (* Debugger control.  Extend DebuggerInterface set up by INITIALISE.  Replaces the original DebuggerInterface. *)
        structure DebuggerInterface:
        sig
            type debugState
            val debugFunction: debugState -> string
            val debugFunctionArg: debugState -> PolyML.NameSpace.Values.value
            val debugFunctionResult: debugState -> PolyML.NameSpace.Values.value
            val debugLocation: debugState -> PolyML.location
            val debugNameSpace: debugState -> PolyML.NameSpace.nameSpace
            val debugLocalNameSpace: debugState -> PolyML.NameSpace.nameSpace
            val debugState: Thread.Thread.thread -> debugState list
            
            val setOnBreakPoint: (PolyML.location * bool ref -> unit) option -> unit
            val setOnEntry: (string * PolyML.location -> unit) option -> unit
            val setOnExit: (string * PolyML.location -> unit) option -> unit
            val setOnExitException: (string * PolyML.location -> exn -> unit) option -> unit
        end =
        struct
            open PolyML.DebuggerInterface
 
            fun debugState(t: Thread.Thread.thread): debugState list =
            let
                val stack = RunCall.loadWord(t, 0w5)
                and static = RunCall.loadWord(t, 0w6)
                and dynamic = RunCall.loadWord(t, 0w7)
                and locationInfo = RunCall.loadWord(t, 0w8)

                (* Turn the chain of saved entries along with the current top entry
                   into a list.  The bottom entry will generally be the state from
                   non-debugging code and needs to be filtered out. *)
                fun toList r =
                    if RunCall.isShort r
                    then []
                    else
                    let
                        val s = RunCall.loadWordFromImmutable(r, 0w0)
                        and d = RunCall.loadWordFromImmutable(r, 0w1)
                        and l = RunCall.loadWordFromImmutable(r, 0w2)
                        and n = RunCall.loadWordFromImmutable(r, 0w3)
                    in
                        if RunCall.isShort s orelse
                           RunCall.isShort l
                        then toList n
                        else (s, d, l) :: toList n
                    end
            in
                if RunCall.isShort static orelse RunCall.isShort locationInfo
                then toList stack
                else (static, dynamic, locationInfo) :: toList stack
            end

            fun searchEnvs match (staticEntry :: statics, dlist as dynamicEntry :: dynamics) =
            (
                case (match (staticEntry, dynamicEntry), staticEntry) of
                    (SOME result, _) => SOME result
                |   (NONE, EnvTypeid _) => searchEnvs match (statics, dynamics)
                |   (NONE, EnvVConstr _) => searchEnvs match (statics, dynamics)
                |   (NONE, EnvValue _) => searchEnvs match (statics, dynamics)
                |   (NONE, EnvException _) => searchEnvs match (statics, dynamics)
                |   (NONE, EnvStructure _) => searchEnvs match (statics, dynamics)
                |   (NONE, EnvStartFunction _) => searchEnvs match (statics, dynamics)
                |   (NONE, EnvEndFunction _) => searchEnvs match (statics, dynamics)
                        (* EnvTConstr doesn't have an entry in the dynamic list *)
                |   (NONE, EnvTConstr _) => searchEnvs match (statics, dlist)
        
            )
    
            |   searchEnvs _ _ = NONE
            (* N.B.  It is possible to have ([EnvTConstr ...], []) in the arguments so we can't assume
               that if either the static or dynamic list is nil and the other non-nil it's an error. *)

            (* Function argument.  This should always be present but if
               it isn't just return unit.  That's probably better than
               an exception here. *)
            fun debugFunctionArg (state: debugState as (cList, rList, _)) =
            let
                val d = (cList, rList)
                fun match (EnvStartFunction(_, _, ty), valu) =
                    SOME(makeAnonymousValue state (ty, valu))
                |   match _ = NONE
            in
                getOpt(searchEnvs match d, unitValue) 
            end

            (* Function result - only valid in exit function. *)
            and debugFunctionResult (state: debugState as (cList, rList, _)) =
            let
                val d = (cList, rList)
                fun match (EnvEndFunction(_, _, ty), valu) =
                    SOME(makeAnonymousValue state(ty, valu))
                |   match _ = NONE
            in
                getOpt(searchEnvs match d, unitValue)
            end

            (* debugFunction just looks at the static data.
               There should always be an EnvStartFunction entry. *)
            fun debugFunction ((cList, _, _): debugState): string =
            (
                case List.find(fn (EnvStartFunction _) => true | _ => false) cList of
                    SOME(EnvStartFunction(s, _, _)) => s
                |   _ => "?"
            )

            fun debugLocation ((_, _, locn): debugState) = locn

            fun nameSpace localOnly (state: debugState as (clist, rlist, _)) : nameSpace =
            let
                val debugEnviron = (clist, rlist)

                (* Lookup and "all" functions for the environment.  We can't easily use a general
                   function for the lookup because we have dynamic entries for values and structures
                   but not for type constructors. *)
                fun lookupValues (EnvValue(name, ty, location) :: ntl, valu :: vl) s =
                        if name = s
                        then SOME(makeValue state (name, ty, location, valu))
                        else lookupValues(ntl, vl) s

                |   lookupValues (EnvException(name, ty, location) :: ntl, valu :: vl) s =
                        if name = s
                        then SOME(makeException state (name, ty, location, valu))
                        else lookupValues(ntl, vl) s

                |   lookupValues (EnvVConstr(name, ty, nullary, count, location) :: ntl, valu :: vl) s =
                        if name = s
                        then SOME(makeConstructor state (name, ty, nullary, count, location, valu))
                        else lookupValues(ntl, vl) s

                |   lookupValues (EnvTConstr _ :: ntl, vl) s = lookupValues(ntl, vl) s
                
                |   lookupValues (EnvStartFunction _ :: ntl, _ :: vl) s =
                        if localOnly then NONE else lookupValues(ntl, vl) s
        
                |   lookupValues (_ :: ntl, _ :: vl) s = lookupValues(ntl, vl) s

                |   lookupValues _ _ =
                     (* The name we are looking for isn't in
                        the environment.
                        The lists should be the same length. *)
                     NONE

                fun allValues (EnvValue(name, ty, location) :: ntl, valu :: vl) =
                        (name, makeValue state (name, ty, location, valu)) :: allValues(ntl, vl)

                |   allValues (EnvException(name, ty, location) :: ntl, valu :: vl) =
                        (name, makeException state (name, ty, location, valu)) :: allValues(ntl, vl)

                |   allValues (EnvVConstr(name, ty, nullary, count, location) :: ntl, valu :: vl) =
                        (name, makeConstructor state (name, ty, nullary, count, location, valu)) :: allValues(ntl, vl)

                |   allValues (EnvTConstr _ :: ntl, vl) = allValues(ntl, vl)

                |   allValues (EnvStartFunction _ :: ntl, _ :: vl) =
                        if localOnly then [] else allValues(ntl, vl)

                |   allValues (_ :: ntl, _ :: vl) = allValues(ntl, vl)
                |   allValues _ = []

                fun lookupTypes (EnvTConstr (name, tCons) :: ntl, vl) s =
                        if name = s
                        then SOME (makeTypeConstr state tCons)
                        else lookupTypes(ntl, vl) s

                |   lookupTypes (EnvStartFunction _ :: ntl, _ :: vl) s =
                        if localOnly then NONE else lookupTypes(ntl, vl) s

                |   lookupTypes (_ :: ntl, _ :: vl) s = lookupTypes(ntl, vl) s
                |   lookupTypes _ _ = NONE

                fun allTypes (EnvTConstr(name, tCons) :: ntl, vl) =
                        (name, makeTypeConstr state tCons) :: allTypes(ntl, vl)
                |   allTypes (EnvStartFunction _ :: ntl, _ :: vl) =
                        if localOnly then [] else allTypes(ntl, vl)
                |   allTypes (_ :: ntl, _ :: vl) = allTypes(ntl, vl)
                |   allTypes _ = []

                fun lookupStructs (EnvStructure (name, rSig, locations) :: ntl, valu :: vl) s =
                        if name = s
                        then SOME(makeStructure state (name, rSig, locations, valu))
                        else lookupStructs(ntl, vl) s

                |   lookupStructs (EnvTConstr _ :: ntl, vl) s = lookupStructs(ntl, vl) s

                |   lookupStructs (EnvStartFunction _ :: ntl, _ :: vl) s =
                        if localOnly then NONE else lookupStructs(ntl, vl) s
                |   lookupStructs (_ :: ntl, _ :: vl) s = lookupStructs(ntl, vl) s
                |   lookupStructs _ _ = NONE

                fun allStructs (EnvStructure (name, rSig, locations) :: ntl, valu :: vl) =
                        (name, makeStructure state (name, rSig, locations, valu)) :: allStructs(ntl, vl)

                |   allStructs (EnvTypeid _ :: ntl, _ :: vl) = allStructs(ntl, vl)
                |   allStructs (EnvStartFunction _ :: ntl, _ :: vl) =
                        if localOnly then [] else allStructs(ntl, vl)
                |   allStructs (_ :: ntl, vl) = allStructs(ntl, vl)
                |   allStructs _ = []

                (* We have a full environment here for future expansion but at
                   the moment only some of the entries are used. *)
                fun noLook _ = NONE
                and noEnter _ = raise Fail "Cannot update this name space"
                and allEmpty _ = []
           in
               {
                    lookupVal = lookupValues debugEnviron,
                    lookupType = lookupTypes debugEnviron,
                    lookupFix = noLook,
                    lookupStruct = lookupStructs debugEnviron,
                    lookupSig = noLook, lookupFunct = noLook, enterVal = noEnter,
                    enterType = noEnter, enterFix = noEnter, enterStruct = noEnter,
                    enterSig = noEnter, enterFunct = noEnter,
                    allVal = fn () => allValues debugEnviron,
                    allType = fn () => allTypes debugEnviron,
                    allFix = allEmpty,
                    allStruct = fn () => allStructs debugEnviron,
                    allSig = allEmpty,
                    allFunct = allEmpty }
            end

            val debugNameSpace = nameSpace false and debugLocalNameSpace = nameSpace true
        end

        local
            open DebuggerInterface

            fun debugLocation(d: debugState): string * PolyML.location =
                (debugFunction d, DebuggerInterface.debugLocation d)

            fun getStack() = debugState(Thread.Thread.self())
            (* These are only relevant when we are stopped at the debugger but
               we need to use globals here so that the debug functions such
               as "variables" and "continue" will work. *)
            val inDebugger = ref false
            (* Current stack and debug level. *)
            val currentStack = ref []
            fun getCurrentStack() =
                if !inDebugger then !currentStack else raise Fail "Not stopped in debugger"
            val debugLevel = ref 0
            (* Set to true to exit the debug loop.  Set by commands such as "continue". *)
            val exitLoop = ref false
            (* Exception packet sent if this was continueWithEx. *)
            val debugExPacket: exn option ref = ref NONE

            (* Call tracing. *)
            val tracing = ref false
            val breakNext = ref false
            (* Single stepping. *)
            val stepDebug = ref false
            val stepDepth = ref ~1 (* Only break at a stack size less than this. *)
            (* Break points.  We have three breakpoint lists: a list of file-line
               pairs, a list of function names and a list of exceptions. *)
            val lineBreakPoints = ref []
            and fnBreakPoints = ref []
            and exBreakPoints = ref []

            fun checkLineBreak (file, line) =
                let
                    fun findBreak [] = false
                     |  findBreak ((f, l) :: rest) =
                          (l = line andalso f = file) orelse findBreak rest
                in
                    findBreak (! lineBreakPoints)
                end

            fun checkFnBreak exact name =
            let
                (* When matching a function name we allow match if the name
                   we're looking for matches the last component of the name
                   we have.  e.g. if we set a break for "f" we match F().S.f . *)
                fun matchName n =
                    if name = n then true
                    else if exact then false
                    else
                    let
                        val nameLen = size name
                        and nLen = size n
                        fun isSeparator #"-" = true
                         |  isSeparator #")" = true
                         |  isSeparator #"." = true
                         |  isSeparator _    = false
                    in
                        nameLen > nLen andalso String.substring(name, nameLen - nLen, nLen) = n
                        andalso isSeparator(String.sub(name, nameLen - nLen - 1))
                    end
            in
                List.exists matchName (! fnBreakPoints)
            end

            (* Get the exception id from an exception packet.  The id is
               the first word in the packet.  It's a mutable so treat it
               as an int ref here.  The packet, though, is immutable. *)
            fun getExnId(ex: exn): int ref = RunCall.loadWordFromImmutable (ex, 0w0)

            fun checkExnBreak(ex: exn) =
                let val exnId = getExnId ex in List.exists (fn n => n = exnId) (! exBreakPoints) end

            fun getArgResult stack get =
                case stack of
                    hd :: _ => Values.print(get hd, FixedInt.fromInt(!printDepth))
                |   _ => PrettyString "?"

            fun printTrace (funName, location, stack, argsAndResult) =
            let
                (* This prints a block with the argument and, if we're exiting the result.
                   The function name is decorated with the location.
                   TODO: This works fine so long as the recursion depth is not too deep
                   but once it gets too wide the pretty-printer starts breaking the lines. *)
                val block =
                    PrettyBlock(0, false, [],
                        [
                            PrettyBreak(FixedInt.fromInt(length stack), 0),
                            PrettyBlock(0, false, [],
                            [
                                PrettyBlock(0, false, [ContextLocation location], [PrettyString funName]),
                                PrettyBreak(1, 3)
                            ] @ argsAndResult)
                        ])
            in
                prettyPrintWithOptionalMarkup (TextIO.print, !lineLength) block
            end

            (* Try to print the appropriate line from the file.*)
            fun printSourceLine(prefix, fileName: string, line: FixedInt.int, funName: string, justLocation) =
            let
                open TextIO
                open PolyML
                (* Use the pretty printer here because that allows us to provide a link to the
                   function in the markup so the IDE can go straight to it. *)
                val prettyOut = prettyPrintWithOptionalMarkup (printOut, !lineLength)
                val lineInfo =
                    concat(
                        [prefix] @
                        (if fileName = "" then [] else [fileName, " "]) @
                        (if line = 0 then [] else [" line:", FixedInt.toString line, " "]) @
                        ["function:", funName])
            in
                (* First just print where we are. *)
                prettyOut(
                    PrettyBlock(0, true,
                        [ContextLocation{file=fileName,startLine=line, endLine=line,startPosition=0,endPosition=0}],
                        [PrettyString lineInfo]));
                (* Try to print it.  This may fail if the file name was not a full path
                   name and we're not in the correct directory. *)
                if justLocation orelse fileName = "" then ()
                else
                let
                    val fd = openIn fileName
                    fun pLine n =
                        case inputLine fd of
                            NONE => ()
                        |   SOME s => if n = 1 then printOut s else pLine(n-1)
                in
                    pLine line;
                    closeIn fd
                end handle IO.Io _ => () (* If it failed simply ignore the error. *)
            end

            (* These functions are installed as global callbacks if necessary. *)
            fun onEntry (funName, location as {file, startLine, ...}: PolyML.location) =
            (
                if ! tracing
                then
                let
                    val stack = getStack()
                    val arg = getArgResult stack debugFunctionArg
                in
                    printTrace(funName, location, stack, [arg])
                end
                else ();
                (* We don't actually break here because at this stage we don't
                   have any variables declared. *)
                (* TODO: If for whatever reason we fail to find the breakpoint we need to cancel
                   the pending break in the exit code.  Otherwise we could try and break
                   in some other code. *)
                if checkLineBreak (file, startLine) orelse checkFnBreak false funName
                then (breakNext := true; setOnBreakPoint(SOME onBreakPoint))
                else ()
            )
            
            and onExit (funName, location) =
            (
                if ! tracing
                then
                let
                    val stack = getStack()
                    val arg = getArgResult stack debugFunctionArg
                    val res = getArgResult stack debugFunctionResult
                in
                    printTrace(funName, location, stack,
                        [arg, PrettyBreak(1, 3), PrettyString "=", PrettyBreak(1, 3), res])
                end
                else ()
            )

            and onExitException(funName, location) exn =
            (
                if ! tracing
                then
                let
                    val stack = getStack()
                    val arg = getArgResult stack debugFunctionArg
                in
                    printTrace(funName, location, stack,
                        [arg, PrettyBreak(1, 3), PrettyString "=", PrettyBreak(1, 3),
                         PrettyString "raised", PrettyBreak(1, 3), PrettyString(exnName exn)])
                end
                else ();
                if checkExnBreak exn
                then enterDebugger ()
                else ()
            )
            
            and onBreakPoint({file, startLine, ...}: PolyML.location, _) =
            (
                if (!stepDebug andalso (!stepDepth < 0 orelse List.length(getStack()) <= !stepDepth)) orelse
                   checkLineBreak (file, startLine) orelse ! breakNext
                then enterDebugger ()
                else () 
            )
            
            (* Set the callbacks when beginning to run some code. *)
            and setCallBacks () =
            (
                setOnEntry(if !tracing orelse not(null(! fnBreakPoints)) then SOME onEntry else NONE);
                setOnExit(if !tracing then SOME onExit else NONE);
                setOnExitException(if !tracing orelse not(null(! exBreakPoints)) then SOME onExitException else NONE);
                setOnBreakPoint(if !tracing orelse ! stepDebug orelse not(null(! lineBreakPoints)) then SOME onBreakPoint else NONE)
            )
            
            (* Clear all callbacks when exiting debuggable code. *)
            and clearCallBacks () =
            (
                setOnEntry NONE;
                setOnExit NONE;
                setOnExitException NONE;
                setOnBreakPoint NONE;
                (* Clear all stepping. *)
                breakNext := false;
                stepDebug := false;
                stepDepth := ~1;
                (* Clear the debugger state *)
                debugLevel := 0;
                currentStack := []
            )

            and enterDebugger () =
            let
                (* Clear the onXXX functions to prevent any recursion. *)
                val () = clearCallBacks ()
                val () = inDebugger := true
                (* Remove any type-ahead. *)
                fun flushInput () =
                    case TextIO.canInput(TextIO.stdIn, 1) of
                        SOME 1 => (TextIO.inputN(TextIO.stdIn, 1); flushInput())
                    |   _ => ()
                val () = flushInput ()

                val () = exitLoop := false
                (* Save the stack on entry.  If we execute any code with
                   debugging enabled while we're in the debugger we could
                   change this. *)
                val () = currentStack := getStack()

                val () =
                    case !currentStack of
                        hd :: _ =>
                            let
                                val (funName, {file, startLine, ...}) = debugLocation hd
                            in
                                printSourceLine("", file, startLine, funName, false)
                            end
                    |   [] => () (* Shouldn't happen. *)

                val compositeNameSpace =
                (* Compose any debugEnv with the global environment.  Create a new temporary environment
                   to contain any bindings made within the shell.  They are discarded when we continue
                   from the break-point.  Previously, bindings were made in the global environment but
                   that is problematic.  It is possible to capture local types in the bindings which
                   could actually be different at the next breakpoint. *)
                let
                    val fixTab = ProtectedTable.create() and sigTab = ProtectedTable.create()
                    and valTab = ProtectedTable.create() and typTab = ProtectedTable.create()
                    and fncTab = ProtectedTable.create() and strTab = ProtectedTable.create()
                    (* The debugging environment depends on the currently selected stack frame. *)
                    fun debugEnv() = debugNameSpace (List.nth(!currentStack, !debugLevel))
                    fun dolookup f t s =
                        case ProtectedTable.lookup t s of NONE => (case f (debugEnv()) s of NONE => f globalNameSpace s | v => v) | v => v
                    fun getAll f t () = ProtectedTable.all t () @ f (debugEnv()) () @ f globalNameSpace ()
                in
                    {
                    lookupFix    = dolookup #lookupFix fixTab,
                    lookupSig    = dolookup #lookupSig sigTab,
                    lookupVal    = dolookup #lookupVal valTab,
                    lookupType   = dolookup #lookupType typTab,
                    lookupFunct  = dolookup #lookupFunct fncTab,
                    lookupStruct = dolookup #lookupStruct strTab,
                    enterFix     = ProtectedTable.enter fixTab,
                    enterSig     = ProtectedTable.enter sigTab,
                    enterVal     = ProtectedTable.enter valTab,
                    enterType    = ProtectedTable.enter typTab,
                    enterFunct   = ProtectedTable.enter fncTab,
                    enterStruct  = ProtectedTable.enter strTab,
                    allFix       = getAll #allFix fixTab,
                    allSig       = getAll #allSig sigTab,
                    allVal       = getAll #allVal valTab,
                    allType      = getAll #allType typTab,
                    allFunct     = getAll #allFunct fncTab,
                    allStruct    = getAll #allStruct strTab
                    }
                end
            in
                topLevel
                    { isDebug = true, nameSpace = compositeNameSpace, exitLoop = fn _ => ! exitLoop,
                      exitOnError = false, isInteractive = true,
                      (* Don't enable debugging for anything run within the debug level. *)
                      startExec = fn () => (), endExec = fn () => () }
                      (* If we type control-C to the debugger we exit it and
                         raise Interrupt within the debuggee without re-enabling
                         any breakpoints. *)
                    handle exn => (inDebugger := false; raise exn);

                inDebugger := false;
                setCallBacks(); (* Re-enable debugging. *)

                (* If this was continueWithEx raise the exception. *)
                case ! debugExPacket of
                    NONE => ()
                |   SOME exn => (debugExPacket := NONE; raise exn)
            end
        in
            (* Normal, non-debugging top-level loop. *)
            fun shell () =
            let
                val argList = CommandLine.arguments()
                fun switchOption option = List.exists(fn s => s = option) argList
                (* Generate mark-up in IDE code when printing if the option has been given
                   on the command line. *)
                val () = useMarkupInOutput := switchOption "--with-markup"
                val exitOnError = switchOption"--error-exit"
                val interactive =
                    switchOption "-i" orelse
                    let
                        open TextIO OS
                        open StreamIO TextPrimIO IO
                        val s = getInstream stdIn
                        val (r, v) = getReader s
                        val RD { ioDesc, ...} = r
                    in
                        setInstream(stdIn, mkInstream(r,v));
                        case ioDesc of
                            SOME io => (kind io = Kind.tty handle SysErr _ => false)
                        |   _  => false
                    end
            in
                topLevel
                    { isDebug = false, nameSpace = globalNameSpace, exitLoop = fn _ => false,
                      isInteractive = interactive, exitOnError = exitOnError,
                      startExec = setCallBacks, endExec = clearCallBacks }
            end

            structure Debug =
            struct
                (* Functions that are only relevant when called from the debugger.  These
                   check the debugging state using getCurrentStack which raises an
                   exception if we're not in the debugger. *)
                (* "step" causes the debugger to be entered on the next call.
                   "stepOver" enters the debugger on the next call when the stack is no larger
                   than it is at present.
                   "stepOut" enters the debugger on the next call when the stack is smaller
                   than it is at present. *)
                fun step () = 
                let
                    val _ = getCurrentStack()
                in
                    stepDebug := true; stepDepth := ~1; exitLoop := true
                end

                and stepOver() =
                let
                    val stack = getCurrentStack()
                in
                    stepDebug := true; stepDepth := List.length stack; exitLoop := true
                end

                and stepOut() =
                let
                    val stack = getCurrentStack()
                in
                    stepDebug := true; stepDepth := List.length stack - 1; exitLoop := true
                end

                and continue () =
                let
                    val _ = getCurrentStack()
                in
                    stepDebug := false; stepDepth := ~1; exitLoop := true
                end

                and continueWithEx exn =
                let
                    val _ = getCurrentStack()
                in
                    stepDebug := false; stepDepth := ~1; exitLoop := true; debugExPacket := SOME exn
                end

                (* Stack traversal. *)
                fun up () =
                let
                    val stack = getCurrentStack()
                in
                    if !debugLevel < List.length stack -1
                    then
                    let
                        val _ = debugLevel := !debugLevel + 1;
                        val (funName, {startLine, file, ...}) =
                            debugLocation(List.nth(stack, !debugLevel))
                    in
                        printSourceLine("", file, startLine, funName, false)
                    end
                    else TextIO.print "Top of stack.\n"
                end
        
                and down () =
                let
                    val stack = getCurrentStack()
                in
                    if !debugLevel = 0
                    then TextIO.print "Bottom of stack.\n"
                    else
                    let
                        val () = debugLevel := !debugLevel - 1;
                        val (funName, {startLine, file, ...}) =
                            debugLocation(List.nth(stack, !debugLevel))
                    in
                        printSourceLine("", file, startLine, funName, false)
                    end
                end

                (* Just print the functions without any other context. *)
                fun stack () : unit =
                let
                    fun printTrace(d, n) =
                    let
                        val (funName, {file, startLine, ...}) = debugLocation d
                        (* If this is the current level prefix it with > *)
                        val prefix = if n = !debugLevel then "> " else "  "
                    in
                        printSourceLine(prefix, file, startLine, funName, true);
                        n+1
                    end
                in
                    ignore (List.foldl printTrace 0 (getCurrentStack()))
                end

                local
                    fun printVal v =
                        prettyPrintWithOptionalMarkup(TextIO.print, !lineLength)
                            (NameSpace.Values.printWithType(v, FixedInt.fromInt(!printDepth), SOME globalNameSpace))
                    fun printStack (stack: debugState) =
                        List.app (fn (_,v) => printVal v) (#allVal (debugNameSpace stack) ())
                in
                    (* Print all variables at the current level. *)
                    fun variables() =
                        printStack (List.nth(getCurrentStack(), !debugLevel))
                    (* Print all the levels. *)
                    and dump() =
                    let
                        fun printLevel stack =
                        let
                            val (funName, _) = debugLocation stack
                        in
                            TextIO.print(concat["Function ", funName, ":"]);
                            printStack stack;
                            TextIO.print "\n"
                        end
                    in
                        List.app printLevel (getCurrentStack())
                    end
                    (* Print local variables at the current level. *)
                    and locals() =
                    let
                        val stack = List.nth(getCurrentStack(), !debugLevel)
                    in
                        List.app (fn (_,v) => printVal v) (#allVal (debugLocalNameSpace stack) ())
                    end
                end

                (* Functions to adjust tracing and breakpointing.  May be called
                   either within or outside the debugger. *)
                fun trace b = tracing := b

                fun breakAt (file, line) =
                    if checkLineBreak(file, line) then () (* Already there. *)
                    else lineBreakPoints := (file, line) :: ! lineBreakPoints
        
                fun clearAt (file, line) =
                let
                    fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                     |  findBreak ((f, l) :: rest) =
                          if l = line andalso f = file
                          then rest else (f, l) :: findBreak rest
                in
                    lineBreakPoints := findBreak (! lineBreakPoints)
                end
         
                fun breakIn name =
                    if checkFnBreak true name then () (* Already there. *)
                    else fnBreakPoints := name :: ! fnBreakPoints
        
                fun clearIn name =
                let
                    fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                     |  findBreak (n :: rest) =
                          if name = n then rest else n :: findBreak rest
                in
                    fnBreakPoints := findBreak (! fnBreakPoints)
                end

                fun breakEx exn =
                    if checkExnBreak exn then  () (* Already there. *)
                    else exBreakPoints := getExnId exn :: ! exBreakPoints

                fun clearEx exn =
                let
                    val exnId = getExnId exn
                    fun findBreak [] = (TextIO.print "No such breakpoint.\n"; [])
                     |  findBreak (n :: rest) =
                          if exnId = n then rest else n :: findBreak rest
                in
                    exBreakPoints := findBreak (! exBreakPoints)
                end

            end
        end
        
        structure CodeTree =
        struct
            open PolyML.CodeTree
            (* Add options to the code-generation phase. *)
            val genCode =
                fn (code, numLocals) =>
                let
                    open Bootstrap Bootstrap.Universal
                    val compilerOut = prettyPrintWithOptionalMarkup(TextIO.print, !lineLength)
                in
                    genCode(code,
                        [
                            tagInject compilerOutputTag compilerOut,
                            tagInject maxInlineSizeTag (FixedInt.fromInt(! maxInlineSize)),
                            tagInject codetreeTag (! codetree),
                            tagInject icodeTag (! icode),
                            tagInject lowlevelOptimiseTag (! lowlevelOptimise),
                            tagInject assemblyCodeTag (! assemblyCode),
                            tagInject codetreeAfterOptTag (! codetreeAfterOpt),
                            tagInject compilerDebugTag (! compilerDebug)
                        ], numLocals)
                end
        end

        (* Original print_depth etc functions. *)
        fun timing      b = Compiler.timing := b
        and print_depth i = Compiler.printDepth := i
        and error_depth i = Compiler.errorDepth := i
        and line_length i = Compiler.lineLength := i

        (* Legacy exception_trace. *)
        structure Exception =
        struct
            open Exception
            fun exception_trace f = f() (* Backwards compatibility *)
        end
        
        (* Include it in the PolyML structure for backwards compatibility. *)
        val exception_trace = Exception.exception_trace

        local
            val systemProfile : int -> (int * string) list =
                RunCall.rtsCallFull1 "PolyProfiling"

            fun printProfile profRes =
            let
                (* Sort in ascending order. *)
                val sorted = quickSort (fn (a, _) => fn (b, _) => a <= b) profRes

                fun doPrint (count, name) =
                let
                    val cPrint = Int.toString count
                    val prefix =
                        CharVector.tabulate(Int.max(0, 10-size cPrint), fn _ => #" ")
                in
                    TextIO.output(TextIO.stdOut, concat[prefix, cPrint, " ", name, "\n"])
                end

                val total = List.foldl (fn ((c,_),s) => c+s) 0 profRes
            in
                List.app doPrint sorted;
                if total = 0 then ()
                else TextIO.print(concat["Total ", Int.toString total, "\n"])
            end
        in

            structure Profiling =
            struct
                datatype profileMode =
                    ProfileTime             (* old mode 1 *)
                |   ProfileAllocations      (* old mode 2 *)
                |   ProfileLongIntEmulation (* old mode 3  - No longer used*)
                |   ProfileTimeThisThread   (* old mode 6 *)
                |   ProfileMutexContention
            
                fun profileStream (stream: (int * string) list -> unit) mode f arg =
                let
                    (* Control profiling.  This may raise Fail if profiling is turned on when it
                       is already on or if there is insufficient memory. *)
                    val code =
                        case mode of
                            ProfileTime =>              1
                        |   ProfileAllocations =>       2
                        |   ProfileLongIntEmulation =>  3
                        |   ProfileTimeThisThread =>    6
                        |   ProfileMutexContention =>   7
                    val _ = systemProfile code (* Discard the result *)
                    val result =
                        f arg handle exn => (stream(systemProfile 0); PolyML.Exception.reraise exn)
                in
                    stream(systemProfile 0);
                    result
                end
            
                fun profile mode f arg = profileStream printProfile mode f arg

                (* Live data profiles show the current state.  We need to run the
                   GC to produce the counts. *)
                datatype profileDataMode =
                    ProfileLiveData
                |   ProfileLiveMutableData

                fun profileDataStream(stream: (int * string) list -> unit) mode =
                let
                    val code =
                        case mode of
                            ProfileLiveData => 4
                        |   ProfileLiveMutableData => 5
                    val _ = systemProfile code (* Discard the result *)
                    val () = PolyML.fullGC()
                in
                    stream(systemProfile 0)
                end
                
                val profileData = profileDataStream printProfile
            end
        end

        (* Saving and loading state. *)
        structure SaveState =
        struct
            type moduleId = Word8Vector.vector

            val showLoadedModules: unit -> moduleId list =
                RunCall.rtsCallFull0 "PolyShowLoadedModules"

            val getModuleInfo: string -> moduleId * (moduleId * string) list =
                RunCall.rtsCallFull1 "PolyGetModuleInfo"

            local
                val getOS: int = LibrarySupport.getOSType()
                (* Path elements are separated by semicolons in Windows but colons in Unix. *)
                val sepInPathList = if getOS = 1 then #";" else #":"

                val loadMod: string -> Universal.universal list * moduleId = RunCall.rtsCallFull1 "PolyLoadModule"
                and systemDir: unit -> string = RunCall.rtsCallFull0 "PolyGetModuleDirectory"

                (* Get the full path name and the module info.  Raises an exception if the module
                   cannot be found. *)
                fun moduleFileName fileName =
                    (* If there is a path separator in the name use the name and don't search further. *)
                    if OS.Path.dir fileName <> ""
                    then (fileName, #2 (getModuleInfo fileName))
                    else
                    let
                        val pathList =
                            case OS.Process.getEnv "POLYMODPATH" of
                                NONE => []
                            |   SOME s => String.fields (fn ch => ch = sepInPathList) s

                        (* Append the system directory to the end unless it's empty *)
                        val sysDir = systemDir()
                        val fullPathList =
                            if sysDir = "" then pathList else pathList @ [sysDir]

                        fun findFile (hd::tl) =
                            (* See if the file exists and is a valid module. *)
                            let
                                val fullName = OS.Path.joinDirFile{dir=hd, file=fileName}
                            in
                                (fullName, #2 (getModuleInfo fullName))
                                    (* If this raises an exception keep looking. *)
                                    handle Fail _ => findFile tl | OS.SysErr _ => findFile tl
                            end
                        |   findFile [] =
                                raise Fail("Unable to find module ``" ^ fileName ^ "''")
                    in
                        findFile fullPathList
                    end

                (* Load the dependencies and then the module itself, accumulating the contents. *)
                fun loadDependencies((fileName, []), otherContents) =
                        (* All the dependencies have been loaded - load the module itself. *)
                    let
                        val (contents, modId) = loadMod fileName
                    in
                        (contents @ otherContents, modId)
                    end

                |   loadDependencies((fileName, (_, "")::otherModules), otherContents) =
                        (* If the file name is empty we don't attempt to load it here.
                           If it isn't already loaded we'll get an error when we attempt
                           to load the parent. *)
                        loadDependencies((fileName, otherModules), otherContents)

                |   loadDependencies((fileName, (depModId, depFileName)::otherModules), otherContents) =
                    let
                        val loadedMods = showLoadedModules()

                        val addContents =
                            (* Have to load the module unless the module is already loaded *)
                            if List.exists(fn id => id = depModId) loadedMods
                            then otherContents
                            else #1 (loadDependencies(moduleFileName depFileName, otherContents))
                    in

                        (* Get the other dependencies and finally the module itself. *)
                        loadDependencies((fileName, otherModules), addContents)
                    end
            in
                fun loadModuleBasic (fileName: string): Universal.universal list * moduleId =
                    loadDependencies(moduleFileName fileName, []);
            end
            
            val releaseModule: moduleId -> unit = RunCall.rtsCallFull1 "PolyReleaseModule"

            val saveChild: string * int -> unit = RunCall.rtsCallFull2 "PolySaveState"

            fun saveState f = saveChild (f, 0);

            val showHierarchy: unit -> string list = RunCall.rtsCallFull0 "PolyShowHierarchy"
            
            local
                val doRename: string * string -> unit = RunCall.rtsCallFull2 "PolyRenameParent"
            in
                fun renameParent{ child: string, newParent: string }: unit = doRename(child, newParent)
            end

            val showParent: string -> string option = RunCall.rtsCallFull1 "PolyShowParent"
            and loadState: string -> unit = RunCall.rtsCallFull1 "PolyLoadState"
            
            local
                val loadHier: string list -> unit = RunCall.rtsCallFull1 "PolyLoadHierarchy"
            in
                (* Load hierarchy takes a list of file names in order with the parents
                   before the children.  It's easier for the RTS if this is reversed. *)
                fun loadHierarchy (s: string list): unit = loadHier (List.rev s)
            end

            (* Module loading and storing. *)
            structure Tags =
            struct
                val structureTag: (string * PolyML.NameSpace.Structures.structureVal) Universal.tag = Universal.tag()
                val functorTag: (string * PolyML.NameSpace.Functors.functorVal) Universal.tag = Universal.tag()
                val signatureTag: (string * PolyML.NameSpace.Signatures.signatureVal) Universal.tag = Universal.tag()
                val valueTag: (string * PolyML.NameSpace.Values.value) Universal.tag = Universal.tag()
                val typeTag: (string * PolyML.NameSpace.TypeConstrs.typeConstr) Universal.tag = Universal.tag()
                val fixityTag: (string * PolyML.NameSpace.Infixes.fixity) Universal.tag = Universal.tag()
                val startupTag: (unit -> unit) Universal.tag = Universal.tag()
            end

            local
                val saveDepMod: string * Universal.universal list * (moduleId * string) list -> moduleId =
                    RunCall.rtsCallFull3 "PolyStoreModule"
            in
                fun saveDependentModuleBasic(_, [], _) = raise Fail "Cannot create an empty module"
                |   saveDependentModuleBasic(fileName, contents, dependencies) =
                        saveDepMod(fileName, contents, dependencies)
                
                (* Simple version without dependencies *)
                fun saveModuleBasic(name, contents) =
                        saveDependentModuleBasic(name, contents, [])
            end

            fun saveDependentModule(s, {structs, functors, sigs, onStartup}, dependencies) =
            let
                fun dolookup (look, tag, kind) s =
                    case look globalNameSpace s of
                        SOME v => Universal.tagInject tag (s, v)
                    |   NONE => raise Fail (concat[kind, " ", s, " has not been declared"])
                val structVals = map (dolookup(#lookupStruct, Tags.structureTag, "Structure")) structs
                val functorVals = map (dolookup(#lookupFunct, Tags.functorTag, "Functor")) functors
                val sigVals = map (dolookup(#lookupSig, Tags.signatureTag, "Signature")) sigs
                val startVal =
                    case onStartup of
                        SOME f => [Universal.tagInject Tags.startupTag f]
                    |   NONE => []
            in
                saveDependentModuleBasic(s, structVals @ functorVals @ sigVals @ startVal, dependencies)
            end
            
            fun saveModule(s, contents) = saveDependentModule(s, contents, [])
            
            fun loadModule s : moduleId =
            let
                val (ulist, modId) = loadModuleBasic s
                (* Find and run the start-up function.  If it raises an exception we
                   don't go further. *)
                val startFn = List.find (Universal.tagIs Tags.startupTag) ulist
                val () =
                    case startFn of SOME f => (Universal.tagProject Tags.startupTag f) () | NONE => ()
                fun extract (tag:'a Universal.tag): Universal.universal list -> 'a list =
                    List.mapPartial(
                        fn s => if Universal.tagIs tag s then SOME(Universal.tagProject tag s) else NONE)
            in
                (* Add the entries and print them in the same way as top-level bindings. *)
                printAndEnter(! printInAlphabeticalOrder, globalNameSpace, TextIO.print, !printDepth)
                {
                    fixes = extract Tags.fixityTag ulist,
                    values = extract Tags.valueTag ulist,
                    structures = extract Tags.structureTag ulist,
                    signatures = extract Tags.signatureTag ulist,
                    functors = extract Tags.functorTag ulist,
                    types = extract Tags.typeTag ulist
                };
                
                modId
            end

        end
        
        val loadModule = SaveState.loadModule

    end
end (* PolyML. *);
