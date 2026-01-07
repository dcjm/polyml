(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    Modified David C.J. Matthews 2008-9, 2015-16, 2020.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(*
    Title:      Poly Make Program.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* This previously contained PolyML.make which was passed through to
   the basis.  It has now been reduced to just "use" and is
   only used during the bootstrap process to compile the basis
   library itself.  *)

functor MAKE_ (

structure COMPILERBODY : COMPILERBODYSIG

structure UNIVERSALTABLE :
sig
  type 'a tag = 'a Universal.tag;
  type univTable;
  type universal = Universal.universal
  
  val makeUnivTable:  unit -> univTable
  val univEnter:  univTable * 'a tag * string * 'a -> unit;
  val univLookup: univTable * 'a tag * string -> 'a option;
  val univDelete: univTable * 'a tag * string -> unit;
    val fold: (string * universal * 'a -> 'a) -> 'a -> univTable -> 'a
end;

structure STRUCTVALS : STRUCTVALSIG;
structure DEBUG: DEBUG
structure PRETTY: PRETTY (* For compilerOutputTag *)
structure LEX: LEXSIG (* For errorMessageProcTag *)

structure VERSION:
    sig
        val versionSuffix: string
    end

sharing STRUCTVALS.Sharing = COMPILERBODY.Sharing
sharing LEX.Sharing = PRETTY.Sharing

) : MAKESIG =

struct
    type univTable  = UNIVERSALTABLE.univTable;
    type values = STRUCTVALS.values
    type typeConstrs = STRUCTVALS.typeConstrs
    type fixStatus = STRUCTVALS.fixStatus
    type structVals = STRUCTVALS.structVals
    type signatures = STRUCTVALS.signatures
    type functors = STRUCTVALS.functors
    type env = STRUCTVALS.env

    open COMPILERBODY

    local
        open UNIVERSALTABLE
        open Thread.Thread
        open Thread.Mutex
    in
        (* Create an environment with a mutex to protect concurrent access. *)
        datatype gEnv = DbEnv of mutex * univTable
       
        (* Lock the mutex during any lookup or entry.  This is primarily to
           avoid the underlying hash table from being rehashed by different
           threads at the same time.  This code should be in a library. *)
        fun protect mutx f =
        let
            (* Turn off interrupts while we have the lock. *)
            val oldAttrs = getAttributes()
            val () = setAttributes[InterruptState InterruptDefer]
              val () = lock mutx
            val result = f()
                handle exn => (unlock mutx; setAttributes oldAttrs; raise exn)
        in
            unlock mutx;
            setAttributes oldAttrs;
            result
        end

        (* Create an environment *)
        fun makeGEnv () : gEnv = DbEnv (mutex(), makeUnivTable()); 

        (* enter a value into an environment *)
        fun dbEnvEnter (DbEnv(mutx, db)) (t : 'a tag) (s : string, v : 'a) : unit =
          protect mutx (fn () => univEnter (db, t, s, v))

        (* find a value in an environment *)
        fun dbEnvLookup (DbEnv(mutx, db)) (t : 'a tag) (s : string) : 'a option =
            protect mutx(fn () => univLookup (db, t, s))

        fun dbEnvAll (DbEnv(mutx, db)) (t : 'a tag) () : (string * 'a) list =
        let
            open Universal UNIVERSALTABLE
            fun filter (s, c, l) = if tagIs t c then (s, tagProject t c) :: l else l
        in
            protect mutx (fn () => fold filter [] db)
        end

        fun gEnvAsEnv gEnv =
             STRUCTVALS.Env {
                lookupFix    = dbEnvLookup gEnv STRUCTVALS.fixVar,
                lookupVal    = dbEnvLookup gEnv STRUCTVALS.valueVar,
                lookupType   = dbEnvLookup gEnv STRUCTVALS.typeConstrVar,
                lookupSig    = dbEnvLookup gEnv STRUCTVALS.signatureVar,
                lookupStruct = dbEnvLookup gEnv STRUCTVALS.structVar,
                lookupFunct  = dbEnvLookup gEnv STRUCTVALS.functorVar,
                
                enterFix     = dbEnvEnter gEnv STRUCTVALS.fixVar,
                enterVal     = dbEnvEnter gEnv STRUCTVALS.valueVar,
                enterType    = dbEnvEnter gEnv STRUCTVALS.typeConstrVar,
                enterSig     = dbEnvEnter gEnv STRUCTVALS.signatureVar,
                enterStruct  = dbEnvEnter gEnv STRUCTVALS.structVar,
                enterFunct   = dbEnvEnter gEnv STRUCTVALS.functorVar,
                
                allValNames  =
                    fn () => map #1 (dbEnvAll gEnv STRUCTVALS.valueVar ()),
                allStructNames =
                    fn () => map #1 (dbEnvAll gEnv STRUCTVALS.structVar ())
                };

        fun gEnvAsNameSpace gEnv: nameSpace =
              {
                lookupFix    = dbEnvLookup gEnv STRUCTVALS.fixVar,
                lookupVal    = dbEnvLookup gEnv STRUCTVALS.valueVar,
                lookupType   = dbEnvLookup gEnv STRUCTVALS.typeConstrVar,
                lookupSig    = dbEnvLookup gEnv STRUCTVALS.signatureVar,
                lookupStruct = dbEnvLookup gEnv STRUCTVALS.structVar,
                lookupFunct  = dbEnvLookup gEnv STRUCTVALS.functorVar,

                enterFix     = dbEnvEnter gEnv STRUCTVALS.fixVar,
                enterVal     = dbEnvEnter gEnv STRUCTVALS.valueVar,
                enterType    = dbEnvEnter gEnv STRUCTVALS.typeConstrVar,
                enterSig     = dbEnvEnter gEnv STRUCTVALS.signatureVar,
                enterStruct  = dbEnvEnter gEnv STRUCTVALS.structVar,
                enterFunct   = dbEnvEnter gEnv STRUCTVALS.functorVar,
                
                allFix     = dbEnvAll gEnv STRUCTVALS.fixVar,
                allVal     = dbEnvAll gEnv STRUCTVALS.valueVar,
                allType    = dbEnvAll gEnv STRUCTVALS.typeConstrVar,
                allSig     = dbEnvAll gEnv STRUCTVALS.signatureVar,
                allStruct  = dbEnvAll gEnv STRUCTVALS.structVar,
                allFunct   = dbEnvAll gEnv STRUCTVALS.functorVar
                };
 
    end;

    (*****************************************************************************)
    (*                  useIntoEnv (runcompiler with ML compiler bound in)       *)
    (*****************************************************************************)
    fun compileIntoEnv (globalEnv : gEnv) : (string * TextIO.instream * Universal.universal list) -> unit =
    let
        val useEnv : nameSpace =
        { 
            lookupFix    = dbEnvLookup globalEnv STRUCTVALS.fixVar,
            lookupVal    = dbEnvLookup globalEnv STRUCTVALS.valueVar,
            lookupType   = dbEnvLookup globalEnv STRUCTVALS.typeConstrVar,
            lookupSig    = dbEnvLookup globalEnv STRUCTVALS.signatureVar,
            lookupStruct = dbEnvLookup globalEnv STRUCTVALS.structVar,
            lookupFunct  = dbEnvLookup globalEnv STRUCTVALS.functorVar,
            enterFix     = dbEnvEnter globalEnv STRUCTVALS.fixVar,
            enterVal     = dbEnvEnter globalEnv STRUCTVALS.valueVar,
            enterType    = dbEnvEnter globalEnv STRUCTVALS.typeConstrVar,
            enterStruct  = dbEnvEnter globalEnv STRUCTVALS.structVar,
            enterSig     = dbEnvEnter globalEnv STRUCTVALS.signatureVar,
            enterFunct   = dbEnvEnter globalEnv STRUCTVALS.functorVar,
            allFix       = dbEnvAll globalEnv STRUCTVALS.fixVar,
            allVal       = dbEnvAll globalEnv STRUCTVALS.valueVar,
            allType      = dbEnvAll globalEnv STRUCTVALS.typeConstrVar,
            allSig       = dbEnvAll globalEnv STRUCTVALS.signatureVar,
            allStruct    = dbEnvAll globalEnv STRUCTVALS.structVar,
            allFunct     = dbEnvAll globalEnv STRUCTVALS.functorVar
        };

        fun use (fileName, inStream, parameters) =
        let            
            val lineNo   = ref 1;
            val eof      = ref false;
            
            fun getChar () : char option =
            case TextIO.input1 inStream of
                eoln as SOME #"\n" =>
                (
                    lineNo := !lineNo + 1;                  
                    eoln
                )
            |   NONE => (eof := true; NONE)
            |   c => c

            fun errorProc {message, hard, location={ file, startLine=line, ... }, ...} =
               TextIO.print(concat
                   [if hard then "Error-" else "Warning-",
                    " in '", file, "', line ", FixedInt.toString line, ".\n",
                    PRETTY.uglyPrint message, "\n"])
        in
            (
                while not (! eof) do
                let
                    open DEBUG Universal
                    
                    (* Compile the code *)
                    val code = 
                        case COMPILERBODY.compiler
                            (useEnv, getChar,
                              parameters @ (* These will be found first and override the defaults. *)
                              [
                                 tagInject PRETTY.compilerOutputTag (PRETTY.prettyPrint(print, 70)),
                                 tagInject lineNumberTag (fn () => !lineNo),
                                 tagInject fileNameTag fileName,
                                 tagInject LEX.errorMessageProcTag errorProc,
                                 tagInject maxInlineSizeTag 80,
                                 tagInject reportUnreferencedIdsTag true,
                                 tagInject reportExhaustiveHandlersTag false, (* True for testing. *)
                                 (* These are only needed for debugging. *)
                                 tagInject PRETTY.printOutputTag (PRETTY.prettyPrint(print, 70)),
                                 tagInject printDepthFunTag(fn () => 20),
                                 tagInject parsetreeTag false,
                                 tagInject codetreeTag false,
                                 tagInject codetreeAfterOptTag false,
                                 tagInject icodeTag false,
                                 tagInject assemblyCodeTag false
                              ] ) of
                        (_, NONE) => raise Fail "Static Errors"
                     |  (_, SOME c) => c
                    (* execute the code and get the resulting declarations. *)
                    val { fixes, values, structures, signatures, functors, types } = code()
                in
                    (* Just enter the values in the environment without printing. *)
                    List.app (#enterFix useEnv) fixes;
                    List.app (#enterVal useEnv) values;
                    List.app (#enterStruct useEnv) structures;
                    List.app (#enterSig useEnv) signatures;
                    List.app (#enterFunct useEnv) functors;
                    List.app (#enterType useEnv) types
                end
            )
            handle Fail s => (* E.g. syntax error. *)
            (
                TextIO.closeIn inStream;
                raise Fail s
            )
            | exn => (* close inStream if an error occurs *)
            (
                print ("Exception- " ^ General.exnName exn ^ " raised\n");
                TextIO.closeIn inStream;
                raise exn
            )
        end (* use *)
    in
        use
    end; (* scope of compileIntoEnv *)

    fun useIntoEnv globalEnv parameters baseName =
    let
        val () = print ("Use: " ^ baseName ^ "\n")
        (* See if there is a path given as a command line argument. *)
        val args = CommandLine.arguments();
        (* If we have -I filename use that as the output name.
           N.B.  polyImport takes the first argument that is not recognised as
           an RTS argument and treats that as the file name so any -I must occur
           AFTER the import file. *)
        fun getPath [] = "." (* Default path *)
          | getPath ("-I" :: path :: _) = path
          | getPath (_::tl) = getPath tl
        open OS.Path
        (* Add the path to the source on to the directory. *)
        val filePath = concat(getPath args, baseName)
        open VERSION
        (* See if we have a version of the file specific to this
           version of the compiler.  For x.ML see if x.VER.ML exists.
           When bootstrapping from one version of the compiler to
           another we need to compile the basis library in both the
           old and new compiler.  If the interface has changed we may
           need version-specific files. *)
        val { base, ext } = splitBaseExt filePath
        val versionName =
            joinBaseExt {
                base = joinBaseExt{base = base, ext = SOME versionSuffix},
                ext = ext }
        val (inStream, fileName) =
            (TextIO.openIn versionName, versionName)
                handle IO.Io _ => (TextIO.openIn filePath, filePath)
    in
        compileIntoEnv globalEnv (fileName, inStream, parameters);
        TextIO.closeIn inStream
    end

    fun shellProc globalEnv () = compileIntoEnv globalEnv ("<stdin>", TextIO.stdIn, [])
    
    fun useStringIntoEnv globalEnv str =
        compileIntoEnv globalEnv (str, TextIO.openString str, [])

    structure Sharing =
    struct
        type env = env
        type gEnv = gEnv
        type values = values
        type typeConstrSet = typeConstrSet
        type fixStatus = fixStatus
        type structVals = structVals
        type signatures = signatures
        type functors = functors
        type ptProperties = ptProperties
    end
end;



