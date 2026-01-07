(* Run the regression tests. *)

(* Some tests are not applicable on some platforms.  If they raise this exception
   they are treated as succeeding. *)
exception NotApplicable;

fun runTests parentDir =
let
    val defaultInlineSize = ! PolyML.Compiler.maxInlineSize
    val defaultLanguageExtensions = ! PolyML.Compiler.languageExtensions

    fun runTests (dirName, expectSuccess) =
    let
        (* Run a file.  Returns true if it succeeds, false if it fails. *)
        fun runTest fileName =
        let
            open PolyML.Compiler
            (* Max inline size is not available as a CP parameter and some tests
               adjust it.  Set it to the default before each test. *)
            val () = maxInlineSize := defaultInlineSize (* Set it to the default *)
            val () = debug := false (* Reset this *)
            val () = narrowOverloadFlexRecord := false
            val () = languageExtensions := defaultLanguageExtensions
            (* First in list is the name with no suffix. *)
            val inStream = TextIO.getInstream(TextIO.openIn fileName)
            val stream = ref inStream

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
            (* Create a private name space for each test otherwise declarations in one
               could affect another. *)
            fun makeSpace(globalLook, globalAll) =
            let
                open HashArray
                val table = hash 10
                infix 8 sub
                fun lookup s =
                    case table sub s of
                        NONE => globalLook s
                    |   SOME r => SOME r
                fun enter(s, v) = update(table, s, v)
                fun all () = fold (fn(s, v, l) => (s, v) :: l) (globalAll()) table
            in
                { lookup = lookup, enter = enter, all = all }
            end
            val { lookupFix, lookupSig, lookupVal, lookupType, lookupFunct, lookupStruct,
                  allFix, allSig, allVal, allType, allFunct, allStruct, ...} = PolyML.globalNameSpace;
            val fixSpace = makeSpace(lookupFix, allFix)
            val sigSpace = makeSpace(lookupSig, allSig)
            val valSpace = makeSpace(lookupVal, allVal)
            val typeSpace = makeSpace(lookupType, allType)
            val funSpace = makeSpace(lookupFunct, allFunct)
            val strSpace = makeSpace(lookupStruct, allStruct)

            val localNameSpace: PolyML.NameSpace.nameSpace =
            {
                lookupFix    = #lookup fixSpace,
                lookupSig    = #lookup sigSpace,
                lookupVal    = #lookup valSpace,
                lookupType   = #lookup typeSpace,
                lookupFunct  = #lookup funSpace,
                lookupStruct = #lookup strSpace,
                enterFix     = #enter fixSpace,
                enterSig     = #enter sigSpace,
                enterVal     = #enter valSpace,
                enterType    = #enter typeSpace,
                enterFunct   = #enter funSpace,
                enterStruct  = #enter strSpace,
                allFix       = #all fixSpace,
                allSig       = #all sigSpace,
                allVal       = #all valSpace,
                allType      = #all typeSpace,
                allFunct     = #all funSpace,
                allStruct    = #all strSpace
            }

            (* The tests in the Fail directory should all raise exceptions
               in the compiler as a result of detecting errors. *)
            exception CompilerException
        in
            (
                while not (TextIO.StreamIO.endOfStream(!stream)) do
                let
                    fun discardOut _ = ()
                    val nameSpace = PolyML.globalNameSpace
    
                    val code =
                        PolyML.compiler(getChar, [CPOutStream discardOut, CPNameSpace localNameSpace])
                            handle Fail "Static Errors" => raise CompilerException
                in
                    code()
                end;
                (* Normal termination: close the stream. *)
                TextIO.StreamIO.closeIn (! stream);
                expectSuccess (* OK if we expected success. *)
            ) handle
                CompilerException => (TextIO.StreamIO.closeIn(!stream); not expectSuccess)
                |   NotApplicable => (TextIO.StreamIO.closeIn(!stream); expectSuccess)
                |   exn => (TextIO.StreamIO.closeIn(!stream); false)

        end;

        open OS.FileSys OS.Path
        val testPath = joinDirFile{dir=parentDir, file=dirName}
        
        (* Get the list of files and sort them.  It's easier to see if the files
           are processed in order and Linux, at least, doesn't sort them. *)
        local
            val dir = openDir testPath
            fun getFiles files =
                case readDir dir of
                    NONE => files
                |   SOME f =>
                        if String.isSuffix "ML" f
                        then getFiles(f::files)
                        else getFiles files

            val files = getFiles []
            val () = closeDir dir

            (* Sort them. *)
            fun qs ([], tail) = tail
            |   qs ([h], tail) = h :: tail
            |   qs (h::t, tail) =
                let
                    val (after, befor) = List.partition (fn s => h <= s) t
                in
                    qs(befor, h :: qs(after, tail))
                end
        in
            val fileList = qs(files, [])
        end
        
        fun runDir ([], fails) = fails  (* Finished *)
        
        |   runDir (f::files, fails) =
            (
                print f; print " => ";
                if runTest(joinDirFile{dir=testPath, file=f})
                then (print "Passed\n"; runDir(files, fails))
                else (print "Failed!!\n"; runDir(files, fails @ [joinDirFile{dir=dirName, file=f}]))
            )
                
        val failedTests = runDir(fileList, [])
    in
        failedTests
    end;
in
    (* Each test in the Succeed directory should succeed and those in the Fail directory should fail. *)
    case runTests("Succeed", true) @ runTests("Fail", false) of
        [] => true (* All succeeded *)
    |   failedTests => (print "\nFailed Tests: "; print(String.concatWith " " failedTests); print "\n"; false)
end;
