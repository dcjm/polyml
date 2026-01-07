(* If we already have int as arbitrary precision we don't need this.  This also
   deals with the problem of building with 5.6 where FixedInt is missing. *)
case Int.precision of
    NONE =>
       let
           fun printError() = print "Module not required\n"
       in
           (* Create an empty module and exit. *)
           PolyML.SaveState.saveModule("IntInfAsInt", {functors=[], sigs=[], structs=[], onStartup=SOME printError });
           OS.Process.exit OS.Process.success
       end
|   SOME _ => ();

local
    val dirName =
        case PolyML.getUseFileName() of
            NONE => "."
        |   SOME s => OS.Path.dir s
in
    fun useBasis fileName =
    let
        open OS.Path
        val {isAbs, vol, arcs} = fromString dirName
        val basisFile = toString{isAbs=isAbs, vol=vol, arcs=arcs @ [parentArc, parentArc, "basis", fileName]}
    in
        use basisFile
    end
    
    fun useLocal fileName =
        use (OS.Path.joinDirFile{dir=dirName, file=fileName})
end;

RunCall.setDefaultIntTypeArbitrary true; (* Set the default for overloadings. *)

useLocal "Integer.sml";
useLocal "List.sml";
useLocal "StringChar.sml";
useLocal "ArrayVector.sml";
useLocal "Word.sml";
useLocal "RealStringCvt.sml";
useLocal "Date.sml";
useLocal "InputOutput.sml";

(* Thread? *)
(* Socket? *)


(* Use saveModuleBasic because we want to include values and types *)
local
    open PolyML.SaveState
    fun dolookup (look, tag, kind) s =
        case look PolyML.globalNameSpace s of
            SOME v => Universal.tagInject tag (s, v)
        |   NONE => raise Fail (concat[kind, " ", s, " has not been declared"])

    val structVals = map (dolookup(#lookupStruct, Tags.structureTag, "Structure"))
        ([
            "IntInf", "LargeInt", "Int", "FixedInt", "List", "Char", "String",
            "Substring", "CharVector", "BoolVector", "Word8Vector", "Vector",
            "CharArray", "BoolArray", "RealArray", "Word8Array", "Array",
            "CharVectorSlice", "RealVectorSlice", "Word8VectorSlice", "VectorSlice",
            "CharArraySlice", "RealArraySlice", "Word8ArraySlice", "ArraySlice",
            "IntVector", "IntArray", "IntVectorSlice", "IntArraySlice", "Text",
            "Array2", "Word8Array2", "CharArray2", "BoolArray2", "IntArray2",
            "RealArray2", "Word", "LargeWord", "SysWord", "Word32", "Word8",
            "IEEEReal", "StringCvt", "Real", "LargeReal", "Date", "TextIO",
            "BinIO"
        ] @ (if LargeWord.wordSize = 64 then ["Word64"] else []))

    val functorVals = map (dolookup(#lookupFunct, Tags.functorTag, "Functor"))
        [
        ]

    val sigVals = map (dolookup(#lookupSig, Tags.signatureTag, "Signature"))
        [
            "INTEGER", "INT_INF", "LIST", "CHAR", "STRING", "SUBSTRING",
            "STRING_CVT", "MONO_VECTOR", "VECTOR", "MONO_ARRAY", "ARRAY",
            "MONO_VECTOR_SLICE", "VECTOR_SLICE", "MONO_ARRAY_SLICE", "ARRAY_SLICE",
            "TEXT", "ARRAY2", "MONO_ARRAY2", "WORD", "IEEE_REAL", "REAL", "DATE",
            "STREAM_IO", "IMPERATIVE_IO", "TEXT_STREAM_IO", "TEXT_IO", "BIN_IO"
        ]

    val typeVals = map (dolookup(#lookupType, Tags.typeTag, "Type"))
        [
            "int"
        ]

    val valueVals = map (dolookup(#lookupVal, Tags.valueTag, "Value"))
        [
            "length", "ord", "chr", "substring", "size", "real", "trunc",
            "floor", "ceil", "round"
        ]

    val startVal = [Universal.tagInject Tags.startupTag (fn () => RunCall.setDefaultIntTypeArbitrary true)]
in
    val _ = saveModuleBasic("IntInfAsInt", structVals @ functorVals @ sigVals @ typeVals @ valueVals @ startVal)
end;

