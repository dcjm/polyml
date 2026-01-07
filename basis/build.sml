(*
    Title:      Standard Basis Library: Commands to build the library
    Copyright   David C.J. Matthews 2000, 2005, 2015-16, 2018-21, 2023

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

(* Thread, Weak and Signal are Poly/ML extensions. *)

val () = Bootstrap.use "basis/InitialBasis.ML";
val () = Bootstrap.use "basis/Universal.ML";
val () = Bootstrap.use "basis/General.sml";
val () = Bootstrap.use "basis/LibrarySupport.sml";
val () = Bootstrap.use "basis/PolyMLException.sml";
val () = Bootstrap.use "basis/Option.sml";
val () = Bootstrap.use "basis/ListSignature.sml";
val () = Bootstrap.use "basis/List.sml";
val () = Bootstrap.use "basis/VectorOperations.sml";
val () = Bootstrap.use "basis/PolyVectorOperations.sml";
val () = Bootstrap.use "basis/VectorSliceOperations.sml";
val () = Bootstrap.use "basis/MONO_VECTOR.sml";
val () = Bootstrap.use "basis/MONO_VECTOR_SLICE.sml";
val () = Bootstrap.use "basis/MONO_ARRAY.sml";
val () = Bootstrap.use "basis/MONO_ARRAY_SLICE.sml";
val () = Bootstrap.use "basis/StringSignatures.sml";
val () = Bootstrap.use "basis/String.sml";

structure Int = struct type int = int end;
val () = Bootstrap.use "basis/INTEGER.sml";
val () = Bootstrap.use "basis/Int.sml";
val () = Bootstrap.use (if Bootstrap.intIsArbitraryPrecision then "basis/IntAsLargeInt.sml" else "basis/IntAsFixedInt.sml");
val () =
    case FixedInt.precision of SOME 31 => Bootstrap.use "basis/Int31.sml" | SOME 63 => Bootstrap.use "basis/Int63.sml" | _ => ();
val () = Bootstrap.use "basis/WordSignature.sml";
val () = Bootstrap.use "basis/LargeWord.sml";
val () = Bootstrap.use "basis/VectorSignature.sml";
val () = Bootstrap.use "basis/VectorSliceSignature.sml";
val () = Bootstrap.use "basis/Vector.sml";
val () = Bootstrap.use "basis/ArraySignature.sml";
val () = Bootstrap.use "basis/ArraySliceSignature.sml"; (* Depends on VectorSlice. *)
val () = Bootstrap.use "basis/Array.sml";

val () = Bootstrap.use "basis/Text.sml"; (* Declares Char, String, CharArray, CharVector *)
val () = Bootstrap.use "basis/Bool.sml";
val () = Bootstrap.use "basis/ListPair.sml";

(* Declare the appropriate additional structures. *)
(* The version of Word32 we use depends on whether this is
   32-bit or 64-bit. *)
val () =
    if LargeWord.wordSize = 32
    then Bootstrap.use "basis/Word32.sml"
    else if Word.wordSize >= 32
    then Bootstrap.use "basis/Word32In64.sml"
    else if LargeWord.wordSize = 64
    then Bootstrap.use "basis/Word32InLargeWord64.sml"
    else ();

val () = Bootstrap.use "basis/Word16.sml";
val () = Bootstrap.use "basis/Word8.sml";
val () = Bootstrap.use "basis/IntInf.sml";
val () = Bootstrap.use "basis/Int32.sml";
val () = Bootstrap.use "basis/Word8Array.sml";
val () = Bootstrap.use "basis/Byte.sml";
val () = Bootstrap.use "basis/BoolArray.sml";
val () = Bootstrap.use "basis/IntArray.sml";
val () = Bootstrap.use "basis/RealArray.sml";
val () = Bootstrap.use "basis/IEEE_REAL.sml";
val () = Bootstrap.use "basis/IEEEReal.sml";
val () = Bootstrap.use "basis/MATH.sig";
structure LargeReal = struct type real = real end;
val () = Bootstrap.use "basis/RealNumbersAsBits.ML";
val () = Bootstrap.use "basis/RealToDecimalConversion.ML";
val () = Bootstrap.use "basis/REAL.sig";
val () = Bootstrap.use "basis/Real.sml"; (* Includes Real32. *)
val () = Bootstrap.use "basis/Time.sml";
val () = Bootstrap.use "basis/DATE.sig";
val () = Bootstrap.use "basis/Date.sml";
val () = Bootstrap.use "basis/Thread.sml"; (* Non-standard. *)
val () = Bootstrap.use "basis/ThreadLib.sml"; (* Non-standard. *)
val () = Bootstrap.use "basis/Timer.sml";
val () = Bootstrap.use "basis/CommandLine.sml";
val () = Bootstrap.use "basis/ExnPrinter.sml";
val () = Bootstrap.use "basis/ForeignConstants.sml";
val () = Bootstrap.use "basis/ForeignMemory.sml";
val () = Bootstrap.useWithParms [Bootstrap.Universal.tagInject Bootstrap.maxInlineSizeTag 1000] "basis/Foreign.sml";
val () = Bootstrap.use "basis/IO.sml";
val () = Bootstrap.use "basis/OS.sml";
val () = Bootstrap.use "basis/PRIM_IO.sml";
val () = Bootstrap.use "basis/PrimIO.sml";
val () = Bootstrap.use "basis/LibraryIOSupport.sml";
val () = Bootstrap.use "basis/STREAM_IO.sml";
val () = Bootstrap.use "basis/BasicStreamIO.sml";
val () = Bootstrap.use "basis/IMPERATIVE_IO.sml";
val () = Bootstrap.use "basis/ImperativeIO.sml";
val () = Bootstrap.use "basis/TextIO.sml";
val () = Bootstrap.use "basis/BinIO.sml";
val () = Bootstrap.use "basis/Socket.sml";
val () = Bootstrap.use "basis/NetProtDB.sml";
val () = Bootstrap.use "basis/NetServDB.sml";
val () = Bootstrap.use "basis/GenericSock.sml";
val () = Bootstrap.use "basis/INetSock.sml";
val () = Bootstrap.use "basis/INet6Sock.sml";
val () = Bootstrap.use "basis/PackReal.sml";
val () =
    if Word.wordSize = 31
    then Bootstrap.use "basis/PackReal32Boxed.sml"
    else Bootstrap.use "basis/PackReal32Tagged.sml";
val () = Bootstrap.use "basis/PackWord.sml";
val () = Bootstrap.use "basis/Array2Signature.sml";
val () = Bootstrap.use "basis/Array2.sml";
val () = Bootstrap.use "basis/IntArray2.sml";
val () = Bootstrap.use "basis/SML90.sml";
val () = Bootstrap.use "basis/Weak.sml";
val () = Bootstrap.use "basis/Signal.sml";
val () = Bootstrap.use "basis/BIT_FLAGS.sml";
val () = Bootstrap.use "basis/SingleAssignment.sml";


(* Build Windows or Unix structure as appropriate. *)
local
    val getOS: int = LibrarySupport.getOSType()
in
    val () =
    if getOS = 0
    then 
    (
        Bootstrap.use "basis/Posix.sml";
        Bootstrap.use "basis/Unix.sml";
        Bootstrap.use "basis/UnixSock.sml"
    )
    else if getOS = 1 then (Bootstrap.use "basis/Windows.sml")
    else ()
end;

val () = Bootstrap.use "basis/HashArray.ML";
val () = Bootstrap.use "basis/UniversalArray.ML";
val () = Bootstrap.use "basis/PrettyPrinter.sml"; (* Add PrettyPrinter to PolyML structure. *)
val () = Bootstrap.use "basis/ASN1.sml";
val () = Bootstrap.use "basis/Statistics.ML"; (* Add Statistics to PolyML structure. *)
val () = Bootstrap.use "basis/InitialPolyML.ML"; (* Relies on OS. *)
val () = Bootstrap.use "basis/FinalPolyML.sml";
val () = Bootstrap.use "basis/TopLevelPolyML.sml"; (* Add rootFunction to Poly/ML. *)

val use = PolyML.use;

(* Copy everything out of the original name space. *)
(* Do this AFTER we've finished compiling PolyML and after adding "use". *)
local
    val values =
       ["!", "*", "+", "-", "/", "::", ":=", "<", "<=", "<>", "=", ">", ">=",
        "@", "Bind", "Chr", "Div", "Domain", "EQUAL", "Empty", "Fail", "GREATER",
        "LESS", "Match", "NONE", "Option", "Overflow", "SOME", "Size", "Span",
        "Subscript", "^", "abs", "app", "before", "ceil", "chr", "concat", "div",
        "exnMessage", "exnName", "explode", "false", "floor", "foldl", "foldr",
        "getOpt", "hd", "ignore", "implode", "isSome", "length", "map",
        "mod", "nil", "not", "null", "o", "ord", "print", "quickSort", "real",
        "ref", "rev", "round", "size", "sort", "str", "substring", "tl", "true",
        "trunc", "use", "valOf", "vector", "~"]

    val fixes =
       ["*", "+", "-", "/", "::", ":=", "<", "<=", "<>", "=", ">", ">=", "@",
         "^", "before", "div", "mod", "o"]

    val sigs =
       ["ARRAY", "ARRAY2", "ARRAY_SLICE", "ASN1", "BIN_IO", "BIT_FLAGS", "BOOL", "BYTE",
        "CHAR", "COMMAND_LINE", "DATE", "FOREIGN", "GENERAL", "GENERIC_SOCK", "IEEE_REAL",
        "IMPERATIVE_IO", "INET6_SOCK", "INET_SOCK", "INTEGER", "INT_INF", "IO",
        "LIST", "LIST_PAIR", "MATH", "MONO_ARRAY", "MONO_ARRAY2",
        "MONO_ARRAY_SLICE", "MONO_VECTOR", "MONO_VECTOR_SLICE", "NET_HOST_DB",
        "NET_PROT_DB", "NET_SERV_DB", "OPTION", "OS", "OS_FILE_SYS", "OS_IO",
        "OS_PATH", "OS_PROCESS", "PACK_REAL", "PACK_WORD", "POSIX",
        "POSIX_ERROR", "POSIX_FILE_SYS", "POSIX_IO", "POSIX_PROCESS",
        "POSIX_PROC_ENV", "POSIX_SIGNAL", "POSIX_SYS_DB", "POSIX_TTY", "PRIM_IO",
        "REAL", "SIGNAL", "SML90", "SOCKET", "STREAM_IO", "STRING", "STRING_CVT",
        "SUBSTRING", "TEXT", "TEXT_IO", "TEXT_STREAM_IO", "THREAD", "TIME",
        "TIMER", "UNIX", "UNIX_SOCK", "VECTOR", "VECTOR_SLICE", "WEAK", "WINDOWS", "WORD"]

    val types =
       ["array", "bool", "char", "exn", "int", "list", "option", "order", "real",
        "ref", "string", "substring", "unit", "vector", "word"]

    val functs = ["ImperativeIO", "PrimIO", "StreamIO"]

    val structs =
       ["Array", "Array2", "ArraySlice", "Asn1", "BinIO", "BinPrimIO", "Bool",
        "BoolArray", "BoolArray2", "BoolVector", "Byte", "Char", "CharArray",
        "CharArray2", "CharArraySlice", "CharVector", "CharVectorSlice",
        "CommandLine", "Date", "FixedInt", "Foreign", "General", "GenericSock",
        "HashArray", "IEEEReal", "INet6Sock", "INetSock", "IO", "Int", "Int32",
        "Int63", "IntArray", "IntArray2", "IntArraySlice", "IntInf", "IntVector",
        "IntVectorSlice", "LargeInt", "LargeReal", "LargeWord", "List",
        "ListPair", "Math", "Net6HostDB", "NetHostDB", "NetProtDB", "NetServDB",
        "OS", "Option", "PackRealBig", "PackRealLittle", "PackReal32Big",
        "PackReal32Little", "PackWord16Big",
        "PackWord16Little", "PackWord32Big", "PackWord32Little", "PackWord8Big",
        "PackWord8Little", "PolyML", "Position", "Posix", "Real", "Real32",
        "RealArray", "RealArray2", "RealArraySlice", "RealVector",
        "RealVectorSlice", "RunCall", "SML90", "Signal", "SingleAssignment",
        "Socket", "String", "StringCvt", "Substring", "SysWord", "Text",
        "TextIO", "TextPrimIO", "Thread", "ThreadLib", "Time", "Timer",
        "Universal", "UniversalArray", "Unix", "UnixSock", "Vector",
        "VectorSlice", "Weak", "Windows", "Word", "Word16", "Word32", "Word64",
        "Word8", "Word8Array", "Word8Array2", "Word8ArraySlice", "Word8Vector",
        "Word8VectorSlice"]

    fun copyOver (enter, lookup) =
    let
        (* Copy over everything in the list if possible.  Some items e.g. the Posix
           structure, may not be present. *)
        fun copy s =
            enter PolyML.globalNameSpace (s, valOf(lookup Bootstrap.globalSpace s))
                handle Option => ()
    in
        List.app copy
    end
in
    val () = copyOver(#enterVal, #lookupVal) values
    val () = copyOver(#enterFix, #lookupFix) fixes
    val () = copyOver(#enterType, #lookupType) types
    val () = copyOver(#enterSig, #lookupSig) sigs
    val () = copyOver(#enterStruct, #lookupStruct) structs
    val () = copyOver(#enterFunct, #lookupFunct) functs
end;

(* Now we've created the new name space we must use PolyML.make/use. N.B. Unlike Bootstrap.use
   these don't automatically look at the -I option. *)



