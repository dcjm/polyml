(*
    Title:      Standard Basis Library: Commands to build the library
    Copyright   David C.J. Matthews 2000, 2005, 2015-16

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
    else if LargeWord.wordSize = 64
    then Bootstrap.use "basis/Word32In64.sml"
    else ();

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
val () = Bootstrap.use "basis/MATH.sml";
val () = Bootstrap.use "basis/MATH.sml";
structure LargeReal = struct type real = real end;
val () = Bootstrap.use "basis/RealSignature.sml";
val () = Bootstrap.use "basis/Real.sml";
val () = Bootstrap.use "basis/Time.sml";
val () = Bootstrap.use "basis/DateSignature.sml";
val () = Bootstrap.use "basis/Date.sml";
val () = Bootstrap.use "basis/Thread.sml"; (* Non-standard. *)
val () = Bootstrap.use "basis/Timer.sml";
val () = Bootstrap.use "basis/CommandLine.sml";
val () = Bootstrap.use "basis/OS.sml";
val () = Bootstrap.use "basis/ExnPrinter.sml"; (* Relies on OS. *)
val () = Bootstrap.use "basis/InitialPolyML.ML"; (* Relies on OS. *)
val () = Bootstrap.use "basis/IO.sml";
val () = Bootstrap.use "basis/PRIM_IO.sml";
val () = Bootstrap.use "basis/PrimIO.sml";
val () = Bootstrap.use "basis/TextPrimIO.sml";
val () = Bootstrap.use "basis/BinPrimIO.sml";
val () = Bootstrap.use "basis/LibraryIOSupport.sml";
val () = Bootstrap.use "basis/STREAM_IO.sml";
val () = Bootstrap.use "basis/BasicStreamIO.sml";
val () = Bootstrap.use "basis/IMPERATIVE_IO.sml";
val () = Bootstrap.use "basis/ImperativeIO.sml";
val () = Bootstrap.use "basis/TextIO.sml";
val () = Bootstrap.use "basis/BinIO.sml";
val () = Bootstrap.use "basis/NetHostDB.sml";
val () = Bootstrap.use "basis/NetProtDB.sml";
val () = Bootstrap.use "basis/NetServDB.sml";
val () = Bootstrap.use "basis/Socket.sml";
val () = Bootstrap.use "basis/GenericSock.sml";
val () = Bootstrap.use "basis/INetSock.sml";
val () = Bootstrap.use "basis/UnixSock.sml";
val () = Bootstrap.use "basis/PackRealBig.sml"; (* also declares PackRealLittle *)
val () = Bootstrap.use "basis/PackWord8Big.sml"; (* also declares Pack8Little. ...*)
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
    then ( Bootstrap.use "basis/Posix.sml"; Bootstrap.use "basis/Unix.sml")
    else if getOS = 1 then (Bootstrap.use "basis/Windows.sml")
    else ()
end;

val () = Bootstrap.use "basis/HashArray.ML";
val () = Bootstrap.use "basis/UniversalArray.ML";
val () = Bootstrap.use "basis/PrettyPrinter.sml"; (* Add PrettyPrinter to PolyML structure. *)
val () = Bootstrap.use "basis/ASN1.sml";
val () = Bootstrap.use "basis/Statistics.ML"; (* Add Statistics to PolyML structure. *)
val () = Bootstrap.use "basis/ForeignConstants.sml";
val () = Bootstrap.use "basis/ForeignMemory.sml";
val () = Bootstrap.useWithParms [Bootstrap.Universal.tagInject Bootstrap.maxInlineSizeTag 1000] "basis/Foreign.sml";
val () = Bootstrap.use "basis/FinalPolyML.sml";
val () = Bootstrap.use "basis/TopLevelPolyML.sml"; (* Add rootFunction to Poly/ML. *)

val use = PolyML.use;

(* Copy everything out of the original name space. *)
(* Do this AFTER we've finished compiling PolyML and after adding "use". *)
val () = List.app (#enterVal PolyML.globalNameSpace) (#allVal Bootstrap.globalSpace ())
and () = List.app (#enterFix PolyML.globalNameSpace) (#allFix Bootstrap.globalSpace ())
and () = List.app (#enterSig PolyML.globalNameSpace) (#allSig Bootstrap.globalSpace ())
and () = List.app (#enterType PolyML.globalNameSpace) (#allType Bootstrap.globalSpace ())
and () = List.app (#enterFunct PolyML.globalNameSpace) (#allFunct Bootstrap.globalSpace ())
and () = List.app (#enterStruct PolyML.globalNameSpace) (#allStruct Bootstrap.globalSpace ())

(* We don't want Bootstrap copied over. *)
val () = PolyML.Compiler.forgetStructure "Bootstrap";

(* Clean out structures and functors which are only used to build
   the library. *)
PolyML.Compiler.forgetValue "it";
PolyML.Compiler.forgetStructure "LibrarySupport";
PolyML.Compiler.forgetStructure "LibraryIOSupport";
PolyML.Compiler.forgetStructure "MachineConstants";
PolyML.Compiler.forgetStructure "ForeignConstants";
PolyML.Compiler.forgetStructure "ForeignMemory";
PolyML.Compiler.forgetFunctor "BasicStreamIO";
PolyML.Compiler.forgetFunctor "VectorOperations";
PolyML.Compiler.forgetFunctor "PolyVectorOperations";
PolyML.Compiler.forgetFunctor "VectorSliceOperations";
PolyML.Compiler.forgetFunctor "BasicImperativeIO";
PolyML.Compiler.forgetFunctor "ASN1";
PolyML.Compiler.forgetSignature "ASN1";

(* Now we've created the new name space we must use PolyML.make/use. N.B. Unlike Bootstrap.use
   these don't automatically look at the -I option. *)



