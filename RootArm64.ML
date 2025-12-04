(*
    Copyright (c) 2021 David C. J. Matthews

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

(* Compiler root file for Arm64.  This gives the "use" instructions necessary to build the
   compiler and suitable for use with an IDE project file.
   It was constructed from the Poly/ML make files. *)
PolyML.print_depth 1;
PolyML.Compiler.reportUnreferencedIds := true;
use "mlsource/MLCompiler/Address.ML";
use "mlsource/MLCompiler/Misc.ML";
use "mlsource/MLCompiler/HashTable.ML";
use "mlsource/MLCompiler/UniversalTable.ML";
use "mlsource/MLCompiler/StronglyConnected.sml";
use "mlsource/MLCompiler/StretchArray.ML";
use "mlsource/MLCompiler/STRUCTVALSIG.sml";
use "mlsource/MLCompiler/PRETTY.sig";
use "mlsource/MLCompiler/LEXSIG.sml";
use "mlsource/MLCompiler/SymbolsSig.sml";
use "mlsource/MLCompiler/COMPILERBODYSIG.sml";
use "mlsource/MLCompiler/DEBUG.sig";
use "mlsource/MLCompiler/MAKESIG.sml";
use "mlsource/MLCompiler/MAKE_.ML";
use "mlsource/MLCompiler/FOREIGNCALL.sig";
use "mlsource/MLCompiler/BUILTINS.sml";
use "mlsource/MLCompiler/CODETREE.sig";
use "mlsource/MLCompiler/STRUCT_VALS.ML";
use "mlsource/MLCompiler/CodeTree/BACKENDINTERMEDIATECODE.sig";
use "mlsource/MLCompiler/CodeTree/BASECODETREE.sig";
use "mlsource/MLCompiler/CodeTree/CODETREEFUNCTIONS.sig";
use "mlsource/MLCompiler/CodeTree/CODEARRAY.sig";
use "mlsource/MLCompiler/CodeTree/CODEGENTREE.sig";
use "mlsource/MLCompiler/CodeTree/GENCODE.sig";
use "mlsource/MLCompiler/CodeTree/CodetreeFunctions.ML";
use "mlsource/MLCompiler/CodeTree/CodetreeStaticLinkAndCases.ML";
use "mlsource/MLCompiler/CodeTree/CodetreeCodegenConstantFunctions.ML";
use "mlsource/MLCompiler/CodeTree/CodetreeLambdaLift.ML";
use "mlsource/MLCompiler/CodeTree/CodetreeRemoveRedundant.ML";
use "mlsource/MLCompiler/CodeTree/CodetreeSimplifier.ML";
use "mlsource/MLCompiler/CodeTree/CodetreeOptimiser.ML";
use "mlsource/MLCompiler/CodeTree/CodeTreeConstruction.ML";
use "mlsource/MLCompiler/Pretty.sml";
use "mlsource/MLCompiler/CodeTree/CodeArray.ML";
use "mlsource/MLCompiler/Debug.ML";
use "mlsource/MLCompiler/CodeTree/BackendIntermediateCode.sml";
use "mlsource/MLCompiler/CodeTree/BaseCodeTree.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64ASSEMBLY.sig";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64Assembly.sml";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64PREASSEMBLY.sig";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64PreAssembly.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64ICODE.sig";
use "mlsource/MLCompiler/CodeTree/INTSET.sig";
use "mlsource/MLCompiler/CodeTree/IntSet.sml";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64ICode.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64ForeignCall.sml";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64IDENTIFYREFERENCES.sig";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64IdentifyReferences.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64PUSHREGISTERS.sig";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64PushRegisters.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64ICODEOPTIMISE.sig";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64ICodeOptimise.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64ALLOCATEREGISTERS.sig";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64AllocateRegisters.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64ICODEGENERATE.sig";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64ICodeToArm64Code.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ARM64ICODETRANSFORM.sig";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64ICodeTransform.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/Arm64CodetreeToICode.ML";
use "mlsource/MLCompiler/CodeTree/Arm64Code/ml_bind.ML";
use "mlsource/MLCompiler/CodeTree/GCode.arm64.ML";
use "mlsource/MLCompiler/CodeTree/ml_bind.ML";
use "mlsource/MLCompiler/StructVals.ML";
use "mlsource/MLCompiler/LEX_.ML";
use "mlsource/MLCompiler/Symbols.ML";
use "mlsource/MLCompiler/Lex.ML";
use "mlsource/MLCompiler/SymsetSig.sml";
use "mlsource/MLCompiler/DATATYPEREPSIG.sml";
use "mlsource/MLCompiler/VALUEOPSSIG.sml";
use "mlsource/MLCompiler/EXPORTTREESIG.sml";
use "mlsource/MLCompiler/STRUCTURESSIG.sml";
use "mlsource/MLCompiler/PARSEDECSIG.sml";
use "mlsource/MLCompiler/COMPILER_BODY.ML";
use "mlsource/MLCompiler/SymSet.ML";
use "mlsource/MLCompiler/TYPETREESIG.sml";
use "mlsource/MLCompiler/COPIERSIG.sml";
use "mlsource/MLCompiler/TYPEIDCODESIG.sml";
use "mlsource/MLCompiler/DATATYPE_REP.ML";
use "mlsource/MLCompiler/VALUE_OPS.ML";
use "mlsource/MLCompiler/TYPE_TREE.ML";
use "mlsource/MLCompiler/UTILITIES_.ML";
use "mlsource/MLCompiler/Utilities.ML";
use "mlsource/MLCompiler/ExportTree.sml";
use "mlsource/MLCompiler/ExportTreeStruct.sml";
use "mlsource/MLCompiler/TypeTree.ML";
use "mlsource/MLCompiler/COPIER.sml";
use "mlsource/MLCompiler/CopierStruct.sml";
use "mlsource/MLCompiler/TYPEIDCODE.sml";
use "mlsource/MLCompiler/TypeIDCodeStruct.sml";
use "mlsource/MLCompiler/DatatypeRep.ML";
use "mlsource/MLCompiler/ValueOps.ML";
use "mlsource/MLCompiler/PARSETREESIG.sml";
use "mlsource/MLCompiler/SIGNATURESSIG.sml";
use "mlsource/MLCompiler/DEBUGGER.sig";
use "mlsource/MLCompiler/STRUCTURES_.ML";
use "mlsource/MLCompiler/DEBUGGER_.sml";
use "mlsource/MLCompiler/Debugger.sml";
use "mlsource/MLCompiler/ParseTree/BaseParseTreeSig.sml";
use "mlsource/MLCompiler/ParseTree/BASE_PARSE_TREE.sml";
use "mlsource/MLCompiler/ParseTree/PrintParsetreeSig.sml";
use "mlsource/MLCompiler/ParseTree/PRINT_PARSETREE.sml";
use "mlsource/MLCompiler/ParseTree/ExportParsetreeSig.sml";
use "mlsource/MLCompiler/ParseTree/EXPORT_PARSETREE.sml";
use "mlsource/MLCompiler/ParseTree/TypeCheckParsetreeSig.sml";
use "mlsource/MLCompiler/ParseTree/TYPECHECK_PARSETREE.sml";
use "mlsource/MLCompiler/ParseTree/MatchCompilerSig.sml";
use "mlsource/MLCompiler/ParseTree/MATCH_COMPILER.sml";
use "mlsource/MLCompiler/ParseTree/CodegenParsetreeSig.sml";
use "mlsource/MLCompiler/ParseTree/CODEGEN_PARSETREE.sml";
use "mlsource/MLCompiler/ParseTree/PARSE_TREE.ML";
use "mlsource/MLCompiler/ParseTree/ml_bind.ML";
use "mlsource/MLCompiler/SIGNATURES.sml";
use "mlsource/MLCompiler/SignaturesStruct.sml";
use "mlsource/MLCompiler/Structures.ML";
use "mlsource/MLCompiler/PARSETYPESIG.sig";
use "mlsource/MLCompiler/SKIPS.sig";
use "mlsource/MLCompiler/PARSE_DEC.ML";
use "mlsource/MLCompiler/SKIPS_.ML";
use "mlsource/MLCompiler/Skips.ML";
use "mlsource/MLCompiler/PARSE_TYPE.ML";
use "mlsource/MLCompiler/ParseType.ML";
use "mlsource/MLCompiler/ParseDec.ML";
use "mlsource/MLCompiler/CompilerBody.ML";
use "mlsource/MLCompiler/CompilerVersion.sml";
use "mlsource/MLCompiler/Make.ML";
use "mlsource/MLCompiler/INITIALISE_.ML";
use "mlsource/MLCompiler/Initialise.ML";
use "mlsource/MLCompiler/ml_bind.ML";
