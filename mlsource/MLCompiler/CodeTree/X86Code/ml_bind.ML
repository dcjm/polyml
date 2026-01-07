(*
    Copyright David C. J. Matthews 2015-19

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

local

    structure X86OutputCode =
        X86OutputCode(
            structure Debug   = Debug
            structure Pretty  = Pretty
            structure CodeArray = CodeArray
        )

    structure X86Optimise =
        X86CodeOptimise(
            structure X86Code = X86OutputCode
        )

    structure X86ForeignCall =
        X86ForeignCall (
            structure X86Code       = X86OutputCode
            structure X86Optimise   = X86Optimise
            structure CodeArray     = CodeArray
        )

    structure X86ICode =
        X86ICode (
            structure X86Code       = X86OutputCode
        )

    structure X86ICodeIdentify =
        X86ICodeIdentifyReferences (
            structure Debug         = Debug
            structure X86ICode         = X86ICode
            structure IntSet        = IntSet
        )

    structure X86ICodeConflicts =
        X86ICodeGetConflictSets (
            structure X86ICode      = X86ICode
            structure IntSet        = IntSet
            structure Identify      = X86ICodeIdentify
        )

    structure X86PushRegs =
        X86PushRegisters (
            structure X86ICode      = X86ICode
            structure IntSet        = IntSet
            structure Identify      = X86ICodeIdentify
        )

    structure X86Opt =
        X86ICodeOptimise (
            structure X86ICode         = X86ICode
            structure IntSet        = IntSet
            structure Identify      = X86ICodeIdentify
            structure X86Code       = X86OutputCode
            structure Debug         = Debug
            structure Pretty        = Pretty
        )

    structure X86IAllocate =
        X86AllocateRegisters (
            structure X86ICode         = X86ICode
            structure Identify      = X86ICodeIdentify
            structure ConflictSets  = X86ICodeConflicts
            structure IntSet        = IntSet
        )

    structure X86ICodeGenerate =
        X86ICodeToX86Code (
            structure Debug         = Debug
            structure X86ICode      = X86ICode
            structure Identify      = X86ICodeIdentify
            structure Pretty        = Pretty
            structure IntSet        = IntSet
            structure X86Code       = X86OutputCode
            structure X86Optimise   = X86Optimise
            structure Strongly      = StronglyConnected
        )
        
    structure X86ICodeTransform =
        X86ICodeTransform (
            structure Debug         = Debug
            structure X86ICode         = X86ICode
            structure Identify      = X86ICodeIdentify
            structure ConflictSets  = X86ICodeConflicts
            structure Allocate      = X86IAllocate
            structure PushRegisters = X86PushRegs
            structure Optimise      = X86Opt
            structure Pretty        = Pretty
            structure IntSet        = IntSet
            structure Codegen       = X86ICodeGenerate
        )
in

    structure X86Code = 
        X86CodetreeToICode (
            structure BackEndTree   = BackendIntermediateCode
            structure Debug         = Debug
            structure X86ICode         = X86ICode
            structure X86Foreign    = X86ForeignCall
            structure ICodeTransform = X86ICodeTransform
            structure CodeArray    = CodeArray
        )

end;
