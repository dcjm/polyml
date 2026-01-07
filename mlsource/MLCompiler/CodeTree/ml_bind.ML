(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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
    structure CodetreeFunctions =
        CodetreeFunctions(structure BaseCodeTree = BaseCodeTree and Strongly = StronglyConnected)

    structure CodetreeBackend =
        CodetreeStaticLinkAndCases(
            structure Pretty        = Pretty
            structure GCode         = GCode
            structure Debug         = Debug
            structure BaseCodeTree  = BaseCodeTree
            structure BackendTree = BackendIntermediateCode
            structure CodetreeFunctions = CodetreeFunctions
            structure CodeArray   = CodeArray
        )

     structure CodetreeLamdbaLift =
        CodetreeLambdaLift (
            structure   BaseCodeTree        = BaseCodeTree
            and         CodetreeFunctions  = CodetreeFunctions
            and         Backend             = CodetreeBackend
            and         Pretty              = Pretty
            and         Debug               = Debug
            and         CodeArray           = CodeArray
        )

    structure CodetreeCodegenConstantFns =
        CodetreeCodegenConstantFunctions (
            structure   BaseCodeTree        = BaseCodeTree
            and         CodetreeFunctions   = CodetreeFunctions
            and         Backend             = CodetreeLamdbaLift
            and         Pretty              = Pretty
            and         Debug               = Debug
            and         CodeArray           = CodeArray
        )

    structure CodetreeRemoveRedundant =
        CodetreeRemoveRedundant(
            structure BaseCodeTree = BaseCodeTree
            structure CodetreeFunctions = CodetreeFunctions
        )

    structure CodetreeSimplifier =
        CodetreeSimplifier(
            structure BaseCodeTree = BaseCodeTree
            and       CodetreeFunctions = CodetreeFunctions
            and       RemoveRedundant   = CodetreeRemoveRedundant
            and       Debug              = Debug
        )

    structure CodetreeOptimiser =
        CodetreeOptimiser(
            structure Pretty        = Pretty
            structure Debug         = Debug
            structure BaseCodeTree  = BaseCodeTree
            structure CodetreeFunctions = CodetreeFunctions
            structure Backend       = CodetreeCodegenConstantFns
            structure RemoveRedundant = CodetreeRemoveRedundant
            structure Simplifier    = CodetreeSimplifier
        )

in
    structure CodeTree = 
        CodeTreeConstruction (
            structure Pretty        = Pretty
            structure Debug         = Debug
            structure BaseCodeTree  = BaseCodeTree
            structure CodetreeFunctions = CodetreeFunctions
            structure Backend = CodetreeCodegenConstantFns
            structure Optimiser     = CodetreeOptimiser
        )
end;


