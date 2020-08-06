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
        CODETREE_FUNCTIONS(structure BASECODETREE = BaseCodeTree and STRONGLY = StronglyConnected)

    structure CodetreeBackend =
        CODETREE_STATIC_LINK_AND_CASES(
            structure PRETTY        = Pretty
            structure GCODE         = GCode
            structure DEBUG         = Debug
            structure BASECODETREE  = BaseCodeTree
            structure BACKENDTREE = BackendIntermediateCode
            structure CODETREE_FUNCTIONS = CodetreeFunctions
            structure CODE_ARRAY   = CODE_ARRAY
        )

     structure CodetreeLamdbaLift =
        CODETREE_LAMBDA_LIFT (
            structure   BASECODETREE        = BaseCodeTree
            and         CODETREE_FUNCTIONS  = CodetreeFunctions
            and         BACKEND             = CodetreeBackend
            and         PRETTY              = Pretty
            and         DEBUG               = Debug
            and         CODE_ARRAY          = CODE_ARRAY
        )

    structure CodetreeCodegenConstantFns =
        CODETREE_CODEGEN_CONSTANT_FUNCTIONS (
            structure   BASECODETREE        = BaseCodeTree
            and         CODETREE_FUNCTIONS  = CodetreeFunctions
            and         BACKEND             = CodetreeLamdbaLift
            and         PRETTY              = Pretty
            and         DEBUG               = Debug
            and         CODE_ARRAY          = CODE_ARRAY
        )

    structure CodetreeRemoveRedundant =
        CODETREE_REMOVE_REDUNDANT(
            structure BASECODETREE = BaseCodeTree
            structure CODETREE_FUNCTIONS = CodetreeFunctions
        )

    structure CodetreeSimplifier =
        CODETREE_SIMPLIFIER(
            structure BASECODETREE = BaseCodeTree
            and       CODETREE_FUNCTIONS = CodetreeFunctions
            and       REMOVE_REDUNDANT   = CodetreeRemoveRedundant
            and       DEBUG              = Debug
        )

    structure CodetreeOptimiser =
        CODETREE_OPTIMISER(
            structure PRETTY        = Pretty
            structure DEBUG         = Debug
            structure BASECODETREE  = BaseCodeTree
            structure CODETREE_FUNCTIONS = CodetreeFunctions
            structure BACKEND       = CodetreeCodegenConstantFns
            structure REMOVE_REDUNDANT = CodetreeRemoveRedundant
            structure SIMPLIFIER    = CodetreeSimplifier
        )

in
    structure CodeTree = 
        CODETREE (
            structure PRETTY        = Pretty
            structure DEBUG         = Debug
            structure BASECODETREE  = BaseCodeTree
            structure CODETREE_FUNCTIONS = CodetreeFunctions
            structure BACKEND = CodetreeCodegenConstantFns
            structure OPTIMISER     = CodetreeOptimiser
        )
end;


