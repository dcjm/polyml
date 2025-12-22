(*
    Copyright (c) 2013, 2015, 2025 David C.J. Matthews
    
    Derived from previous work
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
    structure BaseParseTree =
        BASE_PARSE_TREE(
            structure STRUCTVALS = StructVals
            structure TYPETREE   = TypeTree
            structure DEBUGGER   = Debugger
        )

    structure PrintTree =
        PRINT_PARSETREE(
            structure BASEPARSETREE = BaseParseTree
            structure LEX        = Lex
            structure STRUCTVALS = StructVals
            structure TYPETREE   = TypeTree
            structure VALUEOPS   = ValueOps
            structure PRETTY     = Pretty
            structure MISC          = Misc
         )

    structure ExportTree =
        EXPORT_PARSETREE(
            structure BASEPARSETREE = BaseParseTree
            structure PRINTTREE  = PrintTree
            structure LEX        = Lex
            structure STRUCTVALS = StructVals
            structure EXPORTTREE = ExportTreeStruct
            structure TYPETREE   = TypeTree
            structure DEBUGGER   = Debugger
         )

    structure TypeCheckParsetree =
        TYPECHECK_PARSETREE (
            structure BASEPARSETREE = BaseParseTree
            structure PRINTTREE  = PrintTree
            structure EXPORTTREE = ExportTree
            structure LEX        = Lex
            structure CODETREE   = CodeTree
            structure STRUCTVALS = StructVals
            structure TYPETREE   = TypeTree
            structure VALUEOPS   = ValueOps
            structure UTILITIES  = Utilities
            structure MISC          = Misc
            structure PRETTY     = Pretty
            structure COPIER     = CopierStruct
            structure DATATYPEREP = DatatypeRep
            structure DEBUG      = Debug
        )

    structure MatchCompiler =
        MATCH_COMPILER(
            structure BASEPARSETREE = BaseParseTree
            structure PRINTTREE  = PrintTree
            structure LEX        = Lex
            structure CODETREE   = CodeTree
            structure DEBUGGER   = Debugger
            structure TYPETREE   = TypeTree
            structure TYPEIDCODE = TypeIDCodeStruct
            structure STRUCTVALS = StructVals
            structure VALUEOPS   = ValueOps
            structure DATATYPEREP = DatatypeRep
            structure DEBUG      = Debug
            structure MISC          = Misc
            structure ADDRESS    = Address
        )

    structure CodeGenParseTree =
        CODEGEN_PARSETREE(
            structure BASEPARSETREE = BaseParseTree
            structure PRINTTREE  = PrintTree
            structure EXPORTTREE = ExportTree
            structure MATCHCOMPILER = MatchCompiler
            structure LEX        = Lex
            structure CODETREE   = CodeTree
            structure DEBUGGER   = Debugger
            structure TYPETREE   = TypeTree
            structure TYPEIDCODE = TypeIDCodeStruct
            structure STRUCTVALS = StructVals
            structure VALUEOPS   = ValueOps
            structure DATATYPEREP = DatatypeRep
            structure DEBUG      = Debug
            structure MISC          = Misc
            structure ADDRESS    = Address
        )
    
in
    structure ParseTree =
        PARSE_TREE (
            structure BASEPARSETREE = BaseParseTree
            structure PRINTTREE  = PrintTree
            structure EXPORTTREE = ExportTree
            structure CODEGENPARSETREE = CodeGenParseTree
            structure TYPECHECKTREE = TypeCheckParsetree
            structure LEX        = Lex
            structure STRUCTVALS = StructVals
            structure TYPETREE   = TypeTree
        )
end;

