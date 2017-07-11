(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
    
    Modified David C. J. Matthews 2016.

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

structure Initialise =
    INITIALISE_ (
        structure LEX = Lex
        structure TYPETREE   = TypeTree
        structure STRUCTVALS = StructVals
        structure VALUEOPS   = ValueOps
        structure CODETREE   = CodeTree
        structure ADDRESS    = Address
        structure DEBUG      = Debug
        structure MAKE       = Make
        structure MISC       = Misc
        structure DEBUGGER   = Debugger
        structure VERSION    = CompilerVersion
        structure PRETTY     = Pretty
        structure EXPORTTREE = ExportTreeStruct
        structure DATATYPEREP = DatatypeRep
        structure TYPEIDCODE = TypeIDCodeStruct
        structure PRINTTABLE = PrintTable
        structure UNIVERSALTABLE = UniversalTable
);
