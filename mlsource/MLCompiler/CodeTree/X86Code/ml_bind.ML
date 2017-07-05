(*
    Copyright David C. J. Matthews 2015-16

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
        X86OUTPUTCODE(
            structure DEBUG   = Debug
            structure PRETTY  = Pretty
        )

    structure X86Optimise =
        X86OPTIMISE(
            structure X86CODE = X86OutputCode
        )

    structure X86ForeignCall =
        X86FOREIGNCALL (
            structure DEBUG         = Debug
            structure X86CODE       = X86OutputCode
            structure X86OPTIMISE   = X86Optimise
        )

    structure X86ICode =
        X86ICodeToX86Code (
            structure DEBUG         = Debug
            structure X86CODE       = X86OutputCode
            structure X86OPTIMISE   = X86Optimise
        )
        
    structure X86ICodeTransform =
        X86ICodeTransform (
            structure DEBUG         = Debug
            structure ICODE         = X86ICode
        )

    structure X86OldCodeCons =
        X86FIRST_STAGE(
            structure X86CODE = X86OutputCode
            structure X86OPTIMISE = X86Optimise
    	    structure DEBUG    = Debug
    	    structure MISC     = Misc
    	    structure PRETTY   = Pretty
        )

    structure X86OldCodeGen =
        GENERATE_CODE(
            structure CODECONS = X86OldCodeCons
            structure BACKENDTREE   = BackendIntermediateCode
            structure DEBUG         = Debug
            structure X86FOREIGN    = X86ForeignCall
        )

in

    structure X86Code = 
        X86CodetreeToICode (
            structure BACKENDTREE   = BackendIntermediateCode
            structure DEBUG         = Debug
            structure ICODE         = X86ICode
            structure X86FOREIGN    = X86ForeignCall
            structure ICODETRANSFORM = X86ICodeTransform
            structure X86OLDCODEGEN = X86OldCodeGen
        )

end;
