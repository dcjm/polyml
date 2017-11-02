(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

local
    structure CodeCons =
        INTCODECONS (
            structure DEBUG   = Debug
            structure MISC    = Misc
            structure PRETTY  = Pretty
        )
in
    structure ByteCode = 
        INTGCODE (
            structure CODECONS     = CodeCons
            structure BACKENDTREE  = BackendIntermediateCode
            structure MISC         = Misc
        )
end;
 