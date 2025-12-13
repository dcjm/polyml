(*
    Copyright (c) 2013, 2015, 2025 David C.J. Matthews

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

signature CodegenParsetreeSig =
sig
    type parsetree
    type lexan
    type level
    type codetree
    type codeBinding
    type environEntry

    type debuggerStatus

    val gencode:
        parsetree * lexan * debuggerStatus * level * (int->int) * string *
            (codeBinding list * debuggerStatus -> codeBinding list * debuggerStatus)
            -> codeBinding list * debuggerStatus

    structure Sharing:
    sig
        type parsetree = parsetree
        and  lexan = lexan
        and  codetree = codetree
        and  environEntry = environEntry
        and  level = level
        and  codeBinding = codeBinding
        and  debuggerStatus  = debuggerStatus
    end
end
