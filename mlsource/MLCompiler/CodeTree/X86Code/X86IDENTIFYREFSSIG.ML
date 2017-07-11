(*
    Copyright (c) 2016 David C.J. Matthews

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

signature X86IDENTIFYREFSSIG =
sig
    type 'reg x86ICode and reg and abstract

    datatype pushState = MustPush | MustNotPush | MayPush

    type regState =
    { 
        active: int, defs: int, refs: int, pushState: pushState,
        conflicts: int list, realConflicts: reg list
    }
    
    val identifyRegisters:
        {icode: abstract x86ICode list, maxPRegs: int} -> (abstract x86ICode * int list) list * regState vector

    structure Sharing:
    sig
        type 'reg x86ICode = 'reg x86ICode
        and reg = reg
        and pushState = pushState
        and abstract = abstract
    end;
end;
