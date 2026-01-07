(*
    Copyright David C. J. Matthews 2017-8, 2021

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

signature X86ICODEGENERATE =
sig
    type intSet and extendedBasicBlock and regProperty and reg
    type closureRef
    
    val icodeToX86Code :
        {
            blocks: extendedBasicBlock vector, allocatedRegisters: reg vector, functionName: string,
            stackRequired: int, debugSwitches: Universal.universal list, resultClosure: closureRef,
            profileObject: Address.machineWord
        } -> unit
    
    structure Sharing:
    sig
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and reg                 = reg
        and closureRef          = closureRef
    end
end;
