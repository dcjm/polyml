(*
    Copyright David C. J. Matthews 2017

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

signature X86ICODEGENERATESIG =
sig
    type intSet and extendedBasicBlock and regProperty and reg
    type address = Address.address 
    
    val icodeToX86Code :
        {
            blocks: extendedBasicBlock vector, allocatedRegisters: reg vector, functionName: string,
            stackRequired: int, argRegsUsed: reg list, hasFullClosure: bool, debugSwitches: Universal.universal list
        } -> address
    
    val nGenRegs: int (* Number of general registers. *)
    
    structure Sharing:
    sig
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and reg                 = reg
    end
end;
