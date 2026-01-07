(*
    Copyright David C. J. Matthews 2021

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

signature ARM64ICODETRANSFORM =
sig
    type ('genReg, 'optGenReg, 'fpReg) basicBlock and regProperty and closureRef and preg and pregOrZero

    val codeICodeFunctionToArm64: {blocks: (preg, pregOrZero, preg) basicBlock vector, functionName: string, pregProps: regProperty vector,
                                 ccCount: int, debugSwitches: Universal.universal list, resultClosure: closureRef,
                                 profileObject: Address.machineWord} -> unit

    structure Sharing:
    sig
        type ('genReg, 'optGenReg, 'fpReg) basicBlock  = ('genReg, 'optGenReg, 'fpReg) basicBlock
        and regProperty = regProperty
        and closureRef  = closureRef
        and preg        = preg
        and pregOrZero  = pregOrZero
    end
end;
