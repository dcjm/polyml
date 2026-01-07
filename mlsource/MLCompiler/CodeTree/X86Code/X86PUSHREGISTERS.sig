(*
    Copyright David C. J. Matthews 2017-18

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

signature X86PUSHREGISTERS =
sig
    type extendedBasicBlock and basicBlock and regProperty

    val addRegisterPushes :
        { code: extendedBasicBlock vector, pushVec: bool vector, pregProps: regProperty vector, firstPass: bool } ->
            { code: basicBlock vector, pregProps: regProperty vector, maxStack: int }

    structure Sharing:
    sig
        type extendedBasicBlock = extendedBasicBlock
        and basicBlock = basicBlock
        and regProperty = regProperty
    end
end;
