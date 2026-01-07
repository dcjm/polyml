(*
    Copyright (c) 2016, 2018-20 David C.J. Matthews

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

signature FOREIGNCALL =
sig
    val rtsCallFast: string * int * Universal.universal list -> Address.machineWord
    val rtsCallFastRealtoReal: string * Universal.universal list -> Address.machineWord
    val rtsCallFastRealRealtoReal: string * Universal.universal list -> Address.machineWord
    val rtsCallFastGeneraltoReal: string * Universal.universal list -> Address.machineWord
    val rtsCallFastRealGeneraltoReal: string * Universal.universal list -> Address.machineWord
    val rtsCallFastFloattoFloat: string * Universal.universal list -> Address.machineWord
    val rtsCallFastFloatFloattoFloat: string * Universal.universal list -> Address.machineWord
    val rtsCallFastGeneraltoFloat: string * Universal.universal list -> Address.machineWord
    val rtsCallFastFloatGeneraltoFloat: string * Universal.universal list -> Address.machineWord
    
    type abi and cType
    val abiList: unit -> (string * abi) list
    
    val foreignCall: abi * cType list * cType -> Address.machineWord
    val buildCallBack: abi * cType list * cType -> Address.machineWord
end;
