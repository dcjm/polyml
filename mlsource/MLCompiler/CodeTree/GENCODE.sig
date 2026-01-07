(*
    Copyright (c) 2016, 2017, 2021 David C.J. Matthews

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

signature GENCODE =
sig
    type backendIC and argumentType and machineWord and bicLoadForm and closureRef
    type bicLambdaForm =
    {
        body          : backendIC,
        name          : string,
        closure       : bicLoadForm list,
        argTypes      : argumentType list,
        resultType    : argumentType,
        localCount    : int
    }
    val gencodeLambda: bicLambdaForm * Universal.universal list * closureRef -> unit
   
    structure Foreign: FOREIGNCALL

    structure Sharing:
    sig
        type backendIC = backendIC
        and argumentType = argumentType
        and bicLoadForm = bicLoadForm
        and closureRef = closureRef
    end
end;
