(*
    Title:      Standard Basis Library: NetProtDB Structures and Signatures
    Author:     David Matthews
    Copyright   David Matthews 2000, 2016

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

signature NET_PROT_DB =
sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val protocol : entry -> int
    val getByName : string -> entry option
    val getByNumber : int -> entry option
end;

structure NetProtDB :> NET_PROT_DB =
struct
    type entry = string * string list * int
    
    (* The RTS calls return either zero or the address of the entry. *)
    datatype result = AResult of entry | NoResult

    val name: entry -> string = #1
    val aliases : entry -> string list = #2
    val protocol : entry -> int = #3

    local
        val doCall: string -> result = RunCall.rtsCallFull1 "PolyNetworkGetProtByName"
    in
        fun getByName s =
            case doCall s of AResult r => SOME r | NoResult => NONE
    end

    local
        val doCall: int -> result = RunCall.rtsCallFull1 "PolyNetworkGetProtByNo"
    in
        fun getByNumber n =
            case doCall n of AResult r => SOME r | NoResult => NONE
    end

end;
