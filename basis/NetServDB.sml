(*
    Title:      Standard Basis Library: NetServDB Structures and Signatures
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

signature NET_SERV_DB =
sig
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val port : entry -> int
    val protocol : entry -> string
    val getByName : string * string option -> entry option
    val getByPort : int * string option -> entry option
end;

structure NetServDB :> NET_SERV_DB =
struct
    type entry = string * string list * int * string
    
    (* The RTS calls return either zero or the address of the entry. *)
    datatype result = AResult of entry | NoResult

    val name: entry -> string = #1
    val aliases : entry -> string list = #2
    val port : entry -> int = #3
    val protocol : entry -> string = #4

    local
        val doCall1: string -> result
             = RunCall.rtsCallFull1 "PolyNetworkGetServByName"
        and doCall2: string*string -> result
             = RunCall.rtsCallFull2 "PolyNetworkGetServByNameAndProtocol"
    in
        fun getByName(s, NONE) =
            (case doCall1 s of AResult r => SOME r | NoResult => NONE)

         |  getByName(s, SOME p) =
            (case doCall2(s, p) of AResult r => SOME r | NoResult => NONE)
    end

    local
        val doCall1: int -> result
             = RunCall.rtsCallFull1 "PolyNetworkGetServByPort"
        and doCall2: int*string -> result
             = RunCall.rtsCallFull2 "PolyNetworkGetServByPortAndProtocol"
    in
        fun getByPort(n, NONE) =
            (case doCall1 n of AResult r => SOME r | NoResult => NONE)
         |  getByPort(n, SOME p) =
            (case doCall2(n, p) of AResult r => SOME r | NoResult => NONE)
    end

end;
