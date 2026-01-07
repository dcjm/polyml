(*
    Title:      Standard Basis Library: Generic socket structure and signature.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2016, 2019

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


signature GENERIC_SOCK =
sig
    val socket : Socket.AF.addr_family * Socket.SOCK.sock_type
                    -> ('af, 'sock_type) Socket.sock
    val socketPair : Socket.AF.addr_family
                        * Socket.SOCK.sock_type
                        -> ('af, 'sock_type) Socket.sock
                        * ('af, 'sock_type) Socket.sock
    val socket' : Socket.AF.addr_family
                     * Socket.SOCK.sock_type
                     * int -> ('af, 'sock_type) Socket.sock
    val socketPair' : Socket.AF.addr_family
                         * Socket.SOCK.sock_type
                         * int
                         -> ('af, 'sock_type) Socket.sock
                         * ('af, 'sock_type) Socket.sock
end;

structure GenericSock : GENERIC_SOCK =
struct
    local
        val doCreateSocket: Socket.AF.addr_family * Socket.SOCK.sock_type * int -> OS.IO.iodesc =
            RunCall.rtsCallFull3 "PolyNetworkCreateSocket"
    in
        fun socket' (af, st, p) = LibraryIOSupport.SOCK(doCreateSocket(af, st, p))
    end

    local
        val doCreateSocketPair:
            Socket.AF.addr_family * Socket.SOCK.sock_type * int -> OS.IO.iodesc * OS.IO.iodesc =
                RunCall.rtsCallFull3 "PolyNetworkCreateSocketPair"
    in
        fun socketPair' (af, st, p: int) =
        let
            val (a, b) = doCreateSocketPair (af, st, p)
        in
            (LibraryIOSupport.SOCK a, LibraryIOSupport.SOCK b)
        end
    end
    (* We assume that the default protocol is always zero. *)
    fun socket(af, st) = socket'(af, st, 0)
    fun socketPair(af, st) = socketPair'(af, st, 0)
end;

