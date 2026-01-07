(*
    Title:      Standard Basis Library: Unix socket structure and signature.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2016, 2019

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

signature UNIX_SOCK =
sig
    type unix
    type 'sock_type sock = (unix, 'sock_type) Socket.sock
    type 'mode stream_sock = 'mode Socket.stream sock
    type dgram_sock = Socket.dgram sock
    type sock_addr = unix Socket.sock_addr
    val unixAF : Socket.AF.addr_family
    val toAddr : string -> sock_addr
    val fromAddr : sock_addr -> string
    structure Strm :
    sig
        val socket : unit -> 'mode stream_sock
        val socketPair : unit -> 'mode stream_sock * 'mode stream_sock
    end
    structure DGrm :
    sig
        val socket : unit -> dgram_sock
        val socketPair : unit -> dgram_sock * dgram_sock
    end
end;

structure UnixSock : UNIX_SOCK =
struct
    abstype unix = ABSTRACT with end;
    type 'sock_type sock = (unix, 'sock_type) Socket.sock
    type 'mode stream_sock = 'mode Socket.stream sock
    type dgram_sock = Socket.dgram sock
    type sock_addr = unix Socket.sock_addr

    val unixAF : Socket.AF.addr_family =
        case Socket.AF.fromString "UNIX" of
            NONE => raise OS.SysErr("Missing address family", NONE)
        |   SOME s => s

    val toAddr: string -> sock_addr = RunCall.rtsCallFull1 "PolyNetworkUnixPathToSockAddr"
    and fromAddr: sock_addr -> string = RunCall.rtsCallFull1 "PolyNetworkUnixSockAddrToPath"

    structure Strm =
    struct
        fun socket() = GenericSock.socket(unixAF, Socket.SOCK.stream)
        fun socketPair() = GenericSock.socketPair(unixAF, Socket.SOCK.stream)
    end
    structure DGrm =
    struct
        fun socket() = GenericSock.socket(unixAF, Socket.SOCK.dgram)
        fun socketPair() = GenericSock.socketPair(unixAF, Socket.SOCK.dgram)
    end

end;
