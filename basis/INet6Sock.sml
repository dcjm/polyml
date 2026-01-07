(*
    Title:      Standard Basis Library: IPV6 Sockets
    Author:     David Matthews
    Copyright   David Matthews 2019

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

(* Based on INetSock and NetHostDB.  They're sufficiently similar that
   we could use a common functor. *)
local
    structure INet6Addr :>
    sig
        eqtype in_addr
        type inet
        type sock_addr = inet Socket.sock_addr
        val inetAF : Socket.AF.addr_family
 
        val scan : (char, 'a) StringCvt.reader
                  -> (in_addr, 'a) StringCvt.reader
        val fromString : string -> in_addr option
        val toString : in_addr -> string

        val toAddr : in_addr * int -> sock_addr
        val fromAddr : sock_addr -> in_addr * int
        val any : int -> sock_addr
    end
    =
    struct
        type in_addr = Word8Vector.vector
       
        abstype inet = ABSTRACT with end;

        type sock_addr = inet Socket.sock_addr

        val inetAF =
            case Socket.AF.fromString "INET6" of
                NONE => raise OS.SysErr("Missing address family", NONE)
            |   SOME s => s


        (* Scan an IPv6 address.  Strictly, we should try to recognise sufficient
           of the input stream to produce a single IPv6 address and no more.
           Instead we read the input stream so long as it contains characters
           that could form part of an address i.e. hex characters, colon and
           full-stop (because of the special IPv4 compatibility format)
           then pass the result to inet_pton to try to convert it. *)
        local
            val convert: string -> in_addr = RunCall.rtsCallFull1 "PolyNetworkStringToIP6Address"
        in
            fun scan getc src =
            let
                fun isValid #":" = true
                |   isValid #"." = true
                |   isValid ch =
                        Char.isDigit ch orelse ch >= #"A" andalso ch <= #"F" orelse ch >= #"a" andalso ch <= #"f"
                
                fun whileValid l src =
                    case getc src of
                        NONE => (l, src)
                    |   SOME(ch, src') =>
                            if isValid ch then whileValid(ch :: l) src' else (l, src)

                val (input, src') = whileValid [] src
            in
                SOME(convert(String.implode(List.rev input)), src')
                    handle Fail _ => NONE
            end
        end
    
        val fromString = StringCvt.scanString scan
        (* This conversion could be done in ML but the rules for producing the canonical
           version are complicated when there are zeros. *)
        and toString: in_addr -> string = RunCall.rtsCallFull1 "PolyNetworkIP6AddressToString"

        val toAddr: in_addr * int -> sock_addr = RunCall.rtsCallFull2 "PolyNetworkCreateIP6Address"
        and fromAddr: sock_addr -> in_addr * int = RunCall.rtsCallFull1 "PolyNetworkGetAddressAndPortFromIP6"

        local
            val getAddrAny: unit -> in_addr = RunCall.rtsCallFull0 "PolyNetworkReturnIP6AddressAny"
            val iAddrAny: in_addr = getAddrAny()
        in
            fun any (p: int) : sock_addr = toAddr(iAddrAny, p)
        end

    end

in
    structure Net6HostDB :> NET_HOST_DB where type in_addr = INet6Addr.in_addr where type addr_family = Socket.AF.addr_family =
    struct
        open INet6Addr
        type addr_family = Socket.AF.addr_family
        type entry = string * string list * addr_family * in_addr list
        val name: entry -> string = #1
        (* aliases now always returns the empty list. *)
        val aliases : entry -> string list = #2
        val addrType : entry -> addr_family = #3
        val addrs : entry -> in_addr list = #4
    
        (* Addr returns the first address in the list. There should always
           be at least one entry. *)
        fun addr e =
            case addrs e of
                a :: _ => a
             |  [] => raise OS.SysErr("No address returned", NONE)
    
        val getHostName: unit -> string = RunCall.rtsCallFull0 "PolyNetworkGetHostName"

        local
            type addrInfo = int * Socket.AF.addr_family * int * int * sock_addr * string
            val getAddrInfo: string * addr_family -> addrInfo list =
                RunCall.rtsCallFull2 "PolyNetworkGetAddrInfo"
        in
            fun getByName s =
            (
                case getAddrInfo(s, inetAF) of
                    [] => NONE
                |   l as ((_, family, _, _, _, name) :: _) =>
                        SOME (name, [], family, map (#1 o fromAddr o #5) l)
            ) handle OS.SysErr _ => NONE
        end

        local
            (* This does a reverse lookup of the address to return the name. *)
            val doCall: sock_addr -> string = RunCall.rtsCallFull1 "PolyNetworkGetNameInfo"
        in
            fun getByAddr n =
            (
                (* Create an entry out of this.  We could do a forward look-up
                   of the resulting address but there doesn't seem to be any point. *)
                SOME(doCall(toAddr(n, 0)), [], inetAF, [n])
            ) handle OS.SysErr _ => NONE
        end
     end

    and INet6Sock =
    struct
        open INet6Addr
 
        type 'sock_type sock = (inet, 'sock_type) Socket.sock
        type 'mode stream_sock = 'mode Socket.stream sock

        type dgram_sock = Socket.dgram sock

        local
            val doSetOpt: int * OS.IO.iodesc * int -> unit =
                RunCall.rtsCallFull3 "PolyNetworkSetOption"
            val doGetOpt: int * OS.IO.iodesc -> int =
                RunCall.rtsCallFull2 "PolyNetworkGetOption"
        in
            structure UDP =
            struct
                fun socket () = GenericSock.socket(inetAF, Socket.SOCK.dgram)
                fun socket' p = GenericSock.socket'(inetAF, Socket.SOCK.dgram, p)
            end

            structure TCP =
            struct
                fun socket () = GenericSock.socket(inetAF, Socket.SOCK.stream)
                fun socket' p = GenericSock.socket'(inetAF, Socket.SOCK.stream, p)

                fun getNODELAY(LibraryIOSupport.SOCK s: 'mode stream_sock): bool = doGetOpt(16, s) <> 0

                fun setNODELAY (LibraryIOSupport.SOCK s: 'mode stream_sock, b: bool): unit =
                    doSetOpt(15, s, if b then 1 else 0)
            end
        end 

    end;
end;

(* We can't use the INET_SOCK signature because that binds in NetHostDB. *)
signature INET6_SOCK =
sig
    type inet

    type 'sock_type sock = (inet, 'sock_type) Socket.sock
    type 'mode stream_sock = 'mode Socket.stream sock

    type dgram_sock = Socket.dgram sock
    type sock_addr = inet Socket.sock_addr

    val inetAF : Socket.AF.addr_family
    val toAddr : Net6HostDB.in_addr * int -> sock_addr
    val fromAddr : sock_addr -> Net6HostDB.in_addr * int
    val any : int -> sock_addr
    structure UDP :
    sig
        val socket : unit -> dgram_sock
        val socket' : int -> dgram_sock
    end
    structure TCP :
    sig
        val socket : unit -> 'mode stream_sock
        val socket' : int -> 'mode stream_sock
        val getNODELAY : 'mode stream_sock -> bool
        val setNODELAY : 'mode stream_sock * bool -> unit
    end
end;

structure INet6Sock :> INET6_SOCK = INet6Sock;

local
    (* Install the pretty printer for NetHostDB.in_addr.
       This must be done outside
       the structure if we use opaque matching. *)
    fun printAddr _ _ x = PolyML.PrettyString(Net6HostDB.toString x)
in
    val () = PolyML.addPrettyPrinter printAddr
end;
