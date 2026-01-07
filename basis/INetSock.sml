(*
    Title:      Standard Basis Library: Internet Sockets
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

signature NET_HOST_DB =
sig
    eqtype in_addr
    eqtype addr_family
    type entry
    val name : entry -> string
    val aliases : entry -> string list
    val addrType : entry -> addr_family
    val addr : entry -> in_addr
    val addrs : entry -> in_addr list

    val getByName : string -> entry option
    val getByAddr : in_addr -> entry option
    val getHostName : unit -> string
    val scan : (char, 'a) StringCvt.reader
              -> (in_addr, 'a) StringCvt.reader
    val fromString : string -> in_addr option
    val toString : in_addr -> string
end;

local
    fun power2 0 = 1: LargeInt.int
    |   power2 n = 2 * power2(n-1)
    val p32 = power2 32
    val p24 = power2 24

    fun scan getc src =
    let
        (* Read a number as either decimal, hex or octal up to the
           given limit. Stops when it reaches the limit or finds a
           character it doesn't recognise. *)
        fun readNum base acc limit src =
        let
            fun addDigit d src =
            let
                val n = case acc of SOME(n, _) => n | NONE => 0
                val next = n * LargeInt.fromInt base + LargeInt.fromInt d
            in
                (* If we are below the limit we can continue. *)
                if next < limit
                then readNum base (SOME(next, src)) limit src
                else acc
            end
        in
            case getc src of
                NONE => acc
            |   SOME(ch, src') =>
                    if Char.isDigit ch andalso
                        ch < Char.chr(Char.ord #"0" + base)
                    then addDigit (Char.ord ch - Char.ord #"0") src'
                    else if base = 16 andalso (ch >= #"A" andalso ch <= #"F")
                    then addDigit (Char.ord ch - Char.ord #"A" + 10) src'
                    else if base = 16 andalso (ch >= #"a" andalso ch <= #"f")
                    then addDigit (Char.ord ch - Char.ord #"a" + 10) src'
                    else acc
        end

        (* Read a number.  If it starts with 0x or 0X treat it
           as hex, otherwise if it starts with 0 treat as octal
           otherwise decimal. *)
        fun scanNum limit src =
            case getc src of
                NONE => NONE
            |   SOME (#"0", src') =>
                (
                    case getc src' of
                        SOME(ch, src'') =>
                            if ch = #"x" orelse ch = #"X"
                            then
                                (
                                (* If it is invalid we have still read a
                                   zero so return that. *)
                                case readNum 16 NONE limit src'' of
                                    NONE => SOME(0, src')
                                |   res => res
                                )
                            else (* Octal - include the zero. *)
                                readNum 8 NONE limit src
                    |   NONE => SOME(0, src') (* Just the zero. *)
                )
            |   SOME (_, _) => (* Treat it as a decimal number. *)
                    readNum 10 NONE limit src

        fun scanAddr src limit i acc =
            case scanNum limit src of
                NONE => NONE
            |   SOME(n, src') =>
                let
                    val res = acc*256 + n (* This is the accumulated result. *)
                in
                    (* If the result is more than 24 bits or we've read
                       all the sections we're finished. *)
                    if res >= p24 orelse i = 1 then SOME(res, src')
                    else
                        case getc src' of
                            SOME (#".", src'') =>
                            (
                                (* The limit for sections other than the
                                   first is 256. *)
                                case scanAddr src'' 256 (i-1) res of
                                    NONE => SOME(res, src') (* Return what we had. *)
                                |   r => r
                            )
                        |   _ => SOME(res, src') (* Return what we've got. *)
                end
    in
        scanAddr src p32 4 (* Four sections in all. *) 0
    end (* scan *)
    
    structure INet4Addr :>
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
        type in_addr = LargeInt.int
       
        abstype inet = ABSTRACT with end;

        type sock_addr = inet Socket.sock_addr

        val inetAF =
            case Socket.AF.fromString "INET" of
                NONE => raise OS.SysErr("Missing address family", NONE)
            |   SOME s => s
       
        val scan = scan
        and fromString = StringCvt.scanString scan
    
        fun toString (n: in_addr) =
        let
            fun pr n i =
                (if i > 0 then pr (n div 256) (i-1) ^ "." else "") ^
                    LargeInt.toString (n mod 256)
                
        in
            pr n 3 (* Always generate 4 numbers. *)
        end

        val toAddr: in_addr * int -> sock_addr = RunCall.rtsCallFull2 "PolyNetworkCreateIP4Address"
        and fromAddr: sock_addr -> in_addr * int = RunCall.rtsCallFull1 "PolyNetworkGetAddressAndPortFromIP4"

        local
            val getAddrAny: unit -> in_addr = RunCall.rtsCallFull0 "PolyNetworkReturnIP4AddressAny"
            val iAddrAny: in_addr = getAddrAny()
        in
            fun any (p: int) : sock_addr = toAddr(iAddrAny, p)
        end

    end

in
    structure NetHostDB :> NET_HOST_DB where type in_addr = INet4Addr.in_addr where type addr_family = Socket.AF.addr_family =
    struct
        open INet4Addr
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

    and INetSock =
    struct
        open INet4Addr
 
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


(* These use NetHostDB in the signature which is a bit of a mess. *)

(* Apply type realisation. *)
signature SOCKET = sig include SOCKET end where type AF.addr_family = NetHostDB.addr_family;

signature INET_SOCK =
sig
    type inet

    type 'sock_type sock = (inet, 'sock_type) Socket.sock
    type 'mode stream_sock = 'mode Socket.stream sock

    type dgram_sock = Socket.dgram sock
    type sock_addr = inet Socket.sock_addr

    val inetAF : Socket.AF.addr_family
    val toAddr : NetHostDB.in_addr * int -> sock_addr
    val fromAddr : sock_addr -> NetHostDB.in_addr * int
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

structure INetSock :> INET_SOCK = INetSock;

local
    (* Install the pretty printer for NetHostDB.in_addr.
       This must be done outside
       the structure if we use opaque matching. *)
    fun printAddr _ _ x = PolyML.PrettyString(NetHostDB.toString x)
in
    val () = PolyML.addPrettyPrinter printAddr
end;
