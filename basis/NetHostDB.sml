(*
    Title:      Standard Basis Library: NetHostDB and NetDB Structures and Signatures
    Author:     David Matthews
    Copyright   David Matthews 2000, 2016

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation
    
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

in
    structure NetHostDB :> NET_HOST_DB =
    struct
        type in_addr = LargeInt.int
        and addr_family = int
        type entry = string * string list * addr_family * in_addr list
        val name: entry -> string = #1
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
        
        (* The RTS calls return either zero or the address of the entry. *)
        datatype result = AResult of entry | NoResult

        local
            val doCall: string -> result
                 = RunCall.rtsCallFull1 "PolyNetworkGetHostByName"
        in
            fun getByName s =
                case doCall s of AResult r => SOME r | NoResult => NONE
        end
    
        local
            val doCall: LargeInt.int -> result
                 = RunCall.rtsCallFull1 "PolyNetworkGetHostByAddr"
        in
            fun getByAddr n =
                case doCall n of AResult r => SOME r | NoResult => NONE
        end
    
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
    end;

end;


local
    (* Install the pretty printer for NetHostDB.in_addr.
       This must be done outside
       the structure if we use opaque matching. *)
    fun printAddr _ _ x = PolyML.PrettyString(NetHostDB.toString x)
in
    val () = PolyML.addPrettyPrinter printAddr
end

