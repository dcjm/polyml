(*
    Title:      Standard Basis Library: Generic Sockets
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2015-16

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

signature SOCKET =
sig
    type ('af,'sock_type) sock
    type 'af sock_addr
    type dgram
    type 'mode stream
    type passive
    type active

    structure AF :
    sig
        type addr_family = NetHostDB.addr_family
        val list : unit -> (string * addr_family) list
        val toString   : addr_family -> string
        val fromString : string -> addr_family option
    end

    structure SOCK :
    sig
        eqtype sock_type
        val stream : sock_type
        val dgram : sock_type
        val list : unit -> (string * sock_type) list
        val toString   : sock_type -> string
        val fromString : string -> sock_type option
    end

    structure Ctl :
    sig
         val getDEBUG : ('af, 'sock_type) sock -> bool
         val setDEBUG : ('af, 'sock_type) sock * bool -> unit
         val getREUSEADDR : ('af, 'sock_type) sock -> bool
         val setREUSEADDR : ('af, 'sock_type) sock * bool -> unit
         val getKEEPALIVE : ('af, 'sock_type) sock -> bool
         val setKEEPALIVE : ('af, 'sock_type) sock * bool -> unit
         val getDONTROUTE : ('af, 'sock_type) sock -> bool
         val setDONTROUTE : ('af, 'sock_type) sock * bool -> unit
         val getLINGER : ('af, 'sock_type) sock -> Time.time option
         val setLINGER : ('af, 'sock_type) sock * Time.time option -> unit
         val getBROADCAST : ('af, 'sock_type) sock -> bool
         val setBROADCAST : ('af, 'sock_type) sock * bool -> unit
         val getOOBINLINE : ('af, 'sock_type) sock -> bool
         val setOOBINLINE : ('af, 'sock_type) sock * bool  -> unit
         val getSNDBUF : ('af, 'sock_type) sock -> int
         val setSNDBUF : ('af, 'sock_type) sock * int -> unit
         val getRCVBUF : ('af, 'sock_type) sock -> int
         val setRCVBUF : ('af, 'sock_type) sock * int -> unit
         val getTYPE : ('af, 'sock_type) sock -> SOCK.sock_type
         val getERROR : ('af, 'sock_type) sock -> bool
         val getPeerName : ('af, 'sock_type) sock -> 'af sock_addr
         val getSockName : ('af, 'sock_type) sock -> 'af sock_addr
         val getNREAD : ('af, 'sock_type) sock -> int
         val getATMARK : ('af, active stream) sock -> bool
         end

     val sameAddr : 'af sock_addr * 'af sock_addr -> bool
     val familyOfAddr : 'af sock_addr -> AF.addr_family

     val bind : ('af, 'sock_type) sock * 'af sock_addr -> unit
     val listen : ('af, passive stream) sock * int -> unit
     val accept : ('af, passive stream) sock
                    -> ('af, active stream) sock * 'af sock_addr
     val acceptNB : ('af, passive stream) sock
                    -> (('af, active stream) sock * 'af sock_addr) option
     val connect : ('af, 'sock_type) sock * 'af sock_addr -> unit
     val connectNB : ('af, 'sock_type) sock * 'af sock_addr -> bool
     val close : ('af, 'sock_type) sock -> unit

     datatype shutdown_mode
       = NO_RECVS
       | NO_SENDS
       | NO_RECVS_OR_SENDS

     val shutdown : ('af, 'sock_type stream) sock * shutdown_mode -> unit

     type sock_desc
     val sockDesc : ('af, 'sock_type) sock -> sock_desc
     val sameDesc: sock_desc * sock_desc -> bool

     
     val select:
            { rds: sock_desc list, wrs : sock_desc list, exs : sock_desc list, timeout: Time.time option } ->
            { rds: sock_desc list, wrs : sock_desc list, exs : sock_desc list }
     
     val ioDesc : ('af, 'sock_type) sock -> OS.IO.iodesc

     type out_flags = {don't_route : bool, oob : bool}
     type in_flags = {peek : bool, oob : bool}

     val sendVec : ('af, active stream) sock * Word8VectorSlice.slice -> int
     val sendArr : ('af, active stream) sock * Word8ArraySlice.slice -> int
     val sendVec' : ('af, active stream) sock * Word8VectorSlice.slice
                      * out_flags -> int
     val sendArr' : ('af, active stream) sock * Word8ArraySlice.slice
                      * out_flags -> int
     val sendVecNB : ('af, active stream) sock * Word8VectorSlice.slice -> int option
     val sendArrNB : ('af, active stream) sock * Word8ArraySlice.slice -> int option
     val sendVecNB' : ('af, active stream) sock * Word8VectorSlice.slice
                      * out_flags -> int option
     val sendArrNB' : ('af, active stream) sock * Word8ArraySlice.slice
                      * out_flags -> int option
                      
     val recvVec : ('af, active stream) sock * int -> Word8Vector.vector
     val recvArr : ('af, active stream) sock  * Word8ArraySlice.slice -> int
     val recvVec' : ('af, active stream) sock * int * in_flags
                      -> Word8Vector.vector
     val recvArr' : ('af, active stream) sock * Word8ArraySlice.slice
                      * in_flags -> int
     val recvVecNB : ('af, active stream) sock * int -> Word8Vector.vector option
     val recvArrNB : ('af, active stream) sock  * Word8ArraySlice.slice -> int option
     val recvVecNB' : ('af, active stream) sock * int * in_flags
                      -> Word8Vector.vector option
     val recvArrNB' : ('af, active stream) sock * Word8ArraySlice.slice
                      * in_flags -> int option

     val sendVecTo : ('af, dgram) sock * 'af sock_addr
                       * Word8VectorSlice.slice -> unit
     val sendArrTo : ('af, dgram) sock * 'af sock_addr
                       * Word8ArraySlice.slice -> unit
     val sendVecTo' : ('af, dgram) sock * 'af sock_addr
                        * Word8VectorSlice.slice * out_flags -> unit
     val sendArrTo' : ('af, dgram) sock * 'af sock_addr
                        * Word8ArraySlice.slice * out_flags -> unit
     val sendVecToNB : ('af, dgram) sock * 'af sock_addr
                       * Word8VectorSlice.slice -> bool
     val sendArrToNB : ('af, dgram) sock * 'af sock_addr
                       * Word8ArraySlice.slice -> bool
     val sendVecToNB' : ('af, dgram) sock * 'af sock_addr
                        * Word8VectorSlice.slice * out_flags -> bool
     val sendArrToNB' : ('af, dgram) sock * 'af sock_addr
                        * Word8ArraySlice.slice * out_flags -> bool

     val recvVecFrom : ('af, dgram) sock * int
                         -> Word8Vector.vector * 'sock_type sock_addr
     val recvArrFrom : ('af, dgram) sock * Word8ArraySlice.slice
                         -> int * 'af sock_addr
     val recvVecFrom' : ('af, dgram) sock * int * in_flags
                          -> Word8Vector.vector * 'sock_type sock_addr
     val recvArrFrom' : ('af, dgram) sock * Word8ArraySlice.slice
                          * in_flags -> int * 'af sock_addr
     val recvVecFromNB : ('af, dgram) sock * int
                         -> (Word8Vector.vector * 'sock_type sock_addr) option
     val recvArrFromNB : ('af, dgram) sock * Word8ArraySlice.slice
                         -> (int * 'af sock_addr) option
     val recvVecFromNB' : ('af, dgram) sock * int * in_flags
                          -> (Word8Vector.vector * 'sock_type sock_addr) option
     val recvArrFromNB' : ('af, dgram) sock * Word8ArraySlice.slice
                          * in_flags -> (int * 'af sock_addr) option
end;

structure Socket :> SOCKET =
struct
    (* We don't really need an implementation for these.  *)
    (* TODO: We should really pull the definition of the sock type into a common structure so
       it can be shared by the various socket structures.  In fact it doesn't matter since the
       unary constructor here is compiled as an identity so the underlying representation of
       "SOCK x" will be the same as "x". *)
    datatype ('af,'sock_type) sock = SOCK of OS.IO.iodesc
    and dgram = DGRAM
    and 'mode stream = STREAM
    and passive = PASSIVE
    and active = ACTIVE

    local
        val netCall: int * word -> word = RunCall.rtsCallFull2 "PolyNetworkGeneral"
    in
        fun doNetCall(i: int, arg:'a):'b =
            RunCall.unsafeCast(netCall(i, RunCall.unsafeCast arg))
    end

    structure AF =
    struct
        type addr_family = NetHostDB.addr_family

        local
            val doCall: int*unit -> (string * addr_family) list
                 = doNetCall
        in
            fun list () = doCall(11, ())
        end

        fun toString (af: addr_family) =
        let
            val afs = list()
        in
            (* Do a linear search on the list - it's small. *)
            case List.find (fn (_, af') => af=af') afs of
                NONE => raise OS.SysErr("Missing address family", NONE)
            |   SOME (s, _) => s
        end

        fun fromString s =
        let
            val afs = list()
        in
            (* Do a linear search on the list - it's small. *)
            case List.find (fn (s', _) => s=s') afs of
                NONE => NONE
            |   SOME (_, af) => SOME af
        end
    end

    structure SOCK =
    struct
        datatype sock_type = SOCKTYPE of int

        local
            val doCall: int*unit -> (string * sock_type) list
                 = doNetCall
        in
            fun list () = doCall(12, ())
        end

        fun toString (sk: sock_type) =
        let
            val sks = list()
        in
            (* Do a linear search on the list - it's small. *)
            case List.find (fn (_, sk') => sk=sk') sks of
                NONE => raise OS.SysErr("Missing socket type", NONE)
            |   SOME (s, _) => s
        end

        fun fromString s =
        let
            val sks = list()
        in
            (* Do a linear search on the list - it's small. *)
            case List.find (fn (s', _) => s=s') sks of
                NONE => NONE
            |   SOME (_, sk) => SOME sk
        end

        (* We assume that both of these at least are in the table. *)
        val stream =
            case fromString "STREAM" of
                NONE => raise OS.SysErr("Missing socket type", NONE)
            |   SOME s => s

        val dgram =
            case fromString "DGRAM" of
                NONE => raise OS.SysErr("Missing socket type", NONE)
            |   SOME s => s
    end

    (* Socket addresses are implemented as strings. *)
    datatype 'af sock_addr = SOCKADDR of Word8Vector.vector

    (* Note: The definition did not make these equality type variables.
       The assumption is probably that it works much like equality on
       references. *)
    fun sameAddr (SOCKADDR a, SOCKADDR b) = a = b

    (* Many of these calls involve type variables.  We have to use a cast to
       get the types right. *)
    local
        val doCall = doNetCall
    in
        fun familyOfAddr (sa: 'af sock_addr) = doCall(39, RunCall.unsafeCast sa)
    end

    structure Ctl =
    struct
        local
            val doCall1 = doNetCall
            val doCall2 = doNetCall
        in
            fun getOpt (i:int) (SOCK s) = doCall1(i, s)
            fun setOpt (i: int) (SOCK s, b: bool) = doCall2(i, (s, b))
        end

        fun getDEBUG s = getOpt 18 s
        and setDEBUG s = setOpt 17 s
        and getREUSEADDR s = getOpt 20 s
        and setREUSEADDR s = setOpt 19 s
        and getKEEPALIVE s = getOpt 22 s
        and setKEEPALIVE s = setOpt 21 s
        and getDONTROUTE s = getOpt 24 s
        and setDONTROUTE s = setOpt 23 s
        and getBROADCAST s = getOpt 26 s
        and setBROADCAST s = setOpt 25 s
        and getOOBINLINE s = getOpt 28 s
        and setOOBINLINE s = setOpt 27 s
        and getERROR s = getOpt 34 s
        and getATMARK s = getOpt 45 s

        local
            val doCall1 = doNetCall
            val doCall2 = doNetCall
        in
            fun getSNDBUF (SOCK s) = doCall1(30, s)
            fun setSNDBUF (SOCK s, i: int) = doCall2(29, (s, i))
            fun getRCVBUF (SOCK s) = doCall1(32, s)
            fun setRCVBUF (SOCK s, i: int) = doCall2(31, (s, i))
            fun getTYPE (SOCK s) = SOCK.SOCKTYPE(doCall1(33, s))
                    
            fun getNREAD (SOCK s) = doCall1(44, s)

            fun getLINGER (SOCK s): Time.time option =
            let
                val lTime = doCall1(36, s)
            in
                if lTime < 0 then NONE else SOME(Time.fromSeconds(LargeInt.fromInt lTime))
            end

            fun setLINGER (SOCK s, NONE) =
                (
                    doCall2(35, (s, ~1))
                )
            |   setLINGER (SOCK s, SOME t) =
                let
                    val lTime = LargeInt.toInt(Time.toSeconds t)
                in
                    if lTime < 0
                    then raise OS.SysErr("Invalid time", NONE)
                    else doCall2(35, (s, lTime))
                end
        end

        local
            val doCall = doNetCall
        in
            fun getPeerName (SOCK s): 'af sock_addr = RunCall.unsafeCast(doCall(37, s))

            fun getSockName (SOCK s): 'af sock_addr = RunCall.unsafeCast(doCall(38, s))
        end
        end (* Ctl *)


    (* Run an operation in non-blocking mode.  This catches EWOULDBLOCK and returns NONE,
       otherwise returns SOME result.  Other exceptions are passed back as normal. *)
    val nonBlockingCall = LibraryIOSupport.nonBlocking

    local
        val doCall = doNetCall
    in
        fun accept (SOCK s) = RunCall.unsafeCast(doCall (46, s))
    end

    local
        val doCall = doNetCall
        fun acc sock = doCall (58, RunCall.unsafeCast sock)
    in
        fun acceptNB sock = RunCall.unsafeCast(nonBlockingCall acc sock)
    end

    local
        val doCall = doNetCall
    in
        fun bind (SOCK s, a) = doCall (47, RunCall.unsafeCast (s, a))
    end

    local
        val doCall = doNetCall
    in
        fun connect (SOCK s, a) = doCall (48, RunCall.unsafeCast (s, a))
    end

    local
        val doCall = doNetCall
        fun connct sa = doCall (59, RunCall.unsafeCast sa)
    in
        fun connectNB (SOCK s, a) =
            case nonBlockingCall connct (s,a) of SOME () => true | NONE => false
    end

    fun listen (SOCK s, b) =
        doNetCall (49, (s, b))

    (* We use the normal "close" for streams. *)
    local
        val doCall = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
    in
        fun close (SOCK strm): unit = doCall(7, strm, 0)
    end

    datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS

    local
        val doCall = doNetCall
    in
        fun shutdown (SOCK s, mode) =
        let
            val m =
                case mode of
                    NO_RECVS => 1
                 |  NO_SENDS => 2
                 |  NO_RECVS_OR_SENDS => 3
        in
            doCall (50, (s, m))
        end
    end

    (* The IO descriptor is the underlying socket. *)
    fun ioDesc (SOCK s) = s;

    type out_flags = {don't_route : bool, oob : bool}
    type in_flags = {peek : bool, oob : bool}
    type 'a buf = {buf : 'a, i : int, sz : int option}

    local
        val nullOut = { don't_route = false, oob = false }
        and nullIn = { peek = false, oob = false }

        (* This implementation is copied from the implementation of
           Word8Array.array and Word8Vector.vector. *)
        type address = LibrarySupport.address
        datatype vector = datatype LibrarySupport.Word8Array.vector
        datatype array = datatype LibrarySupport.Word8Array.array
        val wordSize = LibrarySupport.wordSize

        (* Send the data from an array or vector.  Note: the underlying RTS function
           deals with the special case of sending a single byte vector where the
           "address" is actually the byte itself. *)
        local
            val doCall = doNetCall
            fun doSend i a = doCall (i, a)
        in
            fun send (SOCK sock, base: address, offset: int, length: int, rt: bool, oob: bool): int =
                doSend 51 (sock, base, offset, length, rt, oob)
    
            fun sendNB (SOCK sock, base: address, offset: int, length: int, rt: bool, oob: bool): int option =
                nonBlockingCall (doSend 60) (sock, base, offset, length, rt, oob)
        end

        local
            (* Although the underlying call returns the number of bytes written the
               ML functions now return unit. *)
            val doCall = doNetCall
            fun doSendTo i a = doCall (i, a)
        in
            fun sendTo (SOCK sock, addr, base: address, offset: int, length: int, rt: bool, oob: bool): unit =
                doSendTo 52 (RunCall.unsafeCast(sock, addr, base, offset, length, rt, oob))
    
            fun sendToNB (SOCK sock, addr, base: address, offset: int, length: int, rt: bool, oob: bool): bool =
                case nonBlockingCall (doSendTo 61) (RunCall.unsafeCast(sock, addr, base, offset, length, rt, oob)) of
                    NONE => false | SOME _ => true
        end

        local
            val doCall = doNetCall
            fun doRecv i a = doCall (i, a)
        in
            (* Receive the data into an array. *)
            fun recv (SOCK sock, base: address, offset: int, length: int, peek: bool, oob: bool): int =
                doRecv 53 (RunCall.unsafeCast(sock, base, offset, length, peek, oob))

            fun recvNB (SOCK sock, base: address, offset: int, length: int, peek: bool, oob: bool): int option =
                nonBlockingCall (doRecv 62) (RunCall.unsafeCast(sock, base, offset, length, peek, oob))
        end

        local
            val doCall = doNetCall
            fun doRecvFrom i a = doCall (i, a)
        in 
            fun recvFrom (SOCK sock, base: address, offset: int, length: int, peek: bool, oob: bool) =
                RunCall.unsafeCast(doRecvFrom 54 (RunCall.unsafeCast (sock, base, offset, length, peek, oob)))

            fun recvFromNB (SOCK sock, base: address, offset: int, length: int, peek: bool, oob: bool) =
                RunCall.unsafeCast(nonBlockingCall (doRecvFrom 63) (RunCall.unsafeCast (sock, base, offset, length, peek, oob)))
        end
    in
        fun sendVec' (sock, slice: Word8VectorSlice.slice, {don't_route, oob}) =
        let
            val (v, i, length) = Word8VectorSlice.base slice
        in
            send(sock, LibrarySupport.w8vectorAsAddress v, i + Word.toInt wordSize, length, don't_route, oob)
        end
        and sendVec (sock, vbuff) = sendVec'(sock, vbuff, nullOut)
        
        fun sendVecNB' (sock, slice: Word8VectorSlice.slice, {don't_route, oob}) =
        let
            val (v, i, length) = Word8VectorSlice.base slice
        in
            sendNB(sock, LibrarySupport.w8vectorAsAddress v, i + Word.toInt wordSize, length, don't_route, oob)
        end
        and sendVecNB (sock, vbuff) = sendVecNB'(sock, vbuff, nullOut)
    
        fun sendArr' (sock, slice: Word8ArraySlice.slice, {don't_route, oob}) =
        let
            val (Array(_, v), i, length) = Word8ArraySlice.base slice
        in
            send(sock, v, i, length, don't_route, oob)
        end
        and sendArr (sock, vbuff) = sendArr'(sock, vbuff, nullOut)
        
        fun sendArrNB' (sock, slice: Word8ArraySlice.slice, {don't_route, oob}) =
        let
            val (Array(_, v), i, length) = Word8ArraySlice.base slice
        in
            sendNB(sock, v, i, length, don't_route, oob)
        end
        and sendArrNB (sock, vbuff) = sendArrNB'(sock, vbuff, nullOut)
    
        fun sendVecTo' (sock, addr, slice: Word8VectorSlice.slice, {don't_route, oob}) =
        let
            val (v, i, length) = Word8VectorSlice.base slice
        in
            sendTo(sock, addr, LibrarySupport.w8vectorAsAddress v, i + Word.toInt wordSize, length, don't_route, oob)
        end
        and sendVecTo (sock, addr, vbuff) = sendVecTo'(sock, addr, vbuff, nullOut)

        fun sendVecToNB' (sock, addr, slice: Word8VectorSlice.slice, {don't_route, oob}) =
        let
            val (v, i, length) = Word8VectorSlice.base slice
        in
            sendToNB(sock, addr, LibrarySupport.w8vectorAsAddress v, i + Word.toInt wordSize, length, don't_route, oob)
        end
        and sendVecToNB (sock, addr, vbuff) = sendVecToNB'(sock, addr, vbuff, nullOut)

        fun sendArrTo' (sock, addr, slice: Word8ArraySlice.slice, {don't_route, oob}) =
        let
            val (Array(_, v), i, length) = Word8ArraySlice.base slice
        in
            sendTo(sock, addr, v, i, length, don't_route, oob)
        end
        and sendArrTo (sock, addr, vbuff) = sendArrTo'(sock, addr, vbuff, nullOut)

        fun sendArrToNB' (sock, addr, slice: Word8ArraySlice.slice, {don't_route, oob}) =
        let
            val (Array(_, v), i, length) = Word8ArraySlice.base slice
        in
            sendToNB(sock, addr, v, i, length, don't_route, oob)
        end
        and sendArrToNB (sock, addr, vbuff) = sendArrToNB'(sock, addr, vbuff, nullOut)

        fun recvArr' (sock, slice: Word8ArraySlice.slice, {peek, oob}) =
        let
            val (Array(_, v), i, length) = Word8ArraySlice.base slice
        in
            recv(sock, v, i, length, peek, oob)
        end
        and recvArr (sock, vbuff) = recvArr'(sock, vbuff, nullIn)

        fun recvArrNB' (sock, slice: Word8ArraySlice.slice, {peek, oob}) =
        let
            val (Array(_, v), i, length) = Word8ArraySlice.base slice
        in
            recvNB(sock, v, i, length, peek, oob)
        end
        and recvArrNB (sock, vbuff) = recvArrNB'(sock, vbuff, nullIn)
    
        (* To receive a vector first create an array, read into it,
           then copy it to a new vector.  This does involve extra copying
           but it probably doesn't matter too much. *)
        fun recvVec' (sock, size, flags) =
        let
            val arr = Word8Array.array(size, 0w0);
            val recvd = recvArr'(sock, Word8ArraySlice.full arr, flags)
        in
            Word8ArraySlice.vector(Word8ArraySlice.slice(arr, 0, SOME recvd))
        end
        and recvVec (sock, size) = recvVec'(sock, size, nullIn)

        fun recvVecNB' (sock, size, flags) =
        let
            val arr = Word8Array.array(size, 0w0);
        in
            case recvArrNB'(sock, Word8ArraySlice.full arr, flags) of
                NONE => NONE
            |   SOME recvd => SOME(Word8ArraySlice.vector(Word8ArraySlice.slice(arr, 0, SOME recvd)))
        end
        and recvVecNB (sock, size) = recvVecNB'(sock, size, nullIn)

        fun recvArrFrom' (sock, slice: Word8ArraySlice.slice, {peek, oob}) =
        let
            val (Array(_, v), i, length) = Word8ArraySlice.base slice
        in
            recvFrom(sock, v, i, length, peek, oob)
        end
        and recvArrFrom (sock, abuff) = recvArrFrom'(sock, abuff, nullIn)


        fun recvArrFromNB' (sock, slice: Word8ArraySlice.slice, {peek, oob}) =
        let
            val (Array(_, v), i, length) = Word8ArraySlice.base slice
        in
            recvFromNB(sock, v, i, length, peek, oob)
        end
        and recvArrFromNB (sock, abuff) = recvArrFromNB'(sock, abuff, nullIn)

        fun recvVecFrom' (sock, size, flags) =
        let
            val arr = Word8Array.array(size, 0w0);
            val (rcvd, addr) =
                recvArrFrom'(sock, Word8ArraySlice.full arr, flags)
        in
            (Word8ArraySlice.vector(Word8ArraySlice.slice(arr, 0, SOME rcvd)), addr)
        end
        and recvVecFrom (sock, size) = recvVecFrom'(sock, size, nullIn)

        fun recvVecFromNB' (sock, size, flags) =
        let
            val arr = Word8Array.array(size, 0w0);
        in
            case recvArrFromNB'(sock, Word8ArraySlice.full arr, flags) of
                NONE => NONE
            |   SOME (rcvd, addr) =>
                    SOME (Word8ArraySlice.vector(Word8ArraySlice.slice(arr, 0, SOME rcvd)), addr)           
        end
        and recvVecFromNB (sock, size) = recvVecFromNB'(sock, size, nullIn)

    end

    (* "select" call. *)
    datatype sock_desc = SOCKDESC of OS.IO.iodesc
    fun sockDesc (SOCK sock) = SOCKDESC sock (* Create a socket descriptor from a socket. *)
    fun sameDesc (SOCKDESC a, SOCKDESC b) = a = b

    local
        (* The underlying call takes three arrays and updates them with the sockets that are
           in the appropriate state.  It sets inactive elements to ~1. *)
        val doIo: int * (OS.IO.iodesc Vector.vector * OS.IO.iodesc Vector.vector * OS.IO.iodesc Vector.vector * Time.time) ->
                    OS.IO.iodesc Vector.vector * OS.IO.iodesc Vector.vector * OS.IO.iodesc Vector.vector
             = doNetCall
    in
        fun sys_select_block(rds, wrs, exs) = doIo(64, (rds, wrs, exs, Time.zeroTime))
        fun sys_select_poll(rds, wrs, exs) = doIo(65, (rds, wrs, exs, Time.zeroTime))
        (* The time parameter for a wait is the absolute time when the timeout expires. *)
        and sys_select_wait (rds, wrs, exs, t) = doIo(66, (rds, wrs, exs, t))
    end
    
    fun select { rds: sock_desc list, wrs : sock_desc list, exs : sock_desc list, timeout: Time.time option } :
            { rds: sock_desc list, wrs : sock_desc list, exs : sock_desc list } =
    let
        fun sockDescToDesc(SOCKDESC sock) = sock
        (* Create the initial vectors. *)
        val rdVec: OS.IO.iodesc Vector.vector = Vector.fromList(map sockDescToDesc rds)
        val wrVec: OS.IO.iodesc Vector.vector = Vector.fromList(map sockDescToDesc wrs)
        val exVec: OS.IO.iodesc Vector.vector = Vector.fromList(map sockDescToDesc exs)
        open Time
        val (rdResult, wrResult, exResult) =
            (* Do the approriate select. *)
            case timeout of
                NONE => sys_select_block(rdVec, wrVec, exVec)
            |   SOME t => if t <= Time.zeroTime
                          then sys_select_poll(rdVec, wrVec, exVec)
                          else sys_select_wait(rdVec, wrVec, exVec, t + Time.now());
        (* Function to create the results. *)
        fun getResults v = Vector.foldr (fn (sd, l) => SOCKDESC sd :: l) [] v
    in
        (* Convert the results. *)
        { rds = getResults rdResult, wrs = getResults wrResult, exs = getResults exResult }
    end

end;

local
    (* Install the pretty printer for Socket.AF.addr_family
       This must be done outside
       the structure if we use opaque matching. *)
    fun printAF _ _ x = PolyML.PrettyString(Socket.AF.toString x)
    fun printSK _ _ x = PolyML.PrettyString(Socket.SOCK.toString x)
    fun prettySocket _ _ (_: ('a, 'b) Socket.sock) = PolyML.PrettyString "?"
in
    val () = PolyML.addPrettyPrinter printAF
    val () = PolyML.addPrettyPrinter printSK
    val () = PolyML.addPrettyPrinter prettySocket
end;
