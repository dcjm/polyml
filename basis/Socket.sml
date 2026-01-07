(*
    Title:      Standard Basis Library: Generic Sockets
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2015-16, 2019

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
        eqtype addr_family (* = NetHostDB.addr_family *) (* This is a mess: NetHostDB depends on Socket. *)
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

structure Socket :> SOCKET
    where type ('af,'sock_type) sock = ('af,'sock_type) LibraryIOSupport.sock (* So we can use it elsewhere *) =
struct
    (* We don't really need an implementation for these.  *)
    datatype sock = datatype LibraryIOSupport.sock
     
    datatype dgram = DGRAM
    and 'mode stream = STREAM
    and passive = PASSIVE
    and active = ACTIVE

    structure AF =
    struct
        type addr_family = int

        val list: unit -> (string * addr_family) list = RunCall.rtsCallFull0 "PolyNetworkGetAddrList"

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

        val list:unit -> (string * sock_type) list = RunCall.rtsCallFull0 "PolyNetworkGetSockTypeList"

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
    datatype sock_addr = datatype LibraryIOSupport.sock_addr

    (* Note: The definition did not make these equality type variables.
       The assumption is probably that it works much like equality on
       references. *)
    fun sameAddr (SOCKADDR a, SOCKADDR b) = a = b

    local
        (* Because this involves a type variable we need an extra function. *)
        val doCall = RunCall.rtsCallFast1 "PolyNetworkGetFamilyFromAddress"
    in
        fun familyOfAddr (SOCKADDR sa) = doCall sa
    end
    
    
    (* Get the error state as an OS.syserror value.  This is a SysWord.word value. *)
    local
        val sysGetError: OS.IO.iodesc -> SysWord.word =
            RunCall.rtsCallFull1 "PolyNetworkGetSocketError"
    in
        fun getAndClearError(SOCK s): SysWord.word = sysGetError s
    end

    structure Ctl =
    struct
        local
            val doGetOpt: int * OS.IO.iodesc -> int = RunCall.rtsCallFull2 "PolyNetworkGetOption"
            val doSetOpt: int * OS.IO.iodesc * int -> unit = RunCall.rtsCallFull3 "PolyNetworkSetOption"
        in
            fun getOpt (i:int) (SOCK s) : int = doGetOpt(i, s)
            fun setOpt (i: int) (SOCK s, v: int) = doSetOpt(i, s, v)
            fun bv true = 1 | bv false = 0
        end
        
        fun getDEBUG s = getOpt 18 s <> 0
        and setDEBUG(s, b) = setOpt 17 (s, bv b)
        and getREUSEADDR s = getOpt 20 s <> 0
        and setREUSEADDR(s, b) = setOpt 19 (s, bv b)
        and getKEEPALIVE s = getOpt 22 s <> 0
        and setKEEPALIVE(s, b) = setOpt 21 (s, bv b)
        and getDONTROUTE s = getOpt 24 s <> 0
        and setDONTROUTE(s, b) = setOpt 23 (s, bv b)
        and getBROADCAST s = getOpt 26 s <> 0
        and setBROADCAST(s, b) = setOpt 25 (s, bv b)
        and getOOBINLINE s = getOpt 28 s <> 0
        and setOOBINLINE(s, b) = setOpt 27 (s, bv b)
        and getERROR s = getAndClearError s <> 0w0
        and setSNDBUF(s, i: int) = setOpt 29 (s, i)
        and getSNDBUF s = getOpt 30 s
        and setRCVBUF(s, i: int) = setOpt 31 (s, i)
        and getRCVBUF s = getOpt 32 s
        and getTYPE s = SOCK.SOCKTYPE(getOpt 33 s)

        local
            val doGetOpt: OS.IO.iodesc -> bool = RunCall.rtsCallFull1 "PolyNetworkGetAtMark"
        in
            fun getATMARK (SOCK s) = doGetOpt s
        end
        
        local
            val doGetNRead: OS.IO.iodesc -> int = RunCall.rtsCallFull1 "PolyNetworkBytesAvailable"
        in
            fun getNREAD (SOCK s) = doGetNRead s
        end

        local
            val doSetLinger: OS.IO.iodesc * LargeInt.int -> unit = RunCall.rtsCallFull2 "PolyNetworkSetLinger"
            val doGetLinger: OS.IO.iodesc -> LargeInt.int = RunCall.rtsCallFull1 "PolyNetworkGetLinger"
        in
            fun getLINGER (SOCK s): Time.time option =
            let
                val lTime = doGetLinger s (* Returns LargeInt.int *)
            in
                if lTime < 0 then NONE else SOME(Time.fromSeconds lTime)
            end

            fun setLINGER (SOCK s, NONE) =
                (
                    doSetLinger(s, ~1)
                )
            |   setLINGER (SOCK s, SOME t) =
                let
                    val lTime = Time.toSeconds t
                in
                    if lTime < 0
                    then raise OS.SysErr("Invalid time", NONE)
                    else doSetLinger(s, lTime)
                end
        end

        local
            val getPeer: OS.IO.iodesc -> Word8Vector.vector = RunCall.rtsCallFull1 "PolyNetworkGetPeerName"
        in
            fun getPeerName (SOCK s): 'af sock_addr = SOCKADDR(getPeer s)
        end

        local
            val getSock: OS.IO.iodesc -> Word8Vector.vector = RunCall.rtsCallFull1 "PolyNetworkGetSockName"
        in
            fun getSockName (SOCK s): 'af sock_addr = SOCKADDR(getSock s)
        end
        end (* Ctl *)


    (* "select" call. *)
    datatype sock_desc = SOCKDESC of OS.IO.iodesc
    fun sockDesc (SOCK sock) = SOCKDESC sock (* Create a socket descriptor from a socket. *)
    fun sameDesc (SOCKDESC a, SOCKDESC b) = a = b

    (* The underlying call takes three arrays and updates them with the sockets that are
       in the appropriate state.  It sets inactive elements to ~1. *)
    val sysSelect: (OS.IO.iodesc Vector.vector * OS.IO.iodesc Vector.vector * OS.IO.iodesc Vector.vector) * int ->
        OS.IO.iodesc Vector.vector * OS.IO.iodesc Vector.vector * OS.IO.iodesc Vector.vector
         = RunCall.rtsCallFull2 "PolyNetworkSelect"
    
    fun select { rds: sock_desc list, wrs : sock_desc list, exs : sock_desc list, timeout: Time.time option } :
            { rds: sock_desc list, wrs : sock_desc list, exs : sock_desc list } =
    let
        fun sockDescToDesc(SOCKDESC sock) = sock
        (* Create the initial vectors. *)
        val rdVec: OS.IO.iodesc Vector.vector = Vector.fromList(map sockDescToDesc rds)
        val wrVec: OS.IO.iodesc Vector.vector = Vector.fromList(map sockDescToDesc wrs)
        val exVec: OS.IO.iodesc Vector.vector = Vector.fromList(map sockDescToDesc exs)

        (* As with OS.FileSys.poll we call the RTS to check the sockets for up to a second
           and repeat until the time expires. *)
        val finishTime = case timeout of NONE => NONE | SOME t => SOME(t + Time.now())
            
        val maxMilliSeconds = 1000 (* 1 second *)

        fun doSelect() =
        let
            val timeToGo =
                case finishTime of
                    NONE => maxMilliSeconds
                |   SOME finish => LargeInt.toInt(LargeInt.min(LargeInt.max(0, Time.toMilliseconds(finish-Time.now())),
                        LargeInt.fromInt maxMilliSeconds))

            val results as (rdResult, wrResult, exResult) =
                sysSelect((rdVec, wrVec, exVec), timeToGo)
        in
            if timeToGo < maxMilliSeconds orelse Vector.length rdResult <> 0
                orelse Vector.length wrResult <> 0 orelse Vector.length exResult <> 0
            then results
            else doSelect()
        end

        val (rdResult, wrResult, exResult) = doSelect()

        (* Function to create the results. *)
        fun getResults v = Vector.foldr (fn (sd, l) => SOCKDESC sd :: l) [] v
    in
        (* Convert the results. *)
        { rds = getResults rdResult, wrs = getResults wrResult, exs = getResults exResult }
    end

    (* Run an operation in non-blocking mode.  This catches EWOULDBLOCK and returns NONE,
       otherwise returns SOME result.  Other exceptions are passed back as normal. *)
    val nonBlockingCall = LibraryIOSupport.nonBlocking
    
    local
        val accpt: OS.IO.iodesc -> OS.IO.iodesc * Word8Vector.vector = RunCall.rtsCallFull1 "PolyNetworkAccept"
    in
        fun acceptNB (SOCK sk) =
            case nonBlockingCall accpt sk of
                SOME (resSkt, resAddr) => SOME (SOCK resSkt, SOCKADDR resAddr)
            |   NONE => NONE
    end
    
    (* Blocking accept - keep trying until we get a result. *)
    fun accept skt =
        case acceptNB skt of
            SOME result => result
        |   NONE =>
            (
                select{wrs=[], rds=[sockDesc skt], exs=[sockDesc skt], timeout=NONE};
                accept skt
            )

    local
        val doBindCall: OS.IO.iodesc * Word8Vector.vector -> unit = RunCall.rtsCallFull2 "PolyNetworkBind"
    in
        fun bind (SOCK s, SOCKADDR a) = doBindCall(s, a)
    end

    local
        val connct: OS.IO.iodesc * Word8Vector.vector -> unit = RunCall.rtsCallFull2 "PolyNetworkConnect"
    in
        fun connectNB (SOCK s, SOCKADDR a) =
            case nonBlockingCall connct (s,a) of SOME () => true | NONE => false
            
        fun connect (sockAndAddr as (skt, _)) =
            if connectNB sockAndAddr
            then ()
            else
            let
                (* In Windows failure is indicated by the bit being set in
                    the exception set rather than the write set. *)
                val _ = select{wrs=[sockDesc skt], rds=[], exs=[sockDesc skt], timeout=NONE}
                val anyError = getAndClearError skt
                val theError = LibrarySupport.syserrorFromWord anyError
            in
                if anyError = 0w0
                then ()
                else raise OS.SysErr(OS.errorMsg theError, SOME theError)
            end
                
    end

    local
        val doListen: OS.IO.iodesc * int -> unit = RunCall.rtsCallFull2 "PolyNetworkListen"
    in
        fun listen (SOCK s, b) = doListen(s, b)
    end

    (* On Windows sockets and streams are different. *)
    local
        val doCall = RunCall.rtsCallFull1 "PolyNetworkCloseSocket"
    in
        fun close (SOCK strm): unit = doCall(strm)
    end

    datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS

    local
        val doCall: OS.IO.iodesc * int -> unit = RunCall.rtsCallFull2 "PolyNetworkShutdown"
    in
        fun shutdown (SOCK s, mode) =
        let
            val m =
                case mode of
                    NO_RECVS => 1
                 |  NO_SENDS => 2
                 |  NO_RECVS_OR_SENDS => 3
        in
            doCall(s, m)
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

        (* Send the data from an array or vector. *)
        local
            val doSend: OS.IO.iodesc * address * int * int * bool * bool -> int =
                RunCall.rtsCallFull1 "PolyNetworkSend"
        in    
            fun sendNB (SOCK sock, base: address, offset: int, length: int, rt: bool, oob: bool): int option =
                nonBlockingCall doSend (sock, base, offset, length, rt, oob)
            
            fun send (skt as SOCK sock, base, offset, length, rt, oob) =
            (
                (* Wait until we can write. *)
                select{wrs=[sockDesc skt], rds=[], exs=[], timeout=NONE};
                (* Send it.  We should never get a WOULDBLOCK result so if we do we pass that back. *)
                doSend (sock, base, offset, length, rt, oob)
            )
        end

        local
            (* Although the underlying call returns the number of bytes written the
               ML functions now return unit. *)
            val doSend: OS.IO.iodesc * Word8Vector.vector * address * int * int * bool * bool -> int =
                RunCall.rtsCallFull1 "PolyNetworkSendTo"
        in    
            fun sendToNB (SOCK sock, SOCKADDR addr, base: address, offset, length, rt, oob): bool =
                case nonBlockingCall doSend (sock, addr, base, offset, length, rt, oob) of
                    NONE => false | SOME _ => true
            
            fun sendTo (skt as SOCK sock, SOCKADDR addr, base: address, offset, length, rt, oob): unit =
            (
                (* Wait until we can write. *)
                select{wrs=[sockDesc skt], rds=[], exs=[], timeout=NONE};
                doSend (sock, addr, base, offset, length, rt, oob);
                ()
            )
        end

        local
            val doRecv: OS.IO.iodesc * address * int * int * bool * bool -> int =
                RunCall.rtsCallFull1 "PolyNetworkReceive"
        in
            (* Receive the data into an array. *)
            fun recvNB (SOCK sock, base: address, offset: int, length: int, peek: bool, oob: bool): int option =
                nonBlockingCall doRecv (sock, base, offset, length, peek, oob)
            
            fun recv (skt as SOCK sock, base, offset, length, rt, oob) =
            (
                (* Wait until we can read. *)
                select{wrs=[], rds=[sockDesc skt], exs=[], timeout=NONE};
                doRecv (sock, base, offset, length, rt, oob)
            )
        end

        local
            val doRecvFrom: OS.IO.iodesc * address * int * int * bool * bool -> int * Word8Vector.vector =
                RunCall.rtsCallFull1 "PolyNetworkReceiveFrom"
        in 
            fun recvFromNB (SOCK sock, base, offset, length, peek, oob) =
                case nonBlockingCall doRecvFrom (sock, base, offset, length, peek, oob) of
                    SOME(length, addr) => SOME(length, SOCKADDR addr)
                |   NONE => NONE

            fun recvFrom (skt as SOCK sock, base, offset, length, peek, oob) =
            (
                (* Wait until we can read. *)
                select{wrs=[], rds=[sockDesc skt], exs=[], timeout=NONE};
                case doRecvFrom (sock, base, offset, length, peek, oob) of
                    (length, addr) => (length, SOCKADDR addr)
            )
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
