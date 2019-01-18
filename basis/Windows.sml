(*
    Title:      Standard Basis Library: Windows signature and structure
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005, 2012, 2018, 2019

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

signature WINDOWS =
sig
    structure Key :
    sig
        include BIT_FLAGS
        val allAccess : flags
        val createLink : flags
        val createSubKey : flags
        val enumerateSubKeys : flags
        val execute : flags
        val notify : flags
        val queryValue : flags
        val read : flags
        val setValue : flags
        val write : flags
    end
    structure Reg :
    sig
        eqtype hkey
        val classesRoot  : hkey
        val currentUser  : hkey
        val localMachine : hkey
        val users        : hkey
        val performanceData : hkey
        val currentConfig : hkey
        val dynData : hkey
  
        datatype create_result =
              CREATED_NEW_KEY of hkey
            | OPENED_EXISTING_KEY of hkey
        val createKeyEx : hkey * string * Key.flags -> create_result
        val openKeyEx : hkey * string * Key.flags -> hkey
        val closeKey : hkey -> unit
        val deleteKey : hkey * string -> unit
        val deleteValue : hkey * string -> unit
        val enumKeyEx : hkey * int -> string option
        val enumValueEx : hkey * int -> string option
        datatype value =
              SZ of string
            | DWORD of SysWord.word
            | BINARY of Word8Vector.vector
            | MULTI_SZ of string list
            | EXPAND_SZ of string
        val queryValueEx : hkey * string -> value option
        val setValueEx : hkey * string * value -> unit
    end

    structure Config:
    sig
        val platformWin32s : SysWord.word
        val platformWin32Windows : SysWord.word
        val platformWin32NT : SysWord.word
        val platformWin32CE : SysWord.word

        val getVersionEx: unit ->
            { majorVersion: SysWord.word, minorVersion: SysWord.word,
              buildNumber: SysWord.word, platformId: SysWord.word,
              csdVersion: string }

        val getWindowsDirectory: unit -> string
        val getSystemDirectory: unit -> string
        val getComputerName: unit -> string
        val getUserName: unit -> string
    end

    structure DDE :
    sig
        type info
        val startDialog : string * string -> info
        val executeString : info * string * int * Time.time -> unit
        val stopDialog : info -> unit
    end

    val getVolumeInformation :
                string -> {
                            volumeName : string,
                            systemName : string,
                            serialNumber : SysWord.word,
                            maximumComponentLength : int
                          }

    val findExecutable : string -> string option
    val launchApplication : string * string -> unit
    val openDocument : string -> unit
    val simpleExecute : string * string -> OS.Process.status
    type ('a,'b) proc
    val execute : string * string -> ('a, 'b) proc
    val textInstreamOf : (TextIO.instream, 'a) proc -> TextIO.instream
    val binInstreamOf  : (BinIO.instream, 'a) proc -> BinIO.instream
    val textOutstreamOf : ('a, TextIO.outstream) proc -> TextIO.outstream
    val binOutstreamOf  : ('a, BinIO.outstream) proc -> BinIO.outstream
    val reap : ('a, 'b) proc -> OS.Process.status

    structure Status :
    sig
        type status = SysWord.word
        val accessViolation        : status
        val arrayBoundsExceeded    : status
        val breakpoint             : status
        val controlCExit           : status
        val datatypeMisalignment   : status
        val floatDenormalOperand   : status
        val floatDivideByZero      : status
        val floatInexactResult     : status
        val floatInvalidOperation  : status
        val floatOverflow          : status
        val floatStackCheck        : status
        val floatUnderflow         : status
        val guardPageViolation     : status
        val integerDivideByZero    : status
        val integerOverflow        : status
        val illegalInstruction     : status
        val invalidDisposition     : status
        val invalidHandle          : status
        val inPageError            : status
        val noncontinuableException: status
        val pending                : status
        val privilegedInstruction  : status
        val singleStep             : status
        val stackOverflow          : status
        val timeout                : status
        val userAPC                : status
    end
    val fromStatus : OS.Process.status -> Status.status
    val exit : Status.status -> 'a

end

structure Windows :> WINDOWS =
struct
    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
    in
        fun getConst i = SysWord.fromInt(winCall (1006, i))
    end

    structure Key =
    struct
        type flags = SysWord.word
        fun toWord f = f
        fun fromWord f = f
        val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
        fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
        fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
        fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)

        val allAccess : flags = getConst 0
        val createLink : flags = getConst 1
        val createSubKey : flags = getConst 2
        val enumerateSubKeys : flags = getConst 3
        val execute : flags = getConst 4
        val notify : flags = getConst 5
        val queryValue : flags = getConst 6
        val read : flags = getConst 7
        val setValue : flags = getConst 8
        val write : flags = getConst 9

        (* all is probably equivalent to allAccess. *)
        val all = flags[allAccess, createLink, createSubKey, enumerateSubKeys,
                        execute, notify, queryValue, read, setValue, write]

        val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
    end

    structure Reg =
    struct

        datatype hkey =
                PREDEFINED of int
            |   SUBKEY of int (* Actually abstract. *)
        val classesRoot  = PREDEFINED 0
        val currentUser  = PREDEFINED 1
        val localMachine = PREDEFINED 2
        val users        = PREDEFINED 3
        val performanceData = PREDEFINED 4
        val currentConfig = PREDEFINED 5
        val dynData      = PREDEFINED 6
        datatype create_result =
              CREATED_NEW_KEY of hkey
            | OPENED_EXISTING_KEY of hkey
        datatype value =
              SZ of string
            | DWORD of SysWord.word
            | BINARY of Word8Vector.vector
            | MULTI_SZ of string list
            | EXPAND_SZ of string

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
            (* Open one of the root keys. *)
            (* QUESTION: Why is this an option?  The definition asks
               the same question.  I've removed the option type. *)
            fun openRoot args =
                SUBKEY(winCall(1007, args))
            (* Open a sub-key. *)
            and openSubKey args =
                SUBKEY(winCall(1008, args))
        in
            fun openKeyEx(PREDEFINED i, s, f) =
                    openRoot(i, s, SysWord.toInt f)
            |   openKeyEx(SUBKEY i, s, f) =
                    openSubKey(i, s, SysWord.toInt f)
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"

            fun pairToResult (0, k) = CREATED_NEW_KEY (SUBKEY k)
             |  pairToResult (_, k) = OPENED_EXISTING_KEY (SUBKEY k)

            (* Open one of the root keys. *)
            fun createRoot args =
                pairToResult(winCall(1009, args))
            (* Open a sub-key. *)
            and createSubKey args =
                pairToResult(winCall(1010, args))
    
        in
            (* I've retained the third argument in this interface
               which used to be used for VOLATILE (1) or
               NON_VOLATILE (0).  Keys are now always non-volatile. *)
            fun createKeyEx(PREDEFINED i, s, f) =
                    createRoot(i, s, 0, SysWord.toInt f)
            |   createKeyEx(SUBKEY i, s, f) =
                    createSubKey(i, s, 0, SysWord.toInt f)
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            (* TODO: We wouldn't normally expect to close a
               predefined key but it looks as though we might
               have to be able to close HKEY_PERFORMANCE_DATA. *)
            fun closeKey(PREDEFINED _) = ()
            |   closeKey(SUBKEY i) =
                    winCall(1011, i)
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"

            fun unpackString v =
            let
                val len = Word8Vector.length v
            in
                if len = 0 then ""
                else Byte.unpackStringVec(Word8VectorSlice.slice(v, 0, SOME(len -1)))
            end

            fun unpackStringList v =
            let
                val len = Word8Vector.length v
                fun unpack start i =
                    if i >= len orelse Word8Vector.sub(v, i) = 0w0
                    then if i = start then []
                    else Byte.unpackStringVec(Word8VectorSlice.slice(v, start, SOME(i - start))) ::
                            unpack (i+1) (i+1)
                    else unpack start (i+1)
            in
                unpack 0 0
            end

            fun queryResultToValues(t, v) =
                (* Decode the type code and the value.  Strings are null terminated so
                   the last character must be removed. *)
                case t of
                    1 => SZ(unpackString v)
                |   4 => DWORD(PackWord32Little.subVec(v, 0))
                |   2 => EXPAND_SZ(unpackString v)
                |   7 => MULTI_SZ(unpackStringList v)
                |   _ => BINARY v
                
            val errorFileNotFound = valOf(OS.syserror "ERROR_FILE_NOT_FOUND")
        in
            (* The queryValue functions simply return a type and a vector of bytes.
               The type code is decoded and the bytes unpacked appropriately. *)
            fun queryValueEx(key, s) =
                SOME(queryResultToValues(
                    case key of
                        PREDEFINED i => winCall(1012, (i, s))
                    |   SUBKEY i => winCall(1013, (i, s))
                    ))
                    handle ex as OS.SysErr(_, SOME err) =>
                        if err = errorFileNotFound
                        then NONE
                        else raise ex
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            fun deleteValue(PREDEFINED i, s) =
                    (winCall(1022, (i, s)))
            |   deleteValue(SUBKEY i, s) =
                    (winCall(1023, (i, s)))
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
            fun packString s =
            let
                val len = String.size s
                val arr = Word8Array.array(len+1, 0w0)
            in
                Byte.packString(arr, 0, Substring.full s);
                Word8Array.vector arr
            end

            fun packStringList sl =
            let
                (* The string list is packed as a set of null-terminated strings
                   with a final null at the end. *)
                (* TODO: Check for nulls in the strings themselves? *)
                fun totalSize n [] = n
                 |  totalSize n (s::sl) = totalSize (n + String.size s + 1) sl
                val len = totalSize 1 sl
                val arr = Word8Array.array(len, 0w0)
                fun pack _ [] = ()
                  | pack n (s::sl) =
                    (
                    Byte.packString(arr, n, Substring.full s);
                    pack (n + String.size s + 1) sl
                    )
            in
                pack 0 sl;
                Word8Array.vector arr
            end

            fun valuesToTypeVal(SZ s) = (1, packString s)
              | valuesToTypeVal(EXPAND_SZ s) = (2, packString s)
              | valuesToTypeVal(BINARY s) = (3, s)
              | valuesToTypeVal(DWORD n) =
                    let
                        (* Pack the 32 bit value into an array, then extract that. *)
                        val arr = Word8Array.array(4, 0w0)
                    in
                        PackWord32Little.update(arr, 0, n);
                        (4, Word8Array.vector arr)
                    end
              | valuesToTypeVal(MULTI_SZ s) = (7, packStringList s)
        in
            fun setValueEx(key, name, v) =
                let
                    val (t, s) = valuesToTypeVal v
                    val (call, k) =
                        case key of
                            PREDEFINED i => (1016, i)
                        |   SUBKEY i => (1017, i)
                in
                    (winCall(call, (k, name, t, s)))
                end
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            fun enumKeyEx(PREDEFINED i, n) =
                    (winCall(1018, (i, n)))
             |  enumKeyEx(SUBKEY i, n) =
                    (winCall(1019, (i, n)))

            fun enumValueEx(PREDEFINED i, n) =
                    (winCall(1020, (i, n)))
             |  enumValueEx(SUBKEY i, n) =
                    (winCall(1021, (i, n)))
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
            (* In Windows NT RegDeleteKey will fail if the key has subkeys.
               To give the same behaviour in both Windows 95 and NT we have
               to recursively delete any subkeys. *)
            fun basicDeleteKey(PREDEFINED i, s) =
                    (winCall(1014, (i, s)))
            |   basicDeleteKey(SUBKEY i, s) =
                    (winCall(1015, (i, s)))
        in
            fun deleteKey(k, s) =
            let
                val sk = openKeyEx(k, s, Key.enumerateSubKeys)
                fun deleteSubKeys () =
                    case enumKeyEx(sk, 0) of
                        NONE => ()
                    |   SOME name => (deleteKey(sk, name); deleteSubKeys())
            in
                deleteSubKeys() handle exn => (closeKey sk; raise exn);
                closeKey sk;
                basicDeleteKey(k, s)
            end
        end
    end
    
    structure DDE =
    struct
        type info = int (* Actually abstract. *)

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            fun startDialog (service, topic) =
                winCall(1038, (service, topic))
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            (* The timeout and retry count apply only in the case of
               a busy result.  The Windows call takes a timeout parameter
               as the length of time to wait for a response and maybe we
               should use it for that as well. *)
            fun executeString (info, cmd, retry, delay) =
            let
                fun try n =
                    if winCall(1039, (info, cmd))
                    then () (* Succeeded. *)
                    else if n = 0
                    then raise OS.SysErr("DDE Server busy", NONE)
                    else
                        (
                        OS.IO.poll([], SOME delay);
                        try (n-1)
                        )
            in
                try retry
            end
        end

        local
            val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            fun stopDialog (info) = winCall(1040, info)
        end
    end (* DDE *)

    (* No (longer?) in Basis library
    local
        val winCall = RunCall.run_call2 POLY_SYS_os_specific
    in
        fun fileTimeToLocalFileTime t = winCall(1030, t)
        fun localFileTimeToFileTime t = winCall(1031, t)
    end
    *)

    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
    in
        fun getVolumeInformation root =
        let
            val (vol, sys, serial, max) =
                winCall(1032, root)
        in
            { volumeName = vol, systemName = sys,
              serialNumber = SysWord.fromInt serial,
              maximumComponentLength = max }
        end
    end

    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
    in
        fun findExecutable s = SOME(winCall(1033, s)) handle OS.SysErr _ => NONE
    end

    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
    in
        fun openDocument s = winCall(1034, s)
    end

    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
    in
        fun launchApplication (command, arg) =
            winCall(1035, (command, arg))
    end
    
    abstype pid = PID of int with end; (* Abstract *)
    
    datatype ('a,'b) proc =
        WinProc of
        {
            pid: pid,
            result: OS.Process.status option ref,
            closeActions: (unit->unit) list ref,
            lock: Thread.Mutex.mutex
        }

    (* Run a process and return a proces object which will
       allow us to extract the input and output streams. *)
    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
    in
        fun execute(command, arg): ('a,'b) proc =
        let
            val run: pid = winCall (1000, (command, arg))
        in
            WinProc{ pid=run, result=ref NONE, closeActions=ref [], lock=Thread.Mutex.mutex() }
        end
    end

    (* Local function to protect access. *)
    fun winProtect f (w as WinProc{lock, ...}) = ThreadLib.protect lock f w

    local
        val doIo = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
    in
        fun sys_get_buffsize (strm: OS.IO.iodesc): int = doIo(15, strm, 0)
    end

    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"

        fun textInstreamOf'(WinProc{pid, closeActions, ...}) =
        let
            (* Get the underlying file descriptor. *)
            val n = winCall (1001, RunCall.unsafeCast pid)
            val textPrimRd =
                LibraryIOSupport.wrapInFileDescr
                    {fd=n, name="TextPipeInput", initBlkMode=true}
            val streamIo = TextIO.mkInstream(TextIO.StreamIO.mkInstream(textPrimRd, ""))
            fun closeThis() = TextIO.closeIn streamIo
            val () = closeActions := closeThis :: ! closeActions
        in
            streamIo
        end
        
        fun textOutstreamOf'(WinProc{pid, closeActions, ...}) =
        let
            val n = winCall (1002, RunCall.unsafeCast pid)
            val buffSize = sys_get_buffsize n
            val textPrimWr =
                LibraryIOSupport.wrapOutFileDescr{fd=n, name="TextPipeOutput",
                    appendMode=false, initBlkMode=true, chunkSize=buffSize}
            (* Construct a stream. *)
            val streamIo = TextIO.mkOutstream(TextIO.StreamIO.mkOutstream(textPrimWr, IO.LINE_BUF))
            fun closeThis() = TextIO.closeOut streamIo
            val () = closeActions := closeThis :: ! closeActions
        in
            streamIo
        end

        fun binInstreamOf'(WinProc{pid, closeActions, ...}) =
        let
            (* Get the underlying file descriptor. *)
            val n = winCall (1003, RunCall.unsafeCast pid)
            val binPrimRd =
                LibraryIOSupport.wrapBinInFileDescr
                    {fd=n, name="BinPipeInput", initBlkMode=true}
            val streamIo =
                BinIO.mkInstream(BinIO.StreamIO.mkInstream(binPrimRd, Word8Vector.fromList []))
            fun closeThis() = BinIO.closeIn streamIo
            val () = closeActions := closeThis :: ! closeActions
        in
            streamIo
        end
        
        fun binOutstreamOf'(WinProc{pid, closeActions, ...}) =
        let
            val n = winCall (1004, RunCall.unsafeCast pid)
            val buffSize = sys_get_buffsize n
            val binPrimWr =
                LibraryIOSupport.wrapBinOutFileDescr{fd=n, name="BinPipeOutput",
                    appendMode=false, initBlkMode=true, chunkSize=buffSize}
            (* Construct a stream. *)
            val streamIo = BinIO.mkOutstream(BinIO.StreamIO.mkOutstream(binPrimWr, IO.LINE_BUF))
            fun closeThis() = BinIO.closeOut streamIo
            val () = closeActions := closeThis :: ! closeActions
        in
            streamIo
        end
    in
        fun textInstreamOf w = winProtect textInstreamOf' w
        and textOutstreamOf w = winProtect textOutstreamOf' w
        and binInstreamOf w = winProtect binInstreamOf' w
        and binOutstreamOf w = winProtect binOutstreamOf' w
    end

    (* reap - wait until the process finishes and get the result.
       Note: this is defined to be able to return the result repeatedly. *)
    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"

        fun reap'(WinProc{result=ref(SOME r), ...}) = r

        |   reap'(WinProc{pid, result, closeActions, ...}) =
            let
                val _ = List.app(fn f => f()) (!closeActions)
                val _ = closeActions := []
                val res: OS.Process.status = winCall (1005, RunCall.unsafeCast pid)
                val _ = result := SOME res
            in
                res
            end
    in
        fun reap w = winProtect reap' w
    end

    local
        val winCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
    in
        (* Run a process and wait for the result.  Rather than do the
           whole thing as a single RTS call we first start the process
           and then call "reap" to get the result.  This allows this
           to be run as a separate ML process if necessary without
           blocking everything. 
           This is similar to OS.Process.system but differs in that the
           streams are directed to NUL and this runs the executable directly,
           not via cmd.exe/command.com so cannot run DOS commands.
           OS.Process.system waits for the result within the RTS call so
           the whole of ML will be blocked until the process completes. *)
        fun simpleExecute (command, arg) =
        let
            val run: pid = winCall(1037, (command, arg))
            val process = WinProc{ pid=run, result=ref NONE, closeActions=ref [], lock=Thread.Mutex.mutex() }
        in
            reap process
        end
    end


    structure Status =
    struct
        type status = SysWord.word
        
        val accessViolation        = getConst 10
        val arrayBoundsExceeded    = getConst 11
        val breakpoint             = getConst 12
        val controlCExit           = getConst 13
        val datatypeMisalignment   = getConst 14
        val floatDenormalOperand   = getConst 15
        val floatDivideByZero      = getConst 16
        val floatInexactResult     = getConst 17
        val floatInvalidOperation  = getConst 18
        val floatOverflow          = getConst 19
        val floatStackCheck        = getConst 20
        val floatUnderflow         = getConst 21
        val guardPageViolation     = getConst 22
        val integerDivideByZero    = getConst 23
        val integerOverflow        = getConst 24
        val illegalInstruction     = getConst 25
        val invalidDisposition     = getConst 26
        val invalidHandle          = getConst 27
        val inPageError            = getConst 28
        (* This was given as nocontinuableException *)
        val noncontinuableException= getConst 29
        val pending                = getConst 30
        val privilegedInstruction  = getConst 31
        val singleStep             = getConst 32
        val stackOverflow          = getConst 33
        val timeout                = getConst 34
        val userAPC                = getConst 35
    end

    (* The status is implemented as an integer. *)
    fun fromStatus (s: OS.Process.status): Status.status =
        SysWord.fromInt(RunCall.unsafeCast s);

    fun exit (s: Status.status) =
        OS.Process.exit(RunCall.unsafeCast(SysWord.toInt s))

    structure Config =
    struct
        local
            val winCall: int*unit->int*int*int*int*string =
                RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            fun getVersionEx () =
            let
                val (major, minor, build, platform, version) =
                    winCall(1050, ())
            in
                { majorVersion = SysWord.fromInt major,
                  minorVersion = SysWord.fromInt minor,
                  buildNumber = SysWord.fromInt build,
                  platformId = SysWord.fromInt platform,
                  csdVersion = version }
            end
        end

        local
            val winCall: int*unit->string =
                RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        in
            fun getWindowsDirectory () = winCall(1051, ())
            and getSystemDirectory () = winCall(1052, ())
            and getComputerName () = winCall(1053, ())
            and getUserName () = winCall(1054, ())
        end

        val platformWin32s = getConst 36
        val platformWin32Windows = getConst 37
        val platformWin32NT = getConst 38
        val platformWin32CE = getConst 39
    end
end;

local
    (* Add pretty printers to hide internals. *)
    fun prettyRegKey _ _ (_: Windows.Reg.hkey) = PolyML.PrettyString "?"
    and prettyDDEInfo _ _ (_: Windows.DDE.info) = PolyML.PrettyString "?"
    and prettyProc _ _ (_: ('a, 'b) Windows.proc) = PolyML.PrettyString "?"
in
    val () = PolyML.addPrettyPrinter prettyRegKey
    and () = PolyML.addPrettyPrinter prettyDDEInfo
    and () = PolyML.addPrettyPrinter prettyProc
end;
