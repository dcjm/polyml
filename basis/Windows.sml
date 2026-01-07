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
    open Foreign

    val cDWORD = cUint32 (* Defined to be 32-bit unsigned *)

    (* Conversion for a fixed size char array. *)
    fun cCHARARRAY n : string conversion =
    let
        (* Make it a struct of chars *)
        val { size=sizeC, align=alignC, ... } = LowLevel.cTypeChar
        val arraySize = sizeC * Word.fromInt n
        val arrayType: LowLevel.cType =
            { size = arraySize, align = alignC, typeForm = LowLevel.CTypeStruct(List.tabulate (n, fn _ => LowLevel.cTypeChar)) }

        open Memory

        fun load(v: voidStar): string =
        let
            (* It should be null-terminated but just in case... *)
            fun sLen i = if i = Word.fromInt n orelse get8(v, i) = 0w0 then i else sLen(i+0w1)
            val length = sLen 0w0
            fun loadChar i =
                Char.chr(Word8.toInt(get8(v, Word.fromInt i)))
        in
            CharVector.tabulate(Word.toInt length, loadChar)
        end

        fun store(v: voidStar, s: string) =
        let
            (* The length must be less than the size to allow for the null *)
            val sLen = size s
            val _ = sLen < n orelse raise Fail "string too long"
        in
            CharVector.appi(fn(i, ch) => set8(v, Word.fromInt i, Word8.fromInt(Char.ord ch))) s;
            set8(v, Word.fromInt sLen, 0w0);
            fn () => ()
        end
    in
        makeConversion { load = load, store = store, ctype = arrayType }
    end
    
    (* These are called XXX32.DLL on both 32-bit and 64-bit. *)
    fun kernel name = getSymbol(loadLibrary "kernel32.dll") name
    and shell sym = getSymbol(loadLibrary "shell32.DLL") sym
    and advapi sym = getSymbol(loadLibrary "advapi32.DLL") sym 

    (* We need to use the Pascal calling convention on 32-bit Windows. *)
    val winAbi =
        case List.find (fn ("stdcall", _) => true | _ => false) LowLevel.abiList of
            SOME(_, abi) => abi
        |   NONE => LowLevel.abiDefault

    (* As well as setting the abi we can also use the old argument order. *)
    fun winCall1 sym argConv resConv = buildCall1withAbi(winAbi, sym, argConv, resConv)
    and winCall2 sym argConv resConv = buildCall2withAbi(winAbi, sym, argConv, resConv)
    and winCall3 sym argConv resConv = buildCall3withAbi(winAbi, sym, argConv, resConv)
    and winCall5 sym argConv resConv = buildCall5withAbi(winAbi, sym, argConv, resConv)
    and winCall6 sym argConv resConv = buildCall6withAbi(winAbi, sym, argConv, resConv)
    and winCall8 sym argConv resConv = buildCall8withAbi(winAbi, sym, argConv, resConv)
    and winCall9 sym argConv resConv = buildCall9withAbi(winAbi, sym, argConv, resConv)
    and winCall12 sym argConv resConv = buildCall12withAbi(winAbi, sym, argConv, resConv)
    
    val MAX_PATH = 0w260

    (* Convert a C string to ML. *)
    fun fromCstring buff =
    let
        open Memory
        (* We can't use #load cString because the argument is the address of
           the address of the string. *)
        fun sLen i = if get8(buff, i) = 0w0 then i else sLen(i+0w1)
        val length = sLen 0w0
        fun loadChar i =
            Char.chr(Word8.toInt(get8(buff, Word.fromInt i)))
    in
        CharVector.tabulate(Word.toInt length, loadChar)
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

        val queryValue          = 0wx0001
        and setValue            = 0wx0002
        and createSubKey        = 0wx0004
        and enumerateSubKeys    = 0wx0008
        and notify              = 0wx0010
        and createLink          = 0wx0020
        
        val read    = flags[0wx00020000, queryValue, enumerateSubKeys, notify]
        and write   = flags[0wx00020000, setValue, createSubKey]
        
        val execute = read
        
        val allAccess = flags[0wx000f0000, queryValue, setValue, createSubKey, enumerateSubKeys, notify, createLink]

        (* all is probably equivalent to allAccess. *)
        val all = flags[allAccess, createLink, createSubKey, enumerateSubKeys,
                        execute, notify, queryValue, read, setValue, write]

        val intersect = List.foldl (fn (a, b) => SysWord.andb(a,b)) all
    end

    structure Reg =
    struct
        datatype hkey = PREDEFINED of SysWord.word | SUBKEY of Memory.volatileRef

        val classesRoot     = PREDEFINED 0wx80000000
        and currentUser     = PREDEFINED 0wx80000001
        and localMachine    = PREDEFINED 0wx80000002
        and users           = PREDEFINED 0wx80000003
        and performanceData = PREDEFINED 0wx80000004
        and currentConfig   = PREDEFINED 0wx80000005
        and dynData         = PREDEFINED 0wx80000006
        
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
            val invalidHandle = valOf(OS.syserror "ERROR_INVALID_HANDLE")
        in
            (* When a subkey is opened the value is stored in a volatile ref.
               This prevents the key from persisting. *)
            fun getHkeyValue(PREDEFINED w) = Memory.sysWord2VoidStar w
            |   getHkeyValue(SUBKEY v) =
                let
                    val w = Memory.getVolatileRef v
                in
                    if w = 0w0
                    then raise OS.SysErr("ERROR_INVALID_HANDLE", SOME invalidHandle)
                    else Memory.sysWord2VoidStar w
                end
        end
        
        (* Check the result and raise an exception if it's non-zero. *)
        fun checkLResult 0 = ()
        |   checkLResult nonZero =
            let
                val err = Error.fromWord(SysWord.fromInt nonZero)
            in
                raise OS.SysErr(OS.errorMsg err, SOME err)
            end

        local
            val openKey = winCall5 (advapi "RegOpenKeyExA") (cPointer, cString, cDWORD, cDWORD, cStar cPointer) cLong
        in
            fun openKeyEx(hkey, key, flags) =
            let
                val keyRes = ref Memory.null
                val result = openKey(getHkeyValue hkey, key, 0, SysWord.toInt(Key.toWord flags), keyRes)
            in
                checkLResult result;
                SUBKEY(Memory.volatileRef(Memory.voidStar2Sysword(!keyRes)))
            end
        end

        local
            val createKey =
                winCall9 (advapi "RegCreateKeyExA")
                    (cPointer, cString, cDWORD, cPointer, cDWORD, cDWORD, cPointer, cStar cPointer, cStar cDWORD) cLong
        in
            fun createKeyEx(hkey, key, flags) =
            let
                val keyRes = ref Memory.null
                val disp = ref 0
                val REG_OPTION_NON_VOLATILE = 0
                val REG_CREATED_NEW_KEY = 1
                val result =
                    createKey(getHkeyValue hkey, key, 0, Memory.null, REG_OPTION_NON_VOLATILE,
                              SysWord.toInt(Key.toWord flags), Memory.null, keyRes, disp)
            in
                checkLResult result;
                (if ! disp = REG_CREATED_NEW_KEY then CREATED_NEW_KEY else OPENED_EXISTING_KEY)
                    (SUBKEY(Memory.volatileRef(Memory.voidStar2Sysword(!keyRes))))
            end
        end
        
        local
            val regCloseKey = winCall1 (advapi "RegCloseKey") cPointer cLong
        in
            fun closeKey hkey = checkLResult(regCloseKey(getHkeyValue hkey))
        end
        
        local
            val regDeleteValue = winCall2 (advapi "RegDeleteValueA") (cPointer, cString) cLong
        in
            fun deleteValue(hkey, key) = checkLResult(regDeleteValue(getHkeyValue hkey, key))
        end

        local
            val regDeleteKey = winCall2 (advapi "RegDeleteKeyA") (cPointer, cString) cLong
        in
            fun deleteKey(hkey, key) = checkLResult(regDeleteKey(getHkeyValue hkey, key))
        end

        local
            val regEnumKeyEx =
                winCall8 (advapi "RegEnumKeyExA")
                    (cPointer, cDWORD, cPointer, cStar cDWORD, cPointer, cPointer, cPointer, cPointer) cLong
            val regEnumVal =
                winCall8 (advapi "RegEnumValueA")
                    (cPointer, cDWORD, cPointer, cStar cDWORD, cPointer, cPointer, cPointer, cPointer) cLong
                    
            val regQueryInfoKey =
                winCall12 (advapi "RegQueryInfoKeyA")
                    (cPointer, cPointer, cPointer, cPointer, cStar cDWORD, cStar cDWORD, cPointer,
                     cStar cDWORD, cStar cDWORD, cStar cDWORD, cPointer, cPointer) cLong

            fun getInfo hkey =
            let
                open Memory
                val nSubKeys = ref 0 and maxSubKeyLen = ref 0 and nValues = ref 0
                and maxValueNameLen = ref 0 and maxValueLen = ref 0
                val result =
                    regQueryInfoKey(getHkeyValue hkey, null, null, null, nSubKeys, maxSubKeyLen, null, nValues,
                                    maxValueNameLen, maxValueLen, null, null)
            in
                checkLResult result;
                { nSubKeys = !nSubKeys, maxSubKeyLen = !maxSubKeyLen, nValues = ! nValues,
                  maxValueNameLen = !maxValueNameLen, maxValueLen= !maxValueLen}
            end
        in
            fun enumKeyEx(hkey, n) =
            let
                val {nSubKeys, maxSubKeyLen, ...} = getInfo hkey
            in
                if n >= nSubKeys
                then NONE
                else
                let
                    open Memory
                    val buff = malloc(Word.fromInt(maxSubKeyLen + 1))
                    val buffLen = ref(maxSubKeyLen + 1)
                    val result =
                        regEnumKeyEx(getHkeyValue hkey, n, buff, buffLen, null, null, null, null)
                in
                    checkLResult result handle exn as OS.SysErr _ => (free buff; raise exn);
                    SOME(fromCstring buff) before free buff
                end
            end
            
            and enumValueEx(hkey, n) =
            let
                val {nValues, maxValueNameLen, ...} = getInfo hkey
            in
                if n >= nValues
                then NONE
                else
                let
                    open Memory
                    val buff = malloc(Word.fromInt(maxValueNameLen + 1))
                    val buffLen = ref(maxValueNameLen + 1)
                    val result =
                        regEnumVal(getHkeyValue hkey, n, buff, buffLen, null, null, null, null)
                in
                    checkLResult result handle exn as OS.SysErr _ => (free buff; raise exn);
                    SOME(fromCstring buff) before free buff
                end
            end
        end

        local
            val queryVal =
                winCall6 (advapi "RegQueryValueExA") (cPointer, cString, cPointer, cStar cDWORD, cPointer, cStar cDWORD) cLong
            
            val ERROR_MORE_DATA = 234
            val ERROR_FILE_NOT_FOUND = 2

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
            
            (* Repeat allocating the buffer until it's big enough. *)
            fun requery(hkey, valueName, buff, buffSize) =
            let
                open Memory
                val buffSizeRef = ref buffSize
                val typeVal = ref 0
                val result = queryVal(getHkeyValue hkey, valueName, Memory.null, typeVal, buff, buffSizeRef)
            in
                (* Returns 0 (success) if the buffer is null.  Return ERROR_MORE_DATA if it is
                   not null but not big enough. *)
                if result = ERROR_MORE_DATA orelse result = 0 andalso ! buffSizeRef > buffSize
                then
                let
                    val () = free buff
                    val buff = malloc(Word.fromInt(!buffSizeRef))
                in
                    requery(hkey, valueName, buff, !buffSizeRef)
                end
                else if result = 0
                then (* Big enough - return result.  N.B. This could be NULL. *)
                let
                    val v = Word8Vector.tabulate(! buffSizeRef, fn i => get8(buff, Word.fromInt i))
                    val () = free buff
                in
                    case !typeVal of
                        1 => SOME(SZ(unpackString v))
                    |   4 => SOME(DWORD(PackWord32Little.subVec(v, 0)))
                    |   2 => SOME(EXPAND_SZ(unpackString v))
                    |   7 => SOME(MULTI_SZ(unpackStringList v))
                    |   _ => SOME(BINARY v)
                end
                else if result = ERROR_FILE_NOT_FOUND
                then NONE
                else (free buff; checkLResult result; raise Match (* Unused: we've raised an exception *))
            end
                
        in
            fun queryValueEx (hkey, valueName) = requery(hkey, valueName, Memory.null, 0)
        end

        local
            val setValue = winCall6 (advapi "RegSetValueExA") (cPointer, cString, cDWORD, cDWORD, cByteArray, cDWORD) cLong

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
            fun setValueEx(hkey, name, v) =
            let
                val (t, s) = valuesToTypeVal v
                val length = Word8Vector.length s
                val result = setValue(getHkeyValue hkey, name, 0, t, s, length)
            in
                checkLResult result
            end
        end
    end
    
    structure DDE =
    struct
        type info = SysWord.word (* Actually a volatile word containing the conversation handle. *)

        val startDialog: string * string -> info = RunCall.rtsCallFull2 "PolyWindowsDDEStartDialogue"
        and stopDialog: info -> unit = RunCall.rtsCallFull1 "PolyWindowsDDEClose"

        local
            val winCall: info * string -> bool = RunCall.rtsCallFull2 "PolyWindowsDDEExecute"
        in
            (* The timeout and retry count apply only in the case of
               a busy result.  The Windows call takes a timeout parameter
               as the length of time to wait for a response and maybe we
               should use it for that as well. *)
            fun executeString (info, cmd, retry, delay) =
            let
                fun try n =
                    if winCall(info, cmd)
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

    end (* DDE *)

    local
        val getVol =
            winCall8(kernel "GetVolumeInformationA")
                (cString, cPointer, cDWORD, cStar cDWORD, cStar cDWORD, cStar cDWORD, cPointer, cDWORD) cInt
    in
        fun getVolumeInformation root =
        let
            open Memory
            val volBuffer = malloc (MAX_PATH+0w1)
            and sysBuffer = malloc (MAX_PATH+0w1)
            
            val volSerial = ref 0
            and volMaxComponent = ref 0
            and fileSysFlags = ref 0
        in
            if getVol(root, volBuffer, Word.toInt(MAX_PATH+0w1), volSerial, volMaxComponent, fileSysFlags,
                      sysBuffer, Word.toInt(MAX_PATH+0w1)) = 0
            then
            let
                val err = Error.fromWord(Error.getLastError())
            in
                free volBuffer; free sysBuffer;
                raise OS.SysErr(OS.errorMsg err, SOME err)
            end
            else
            { volumeName = fromCstring volBuffer, systemName = fromCstring sysBuffer,
              serialNumber = SysWord.fromInt (!volSerial),
              maximumComponentLength = ! volMaxComponent} before (free volBuffer; free sysBuffer)
        end
    end

    local
        val findExeca: string * int * Memory.voidStar -> int =
            winCall3 (shell "FindExecutableA") (cString, cInt, cPointer) cInt
    in
        fun findExecutable s =
        let
            open Memory
            val buff = malloc MAX_PATH
            val result =
                if findExeca(s, 0 (* NULL *), buff) > 32
                then SOME(fromCstring buff)
                else NONE
        in
            free buff;
            result
        end
    end

    local
        val shellExecuteInfo =
            cStruct15(cDWORD, cUlong, cPointer (*HWND*), cString, cString, cOptionPtr cString, cOptionPtr cString, cInt,
                     cPointer (*HINSTANCE*), cPointer, cPointer (*cString*), cPointer (*HKEY*), cDWORD, cPointer (*HANDLE*), cPointer (*HANDLE*))
        val {ctype={size, ...}, ...} = breakConversion shellExecuteInfo
        val shellExec = winCall1 (shell "ShellExecuteExA") (cStar shellExecuteInfo) cInt
        (* We probably want handle the error ourselves rather than popping up a UI. *)
        val SEE_MASK_FLAG_NO_UI = 0x00000400
    in
        fun openDocument file =
        let
            val r =
                ref(Word.toInt size, SEE_MASK_FLAG_NO_UI, Memory.null, "open", file, NONE, NONE, 1 (* SW_SHOWNORMAL *),
                    Memory.null, Memory.null, Memory.null, Memory.null, 0, Memory.null, Memory.null)
        in
            if shellExec r = 0
            then
            let
                val err = Error.fromWord(Error.getLastError())
            in
                raise OS.SysErr(OS.errorMsg err, SOME err)
            end
            else ()
        end
        
        and launchApplication (command, arg) =
        let
            val r =
                ref(Word.toInt size, SEE_MASK_FLAG_NO_UI, Memory.null, "open", command, SOME arg, NONE, 1 (* SW_SHOWNORMAL *),
                    Memory.null, Memory.null, Memory.null, Memory.null, 0, Memory.null, Memory.null)
        in
            if shellExec r = 0
            then
            let
                val err = Error.fromWord(Error.getLastError())
            in
                raise OS.SysErr(OS.errorMsg err, SOME err)
            end
            else ()
        end
        
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
        val winCall = RunCall.rtsCallFull2 "PolyWindowsExecute"
    in
        fun execute(command, arg): ('a,'b) proc =
        let
            val run: pid = winCall (command, arg)
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
        val winCall: pid * bool * bool -> OS.IO.iodesc =
            RunCall.rtsCallFull3 "PolyWindowsOpenProcessHandle"

        fun textInstreamOf'(WinProc{pid, closeActions, ...}) =
        let
            (* Get the underlying file descriptor. *)
            val n = winCall(pid, true, true)
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
            val n = winCall (pid, false, true)
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
            val n = winCall (pid, true, false)
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
            val n = winCall (pid, false, false)
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
        val winCall: pid -> OS.Process.status = RunCall.rtsCallFull1 "PolyWindowsGetProcessResult"

        fun reap'(WinProc{result=ref(SOME r), ...}) = r

        |   reap'(WinProc{pid, result, closeActions, ...}) =
            let
                val _ = List.app(fn f => f()) (!closeActions)
                val _ = closeActions := []
                val res: OS.Process.status = winCall pid
                val _ = result := SOME res
            in
                res
            end
    in
        fun reap w = winProtect reap' w
    end

    local
        val winCall: string * string -> pid = RunCall.rtsCallFull2 "PolyWindowsSimpleExecute"
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
            val run: pid = winCall(command, arg)
            val process = WinProc{ pid=run, result=ref NONE, closeActions=ref [], lock=Thread.Mutex.mutex() }
        in
            reap process
        end
    end


    structure Status =
    struct
        type status = SysWord.word
        
        val accessViolation        = 0wxC0000005
        val arrayBoundsExceeded    = 0wxC000008C
        val breakpoint             = 0wx80000003
        val controlCExit           = 0wxC000013A
        val datatypeMisalignment   = 0wx80000002
        val floatDenormalOperand   = 0wxC000008D
        val floatDivideByZero      = 0wxC000008E
        val floatInexactResult     = 0wxC000008F
        val floatInvalidOperation  = 0wxC0000090
        val floatOverflow          = 0wxC0000091
        val floatStackCheck        = 0wxC0000092
        val floatUnderflow         = 0wxC0000093
        val guardPageViolation     = 0wx80000001
        val integerDivideByZero    = 0wxC0000094
        val integerOverflow        = 0wxC0000095
        val illegalInstruction     = 0wxC000001D
        val invalidDisposition     = 0wxC0000026
        val invalidHandle          = 0wxC0000008
        val inPageError            = 0wxC0000006
        (* This was given as nocontinuableException *)
        val noncontinuableException= 0wxC0000025
        val pending                = 0wx103
        val privilegedInstruction  = 0wxC0000096
        val singleStep             = 0wx80000004
        val stackOverflow          = 0wxC00000FD
        val timeout                = 0wx102
        val userAPC                = 0wxC0
    end

    (* The status is implemented as an integer. *)
    fun fromStatus (s: OS.Process.status): Status.status =
        SysWord.fromInt(RunCall.unsafeCast s);

    fun exit (s: Status.status) =
        OS.Process.exit(RunCall.unsafeCast(SysWord.toInt s))

    structure Config =
    struct
        local
            val osVersionInfo =
                cStruct6(cDWORD, cDWORD, cDWORD, cDWORD, cDWORD, cCHARARRAY 128)
            val { ctype={size, ...}, ...} = breakConversion osVersionInfo
            val callGetVersion =
                winCall1 (kernel "GetVersionExA") (cStar osVersionInfo) cInt
        in
            fun getVersionEx () =
            let
                val r = ref(Word.toInt size, 0, 0, 0, 0, "")
            in
                if callGetVersion r = 0
                then 
                let
                    val err = Error.fromWord(Error.getLastError())
                in
                    raise OS.SysErr(OS.errorMsg err, SOME err)
                end
                else
                let
                    val (_, major, minor, build, platform, version) = !r
                in
                    { majorVersion = SysWord.fromInt major,
                      minorVersion = SysWord.fromInt minor,
                      buildNumber = SysWord.fromInt build,
                      platformId = SysWord.fromInt platform,
                      csdVersion = version }
                end
            end
        end

        local
            (* Get Windows directory and System directory. *)
            val getWinDir: Memory.voidStar * int -> int =
                winCall2(kernel "GetWindowsDirectoryA") (cPointer, cUint) cUint
            and getSysDir: Memory.voidStar * int -> int =
                winCall2(kernel "GetSystemDirectoryA") (cPointer, cUint) cUint
                
            fun getDirectory getDir () =
            let
                open Memory
                val buff = malloc MAX_PATH
                val result = getDir(buff, Word.toInt MAX_PATH)
            in
                if result = 0
                then
                let
                    val err = Error.fromWord(Error.getLastError())
                in
                    free buff;
                    raise OS.SysErr(OS.errorMsg err, SOME err)
                end
                else fromCstring buff before free buff
            end
        in
            val getWindowsDirectory = getDirectory getWinDir
            and getSystemDirectory = getDirectory getSysDir
        end

        local
            val getCompName: Memory.voidStar * int ref -> int =
                winCall2(kernel "GetComputerNameA") (cPointer, cStar cDWORD) cInt
            and getUsrName: Memory.voidStar * int ref -> int =
                winCall2(advapi "GetUserNameA") (cPointer, cStar cDWORD) cUint
            
            val MAX_COMPUTERNAME_LENGTH = 015
            and UNLEN = 256

            fun getName (getNm, len) () =
            let
                open Memory
                val buff = malloc(Word.fromInt len)
                val result = getNm(buff, ref len)
            in
                if result = 0
                then
                let
                    val err = Error.fromWord(Error.getLastError())
                in
                    free buff;
                    raise OS.SysErr(OS.errorMsg err, SOME err)
                end
                else fromCstring buff before free buff
            end
        in
            val getComputerName = getName(getCompName, MAX_COMPUTERNAME_LENGTH+1)
            and getUserName = getName(getUsrName, UNLEN+1)
        end

        (* All of these are long since dead *)
        val platformWin32s          = 0w0
        val platformWin32Windows    = 0w1
        val platformWin32NT         = 0w2
        val platformWin32CE         = 0w3
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
