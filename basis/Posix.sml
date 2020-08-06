(*
    Title:      Standard Basis Library: Posix structure and signature.
    Copyright   David Matthews 2000, 2016-17, 2019-2020

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

signature POSIX_ERROR =
sig
    type syserror = OS.syserror (* G&R 2004 has an error *)

    val toWord   : syserror -> SysWord.word
    val fromWord : SysWord.word -> syserror
    val errorMsg : syserror -> string
    val errorName : syserror -> string
    val syserror  : string -> syserror option

    val acces : syserror
    val again : syserror
    val badf : syserror
    val badmsg : syserror
    val busy : syserror
    val canceled (* sic *) : syserror
    val child : syserror
    val deadlk : syserror
    val dom : syserror
    val exist : syserror
    val fault : syserror
    val fbig : syserror
    val inprogress : syserror
    val intr : syserror
    val inval : syserror
    val io : syserror
    val isdir : syserror
    val loop : syserror
    val mfile : syserror
    val mlink : syserror
    val msgsize : syserror
    val nametoolong : syserror
    val nfile : syserror
    val nodev : syserror
    val noent : syserror
    val noexec : syserror
    val nolck : syserror
    val nomem : syserror
    val nospc : syserror
    val nosys : syserror
    val notdir : syserror
    val notempty : syserror
    val notsup : syserror
    val notty : syserror
    val nxio : syserror
    val perm : syserror
    val pipe : syserror
    val range : syserror
    val rofs : syserror
    val spipe : syserror
    val srch : syserror
    val toobig : syserror
    val xdev : syserror
end;

signature POSIX_SIGNAL =
sig
    eqtype signal
    val toWord   : signal -> SysWord.word
    val fromWord : SysWord.word -> signal
    val abrt : signal
    val alrm : signal
    val bus : signal
    val fpe : signal
    val hup : signal
    val ill : signal
    val int : signal
    val kill : signal
    val pipe : signal
    val quit : signal
    val segv : signal
    val term : signal
    val usr1 : signal
    val usr2 : signal
    val chld : signal
    val cont : signal
    val stop : signal
    val tstp : signal
    val ttin : signal
    val ttou : signal
end;

signature POSIX_PROCESS =
sig
    eqtype signal
    eqtype pid
    val wordToPid : SysWord.word -> pid
    val pidToWord : pid -> SysWord.word

    val fork : unit -> pid option
    val exec  : string * string list -> 'a
    val exece : string * string list * string list -> 'a
    val execp : string * string list -> 'a

    datatype waitpid_arg =
        W_ANY_CHILD | W_CHILD of pid | W_SAME_GROUP | W_GROUP of pid
    datatype exit_status =
        W_EXITED | W_EXITSTATUS of Word8.word
        | W_SIGNALED (* sic *) of signal | W_STOPPED of signal

    val fromStatus : OS.Process.status -> exit_status

    structure W:
    sig
        include BIT_FLAGS
        val untraced : flags
    end

    val wait : unit -> pid * exit_status
    val waitpid : waitpid_arg * W.flags list -> pid * exit_status
    val waitpid_nh : waitpid_arg * W.flags list -> (pid * exit_status) option

    val exit : Word8.word -> 'a

    datatype killpid_arg = K_PROC of pid | K_SAME_GROUP | K_GROUP of pid

    val kill : killpid_arg * signal -> unit
    val alarm : Time.time -> Time.time
    val pause : unit -> unit
    (* QUESTION: Why does sleep return a Time.time ? Is it intended to be the
       time remaining?  Assume so. *)
    val sleep : Time.time -> Time.time
end;

signature POSIX_PROC_ENV =
sig
    eqtype pid
    eqtype uid
    eqtype gid
    eqtype file_desc
    val uidToWord : uid -> SysWord.word
    val wordToUid : SysWord.word -> uid
    val gidToWord : gid -> SysWord.word
    val wordToGid : SysWord.word -> gid
    val getpid  : unit -> pid
    val getppid : unit -> pid
    val getuid  : unit -> uid
    val geteuid : unit -> uid
    val getgid  : unit -> gid
    val getegid : unit -> gid
    val setuid : uid -> unit
    val setgid : gid -> unit
    val getgroups : unit -> gid list
    val getlogin : unit -> string
    val getpgrp : unit -> pid
    val setsid : unit -> pid
    val setpgid : {pid : pid option, pgid : pid option} -> unit
    val uname : unit -> (string * string) list
    val time : unit -> Time.time
    val times : unit
               -> {
                 elapsed : Time.time,
                 utime : Time.time,
                 stime : Time.time,
                 cutime : Time.time,
                 cstime : Time.time
               }

    val getenv : string -> string option
    val environ : unit -> string list
    val ctermid : unit -> string
    val ttyname : file_desc -> string
    val isatty : file_desc -> bool
    val sysconf : string -> SysWord.word
end;

signature POSIX_FILE_SYS =
sig
    eqtype uid
    eqtype gid
    eqtype file_desc
    val fdToWord : file_desc -> SysWord.word
    val wordToFD : SysWord.word -> file_desc
    val fdToIOD : file_desc -> OS.IO.iodesc
    val iodToFD : OS.IO.iodesc -> file_desc option
    type dirstream
    val opendir : string -> dirstream
    val readdir : dirstream -> string option
    val rewinddir : dirstream -> unit
    val closedir : dirstream -> unit
    val chdir : string -> unit
    val getcwd : unit -> string

    val stdin  : file_desc
    val stdout : file_desc
    val stderr : file_desc

    structure S :
    sig
        eqtype mode
        include BIT_FLAGS
            where type flags = mode
        val irwxu : mode
        val irusr : mode
        val iwusr : mode
        val ixusr : mode
        val irwxg : mode
        val irgrp : mode
        val iwgrp : mode
        val ixgrp : mode
        val irwxo : mode
        val iroth : mode
        val iwoth : mode
        val ixoth : mode
        val isuid : mode
        val isgid : mode
    end

    structure O:
    sig
        include BIT_FLAGS
        val append : flags
        val excl : flags
        val noctty : flags
        val nonblock : flags
        val sync : flags
        val trunc : flags
    end

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR
    val openf   : string * open_mode * O.flags -> file_desc
    val createf : string * open_mode * O.flags * S.mode -> file_desc
    val creat : string * S.mode -> file_desc
    val umask : S.mode -> S.mode
    val link : {old : string, new : string} -> unit
    val mkdir : string * S.mode -> unit
    val mkfifo : string * S.mode -> unit
    val unlink : string -> unit
    val rmdir : string -> unit
    val rename : {old : string, new : string} -> unit
    val symlink : {old : string, new : string} -> unit
    val readlink : string -> string

    eqtype dev
    val wordToDev : SysWord.word -> dev
    val devToWord : dev -> SysWord.word

    eqtype ino
    val wordToIno : SysWord.word -> ino
    val inoToWord : ino -> SysWord.word

    structure ST:
    sig
        type stat
        val isDir  : stat -> bool
        val isChr  : stat -> bool
        val isBlk  : stat -> bool
        val isReg  : stat -> bool
        val isFIFO : stat -> bool
        val isLink : stat -> bool
        val isSock : stat -> bool
        val mode : stat -> S.mode
        val ino : stat -> ino
        val dev : stat -> dev
        val nlink : stat -> int
        val uid : stat -> uid
        val gid : stat -> gid
        val size : stat -> Position.int
        val atime : stat -> Time.time
        val mtime : stat -> Time.time
        val ctime : stat -> Time.time
    end

    val stat  : string -> ST.stat
    val lstat : string -> ST.stat
    val fstat : file_desc -> ST.stat

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    val access : string * access_mode list -> bool
    val chmod : string * S.mode -> unit
    val fchmod : file_desc * S.mode -> unit
    val chown : string * uid * gid -> unit
    val fchown : file_desc * uid * gid -> unit
    val utime : string * {actime : Time.time, modtime : Time.time} option -> unit
    val ftruncate : file_desc * Position.int -> unit
    val pathconf  : string * string -> SysWord.word option
    val fpathconf : file_desc * string -> SysWord.word option
end;

signature POSIX_IO =
sig
    eqtype file_desc
    eqtype pid
    val pipe: unit -> {infd : file_desc, outfd : file_desc}
    val dup: file_desc -> file_desc
    val dup2: {old : file_desc, new : file_desc} -> unit
    val close: file_desc -> unit
    val readVec : file_desc * int -> Word8Vector.vector
    val readArr: file_desc * Word8ArraySlice.slice -> int
    val writeVec: file_desc * Word8VectorSlice.slice -> int
    val writeArr: file_desc * Word8ArraySlice.slice -> int

    datatype whence = SEEK_SET | SEEK_CUR | SEEK_END

    structure FD:
    sig
        include BIT_FLAGS
        val cloexec: flags
    end

    structure O:
    sig
        include BIT_FLAGS
        val append : flags
        val nonblock : flags
        val sync : flags
    end

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

    val dupfd : {old : file_desc, base : file_desc} -> file_desc
    val getfd : file_desc -> FD.flags
    val setfd : file_desc * FD.flags -> unit
    val getfl : file_desc -> O.flags * open_mode
    val setfl : file_desc * O.flags -> unit
    val lseek : file_desc * Position.int * whence -> Position.int
    val fsync : file_desc -> unit

    datatype lock_type = F_RDLCK | F_WRLCK | F_UNLCK

    structure FLock:
    sig
        type flock
        val flock : {
                     ltype : lock_type,
                     whence : whence,
                     start : Position.int,
                     len : Position.int,
                     pid : pid option
                   } -> flock
        val ltype : flock -> lock_type
        val whence : flock -> whence
        val start : flock -> Position.int
        val len : flock -> Position.int
        val pid : flock -> pid option
    end

    val getlk : file_desc * FLock.flock -> FLock.flock
    val setlk : file_desc * FLock.flock -> FLock.flock
    val setlkw : file_desc * FLock.flock -> FLock.flock

    val mkBinReader:
        { fd : file_desc, name : string, initBlkMode : bool } -> BinPrimIO.reader
    val mkTextReader:
        { fd : file_desc, name : string, initBlkMode : bool } -> TextPrimIO.reader

    val mkBinWriter:
        { fd : file_desc, name : string, appendMode : bool,
          initBlkMode : bool, chunkSize : int } -> BinPrimIO.writer
    val mkTextWriter:
        { fd : file_desc, name : string, appendMode : bool,
          initBlkMode : bool, chunkSize : int } -> TextPrimIO.writer

end;

signature POSIX_SYS_DB =
sig
    eqtype uid
    eqtype gid
    structure Passwd :
    sig
        type passwd
        val name : passwd -> string
        val uid : passwd -> uid
        val gid : passwd -> gid
        val home : passwd -> string
        val shell : passwd -> string
    end
    structure Group :
        sig
        type group
        val name : group -> string
        val gid : group -> gid
        val members : group -> string list
        end
    val getgrgid : gid -> Group.group
    val getgrnam : string -> Group.group
    val getpwuid : uid -> Passwd.passwd
    val getpwnam : string -> Passwd.passwd
end;

signature POSIX_TTY =
sig
    eqtype pid
    eqtype file_desc
    structure V :
    sig
        val eof   : int
        val eol   : int
        val erase : int
        val intr  : int
        val kill  : int
        val min   : int
        val quit  : int
        val susp  : int
        val time  : int
        val start : int
        val stop  : int
        val nccs : int

        type cc
        val cc : (int * char) list -> cc
        val update : cc * (int * char) list -> cc
        val sub : cc * int -> char
    end
    structure I :
    sig
        include BIT_FLAGS
        val brkint : flags
        val icrnl : flags
        val ignbrk : flags
        val igncr : flags
        val ignpar : flags
        val inlcr : flags
        val inpck : flags
        val istrip : flags
        val ixoff : flags
        val ixon : flags
        val parmrk : flags
    end
    structure O :
    sig
        include BIT_FLAGS
        val opost : flags
    end
    structure C :
    sig
        include BIT_FLAGS
        val clocal : flags
        val cread : flags
        val cs5 : flags
        val cs6 : flags
        val cs7 : flags
        val cs8 : flags
        val csize : flags
        val cstopb : flags
        val hupcl : flags
        val parenb : flags
        val parodd : flags
    end
    structure L :
    sig
        include BIT_FLAGS
        val echo : flags
        val echoe : flags
        val echok : flags
        val echonl : flags
        val icanon : flags
        val iexten : flags
        val isig : flags
        val noflsh : flags
        val tostop : flags
    end
    eqtype speed
    val compareSpeed : speed * speed -> order
    val speedToWord : speed -> SysWord.word
    val wordToSpeed : SysWord.word -> speed
    val b0 : speed
    val b50    : speed
    val b75    : speed
    val b110   : speed
    val b134   : speed
    val b150   : speed
    val b200   : speed
    val b300   : speed
    val b600   : speed
    val b1200  : speed
    val b1800  : speed
    val b2400  : speed
    val b4800  : speed
    val b9600  : speed
    val b19200 : speed
    val b38400 : speed
    type termios
    val termios : {
           iflag : I.flags,
           oflag : O.flags,
           cflag : C.flags,
           lflag : L.flags,
           cc : V.cc,
           ispeed : speed,
           ospeed : speed
         } -> termios
    val fieldsOf : termios
          -> {
            iflag : I.flags,
            oflag : O.flags,
            cflag : C.flags,
            lflag : L.flags,
            cc : V.cc,
            ispeed : speed,
            ospeed : speed
          }
    val getiflag : termios -> I.flags
    val getoflag : termios -> O.flags
    val getcflag : termios -> C.flags
    val getlflag : termios -> L.flags
    val getcc : termios -> V.cc
    structure CF :
    sig
        val getospeed : termios -> speed
        val setospeed : termios * speed -> termios
        val getispeed : termios -> speed
        val setispeed : termios * speed -> termios
    end
    structure TC :
    sig
        eqtype set_action
        val sanow : set_action
        val sadrain : set_action
        val saflush : set_action
        eqtype flow_action
        val ooff : flow_action
        val oon : flow_action
        val ioff : flow_action
        val ion : flow_action
        eqtype queue_sel
        val iflush : queue_sel
        val oflush : queue_sel
        val ioflush : queue_sel
        val getattr : file_desc -> termios
        val setattr : file_desc * set_action * termios -> unit
        val sendbreak : file_desc * int -> unit
        val drain : file_desc -> unit
        val flush : file_desc * queue_sel -> unit
        val flow : file_desc * flow_action -> unit
    end
    val getpgrp : file_desc -> pid
    val setpgrp : file_desc * pid -> unit
end;

signature POSIX =
sig
    structure Error : POSIX_ERROR
    structure Signal : POSIX_SIGNAL
    structure Process : POSIX_PROCESS
        where type signal = Signal.signal
    structure ProcEnv : POSIX_PROC_ENV
        where type pid = Process.pid
    structure FileSys : POSIX_FILE_SYS
        where type file_desc = ProcEnv.file_desc
        where type uid = ProcEnv.uid
        where type gid = ProcEnv.gid
    structure IO : POSIX_IO
        where type pid = Process.pid
        where type file_desc = ProcEnv.file_desc
        where type open_mode = FileSys.open_mode
    structure SysDB : POSIX_SYS_DB
        where type uid = ProcEnv.uid
        where type gid = ProcEnv.gid
    structure TTY : POSIX_TTY
        where type pid = Process.pid
        where type file_desc = ProcEnv.file_desc
end;

structure Posix :> 
    sig include POSIX
    (* I'm not sure if it's legal to use where type with
       a datatype.  The alternative is to copy the whole
       of the signature and use datatype replication. *)
        where type FileSys.access_mode = OS.FileSys.access_mode
    sharing type Process.pid = ProcEnv.pid = IO.pid = TTY.pid
    sharing type ProcEnv.uid = FileSys.uid = SysDB.uid
    sharing type ProcEnv.gid = FileSys.gid = SysDB.gid
    sharing type ProcEnv.file_desc = FileSys.file_desc =
            IO.file_desc = TTY.file_desc
    end
    (* Posix.Signal.signal is made the same as int so that we can
       pass the values directly to our (non-standard) Signal.signal
       function.  Since there isn't a standard way of handling
       signals this is the best we can do. *)
    where type Signal.signal = int
    where type FileSys.dirstream = OS.FileSys.dirstream
    =
struct
    local
        val processEnvGeneralCall = RunCall.rtsCallFull2 "PolyProcessEnvGeneral"
        and osSpecificGeneralCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
        and timingGeneralCall = RunCall.rtsCallFull2 "PolyTimingGeneral"
    in
        fun processEnvGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(processEnvGeneralCall(RunCall.unsafeCast(code, arg)))
        and osSpecificGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(osSpecificGeneralCall(RunCall.unsafeCast(code, arg)))
        and timingGeneral(code: int, arg:'a):'b = RunCall.unsafeCast(timingGeneralCall(RunCall.unsafeCast(code, arg)))
    end

    fun getConst i : SysWord.word = osSpecificGeneral (4, i)

    structure BitFlags =
    (* This structure is used as the basis of all the BIT_FLAGS structures. *)
    struct
        type flags = SysWord.word
        fun toWord f = f
        fun fromWord f = f
        val flags = List.foldl (fn (a, b) => SysWord.orb(a,b)) 0w0
        fun allSet (fl1, fl2) = SysWord.andb(fl1, fl2) = fl1
        fun anySet (fl1, fl2) = SysWord.andb(fl1, fl2) <> 0w0
        fun clear (fl1, fl2) = SysWord.andb(SysWord.notb fl1, fl2)
    end

    structure Error =
    struct
        type syserror = OS.syserror (* Implemented as a SysWord.word value. *)
        val errorMsg = OS.errorMsg
        fun toWord (s: syserror): SysWord.word = RunCall.unsafeCast s
        and fromWord (w: SysWord.word) : syserror = RunCall.unsafeCast w

        val toobig = fromWord(getConst 0)
        and acces = fromWord(getConst 1)
        and again = fromWord(getConst 2)
        and badf = fromWord(getConst 3)
        and badmsg = fromWord(getConst 4)
        and busy = fromWord(getConst 5)
        and canceled (* sic *) = fromWord(getConst 6)
        and child = fromWord(getConst 7)
        and deadlk = fromWord(getConst 8)
        and dom = fromWord(getConst 9)
        and exist = fromWord(getConst 10)
        and fault = fromWord(getConst 11)
        and fbig = fromWord(getConst 12)
        and inprogress = fromWord(getConst 13)
        and intr = fromWord(getConst 14)
        and inval = fromWord(getConst 15)
        and io = fromWord(getConst 16)
        and isdir = fromWord(getConst 17)
        and loop = fromWord(getConst 18)
        and mfile = fromWord(getConst 19)
        and mlink = fromWord(getConst 20)
        and msgsize = fromWord(getConst 21)
        and nametoolong = fromWord(getConst 22)
        and nfile = fromWord(getConst 23)
        and nodev = fromWord(getConst 24)
        and noent = fromWord(getConst 25)
        and noexec = fromWord(getConst 26)
        and nolck = fromWord(getConst 27)
        and nomem = fromWord(getConst 28)
        and nospc = fromWord(getConst 29)
        and nosys = fromWord(getConst 30)
        and notdir = fromWord(getConst 31)
        and notempty = fromWord(getConst 32)
        and notsup = fromWord(getConst 33)
        and notty = fromWord(getConst 34)
        and nxio = fromWord(getConst 35)
        and perm = fromWord(getConst 36)
        and pipe = fromWord(getConst 37)
        and range = fromWord(getConst 38)
        and rofs = fromWord(getConst 39)
        and spipe = fromWord(getConst 40)
        and srch = fromWord(getConst 41)
        and xdev = fromWord(getConst 42)
        
        val errNames =
        [
            (acces, "acces"),
            (again, "again"),
            (badf, "badf"),
            (badmsg, "badmsg"),
            (busy, "busy"),
            (canceled, "canceled"),
            (child, "child"),
            (deadlk, "deadlk"),
            (dom, "dom"),
            (exist, "exist"),
            (fault, "fault"),
            (fbig, "fbig"),
            (inprogress, "inprogress"),
            (intr, "intr"),
            (inval, "inval"),
            (io, "io"),
            (isdir, "isdir"),
            (loop, "loop"),
            (mfile, "mfile"),
            (mlink, "mlink"),
            (msgsize, "msgsize"),
            (nametoolong, "nametoolong"),
            (nfile, "nfile"),
            (nodev, "nodev"),
            (noent, "noent"),
            (noexec, "noexec"),
            (nolck, "nolck"),
            (nomem, "nomem"),
            (nospc, "nospc"),
            (nosys, "nosys"),
            (notdir, "notdir"),
            (notempty, "notempty"),
            (notsup, "notsup"),
            (notty, "notty"),
            (nxio, "nxio"),
            (perm, "perm"),
            (pipe, "pipe"),
            (range, "range"),
            (rofs, "rofs"),
            (spipe, "spipe"),
            (srch, "srch"),
            (toobig, "toobig"),
            (xdev, "xdev")
        ]
        
        (* These are defined to return the names above. *)
        fun errorName n =
            case List.find (fn (e, _) => e = n) errNames of
                SOME(_, s) => s
            |   NONE => OS.errorName n
        
        fun syserror s =
            case List.find (fn (_, t) => s = t) errNames of
                SOME(e, _) => SOME e
            |   NONE => OS.syserror s
    end;

    structure Signal =
    struct
        type signal = int
        val toWord = SysWord.fromInt
        and fromWord = SysWord.toInt
        (* These signal values are probably defined to correspond
           to particular numbers but there's no harm in getting
           them from the RTS. *)
        val abrt = fromWord(getConst 43)
        and alrm = fromWord(getConst 44)
        and bus = fromWord(getConst 45)
        and fpe = fromWord(getConst 46)
        and hup = fromWord(getConst 47)
        and ill = fromWord(getConst 48)
        and int = fromWord(getConst 49)
        and kill = fromWord(getConst 50)
        and pipe = fromWord(getConst 51)
        and quit = fromWord(getConst 52)
        and segv = fromWord(getConst 53)
        and term = fromWord(getConst 54)
        and usr1 = fromWord(getConst 55)
        and usr2 = fromWord(getConst 56)
        and chld = fromWord(getConst 57)
        and cont = fromWord(getConst 58)
        and stop = fromWord(getConst 59)
        and tstp = fromWord(getConst 60)
        and ttin = fromWord(getConst 61)
        and ttou = fromWord(getConst 62)
        end;
    
    structure Process =
    struct
        type signal = Signal.signal
        type pid = int
        val pidToWord = SysWord.fromInt
        and wordToPid = SysWord.toInt
        
        datatype waitpid_arg =
            W_ANY_CHILD | W_CHILD of pid | W_SAME_GROUP | W_GROUP of pid
        datatype exit_status =
            W_EXITED | W_EXITSTATUS of Word8.word
            | W_SIGNALED of signal | W_STOPPED of signal
        datatype killpid_arg = K_PROC of pid | K_SAME_GROUP | K_GROUP of pid

        structure W =
        struct
            open BitFlags
            val untraced = getConst 133
            val nohang = getConst 134 (* Not exported. *)
            val all = flags [ untraced, nohang]
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a, b)) all
        end

        local
            val doCall = osSpecificGeneral
        in
            fun fork () =
                case doCall(5, ()) of
                    0 => NONE (* Parent *)
                |   n => SOME n (* Child *)
        end

        local
            val doCall = osSpecificGeneral
        in
            (* Map the pid argument to positive, zero or
               negative. *)
            fun kill (K_PROC pid, si) = doCall(6,(pid, si))
              | kill (K_SAME_GROUP, si) = doCall(6, (0, si))
              | kill (K_GROUP pid, si) = doCall(6, (~pid, si))
        end

        local
            val doCall = osSpecificGeneral
        in
            (* The format of a result may well be sufficiently fixed
               that we could decode it without calling the RTS.  It's
               probably worth the small cost to make maintenance easier. *)
            fun fromStatus (stat: OS.Process.status): exit_status =
            case (doCall(15, stat)) of
                (1, 0) => W_EXITED
            |   (1, n) => W_EXITSTATUS(Word8.fromInt n)
            |   (2, n) => W_SIGNALED n
            |   (3, n) => W_STOPPED n
            |   _ => raise Fail "Unknown result status"
        end

        local
            val doCall = osSpecificGeneral
            fun doWait(kind: int, pid: pid, flags: W.flags list) =
            let
                val (pid, status) =
                    doCall(14, (kind, pid,
                        SysWord.toInt(W.flags flags)))
            in
                (pid, fromStatus status)
            end
        in
            fun waitpid(W_ANY_CHILD, flags) = doWait(0, 0, flags)
            |   waitpid(W_CHILD pid, flags) = doWait(1, pid, flags)
            |   waitpid(W_SAME_GROUP, flags) = doWait(2, 0, flags)
            |   waitpid(W_GROUP pid, flags) = doWait(3, pid, flags)

            fun wait() = waitpid(W_ANY_CHILD, [])

            fun waitpid_nh(wpa, flags) =
            let
                val (pid, status) = waitpid(wpa, W.nohang :: flags)
            in
                if pid = 0 then NONE else SOME(pid, status)
            end
        end

        fun exec(p, args) =
            osSpecificGeneral(17, (p, args))
        and exece(p, args, env) =
            osSpecificGeneral(18, (p, args, env))
        and execp(p, args) =
            osSpecificGeneral(19, (p, args))

        (* This is supposed to call C "exit" function so we must use PolyFinish here. *)
        local
            val doExit: Word8.word -> unit = RunCall.rtsCallFull1 "PolyFinish"
        in
            fun exit w =
            (
                doExit w;
                raise Bind (* Never executed but gives the correct result type.*)
            )
        end

        local
            val doCall = osSpecificGeneral
            fun toAbsolute t =
                if t < Time.zeroTime
                then raise OS.SysErr("Invalid time", NONE)
                else t + Time.now()
            (* Because of rounding we may get a negative time.  In that
               case we return zero. *)
            fun endTime t =
            let
                val now = Time.now()
            in
                if t > now then t-now else Time.zeroTime
            end
        in
            (* This previously used absolute times.  Now uses relative. *)
            fun alarm t = doCall(20, t)

            fun sleep t =
            let
                val finish = toAbsolute t
            in
                (* We need to pass in the absolute time here.  That's
                   because the process scheduler retries the
                   function until a signal occurs or the time expires. *)
                (* The result is zero if it returns successfully.  If
                   an exception is raised we return the remaining
                   time.  We assume that this only happens because
                   the process is interrupted.  We don't handle the
                   Interrupt exception, though. *)
                (doCall(22, finish); Time.zeroTime) handle OS.SysErr _ => 
                    endTime finish
            end
        end

        local
            val doCall = osSpecificGeneral
        in
            fun pause() = doCall(21, ())
        end
    end;
 
    structure ProcEnv =
    struct
        type pid = Process.pid and file_desc = OS.IO.iodesc
        type uid = int and gid = int
        val uidToWord = SysWord.fromInt
        and wordToUid = SysWord.toInt
        and gidToWord = SysWord.fromInt
        and wordToGid = SysWord.toInt

        local
            val doCall = osSpecificGeneral
        in
            fun getpid () = doCall(7, ())
            and getppid () = doCall(8, ())
            and getuid () = doCall(9, ())
            and geteuid () = doCall(10, ())
            and getgid () = doCall(11, ())
            and getegid () = doCall(12, ())
            and getpgrp () = doCall(13, ())
            and setsid () = doCall(27, ())
        end

        val getenv = OS.Process.getEnv

        fun environ() = processEnvGeneral(21, ())

        local
            val doCall = osSpecificGeneral
        in
            fun setuid(u: uid) = doCall(23, u)
            and setgid(g: gid) = doCall(24, g)
        end

        local
            val doCall = osSpecificGeneral
        in
            fun getgroups() = doCall(25, ())
        end

        local
            val doCall = osSpecificGeneral
        in
            fun getlogin() = doCall(26, ())
            and ctermid() = doCall(30, ())
        end

        local
            val doCall = osSpecificGeneral
        in
            (* In each case NONE as an argument is taken as 0. *)
            fun setpgid{pid, pgid} = doCall(28, (getOpt(pid, 0), getOpt(pgid, 0)))
        end

        local
            val doCall = osSpecificGeneral
        in
            fun uname() = doCall(29, ())
        end

        val time = Time.now

        fun times() =
        let
            (* Apart from the child times all these could be obtained
               by calling the Timer functions. *)
            val doCall: int*unit -> Time.time = timingGeneral
            fun getUserTime() = doCall(7, ())
            and getSysTime() = doCall(8, ())
            and getRealTime() = doCall(10, ())
            and getChildUserTime() = doCall(11, ())
            and getChildSysTime() = doCall(12, ())
        in
            { elapsed=getRealTime(), utime=getUserTime(), stime=getSysTime(),
              cutime=getChildUserTime(), cstime=getChildSysTime()}
        end

        local
            val doCall = osSpecificGeneral
        in
            fun ttyname(f: file_desc) = doCall(31, f)
        end

        local
            val doCall = osSpecificGeneral
        in
            fun isatty(f: file_desc) = doCall(32, f)
        end

        local
            val doCall = osSpecificGeneral
        in
            fun sysconf(s: string) = SysWord.fromInt(doCall(33, s))
        end
    end;

    structure FileSys =
    struct
        type uid = ProcEnv.uid and gid = ProcEnv.gid
        type file_desc = OS.IO.iodesc
        type dirstream = OS.FileSys.dirstream
        datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

        structure O =
        struct
            open BitFlags
            val append = getConst 66
            and excl = getConst 67
            and noctty = getConst 68
            and nonblock = getConst 69
            and sync = getConst 70
            and trunc = getConst 71
            val all = flags [append, excl, noctty, nonblock, sync, trunc]
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a, b)) all
        end

        local
            val doIo: int*file_desc*unit -> int = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun fdToWord (f: file_desc) = SysWord.fromInt(doIo(30, f, ()))
        end

        (* file_desc and OS.IO.iodesc are the same. *)
        fun fdToIOD i = i
        and iodToFD i = SOME i
        
        val opendir = OS.FileSys.openDir
        and readdir = OS.FileSys.readDir
        and rewinddir = OS.FileSys.rewindDir
        and closedir = OS.FileSys.closeDir
        and chdir = OS.FileSys.chDir
        and getcwd = OS.FileSys.getDir
        and unlink = OS.FileSys.remove
        and rmdir = OS.FileSys.rmDir
        and rename = OS.FileSys.rename
        and readlink = OS.FileSys.readLink

        local
            val persistentFD: int -> file_desc = RunCall.rtsCallFull1 "PolyPosixCreatePersistentFD"
        in
            (* Use persistent file descriptors here.  i.e. don't reset them to "invalid" if they are
               read into a new session.  We always want that for 0, 1 and 2 but it's not clear whether
               that is correct for other file descriptors.  Since this is a low-level function
               assume that the caller understands the issues. *)
            val wordToFD = persistentFD o SysWord.toInt
        end

        val stdin  = wordToFD 0w0 (* Must be persistent. *)
        and stdout = wordToFD 0w1
        and stderr = wordToFD 0w2

        structure S =
        struct
            open BitFlags
            type mode = flags
            val irusr : mode = getConst 145
            and iwusr : mode = getConst 146
            and ixusr : mode = getConst 147
            val irwxu : mode = flags[irusr, iwusr, ixusr]
            val irgrp : mode = getConst 148
            and iwgrp : mode = getConst 149
            and ixgrp : mode = getConst 150
            val irwxg : mode = flags[irgrp, iwgrp, ixgrp]
            val iroth : mode = getConst 151
            and iwoth : mode = getConst 152
            and ixoth : mode = getConst 153
            val irwxo : mode = flags[iroth, iwoth, ixoth]
            val isuid : mode = getConst 154
            val isgid : mode = getConst 155
            val all = flags [irwxu, irwxg, irwxo, isuid, isgid]
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a, b)) all
        end

        local
            val o_rdonly = getConst 63
            and o_wronly = getConst 64
            and o_rdwr = getConst 65

            fun toBits O_RDONLY = o_rdonly
             |  toBits O_WRONLY = o_wronly
             |  toBits O_RDWR = o_rdwr

            val doIo = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun openf(name, mode, flags) =
            let
                val bits = SysWord.orb(flags, toBits mode)
            in
                doIo(70, 0, (name, SysWord.toInt bits, 0))
            end
            
            and createf(name, mode, flags, smode) =
            let
                val bits = SysWord.orb(flags, toBits mode)
            in
                doIo(71, 0, (name, SysWord.toInt bits, SysWord.toInt smode))
            end
        end

        fun creat(s, m) = createf(s, O_WRONLY, O.trunc, m)

        local
            val doCall = osSpecificGeneral
        in
            fun umask m = SysWord.fromInt(doCall(50, SysWord.toInt m))
        end

        local
            val doCall = osSpecificGeneral
        in
            fun link{old, new} = doCall(51, (old, new))
            and symlink{old, new} = doCall(54, (old, new))
        end

        local
            val doCall = osSpecificGeneral
        in
            fun mkdir(name, mode) = doCall(52, (name, SysWord.toInt mode))
            and mkfifo(name, mode) = doCall(53, (name, SysWord.toInt mode))
            and chmod(name, mode) = doCall(59, (name, SysWord.toInt mode))
        end

        type dev = LargeInt.int and ino = LargeInt.int
        val wordToDev = SysWord.toLargeInt
        and devToWord = SysWord.fromLargeInt
        and wordToIno = SysWord.toLargeInt
        and inoToWord = SysWord.fromLargeInt

        structure ST =
        struct
            type stat = { mode: S.mode, kind: int, ino: ino, dev: dev,
                      nlink: int, uid: uid, gid: gid, size: Position.int,
                      atime: Time.time, mtime: Time.time, ctime: Time.time }
            (* The "kind" information is encoded by "stat" *)
            fun isDir({ kind, ...} : stat) = kind = 1
            and isChr({ kind, ...} : stat) = kind = 2
            and isBlk({ kind, ...} : stat) = kind = 3
            and isReg({ kind, ...} : stat) = kind = 0
            and isFIFO({ kind, ...} : stat) = kind = 4
            and isLink({ kind, ...} : stat) = kind = 5
            and isSock({ kind, ...} : stat) = kind = 6

            val mode : stat -> S.mode = #mode
            and ino : stat -> ino = #ino
            val dev : stat -> dev = #dev
            val nlink : stat -> int = #nlink
            val uid : stat -> uid = #uid
            val gid : stat -> gid = #gid
            val size : stat -> Position.int = #size
            val atime : stat -> Time.time = #atime
            val mtime : stat -> Time.time = #mtime
            val ctime : stat -> Time.time = #ctime
        end

        local
            val doCall1 = osSpecificGeneral
            val doCall2 = osSpecificGeneral
            fun convStat(mode, kind, ino, dev, nlink, uid, gid, size,
                     atime, mtime, ctime) =
                { mode = SysWord.fromInt mode, kind = kind, ino = ino,
                  dev = dev, nlink = nlink, uid = uid, gid = gid,
                  size = size, atime = atime, mtime = mtime, ctime = ctime }
        in
            fun stat name = convStat(doCall1(55, name))
            and lstat name = convStat(doCall1(56, name))
            and fstat f = convStat(doCall2(57, f))
        end
        

        datatype access_mode = datatype OS.FileSys.access_mode

        local
            val doCall = osSpecificGeneral
            val rOK = getConst 156 and wOK = getConst 157
            and eOK = getConst 158 and fOK = getConst 159
            fun abit A_READ = rOK
             |  abit A_WRITE = wOK
             |  abit A_EXEC = eOK
            val abits = List.foldl (fn (a, b) => SysWord.orb(abit a,b)) 0w0
        in
            (* If the bits are nil it tests for existence of the file. *)
            fun access(name, []) = doCall(58, (name, SysWord.toInt(fOK)))
             |  access(name, al) = doCall(58, (name, SysWord.toInt(abits al)))
            
        end

        local
            val doCall = osSpecificGeneral
        in
            fun fchmod(fd, mode) = doCall(60, (fd, SysWord.toInt mode))
        end
        local
            val doCall = osSpecificGeneral
        in
            fun chown(name, uid, gid) = doCall(61, (name, uid, gid))
        end
        local
            val doCall = osSpecificGeneral
        in
            fun fchown(fd, uid, gid) = doCall(62, (fd, uid, gid))
        end
        local
            val doCall1 = osSpecificGeneral
            and doCall2 = osSpecificGeneral
        in
            fun utime (name, NONE) = doCall1(64, name)
             |  utime (name, SOME{actime, modtime}) =
                doCall2(63, (name, actime, modtime))
        end
        local
            val doCall = osSpecificGeneral
        in
            fun ftruncate(fd, size) = doCall(65, (fd, size))
        end

        local
            val doCall = osSpecificGeneral
        in
            fun pathconf(name, var) =
            let
                val res = doCall(66, (name, var))
            in
                if res < 0 then NONE
                else SOME(SysWord.fromInt res)
            end
        end
        local
            val doCall = osSpecificGeneral
        in
            fun fpathconf(fd, var) =
            let
                val res = doCall(67, (fd, var))
            in
                if res < 0 then NONE
                else SOME(SysWord.fromInt res)
            end
        end
    end;

    structure IO =
    struct
        type file_desc = OS.IO.iodesc and pid = Process.pid
        structure FD =
        struct
            open BitFlags
            val cloexec: flags = getConst 132
            val all = flags [cloexec]
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a, b)) all
        end

        (* Posix.IO.O seems to be a cut-down version of Posix.FileSys.O.
           It seems to me that one structure would suffice. *)
        structure O = FileSys.O

        datatype open_mode = datatype FileSys.open_mode

        local
            val doIo = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun close (strm: file_desc): unit = doIo(7, strm, 0)
        end

        local
            val doIo = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
        in
            fun readVec (strm: file_desc, len: int): Word8Vector.vector =
                doIo(26, strm, len)
        end

        local
            val doCall = osSpecificGeneral
        in
            fun pipe() =
            let
                val (inf, outf) = doCall(110, ())
            in
                { infd=inf, outfd=outf }
            end
        end

        local
            val doCall = osSpecificGeneral
        in
            fun dup fd = doCall(111, fd)
        end

        local
            val doCall = osSpecificGeneral
        in
            fun dup2{old, new} = doCall(112, (old, new))
        end

        local
            val doCall = osSpecificGeneral
        in
            fun dupfd{old, base} = doCall(113, (old, base))
        end

        local
            val doCall = osSpecificGeneral
            val o_rdonly = getConst 63
            and o_wronly = getConst 64
            and o_accmode = getConst 166 (* Access mode mask. *)
        in
            fun getfd fd = SysWord.fromInt(doCall(114, fd))
            and getfl fd =
            let
                val res = SysWord.fromInt(doCall(116, fd))
                (* Separate out the mode bits. *)
                val flgs = SysWord.andb(res, SysWord.notb o_accmode)
                val mode = SysWord.andb(res, o_accmode)
                val omode = if mode = o_rdonly then O_RDONLY
                    else if mode = o_wronly then O_WRONLY
                    else O_RDWR
            in
                (flgs, omode)
            end
        end
        local
            val doCall = osSpecificGeneral
        in
            fun setfd(fd, flags) = doCall(115, (fd, SysWord.toInt flags))
            and setfl(fd, flags) = doCall(117, (fd, SysWord.toInt flags))
        end

        datatype whence = SEEK_SET | SEEK_CUR | SEEK_END

        local
            val seekSet = SysWord.toInt(getConst 160)
            and seekCur = SysWord.toInt(getConst 161)
            and seekEnd = SysWord.toInt(getConst 162)
        in
            (* Convert the datatype to the corresponding int. *)
            fun seekWhence SEEK_SET = seekSet
             |  seekWhence SEEK_CUR = seekCur
             |  seekWhence SEEK_END = seekEnd
            fun whenceSeek s =
                if s = seekSet then SEEK_SET
                else if s = seekCur then SEEK_CUR
                else SEEK_END
        end
        local
            val doCall = osSpecificGeneral
        in
            fun lseek(fd, pos, whence) = doCall(118, (fd, pos, seekWhence whence))
        end

        local
            val doCall = osSpecificGeneral
        in
            fun fsync fd = doCall(119, fd)
        end

        datatype lock_type = F_RDLCK | F_WRLCK | F_UNLCK

        structure FLock =
        struct
            val fRdlck = SysWord.toInt(getConst 163)
            and fWrlck = SysWord.toInt(getConst 164)
            and fUnlck = SysWord.toInt(getConst 165)

            type flock = int (* lock type *) *
                     int (* whence *) *
                     Position.int (* start *) *
                     Position.int (* len *) *
                     pid

            fun flock{ltype, whence, start, len, pid} =
            let
                val lt =
                    case ltype of
                      F_RDLCK => fRdlck
                    | F_WRLCK => fWrlck
                    | F_UNLCK => fUnlck
            in
                (lt, seekWhence whence, start, len, getOpt(pid, ~1))
            end

            fun ltype (lt, _, _, _, _) =
                if lt = fRdlck then F_RDLCK
                else if lt = fWrlck then F_WRLCK
                else F_UNLCK

            fun whence (fl: flock) = whenceSeek(#2 fl)
            val start : flock -> Position.int = #3
            val len : flock -> Position.int = #4
            fun pid (_, _, _, _, pid) = if pid < 0 then NONE else SOME pid
        end

        local
            val doCall = osSpecificGeneral
        in
            fun getlk(fd, (t, w, s, l, p)) = doCall(120, (fd, t, w, s, l, p))
            (* Note: the return type of setlk and setlkw is Flock.lock
               not unit.  I assume they simply return their argument. *)
            and setlk(fd, (t, w, s, l, p)) = doCall(121, (fd, t, w, s, l, p))
            and setlkw(fd, (t, w, s, l, p)) = doCall(122, (fd, t, w, s, l, p))
        end

        val readArr = LibraryIOSupport.readBinArray
        and writeVec = LibraryIOSupport.writeBinVec
        and writeArr = LibraryIOSupport.writeBinArray

        val mkTextReader = LibraryIOSupport.wrapInFileDescr
        and mkTextWriter = LibraryIOSupport.wrapOutFileDescr
        val mkBinReader = LibraryIOSupport.wrapBinInFileDescr
        and mkBinWriter = LibraryIOSupport.wrapBinOutFileDescr
    end;

    structure SysDB =
    struct
        type uid = ProcEnv.uid and gid = ProcEnv.gid
        structure Passwd =
        struct
            type passwd = string * uid * gid * string * string
            val name: passwd->string = #1
            and uid: passwd->uid = #2
            and gid: passwd->gid = #3
            and home: passwd->string = #4
            and shell: passwd->string = #5
        end
        structure Group =
        struct
            type group = string * gid * string list
            val name: group->string = #1
            and gid: group->gid = #2
            and members: group->string list = #3
        end

        local
            val doCall = osSpecificGeneral
        in
            fun getpwnam (s: string): Passwd.passwd = doCall(100, s)
        end
        local
            val doCall = osSpecificGeneral
        in
            fun getpwuid (u: uid): Passwd.passwd = doCall(101, u)
        end
        local
            val doCall = osSpecificGeneral
        in
            fun getgrnam (s: string): Group.group = doCall(102, s)
        end
        local
            val doCall = osSpecificGeneral
        in
            fun getgrgid (g: gid): Group.group = doCall(103, g)
        end
    end;

    structure TTY =
    struct
        type pid = Process.pid and file_desc = OS.IO.iodesc

        structure V =
        struct
            val eof = SysWord.toInt(getConst 72)
            and eol = SysWord.toInt(getConst 73)
            and erase = SysWord.toInt(getConst 74)
            and intr  = SysWord.toInt(getConst 75)
            and kill = SysWord.toInt(getConst 76)
            and min   = SysWord.toInt(getConst 77)
            and quit  = SysWord.toInt(getConst 78)
            and susp  = SysWord.toInt(getConst 79)
            and time  = SysWord.toInt(getConst 80)
            and start = SysWord.toInt(getConst 81)
            and stop  = SysWord.toInt(getConst 82)
            and nccs = SysWord.toInt(getConst 83)

            type cc = string

            fun cc l =
            (* Generate a string using the values given and
               defaulting the rest to NULL. *)
            let
                fun find [] _ = #"\000"
                 |  find ((n, c)::l) i =
                    if i = n then c else find l i
            in
                CharVector.tabulate(nccs, find l)
            end

            (* Question: What order does this take? E.g. What is
               the result of update(cc, [(eof, #"a"), (eof, #"b")]) ?
               Assume that earlier entries take precedence.  That
               also affects the processing of exceptions. *)
            fun update(cc, l) =
            let
                fun find [] i = String.sub(cc, i)
                 |  find ((n, c)::l) i =
                    if i = n then c else find l i
            in
                CharVector.tabulate(nccs, find l)
            end

            val sub = String.sub
        end

        structure I =
        struct
            open BitFlags
            val brkint = getConst 84
            and icrnl = getConst 85
            and ignbrk = getConst 86
            and igncr = getConst 87
            and ignpar = getConst 88
            and inlcr = getConst 89
            and inpck = getConst 90
            and istrip = getConst 91
            and ixoff = getConst 92
            and ixon = getConst 93
            and parmrk = getConst 94
            val all = flags [brkint, icrnl, ignbrk, igncr, ignpar,
                     inlcr, inpck, istrip, ixoff, ixon, parmrk]
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a, b)) all
        end

        structure O =
        struct
            open BitFlags
            val opost = getConst 95
            val all = flags [opost]
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a, b)) all
        end
    
        structure C =
        struct
            open BitFlags
            val clocal = getConst 96
            and cread = getConst 97
            and cs5 = getConst 98
            and cs6 = getConst 99
            and cs7 = getConst 100
            and cs8 = getConst 101
            and csize = getConst 102
            and cstopb = getConst 103
            and hupcl = getConst 104
            and parenb = getConst 105
            and parodd = getConst 106
            val all = flags [clocal, cread, cs5, cs6, cs7, cs8, csize,
                     cstopb, hupcl, parenb, parodd]
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a, b)) all
        end

        structure L =
        struct
            open BitFlags
            val echo = getConst 107
            and echoe = getConst 108
            and echok = getConst 109
            and echonl = getConst 110
            and icanon = getConst 111
            and iexten = getConst 112
            and isig = getConst 113
            and noflsh = getConst 114
            and tostop = getConst 115
            val all = flags [echo, echoe, echok, echonl, icanon,
                     iexten, isig, noflsh, tostop]
            val intersect = List.foldl (fn (a, b) => SysWord.andb(a, b)) all
        end

        type speed = int
        (* compareSpeed is supposed to compare by the baud rate, not
           by the encoding.  Provided the encoding maintains the
           ordering then that's fine.  Maybe we should have an RTS call. *)
        val compareSpeed : speed * speed -> order = Int.compare
        and speedToWord : speed -> SysWord.word = SysWord.fromInt
        and wordToSpeed : SysWord.word -> speed = SysWord.toInt
        val b0     : speed = SysWord.toInt(getConst 116)
        and b50    : speed = SysWord.toInt(getConst 117)
        and b75    : speed = SysWord.toInt(getConst 118)
        and b110   : speed = SysWord.toInt(getConst 119)
        and b134   : speed = SysWord.toInt(getConst 120)
        and b150   : speed = SysWord.toInt(getConst 121)
        and b200   : speed = SysWord.toInt(getConst 122)
        and b300   : speed = SysWord.toInt(getConst 123)
        and b600   : speed = SysWord.toInt(getConst 124)
        and b1200  : speed = SysWord.toInt(getConst 125)
        and b1800  : speed = SysWord.toInt(getConst 126)
        and b2400  : speed = SysWord.toInt(getConst 127)
        and b4800  : speed = SysWord.toInt(getConst 128)
        and b9600  : speed = SysWord.toInt(getConst 129)
        and b19200 : speed = SysWord.toInt(getConst 130)
        and b38400 : speed = SysWord.toInt(getConst 131)

        type termios = {
            iflag : I.flags,
            oflag : O.flags,
            cflag : C.flags,
            lflag : L.flags,
            cc : V.cc,
            ispeed : speed,
            ospeed : speed
            }
        fun termios t = t
        and fieldsOf t = t
        val getiflag : termios -> I.flags = #iflag
        and getoflag : termios -> O.flags = #oflag
        and getcflag : termios -> C.flags = #cflag
        and getlflag : termios -> L.flags = #lflag
        and getcc : termios -> V.cc = #cc

        structure CF =
        struct
            val getospeed : termios -> speed = #ospeed
            and getispeed : termios -> speed = #ispeed
            fun setospeed ({ iflag, oflag, cflag, lflag, cc, ispeed, ... }, speed) =
                { iflag=iflag, oflag=oflag, cflag=cflag, lflag=lflag,
                  cc=cc, ispeed = ispeed, ospeed = speed }
            fun setispeed ({ iflag, oflag, cflag, lflag, cc, ospeed, ... }, speed) =
                { iflag=iflag, oflag=oflag, cflag=cflag, lflag=lflag,
                  cc=cc, ispeed = speed, ospeed = ospeed }
        end

        structure TC =
        struct
            type set_action = int
            val sanow : set_action = SysWord.toInt(getConst 135)
            val sadrain : set_action = SysWord.toInt(getConst 136)
            val saflush : set_action = SysWord.toInt(getConst 137)

            type flow_action = int
            val ooff : flow_action = SysWord.toInt(getConst 138)
            val oon : flow_action = SysWord.toInt(getConst 139)
            val ioff : flow_action = SysWord.toInt(getConst 140)
            val ion : flow_action = SysWord.toInt(getConst 141)

            type queue_sel = int
            val iflush : queue_sel = SysWord.toInt(getConst 142)
            val oflush : queue_sel = SysWord.toInt(getConst 143)
            val ioflush : queue_sel = SysWord.toInt(getConst 144)

            local
                val doCall = osSpecificGeneral
            in
                fun getattr f =
                let
                    val (iflag, oflag, cflag, lflag, cc, ispeed, ospeed)
                         = doCall(150, f)
                in
                    {
                      iflag=SysWord.fromInt iflag,
                      oflag=SysWord.fromInt oflag,
                      cflag=SysWord.fromInt cflag,
                      lflag=SysWord.fromInt lflag,
                      cc=cc,
                      ispeed = ispeed,
                      ospeed = ospeed }
                end
            end

            local
                val doCall = osSpecificGeneral
            in
                fun setattr (f, sa,
                    {iflag, oflag, cflag, lflag, cc, ispeed, ospeed}) =
                    doCall(151, (f, sa, SysWord.toInt iflag,
                             SysWord.toInt oflag, SysWord.toInt cflag,
                             SysWord.toInt lflag, cc, ispeed, ospeed))
            end

            local
                val doCall = osSpecificGeneral
            in
                fun sendbreak (f, d) = doCall(152, (f, d))
            end
            local
                val doCall = osSpecificGeneral
            in
                fun drain f = doCall(153, f)
            end
            local
                val doCall = osSpecificGeneral
            in
                fun flush (f, qs) = doCall(154, (f, qs))
            end
            local
                val doCall = osSpecificGeneral
            in
                fun flow (f, fa) = doCall(155, (f, fa))
            end
        end

        local
            val doCall = osSpecificGeneral
        in
            fun getpgrp (f: file_desc): pid = doCall(156, f)
        end
        local
            val doCall = osSpecificGeneral
        in
            fun setpgrp (f: file_desc, p: pid): unit = doCall(157, (f,p))
        end
    end
end;

local
    (* Install the pretty printers for pid, uid, gid.  Don't install one for signal
       because it's now the same as int. *)
    fun ppid _ _ x = PolyML.PrettyString(Int.toString(SysWord.toInt(Posix.Process.pidToWord x)))
    and puid _ _ x = PolyML.PrettyString(Int.toString(SysWord.toInt(Posix.ProcEnv.uidToWord x)))
    and pgid _ _ x = PolyML.PrettyString(Int.toString(SysWord.toInt(Posix.ProcEnv.gidToWord x)))
in
    val () = PolyML.addPrettyPrinter ppid
    val () = PolyML.addPrettyPrinter puid
    val () = PolyML.addPrettyPrinter pgid
end;
