(*
    Title:      Standard Basis Library: Unix structure and signature.
    Author:     David Matthews
    Copyright   David Matthews 2000,2008, 2019, 2020

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

signature UNIX =
sig
    type ('a,'b) proc
    type signal
    datatype exit_status
       = W_EXITED
       | W_EXITSTATUS of Word8.word
       | W_SIGNALED (* sic *) of signal
       | W_STOPPED of signal
    val fromStatus : OS.Process.status -> exit_status
    val executeInEnv : string * string list * string list -> ('a, 'b) proc
    val execute : string * string list -> ('a, 'b) proc
    val textInstreamOf : (TextIO.instream, 'a) proc  -> TextIO.instream
    val binInstreamOf  : (BinIO.instream, 'a) proc -> BinIO.instream
    val textOutstreamOf : ('a, TextIO.outstream) proc -> TextIO.outstream
    val binOutstreamOf  : ('a, BinIO.outstream) proc -> BinIO.outstream
    val streamsOf : (TextIO.instream, TextIO.outstream) proc
                       -> TextIO.instream * TextIO.outstream
    val reap : ('a, 'b) proc -> OS.Process.status
    val kill : ('a, 'b) proc * signal -> unit
    val exit : Word8.word -> 'a
end;

structure Unix :>
    sig
        (* We have to copy the signature since we can't establish the
           connection between exit_status and Posix.Process.exit_status
           with a "where type". *)
        type ('a,'b) proc
        type signal = Posix.Signal.signal
        datatype exit_status = datatype Posix.Process.exit_status
        val fromStatus : OS.Process.status -> exit_status
        val executeInEnv : string * string list * string list -> ('a, 'b) proc
        val execute : string * string list -> ('a, 'b) proc
        val textInstreamOf : (TextIO.instream, 'a) proc  -> TextIO.instream
        val binInstreamOf  : (BinIO.instream, 'a) proc -> BinIO.instream
        val textOutstreamOf : ('a, TextIO.outstream) proc -> TextIO.outstream
        val binOutstreamOf  : ('a, BinIO.outstream) proc -> BinIO.outstream
        val streamsOf : (TextIO.instream, TextIO.outstream) proc
                           -> TextIO.instream * TextIO.outstream
        val reap : ('a, 'b) proc -> OS.Process.status
        val kill : ('a, 'b) proc * signal -> unit
        val exit : Word8.word -> 'a
    end = 
struct
    type ('a,'b) proc =
     { pid: Posix.Process.pid,
       infd: Posix.IO.file_desc,
       outfd: Posix.IO.file_desc,
       (* We have to remember the result status. *)
       result: OS.Process.status option ref
     }
    type signal = Posix.Signal.signal
    datatype exit_status = datatype Posix.Process.exit_status

    val fromStatus = Posix.Process.fromStatus

    fun kill({pid, ... }: ('a, 'b) proc, signal) =
        Posix.Process.kill(Posix.Process.K_PROC pid, signal)

    (* Create a new process running a command and with pipes connecting the
       standard input and output.
       The command is supposed to be an executable and we should raise an
       exception if it is not.  Since the exece is only done in the child we
       need to test whether we have an executable at the beginning.
       The definition does not say whether the first of the user-supplied
       arguments includes the command or not.  Assume that only the "real"
       arguments are provided and pass the last component of the command
       name in the exece call. *)
(*
    fun executeInEnv (cmd, args, env) =
    let
        open Posix
        (* Test first for presence of the file and then that we
           have correct access rights. *)
        val s = FileSys.stat cmd (* Raises SysErr if the file doesn't exist. *)
        val () =
           if not (FileSys.ST.isReg s) orelse not (FileSys.access(cmd, [FileSys.A_EXEC]))
           then raise OS.SysErr(OS.errorMsg Error.acces, SOME Error.acces)
           else ()
        val toChild = IO.pipe()
        and fromChild = IO.pipe()
    in
        case Process.fork() of
            NONE => (* In the child *)
            ((
            (* Should really clean up the signals here and
               turn off timers. *)
            (* Close the unwanted ends of the pipes and
               set the required ends up as stdin and stdout. *)
            IO.close(#outfd toChild);
            IO.close(#infd fromChild);
            IO.dup2{old= #infd toChild, new=Posix.FileSys.stdin};
            IO.dup2{old= #outfd fromChild, new=Posix.FileSys.stdout};
            IO.close(#infd toChild);
            IO.close(#outfd fromChild);
            (* Run the command. *)
            Process.exece(cmd, OS.Path.file cmd :: args, env);
            (* If we get here the exec must have failed -
               terminate this process.  We're supposed to
               set the error code to 126 in this case. *)
            OS.Process.terminate(RunCall.unsafeCast 0w126)
            ) handle _ => OS.Process.terminate(RunCall.unsafeCast 0w126)
            )
        |   SOME pid => (* In the parent *)
            (
            IO.close(#infd toChild);
            IO.close(#outfd fromChild);
            {pid=pid, infd= #infd fromChild, outfd= #outfd toChild, result = ref NONE}
            )
    end
*)

    (* The ML code proved problematic so it has been replaced with C code.
       It's not clear what the problem was but it could be with garbage collection in
       the child before the exec. *)
    local
        val unixExec = RunCall.rtsCallFull3 "PolyUnixExecute"
    in
        fun executeInEnv(cmd, args, env) =
        let
            open Posix
            (* Test first for presence of the file and then that we
               have correct access rights.  This allows us to report the
               problem in the call rather than having exec fail. *)
            val s = FileSys.stat cmd (* Raises SysErr if the file doesn't exist. *)
            val () =
                if not (FileSys.ST.isReg s) orelse not (FileSys.access(cmd, [FileSys.A_EXEC]))
                then raise OS.SysErr(OS.errorMsg Error.acces, SOME Error.acces)
                else ()

            val (pid, toChild, fromChild) = unixExec(cmd, OS.Path.file cmd :: args, env)
        in
            {pid=pid, infd=fromChild, outfd=toChild, result = ref NONE}
        end
    end

    fun execute (cmd, args) =
        executeInEnv(cmd, args, Posix.ProcEnv.environ())

    local (* Internal function to get the preferred buffer size. *)
        val doIo = RunCall.rtsCallFull3 "PolyBasicIOGeneral"
    in
        fun sys_get_buffsize (strm: OS.IO.iodesc): int = doIo(15, strm, 0)
    end

    fun textInstreamOf {infd, ...} =
    let
        val n = Posix.FileSys.fdToIOD infd
        val textPrimRd =
            LibraryIOSupport.wrapInFileDescr
                {fd=n, name="TextPipeInput", initBlkMode=true}
        val streamIo = TextIO.StreamIO.mkInstream(textPrimRd, "")
    in
        TextIO.mkInstream streamIo
    end
        
    fun textOutstreamOf {outfd, ...} =
    let
        val n = Posix.FileSys.fdToIOD outfd
        val buffSize = sys_get_buffsize n
        val textPrimWr =
            LibraryIOSupport.wrapOutFileDescr{fd=n, name="TextPipeOutput",
                appendMode=false, initBlkMode=true, chunkSize=buffSize}
        (* Construct a stream. *)
        val streamIo = TextIO.StreamIO.mkOutstream(textPrimWr, IO.LINE_BUF)
    in
        TextIO.mkOutstream streamIo
    end

    fun binInstreamOf {infd, ...} =
    let
        val n = Posix.FileSys.fdToIOD infd
        val binPrimRd =
            LibraryIOSupport.wrapBinInFileDescr{fd=n, name="BinPipeInput", initBlkMode=true}
        val streamIo =
            BinIO.StreamIO.mkInstream(binPrimRd, Word8Vector.fromList [])
    in
        BinIO.mkInstream streamIo
    end
        
    fun binOutstreamOf {outfd, ...} =
    let
        val n = Posix.FileSys.fdToIOD outfd
        val buffSize = sys_get_buffsize n
        val binPrimWr =
            LibraryIOSupport.wrapBinOutFileDescr{fd=n, name="BinPipeOutput",
                appendMode=false, chunkSize=buffSize, initBlkMode=true}
        (* Construct a stream. *)
        val streamIo = BinIO.StreamIO.mkOutstream(binPrimWr, IO.LINE_BUF)
    in
        BinIO.mkOutstream streamIo
    end

    fun streamsOf p = (textInstreamOf p, textOutstreamOf p)

    (* Internal function - inverse of Posix.Process.fromStatus. *)
    local
        val doCall = RunCall.rtsCallFull2 "PolyOSSpecificGeneral"
    in
        fun toStatus W_EXITED: OS.Process.status = doCall(16, (1, 0))
         |  toStatus(W_EXITSTATUS w) = doCall(16, (1, Word8.toInt w))
         |  toStatus(W_SIGNALED s) =
            doCall(16, (2, SysWord.toInt(Posix.Signal.toWord s)))
         |  toStatus(W_STOPPED s) = 
            doCall(16, (3, SysWord.toInt(Posix.Signal.toWord s)))
    end

    fun reap {result = ref(SOME r), ...} = r
    |   reap(p as {pid, infd, outfd, result}) =
    let
        val () = Posix.IO.close infd;
        val () = Posix.IO.close outfd;
        val (_, status) =
            Posix.Process.waitpid(Posix.Process.W_CHILD pid, [])
    in
        (* If the process is only stopped we need to wait again. *)
        case status of
            W_STOPPED _ => reap p
        |   _ => let val s = toStatus status in result := SOME s; s end
    end

    fun exit w = OS.Process.exit(toStatus (W_EXITSTATUS w))
end;
