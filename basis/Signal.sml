(*
    Title:      Signal structure and signature.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2008, 2019-20

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

(**
Although the `Posix` structure in the Standard Basis Library provides functions
which send signals to a process there is no standard method of handling signals. The
`Signal` structure has been added to Poly/ML to allow signals to be blocked or
handled.
**)
signature SIGNAL =
sig
    datatype sig_handle = SIG_DFL | SIG_IGN | SIG_HANDLE of int->unit
    val signal: int * sig_handle -> sig_handle
end;

structure Signal: SIGNAL =
struct
    datatype sig_handle = SIG_DFL | SIG_IGN | SIG_HANDLE of int->unit
    local
        val setHandler = RunCall.rtsCallFull2 "PolySetSignalHandler"
    in
        fun signal(s, cmd) =
        let
            val c =
                case cmd of
                    SIG_DFL => 0
                |   SIG_IGN => 1
                |   SIG_HANDLE f => RunCall.unsafeCast f
        in
            case setHandler(s, c) of
                0 => SIG_DFL
            |   1 => SIG_IGN
            |   f => SIG_HANDLE(RunCall.unsafeCast f)
        end
    end
    
    local
        datatype sigHandle = SigHandle of (int->unit) * int | WeakMarker
        val waitForSig = RunCall.rtsCallFull0 "PolyWaitForSignal"
        open Thread

        fun sigThread(): unit =
        let
            (* This call to the RTS returns either a pair of a signal
               and a handler or a flag indicating that some wek reference
               has been set to NONE.  These aren't logically related but
               it's convenient to use a single thread for both. *)
            val nextSig: sigHandle = waitForSig()

            (* When we get a WeakMarker message we need to broadcast
               on this condition variable. *)
            fun broadCastWeak haveLock () =
            (
                if haveLock then () else Mutex.lock Weak.weakLock;
                ConditionVar.broadcast Weak.weakSignal;
                Mutex.unlock Weak.weakLock
            )
                
        in
            case nextSig of
                SigHandle (handler, signal) => (handler signal handle _ => ())
            |   WeakMarker =>
                    (* If the lock is free we can do the broadcast now but
                       to avoid waiting and being unable to handle any signals
                       we fork off a thread if we can't. *)
                    if Mutex.trylock Weak.weakLock
                    then broadCastWeak true ()
                    else (Thread.fork(broadCastWeak false, []); ());
            sigThread() (* Forever. *)
        end
        
        fun forkThread() =
            (Thread.fork(sigThread, []); ()) handle Thread _ => print "Unable to create signal thread\n"

    in
        (* Run this thread now and also start one each time we start up. *)
        val _ = forkThread()
        val _ = LibrarySupport.addOnEntry forkThread
    end
end;
(**
The `Signal.signal` function takes as its arguments a signal number and an
action and returns the previous action. The action may be `SIG_DFL`,
indicating the default action, `SIG_IGN`, indicating that the signal should be
ignored (blocked) or `SIG_HANDLE`, which allows a handler function to be installed.

Signals are represented as integers using the normal Unix signal numbering. In
the Unix implementations of Poly/ML the type `Posix.Signal.signal` is the same as `int`
so the constants from `Posix.Signal` can be used as arguments to `Signal.signal`.

The default action depends on the signal. For some signals it is to ignore the
signal, for others the process is killed. See the signal man page in Unix for a list
of the default actions.

A handler function installed using `SIG_HANDLE` is run as a separate thread
some time after a signal arrives. 

Some signals are used internally by Poly/ML.  In particular `SIGVTALRM` is used
by the profiling mechanism.

The Signal structure is provided in the Windows implementation but only the 
console interrupt signal (2) has effect.

**)
