(*
    Title:      Thread package for ML.
    Author:     David C. J. Matthews
    Copyright (c) 2007-2014, 2018-21

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

(* This signature and structure are not part of the standard basis library
   but are included here because they depend on the Time structure and are
   in turn dependencies of the BasicIO structure. *)

(*!Earlier versions of Poly/ML have provided a form of concurrent execution through 
  the Process structure. Version 5.1 introduces 
  new thread primitives in the Thread structure. This structure is modelled on 
  the Posix thread (pthread) package but simplified and modified for ML. The aim 
  is to provide an efficient implementation of parallelism particularly to enable 
  ML programs to make use of multi-core processors while minimising the changes 
  needed to existing code. The Process structure will continue to be available 
  as a library written on top of these primitives but new programs should use 
  the Thread structure directly.
  
The thread package differs from pthreads in a number of ways. 
There is no join function to wait for the completion of a thread. 
This can be written using mutexes and condition variables. 
Cancellation and signal handling are combined into the interrupt
functions. (The Poly/ML Signal structure handles signals for all the
threads together).  The effect of explicit cancellation is achieved
using the interrupt function.  This causes an interrupt to be
generated in a specific thread.  Alternatively an interrupt can be
broadcast to all threads.  This is most likely to be used
interactively to kill threads that appear to have gone out of
control.  The normal top-level handler for a console interrupt will
generate this.  Threads can choose how or whether they respond to
these interrupts.  A thread that is doing processor-intensive work
probably needs to be able to be interrupted asynchronously whereas if
it is communicating with other threads the presence of asynchronous
interrupts makes correct programming difficult.
*)

signature THREAD =
sig
    (*!The Thread exception can be raised by various of the functions in the
       structure if they detect an error.*)
    exception Thread of string (* Raised if an operation fails. *)
    
    structure Thread:
    sig
        (*!The type of a thread identifier.*)
        eqtype thread
        
        (* Thread attributes - This may be extended. *)
        (*!The type of a thread attribute. Thread attributes are
            properties of the thread that are set initially when the thread is
            created but can subsequently be modified by the thread itself.  The
            thread attribute type may be extended in the future to include things
            like scheduling priority. The current thread attributes control the
            way interrupt exceptions are delivered to the thread.
            
            `EnableBroadcastInterrupt` controls whether the thread will receive an interrupt sent using
            `broadcastInterrupt` or as a result of pressing the console interrupt
            key. If this is false the thread will not receive them.  The default
            for a new thread if this is not specified is false.
            
            `InterruptState` controls when and whether interrupts are delivered to the 
            thread. This includes broadcast interrupts and also interrupts directed at 
            a specific thread with the interrupt call.
            `InterruptDefer` means the thread 
            will not receive any interrupts. However, if the thread has previously been 
            interrupted the interrupt may be delivered when the thread calls setAttributes 
            to change its interrupt state. `InterruptSynch`
            means interrupts are delivered 
            synchronously. An interrupt will be delayed until an interruption point. An 
            interruption point is one of: `testInterrupt`,
            `ConditionVar.wait`, `ConditionVar.waitUntil`
            and various library calls that may block, such as IO calls, pause etc. N.B. 
            `Mutex.lock` is not an interruption point even though it can result in a thread 
            blocking for an indefinite period. `InterruptAsynch` means interrupts are delivered 
            asynchronously i.e. at a suitable point soon after they are triggered.
            `InterruptAsynchOnce`
            means that only a single interrupt is delivered asynchronously after which 
            the interrupt state is changed to `InterruptSynch`. It allows a thread to tidy 
            up and if necessary indicate that it has been interrupted without the risk 
            of a second asynchronous interrupt occurring in the handler for the first 
            interrupt. If this attribute is not specified when a thread is created the 
            default is `InterruptSynch`.
            
            `MaximumMLStack` was added in version 5.5.3. It controls the maximum size the 
            ML stack may grow to. It is an option type where NONE allows the stack to 
            grow to the limit of the available memory whereas SOME n limits the stack 
            to n words. This is approximate since there is some rounding involved. When 
            the limit is reached the thread is sent an Interrupt exception.*)
        datatype threadAttribute =
            (* Does this thread accept a broadcast interrupt?  The default is not to
               accept broadcast interrupts. *)
            EnableBroadcastInterrupt of bool
            (* How to handle interrupts.  The default is to handle interrupts synchronously.  *)
        |   InterruptState of interruptState
            (* Maximum size of the ML stack in words. NONE means unlimited *)
        |   MaximumMLStack of int option
        
        and interruptState =
            InterruptDefer (* Defer any interrupts. *)
        |   InterruptSynch  (* Interrupts are delivered synchronously. An interrupt
               will be delayed until an interruption point.  An interruption point
               is one of: testInterrupt, ConditionVar.wait, ConditionVar.waitUntil
               and various library calls that may block, such as IO calls, pause etc.
               N.B.  Mutex.lock is not an interruption point even though it can result
               in a thread blocking for an indefinite period.  *)
        |   InterruptAsynch (* Interrupts are delivered asynchronously i.e. at a suitable
               point soon after they are triggered. *)
        |   InterruptAsynchOnce (* As InterruptAsynch except that only a single interrupt
               is delivered asynchronously after which the interrupt state is changed to
               InterruptSynch.  It allows a thread to tidy up and if necessary indicate
               that it has been interrupted without the risk of a second asynchronous
               interrupt occurring in the handler for the first interrupt. *)
        
        (*!Fork a thread. Starts a new thread running 
          the function argument. The attribute list gives initial values for thread attributes 
          which can be modified by the thread itself. Any unspecified attributes take 
          default values. The thread is terminated when the thread function returns, if 
          it raises an uncaught exception or if it calls `exit`;*)
        val fork: (unit->unit) * threadAttribute list -> thread

        (*!Terminate this thread. *)
        val exit: unit -> unit
        (*!Test if a thread is still running or has terminated.  This function should be
          used with care.  The thread may be on the point of terminating and still appear
          to be active.*)
        val isActive: thread -> bool
        
        (*!Test whether thread ids are the same.  This is provided for backwards compatibility
          since `thread` is an eqtype. *)
        val equal: thread * thread -> bool
        (*!Return the thread identifier for the current thread. *)
        val self: unit -> thread
        
        exception Interrupt (* = SML90.Interrupt *)
        (*!Send an Interrupt exception to a specific thread.  When and indeed whether
           the exception is actually delivered will depend on the interrupt state
           of the target thread.  Raises Thread if the thread is no longer running,
           so an exception handler should be used unless the thread is known to
           be blocked. *)
        val interrupt: thread -> unit
        (*!Send an interrupt exception to every thread which is set to accept it. *)
        val broadcastInterrupt: unit -> unit
        (*!If this thread is handling interrupts synchronously, test to see 
           if it has been interrupted.  If so it raises the
           `Interrupt` exception. *)
        val testInterrupt: unit -> unit
        (*!Terminate a thread. This should be used as a last resort.  Normally
           a thread should be allowed to clean up and terminate by using the
           interrupt call.  Raises Thread if the thread is no longer running,
           so an exception handler should be used unless the thread is known to
           be blocked. *)
        val kill: thread -> unit
        
        (*!Get and set thread-local store for the calling thread. The store is a
           tagged associative memory which is initially empty for a new thread.
           A thread can call setLocal to add or replace items in its store and
           call getLocal to return values if they exist.  The Universal structure
           contains functions to make new tags as well as injection, projection and
           test functions. *)
        val getLocal: 'a Universal.tag -> 'a option
        and setLocal: 'a Universal.tag * 'a -> unit
        
        (*!Change the specified attribute(s) for the calling thread.  Unspecified
           attributes remain unchanged. *)
        val setAttributes: threadAttribute list -> unit
        (*!Get the values of attributes. *)
        val getAttributes: unit -> threadAttribute list

        (*!Return the number of processors that will be used to run threads
           and the number of physical processors if that is available. *)
        val numProcessors: unit -> int
        and numPhysicalProcessors: unit -> int option
    end
        
    structure Mutex:
    sig
        (*!A mutex provides simple mutual exclusion.  A thread can lock
           a mutex and until it unlocks it no other thread will be able to lock it.
           Locking and unlocking are intended to be fast in the situation when
           there is no other process attempting to lock the mutex.
           These functions may not work correctly if an asynchronous interrupt
           is delivered during the calls.  A thread should use synchronous interrupt
           when using these calls. *)
        type mutex
        (*!Make a new mutex *)
        val mutex: unit -> mutex
        (*!Lock a mutex.  If the mutex is currently locked the thread is
           blocked until it is unlocked.  If a thread tries to lock a mutex that
           it has previously locked the thread will deadlock.
           N.B.  `thread` is not an interruption point
           (a point where synchronous
           interrupts are delivered) even though a thread can be blocked indefinitely. *)
        val lock: mutex -> unit
        (*!Unlock a mutex and allow any waiting threads to run.  The behaviour
           if the mutex was not previously locked by the calling thread is undefined.  *)
        val unlock: mutex -> unit
        (*!Attempt to lock the mutex.  Returns true if the mutex was not
           previously locked and has now been locked by the calling thread.  Returns
           false if the mutex was previously locked, including by the calling thread. *)
        val trylock: mutex -> bool
    end
    
    structure ConditionVar:
    sig
        (*!Condition variables are used to provide communication
           between threads.  A condition variable is used in conjunction with a mutex
           and usually a reference to establish and test changes in state.  The normal
           use is for one thread to lock a mutex, test the reference and then wait on
           the condition variable, releasing the lock on the mutex while it does so.
           Another thread may then lock the mutex, update the reference, unlock the
           mutex, and signal the condition variable.  This wakes up the first thread
           and reacquires the lock allowing the thread to test the updated reference
           with the lock held.
           More complex communication mechanisms, such as blocking channels, can
           be written in terms of condition variables. *)
        type conditionVar
        (*!Make a new condition variable. *)
        val conditionVar: unit -> conditionVar
        (*!Release the mutex and block until the condition variable is signalled. When 
            wait returns the mutex will have been re-acquired.
            
            If the thread is handling interrupts synchronously this function can be interrupted 
            using the `Thread.interrupt` function or, if the thread is set to 
            accept broadcast interrupts, `Thread.broadcastInterrupt`. The thread 
            will re-acquire the mutex before the exception is delivered. An exception 
            will only be delivered in this case if the interrupt is sent before the condition 
            variable is signalled. If the interrupt is sent after the condition variable 
            is signalled the function will return normally even if it has not yet re-acquired 
            the mutex. The interrupt state will be delivered on the next call to &quot;wait&quot;, 
            `Thread.testInterrupt` or other blocking call.
            
            A thread should never call this function if it may receive an asynchronous 
            interrupt. It should always set its interrupt state to either
            `InterruptSynch` 
            or `InterruptDefer` beforehand.
            An asynchronous interrupt may leave the condition 
            variable and the mutex in an indeterminate state and could lead to deadlock.
            
            A condition variable should only be associated with one mutex at a time. 
            All the threads waiting on a condition variable should pass the same mutex 
            as argument.*)
        val wait: conditionVar * Mutex.mutex -> unit
        (*!As wait except that it blocks until either the condition
           variable is signalled or the time (absolute) is reached.  Either way
           the mutex is reacquired so there may be a further delay if it is held
           by another thread.  *)
        val waitUntil: conditionVar * Mutex.mutex * Time.time -> bool
        (*!Wake up one thread if any are waiting on the condition variable. 
          If there are several threads waiting for the condition variable one will be 
          selected to run and will run as soon as it has re-acquired the lock.*)
        val signal: conditionVar -> unit
        (*!Wake up all threads waiting on the condition variable. *)
        val broadcast: conditionVar -> unit
    end

end;

structure Thread :> THREAD =
struct
    exception Thread = RunCall.Thread

    structure Thread =
    struct
        open Thread (* Created in INITIALISE with thread type and self function. *)

        (* Equality is pointer equality. *)
        val equal : thread*thread->bool = op =

        datatype threadAttribute =
            EnableBroadcastInterrupt of bool
        |   InterruptState of interruptState
        |   MaximumMLStack of int option
        
        and interruptState =
            InterruptDefer
        |   InterruptSynch
        |   InterruptAsynch
        |   InterruptAsynchOnce 

        (* Convert attributes to bits and a mask. *)
        fun attrsToWord (at: threadAttribute list): Word.word * Word.word =
        let
            (* Check that a particular attribute appears only once.
               As well as accumulating the actual bits in the result we
               also accumulate the mask of bits.  If any of these
               reappear we raise an exception. *)
            fun checkRepeat(r, acc, set, mask) =
                if Word.andb(set, mask) <> 0w0
                then raise Thread "The same attribute appears more than once in the list"
                else convert(r, acc,  Word.orb(set, mask))

            and convert([], acc, set) = (acc, set)
              | convert(EnableBroadcastInterrupt true :: r, acc, set) =
                    checkRepeat(r, Word.orb(acc, 0w1), set, 0w1)
              | convert(EnableBroadcastInterrupt false :: r, acc, set) =
                    checkRepeat(r, acc (* No bit *), set, 0w1)
              | convert(InterruptState s :: r, acc, set) =
                    checkRepeat(r, Word.orb(setIstateBits s, acc), set, 0w6)
              | convert(MaximumMLStack _ :: r, acc, set) =
                    convert(r, acc, set)
        in
            convert(at, 0w0, 0w0)
        end
        
        and setIstateBits InterruptDefer = 0w0
          | setIstateBits InterruptSynch = 0w2
          | setIstateBits InterruptAsynch = 0w4
          | setIstateBits InterruptAsynchOnce = 0w6

        fun getIstateBits(w: Word.word): interruptState =
        let
            val ibits = Word.andb(w, 0w6)
        in
            if ibits = 0w0
            then InterruptDefer
            else if ibits = 0w2
            then InterruptSynch
            else if ibits = 0w4
            then InterruptAsynch
            else InterruptAsynchOnce
        end

        fun wordToAttrs w =
        let
            (* Enable broadcast - true if bottom bit is set. *)
            val bcast = EnableBroadcastInterrupt(Word.andb(w, 0w1) = 0w1)
        in
            [bcast, InterruptState(getIstateBits w)]
        end
        
        exception Interrupt = RunCall.Interrupt

        (* The thread id is opaque outside this structure but is actually a six
           word mutable object.
           Word 0: Index into thread table (used inside the RTS only)
           Word 1: Flags: initialised by the RTS and set by this code
           Word 2: Thread local store: read and set by this code.
           Word 3: IntRequest: Set by the RTS if there is an interrupt pending
           Word 4: Maximum ML stack size.  Unlimited is stored here as zero
           *)
        val threadIdFlags       = 0w1
        and threadIdThreadLocal = 0w2
        and threadIdIntRequest  = 0w3
        and threadIdStackSize   = 0w4

        fun getLocal (t: 'a Universal.tag) : 'a option =
        let
            val root: Universal.universal ref list =
                RunCall.loadWord(self(), threadIdThreadLocal)

            fun doFind [] = NONE
              | doFind ((ref v)::r) =
                    if Universal.tagIs t v
                    then SOME(Universal.tagProject t v)
                    else doFind r
        in
            doFind root
        end
        
        fun setLocal (t: 'a Universal.tag, newVal: 'a) : unit =
        let
            (* See if we already have this in the list. *)
            val root: Universal.universal ref list =
                RunCall.loadWord(self(), threadIdThreadLocal)

            fun doFind [] =
                    (* Not in the list - Add it. *)
                    RunCall.storeWord
                        (self(), threadIdThreadLocal,
                         ref (Universal.tagInject t newVal) :: root)
              | doFind (v::r) =
                    if Universal.tagIs t (!v)
                        (* If it's in the list update it. *)
                    then v := Universal.tagInject t newVal
                    else doFind r

        in
            doFind root
        end
        
        local
            val threadTestInterrupt: unit -> unit = RunCall.rtsCallFull0 "PolyThreadTestInterrupt"
        in
            fun testInterrupt() =
                (* If there is a pending request the word in the thread object
                   will be non-zero. *)
                if RunCall.loadWord(self(), threadIdIntRequest) <> 0
                then threadTestInterrupt()
                else ()
        end

        val exit: unit -> unit = RunCall.rtsCallFull0 "PolyThreadKillSelf"
        and isActive: thread -> bool = RunCall.rtsCallFast1 "PolyThreadIsActive"
        and broadcastInterrupt: unit -> unit = RunCall.rtsCallFull0 "PolyThreadBroadcastInterrupt"

        local
            fun getAttrWord (me: thread) : Word.word =
                RunCall.loadWord(me, threadIdFlags)

            fun getStackSizeAsInt (me: thread) : int =
                RunCall.loadWord(me, threadIdStackSize)

            and getStackSize me : int option =
                case getStackSizeAsInt me of
                    0 => NONE
                |   s => SOME s

            fun newStackSize ([], default) = default
            |   newStackSize (MaximumMLStack NONE :: _, _) = 0
            |   newStackSize (MaximumMLStack (SOME n) :: _, _) =
                    if n <= 0 then raise Thread "The stack size must be greater than zero" else n
            |   newStackSize (_ :: l, default) = newStackSize (l, default)
            
            val threadMaxStackSize: int -> unit = RunCall.rtsCallFull1 "PolyThreadMaxStackSize"
        in
            (* Set attributes.  Only changes the values that are specified.  The
               others remain the same. *)
            fun setAttributes (attrs: threadAttribute list) : unit =
            let
                val me = self()
                val oldValues: Word.word = getAttrWord me
                val (newValue, mask) = attrsToWord attrs
                val stack = newStackSize(attrs, getStackSizeAsInt me)
            in
                RunCall.storeWord (self(), threadIdFlags,
                    Word.orb(newValue, Word.andb(Word.notb mask, oldValues)));
                if stack = getStackSizeAsInt me
                then () else threadMaxStackSize stack;
                (* If we are now handling interrupts asynchronously check whether
                   we have a pending interrupt now.  This will only be effective
                   if we were previously handling them synchronously or blocking
                   them. *)
                if Word.andb(newValue, 0w4) = 0w4
                then testInterrupt()
                else ()
            end
                
            fun getAttributes() : threadAttribute list =
            let
                val me = self()
            in
                MaximumMLStack (getStackSize me) :: wordToAttrs(getAttrWord me)
            end

            (* These are used in the ConditionVar structure.  They affect only the
               interrupt handling bits. *)
            fun getInterruptState(): interruptState = getIstateBits(getAttrWord(self()))
            and setInterruptState(s: interruptState): unit =
                RunCall.storeWord (self(), threadIdFlags,
                    Word.orb(setIstateBits s, Word.andb(Word.notb 0w6, getAttrWord(self()))))

            local
                (* The default for a new thread is to ignore broadcasts and handle explicit
                   interrupts synchronously. *)
                val (defaultAttrs, _) =
                    attrsToWord[EnableBroadcastInterrupt false, InterruptState InterruptSynch]
                val threadForkFunction:
                    (unit->unit) * word * int -> thread = RunCall.rtsCallFull3 "PolyThreadForkThread"
            in
                fun fork(f:unit->unit, attrs: threadAttribute list): thread =
                let
                    (* Any attributes specified explicitly override the defaults. *)
                    val (attrWord, mask) = attrsToWord attrs
                    val attrValue = Word.orb(attrWord, Word.andb(Word.notb mask, defaultAttrs))
                    val stack = newStackSize(attrs, 0 (* Default is unlimited *))
                    (* Run the function and exit whether it returns normally or raises an exception. *)
                    fun threadFunction () = (f() handle _ => ()) before exit()
                in
                    threadForkFunction(threadFunction, attrValue, stack)
                end
            end
        end

        local
            (* Send an interrupt to a thread.  If it returns false
               the thread did not exist and this should raise an exception. *)
            val threadSendInterrupt: thread -> bool = RunCall.rtsCallFast1 "PolyThreadInterruptThread"
        in
            fun interrupt(t: thread) =
                if threadSendInterrupt t
                then ()
                else raise Thread "Thread does not exist"
        end

        local
            val threadKillThread: thread -> bool = RunCall.rtsCallFast1 "PolyThreadKillThread"
        in
            fun kill(t: thread) =
                if threadKillThread t
                then ()
                else raise Thread "Thread does not exist"
        end

        val numProcessors: unit -> int = RunCall.rtsCallFast0 "PolyThreadNumProcessors"

        local
            val numberOfPhysical: unit -> int =
                RunCall.rtsCallFast0 "PolyThreadNumPhysicalProcessors"
        in
            fun numPhysicalProcessors(): int option =
                (* It is not always possible to get this information *)
                case numberOfPhysical() of 0 => NONE | n => SOME n
        end
    end
    
    structure Mutex =
    struct
        type mutex = Word.word ref (* This is a ref of some sort but may be a volatile byte or word ref. *)
        open Thread  (* createMutex, lockMutex, tryLockMutex and unlockMutex are set up by Initialise. *)
        
        val mutex = createMutex
        
        (* lockMutex, tryLockMutex and unlockMutex are now architecture-specific code. *)
        
        val threadMutexBlock: mutex -> unit = RunCall.rtsCallFull1 "PolyThreadMutexBlock"
        val threadMutexUnlock: mutex -> unit = RunCall.rtsCallFull1 "PolyThreadMutexUnlock"

        fun lock (m: mutex): unit =
            if lockMutex m
            then ()
            else (* It's locked.  We return some time after the lock is released. *)
            (
                threadMutexBlock m;
                lock m (* Try again. *)
            )

        fun unlock (m: mutex): unit =
            if unlockMutex m
            then () (* No contention. *)
            else (* Another thread has blocked and we have to release it. *)
               threadMutexUnlock m

        val trylock = tryLockMutex
    end

    structure ConditionVar =
    struct
        open Thread

        (* A condition variable contains a lock and a list of suspended threads. *)
        type conditionVar = { lock: Mutex.mutex, threads: thread list ref }
        fun conditionVar(): conditionVar =
            { lock = Mutex.mutex(), threads = LibrarySupport.volatileListRef() }

        local
            val threadCondVarWait: Mutex.mutex -> unit = RunCall.rtsCallFull1 "PolyThreadCondVarWait"
            and threadCondVarWaitUntil: Mutex.mutex * Time.time -> unit = RunCall.rtsCallFull2 "PolyThreadCondVarWaitUntil"
        in
            fun innerWait({lock, threads}: conditionVar, m: Mutex.mutex, t: Time.time option) : bool =
            let
                val me = self() (* My thread id. *)
                
                fun waitAgain() =
                let
                    fun doFind [] = false | doFind(h::t) = equal(h, me) orelse doFind t
                    
                    fun removeThis [] = raise Fail "Thread missing in list"
                     |  removeThis (h::t) = if equal(h, me) then t else h :: removeThis t
                     
                    val () =
                        case t of
                            SOME time => threadCondVarWaitUntil(lock, time)
                        |   NONE => threadCondVarWait lock

                    val () = Mutex.lock lock (* Get the lock again.  *)
                    
                    (* Are we still on the list?  If so we haven't been explicitly woken
                       up.  We've either timed out, been interrupted or simply returned
                       because the RTS needed to process some asynchronous results.  *)
                    val stillThere = doFind(!threads)
                    open Time (* For >= *)
                in
                    if not stillThere
                    then (* We're done. *)
                    (
                        Mutex.unlock lock;
                        true
                    )
                    else if (case t of NONE => false | SOME t => Time.now() >= t)
                    then (* We've timed out. *)
                    (
                        threads := removeThis(! threads);
                        Mutex.unlock lock;
                        false
                    )
                    else
                    (
                        (* See if we've been interrupted.  If so remove ourselves
                           and exit. *)
                        testInterrupt()
                            handle exn => (threads := removeThis(! threads); Mutex.unlock lock; raise exn);
                        (* Otherwise just keep waiting. *)
                        waitAgain()
                    )
                end  
            in
                Mutex.lock lock; (* Lock the internal mutex. *)
                Mutex.unlock m; (* Unlock the external mutex *)
                threads := me :: !threads; (* Add ourselves to the list. *)
                waitAgain() (* Wait and return the result when we're done. *)
            end

            fun doWait(c: conditionVar, m: Mutex.mutex, t: Time.time option) : bool =
            let
                val originalIntstate = getInterruptState()
                (* Set this to handle interrupts synchronously unless we're already
                   ignoring them. *)
                val () =
                    if originalIntstate = InterruptDefer
                    then ()
                    else setInterruptState InterruptSynch;
                    
                (* Wait for the condition.  If it raises an exception we still
                   need to reacquire the lock unless we were handling interrupts
                   asynchronously. *)
                val result =
                    innerWait(c, m, t) handle exn =>
                        (
                            (* We had an exception.  If we were handling exceptions synchronously
                               we reacquire the lock.  If it was set to InterruptAsynchOnce this
                               counts as a single asynchronous exception and we restore the
                               state as InterruptSynch. *)
                            case originalIntstate of
                                InterruptDefer => (* Shouldn't happen?  *) Mutex.lock m
                            |   InterruptSynch => Mutex.lock m
                            |   InterruptAsynch => setInterruptState InterruptAsynch
                            |   InterruptAsynchOnce => setInterruptState InterruptSynch;

                            raise exn (* Reraise the exception*)
                        )
            in
                (* Restore the original interrupt state first. *)
                setInterruptState originalIntstate;
                (* Normal return.  Reacquire the lock before returning. *)
                Mutex.lock m;
                result
            end

            fun wait(c: conditionVar, m: Mutex.mutex) : unit =
                (doWait(c, m, NONE); ())
            and waitUntil(c: conditionVar, m: Mutex.mutex, t: Time.time) : bool =
                doWait(c, m, SOME t)
        end
        
        local
            (* This call wakes up the specified thread.  If the thread has already been
               interrupted and is not ignoring interrupts it returns false.  Otherwise
               it wakes up the thread and returns true.  We have to use this because
               we define that if a thread is interrupted before it is signalled then
               it raises Interrupt. *)
            val threadCondVarWake: thread -> bool = RunCall.rtsCallFast1 "PolyThreadCondVarWake"
            
            (* Wake a single thread if we can (signal). *)
            fun wakeOne [] = []
            |   wakeOne (thread::rest) =
                    if threadCondVarWake thread
                    then rest
                    else thread :: wakeOne rest
            (* Wake all threads (broadcast). *)
            fun wakeAll [] = [] (* Always returns the empty list. *)
            |   wakeAll (thread::rest) = (threadCondVarWake thread; wakeAll rest)
            
            fun signalOrBroadcast({lock, threads}: conditionVar, wakeThreads) : unit =
            let
                val originalState = getInterruptState()
            in
                (* Set this to handle interrupts synchronously unless we're already
                   ignoring them.  We need to do this to avoid an asynchronous
                   interrupt which could leave the internal lock in an inconsistent state. *)
                if originalState = InterruptDefer
                then ()
                else setInterruptState InterruptSynch;
                (* Get the condition var lock. *)
                Mutex.lock lock;
                threads := wakeThreads(! threads);
                Mutex.unlock lock;
                setInterruptState originalState; (* Restore original state. *)
                (* Test if we were interrupted while we were handling
                   interrupts synchronously. *)
                if originalState = InterruptAsynch orelse originalState = InterruptAsynchOnce
                then testInterrupt()
                else ()
            end
        in
            fun signal cv = signalOrBroadcast(cv, wakeOne)
            and broadcast cv = signalOrBroadcast(cv, wakeAll)
        end
    end
end;

local
    fun prettyMutex _ _ (_: Thread.Mutex.mutex) = PolyML.PrettyString "?"
    and prettyThread _ _ (_: Thread.Thread.thread) = PolyML.PrettyString "?"
    and prettyCondVar _ _ (_: Thread.ConditionVar.conditionVar) = PolyML.PrettyString "?"
in
    val () = PolyML.addPrettyPrinter prettyMutex
    and () = PolyML.addPrettyPrinter prettyThread
    and () = PolyML.addPrettyPrinter prettyCondVar
end;


