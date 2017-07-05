(*
    Title:      Thread package for ML.
    Author:     David C. J. Matthews
    Copyright (c) 2007-2014

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

(* This signature and structure are not part of the standard basis library
   but are included here because they depend on the Time structure and are
   in turn dependencies of the BasicIO structure. *)

signature THREAD =
sig
    exception Thread of string (* Raised if an operation fails. *)
    
    structure Thread:
    sig
        type thread;
        
        (* Thread attributes - This may be extended. *)
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
        
        (* fork: Fork a thread.  Starts a new thread running the function argument.  The
           attribute list gives initial values for thread attributes which can be
           modified by the thread itself.  Any unspecified attributes take default values.
           The thread is terminated when the thread function returns, if it
           raises an uncaught exception or if it calls "exit". *)
        val fork: (unit->unit) * threadAttribute list -> thread
        (* exit: Terminate this thread. *)
        val exit: unit -> unit
        (* isActive: Test if a thread is still running or has terminated. *)
        val isActive: thread -> bool
        
        (* Test whether thread ids are the same. *)
        val equal: thread * thread -> bool
        (* Get my own ID. *)
        val self: unit -> thread
        
        exception Interrupt (* = SML90.Interrupt *)
        (* Send an Interrupt exception to a specific thread.  When and indeed whether
           the exception is actually delivered will depend on the interrupt state
           of the target thread.  Raises Thread if the thread is no longer running,
           so an exception handler should be used unless the thread is known to
           be blocked. *)
        val interrupt: thread -> unit
        (* Send an interrupt exception to every thread which is set to accept it. *)
        val broadcastInterrupt: unit -> unit
        (* If this thread is handling interrupts synchronously, test to see 
           if it has been interrupted.  If so it raises the Interrupt
           exception. *)
        val testInterrupt: unit -> unit
        (* Terminate a thread. This should be used as a last resort.  Normally
           a thread should be allowed to clean up and terminate by using the
           interrupt call.  Raises Thread if the thread is no longer running,
           so an exception handler should be used unless the thread is known to
           be blocked. *)
        val kill: thread -> unit
        
        (* Get and set thread-local store for the calling thread. The store is a
           tagged associative memory which is initially empty for a new thread.
           A thread can call setLocal to add or replace items in its store and
           call getLocal to return values if they exist.  The Universal structure
           contains functions to make new tags as well as injection, projection and
           test functions. *)
        val getLocal: 'a Universal.tag -> 'a option
        val setLocal: 'a Universal.tag * 'a -> unit
        
        (* Change the specified attribute(s) for the calling thread.  Unspecified
           attributes remain unchanged. *)
        val setAttributes: threadAttribute list -> unit
        (* Get the values of attributes. *)
        val getAttributes: unit -> threadAttribute list

        (* Return the number of processors that will be used to run threads. *)
        val numProcessors: unit -> int
        (* and the number of physical processors if that is available. *)
        and numPhysicalProcessors: unit -> int option
    end
        
    structure Mutex:
    sig
        (* Mutexes.  A mutex provides simple mutual exclusion.  A thread can lock
           a mutex and until it unlocks it no other thread will be able to lock it.
           Locking and unlocking are intended to be fast in the situation when
           there is no other process attempting to lock the mutex.  *)
        type mutex
        (* mutex: Make a new mutex *)
        val mutex: unit -> mutex
        (* lock:  Lock a mutex.  If the mutex is currently locked the thread is
           blocked until it is unlocked.  If a thread tries to lock a mutex that
           it has previously locked the thread will deadlock.
           N.B.  "lock" is not an interruption point (a point where synchronous
           interrupts are delivered) even though a thread can be blocked indefinitely. *)
        val lock: mutex -> unit
        (* unlock:  Unlock a mutex and allow any waiting threads to run.  The behaviour
           if the mutex was not previously locked by the calling thread is undefined.  *)
        val unlock: mutex -> unit
        (* trylock: Attempt to lock the mutex.  Returns true if the mutex was not
           previously locked and has now been locked by the calling thread.  Returns
           false if the mutex was previously locked, including by the calling thread. *)
        val trylock: mutex -> bool
        
        (* These functions may not work correctly if an asynchronous interrupt
           is delivered during the calls.  A thread should use synchronous interrupt
           when using these calls. *)
    end
    
    structure ConditionVar:
    sig
        (* Condition variables.  Condition variables are used to provide communication
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
        (* conditionVar: Make a new condition variable. *)
        val conditionVar: unit -> conditionVar
        (* wait: Release the mutex and block until the condition variable is
           signalled.  When wait returns the mutex has been re-acquired.
           If thread is handling interrupts synchronously a call to "wait" may cause
           an Interrupt exception to be delivered.
           (The implementation must ensure that if an Interrupt is delivered as well
           as signal waking up a single thread that the interrupted thread does not
           consume the "signal".)
           The mutex is (re)acquired before Interrupt is delivered.  *)
        val wait: conditionVar * Mutex.mutex -> unit
        (* waitUntil: As wait except that it blocks until either the condition
           variable is signalled or the time (absolute) is reached.  Either way
           the mutex is reacquired so there may be a further delay if it is held
           by another thread.  *)
        val waitUntil: conditionVar * Mutex.mutex * Time.time -> bool
        (* signal: Wake up one thread if any are waiting on the condition variable. *)
        val signal: conditionVar -> unit
        (* broadcast: Wake up all threads waiting on the condition variable. *)
        val broadcast: conditionVar -> unit
    end

end;

structure Thread :> THREAD =
struct
    open RuntimeCalls (* for POLY_SYS and EXC numbers *)
    
    exception Thread = RunCall.Thread
    
    (* Create non-overwritable mutables for mutexes and condition variables.
       A non-overwritable mutable in the executable or a saved state is not
       overwritten when a saved state further down the hierarchy is loaded. *)
    val nvref = LibrarySupport.noOverwriteRef
    
    structure Thread =
    struct
        
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

        type thread = Word.word ref (* Actually this is a multi-word mutable object. *)
        (* Equality is pointer equality. *)
        val equal : thread*thread->bool = RunCall.pointerEq
        (* Return our own thread object. *)
        val self: unit->thread = RunCall.run_call0 POLY_SYS_thread_self
        
        fun getLocal (t: 'a Universal.tag) : 'a option =
        let
            val root: Universal.universal ref list =
                RunCall.run_call2 POLY_SYS_load_word(self(), threadIdThreadLocal)

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
                RunCall.run_call2 POLY_SYS_load_word(self(), threadIdThreadLocal)

            fun doFind [] =
                    (* Not in the list - Add it. *)
                    RunCall.run_call3 POLY_SYS_assign_word
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
            val doCall: int*unit->unit = RunCall.run_call2 POLY_SYS_thread_dispatch
        in
            fun testInterrupt() =
                (* If there is a pending request the word in the thread object
                   will be non-zero. *)
                if RunCall.run_call2 POLY_SYS_load_word(self(), threadIdIntRequest) <> 0
                then doCall(11, ())
                else ()
        end

        local
            fun getAttrWord (me: thread) : Word.word =
                RunCall.run_call2 POLY_SYS_load_word(me, threadIdFlags)

            (* Just for the moment we check the length of the thread ID to be
               sure we have the appropriate fields present. *)
            val System_length: thread -> word = RunCall.run_call1 POLY_SYS_get_length

            fun getStackSizeAsInt (me: thread) : int =
                if System_length me <= threadIdStackSize
                then 0
                else RunCall.run_call2 POLY_SYS_load_word(me, threadIdStackSize)

            and getStackSize me : int option =
                case getStackSizeAsInt me of
                    0 => NONE
                |   s => SOME s

            fun newStackSize ([], default) = default
            |   newStackSize (MaximumMLStack NONE :: _, _) = 0
            |   newStackSize (MaximumMLStack (SOME n) :: _, _) =
                    if n <= 0 then raise Thread "The stack size must be greater than zero" else n
            |   newStackSize (_ :: l, default) = newStackSize (l, default)
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
                RunCall.run_call3 POLY_SYS_assign_word (self(), threadIdFlags,
                    Word.orb(newValue, Word.andb(Word.notb mask, oldValues)));
                if stack = getStackSizeAsInt me
                then () else RunCall.run_call2 POLY_SYS_thread_dispatch (15, stack);
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
                RunCall.run_call3 POLY_SYS_assign_word (self(), threadIdFlags,
                    Word.orb(setIstateBits s, Word.andb(Word.notb 0w6, getAttrWord(self()))))

            local
                (* The default for a new thread is to ignore broadcasts and handle explicit
                   interrupts synchronously. *)
                val (defaultAttrs, _) =
                    attrsToWord[EnableBroadcastInterrupt false, InterruptState InterruptSynch]
                val doCall = RunCall.run_call2 POLY_SYS_thread_dispatch
            in
                fun fork(f:unit->unit, attrs: threadAttribute list): thread =
                let
                    (* Any attributes specified explicitly override the defaults. *)
                    val (attrWord, mask) = attrsToWord attrs
                    val attrValue = Word.orb(attrWord, Word.andb(Word.notb mask, defaultAttrs))
                    val stack = newStackSize(attrs, 0 (* Default is unlimited *))
                in
                    doCall(7, (f, attrValue, stack))
                end
            end
        end

        val exit: unit -> unit = RunCall.run_call0 POLY_SYS_kill_self
        
        local
            val doCall: int*thread->bool = RunCall.run_call2 POLY_SYS_thread_dispatch
        in
            fun isActive(t: thread): bool = doCall(8, t)
        end
        
        local
            val doCall: int*unit->unit = RunCall.run_call2 POLY_SYS_thread_dispatch
        in
            fun broadcastInterrupt() = doCall(10, ())
        end

        local
            val doCall: int*thread->unit = RunCall.run_call2 POLY_SYS_thread_dispatch
        in
            fun kill(t: thread) = doCall(12, t)
            and interrupt(t: thread) = doCall(9, t)
        end

        local
            val doCall = RunCall.run_call2 POLY_SYS_thread_dispatch
        in
            fun numProcessors():int = doCall(13, 0)

            and numPhysicalProcessors(): int option =
                (* It is not always possible to get this information *)
                case doCall(14, 0) of 0 => NONE | n => SOME n
        end
    end
    
    structure Mutex =
    struct
        type mutex = Word.word ref
        fun mutex() = nvref 0w1; (* Initially unlocked. *)
        val atomicIncr: Word.word ref -> Word.word = RunCall.run_call1 POLY_SYS_atomic_incr
        and atomicDecr: Word.word ref -> Word.word = RunCall.run_call1 POLY_SYS_atomic_decr
        and atomicReset: Word.word ref -> unit     = RunCall.run_call1 POLY_SYS_atomic_reset

        val doCall: int * mutex -> unit = RunCall.run_call2 POLY_SYS_thread_dispatch

        (* A mutex is implemented as a Word.word ref.  It is initially set to 1 and locked
           by atomically decrementing it.  If it was previously unlocked the result will
           by zero but if it was already locked it will be some negative value.  When it
           is unlocked it is atomically incremented.  If there was no contention the result
           will again be 1 but if some other thread tried to lock it the result will be
           zero or negative.  In that case the unlocking thread needs to call in to the
           RTS to wake up the blocked thread.

           The cost of contention on the lock is very high.  To try to avoid this we
           first loop (spin) to see if we can get the lock without contention.  *)

        val spin_cycle = 20000
        fun spin (m: mutex, c: int) =
           if ! m = 0w1 then ()
           else if c = spin_cycle then ()
           else spin(m, c+1);

        fun lock (m: mutex): unit =
        let
            val () = spin(m, 0)
            val newValue = atomicDecr m
        in
            if newValue = 0w0
            then () (* We've acquired the lock. *)
            else (* It's locked.  We return when we have the lock. *)
            (
                doCall(1, m);
                lock m (* Try again. *)
            )
        end

        fun unlock (m: mutex): unit =
        let
            val newValue = atomicIncr m
        in
            if newValue = 0w1
            then () (* No contention. *)
            else
                (* Another thread has blocked and we have to release it.  We can safely
                   set the value to 1 here to release the lock.  If another thread
                   acquires it before we have woken up the other threads that's fine.
                   Equally, if another thread decremented the count and saw it was
                   still locked it will enter the RTS and try to acquire the lock
                   there.
                   It's probably better to reset it here rather than within the RTS
                   since it allows another thread to acquire the lock immediately
                   rather than after the rather long process of entering the RTS.
                   Resetting this needs to be atomic with respect to atomic increment
                   and decrement.  That's not a problem on X86 so a simple assignment
                   is sufficient but in the interpreter at least it's necessary to
                   acquire a lock. *)
            (
                atomicReset m;
                doCall(2, m)
            )
        end

        (* Try to lock the mutex.  If it was previously unlocked then lock it and
           return true otherwise return false.  Because we don't block here there is
           the possibility that the thread that has locked it could release the lock
           shortly afterwards.  The check for !m = 0w1 is an optimisation and nearly
           all the time it avoids the call to atomicDecr setting m to a negative value.
           There is a small chance that another thread could lock the mutex between the
           test for !m = 0w1 and the atomicDecr.  In that case the atomicDecr would
           return a negative value and the function that locked the mutex will have to
           call into the RTS to reset it when it is unlocked.  *)
        fun trylock (m: mutex): bool =
            if !m = 0w1 andalso atomicDecr m = 0w0
            then true (* We've acquired the lock. *)
            else false (* The lock was taken. *)
    end

    structure ConditionVar =
    struct
        open Thread

        (* A condition variable contains a lock and a list of suspended threads. *)
        type conditionVar = { lock: Mutex.mutex, threads: thread list ref }
        fun conditionVar(): conditionVar =
            { lock = Mutex.mutex(), threads = nvref nil }

        (* To avoid duplicating the code we use zero to represent an infinite wait.
           Since that's a valid time in the past we check that it isn't used in
           waitUntil before doing anything else. *)
        val infinity = Time.zeroTime;

        local
            val doCall = RunCall.run_call2 POLY_SYS_thread_dispatch
            fun Sleep(mt: Mutex.mutex * Time.time): unit = doCall(3, mt)
        in
            fun innerWait({lock, threads}: conditionVar, m: Mutex.mutex, t: Time.time) : bool =
            let
                val me = self() (* My thread id. *)
                
                fun waitAgain() =
                let
                    fun doFind [] = false | doFind(h::t) = equal(h, me) orelse doFind t
                    
                    fun removeThis [] = raise Fail "Thread missing in list"
                     |  removeThis (h::t) = if equal(h, me) then t else h :: removeThis t
                     
                    val () = Sleep(lock, t) (* Atomically release the lock and wait. *)

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
                    else if t <> infinity andalso Time.now() >= t
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

            fun doWait(c: conditionVar, m: Mutex.mutex, t: Time.time) : bool =
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
                (doWait(c, m, infinity); ())
            and waitUntil(c: conditionVar, m: Mutex.mutex, t: Time.time) : bool =
                if t = infinity
                then false (* This has already happened. *)
                else doWait(c, m, t)
        end
        
        local
            val doCall = RunCall.run_call2 POLY_SYS_thread_dispatch
            (* This call wakes up the specified thread.  If the thread has already been
               interrupted and is not ignoring interrupts it returns false.  Otherwise
               it wakes up the thread and returns true.  We have to use this because
               we define that if a thread is interrupted before it is signalled then
               it raises Interrupt. *)
            fun doWake(t: thread): bool = doCall(4, t)
            
            (* Wake a single thread if we can (signal). *)
            fun wakeOne [] = []
            |   wakeOne (thread::rest) =
                    if doWake thread
                    then rest
                    else thread :: wakeOne rest
            (* Wake all threads (broadcast). *)
            fun wakeAll [] = [] (* Always returns the empty list. *)
            |   wakeAll (thread::rest) = (doWake thread; wakeAll rest)
            
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

structure ThreadLib:
sig
    val protect: Thread.Mutex.mutex -> ('a -> 'b) -> 'a -> 'b
end =
struct
    (* This applies a function while a mutex is being held. 
       Although this can be defined in terms of Thread.Thread.getAttributes it's
       defined here using the underlying calls.  The original version with
       getAttributes appeared as a major allocation hot-spot when building the
       compiler because "protect" is called round every access to the global
       name-space. *)
    fun protect m f a =
    let
        open Thread.Thread Thread.Mutex
        open RuntimeCalls Word
        (* Set this to handle interrupts synchronously except if we are blocking
           them.  We don't want to get an asynchronous interrupt while we are
           actually locking or unlocking the mutex but if we have to block to do
           IO then we should allow an interrupt at that point. *)
        val oldAttrs: Word.word = RunCall.run_call2 POLY_SYS_load_word(self(), 1)
        val () =
            if andb(oldAttrs, 0w6) = 0w0 (* Already deferred? *)
            then ()
            else RunCall.run_call3 POLY_SYS_assign_word (self(), 1,
                    orb(andb(notb 0w6, oldAttrs), 0w4))
        fun restoreAttrs() =
        (
            RunCall.run_call3 POLY_SYS_assign_word (self(), 1, oldAttrs);
            if andb(oldAttrs, 0w4) = 0w4 then testInterrupt() else ()
        )
        val () = lock m
        val result = f a
            handle exn =>
            (
                unlock m; restoreAttrs();
                (* Reraise the exception preserving the location information. *)
                PolyML.Exception.reraise exn
            )
    in
        unlock m;
        restoreAttrs();
        result
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


