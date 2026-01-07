(*
    Copyright David C. J. Matthews 2016-23, 2026

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

functor Arm64AllocateRegisters(
    structure Arm64ICode: ARM64ICODE
    structure Identify: ARM64IDENTIFYREFERENCES
    structure IntSet: INTSET
    
    sharing Arm64ICode.Sharing = Identify.Sharing = IntSet
): ARM64ALLOCATEREGISTERS =
struct
    open Arm64ICode
    open Identify
    open IntSet
    
    exception InternalError = Misc.InternalError

    datatype allocateResult =
        AllocateSuccess of basicBlockConcrete vector
    |   AllocateFailure of intSet list

    (* General registers.  X24 is used as the global heap base in 32-in-64. X30 is the
       return address set by blr but is otherwise a general register.
       Put the argument registers at the end of the list so they'll only be
       used when hinted. *)
    val generalRegisters =
        map GenReg ([X9, X10, X11, X12, X13, X14, X15, X19, X20, X21, X22, X23,
                      X0, X1, X2, X3, X4, X5, X6, X7, X8, X30] @
                      (case archType of ArchC32 _ => [] | ArchNative => [X24]))

    val floatingPtRegisters =
        map FPReg [V7, V6, V5, V4, V3, V2, V1]
    type conflictState =
    {
        conflicts: intSet, realConflicts: reg list
    }
    
    type triple = {instr: iCodeAbstract, current: intSet, active: intSet}

    exception InternalError = Misc.InternalError

    (* Get the conflict states, allocate registers and return the code with the allocated
       registers if it is successful. *)
    fun allocateRegisters{blocks, regProps, maxPRegs, ...} =
    let
        (* Other registers that conflict with this i.e. cannot share the same
           real register. *)
        val regConflicts = Array.array(maxPRegs, emptySet)
        (* Real registers that cannot be used for this because they are needed for
           an instruction.  Only X30 in calls and RTS traps. *)
        and regRealConflicts = Array.array(maxPRegs, []: reg list)
 
        fun addConflictsTo(addTo, conflicts) =
            List.app(fn aReg => Array.update(regConflicts, aReg, union(Array.sub(regConflicts, aReg), conflicts))) addTo

        (* To reserve a register we need to add the real register to the
           real conflict sets of all the abstract conflicts. *)
        local
            fun isInset reg set = List.exists (fn r => r = reg) set
        in
            fun reserveRegister(reserveFor, reg) =
            let
                val absConflicts = Array.sub(regConflicts, reserveFor)
                fun addConflict i =
                    if i = reserveFor then () else addRealConflict (i, reg)
            in
                List.app addConflict (setToList absConflicts)
            end

            and addRealConflict (i, reg) =
            let
                val currentConflicts = Array.sub(regRealConflicts, i)
            in
                if isInset reg currentConflicts
                then ()
                else Array.update(regRealConflicts, i, reg :: currentConflicts)
            end
        end
      
        fun conflictsForInstr passThrough {instr, current, ...} =
        let
            val {sources, dests} = getInstructionRegisters instr
            fun regNo(PReg i) = i
            val destRegNos = map regNo dests
            and sourceRegNos = map regNo sources
            val destSet = listToSet destRegNos
            val afterRemoveDests = minus(current, destSet)
            
            local
                (* In almost all circumstances the destination and sources don't
                   conflict and the same register can be used as a destination and
                   a source.  BoxLarge can only store the value after the memory
                   has been allocated.  BitFieldInsert has to copy the "destAsSource"
                   value into the destination so cannot use the same register for
                   the "source". *)
                val postInstruction =
                    case instr of
                        BoxLarge _ => destRegNos @ sourceRegNos
                    |   BoxTagFloat _ => destRegNos @ sourceRegNos (* Not sure about this. *)
                    |   BitFieldInsert{source, ...} => regNo source :: destRegNos
                    |   _ => destRegNos
            in
                (* If there is more than one destination they conflict with each other. *)
                val () = addConflictsTo(postInstruction, listToSet postInstruction);
                (* Mark conflicts for the destinations, i.e. after the instruction.
                   The destinations conflict with the registers that are used
                   subsequently. *)
                val () = addConflictsTo(postInstruction, current);
                val () = addConflictsTo(postInstruction, passThrough);

                (* Mark conflicts for the sources i.e. before the instruction. *)
                (* Sources must be set up as conflicts with each other i.e. when we
                   come to allocate registers we must choose different real registers
                   for different abstract registers. *)
                (* TODO: It may may well no longer be necessary to make sources
                   conflict with each other.  It was probably a consequence of
                   the old process of allocating registers from the end back. *)
                val () = addConflictsTo(sourceRegNos, listToSet sourceRegNos)
                val () = addConflictsTo(sourceRegNos, afterRemoveDests);
                val () = addConflictsTo(sourceRegNos, passThrough)
            end
            
            (* I'm not sure if this is needed.  There was a check in the old code to ensure that
               different registers were used for loop variables even if they were actually unused.
               This may happen anyway.
               Comment and code copied from X86 version.  Retain it for the moment. *)
            val () =
                case instr of
                    JumpLoop{regArgs, ...} =>
                    let
                        val destRegs = List.foldl(fn ({dst=PReg loopReg, ...}, dests) => loopReg :: dests) [] regArgs
                    in
                        addConflictsTo(destRegs, listToSet destRegs);
                        addConflictsTo(destRegs, current);
                        addConflictsTo(destRegs, passThrough)
                    end
                |   _ => ()

            (* Certain instructions are specific as to the real registers. *)
            (* We cannot use X30 in any of the unpushed registers in a save set. *)
            fun checkUnsaved{gcUpdate=_, noUpdate} = List.app(fn (PReg r) => addRealConflict(r, GenReg X30)) noUpdate
            val () =
                case instr of
                    ReturnResultFromFunction{ returnReg=PReg retReg, results, ... } =>
                        (* We can't use any of the registers we're using to carry results, typically X0,
                           to hold the return address. *)
                        List.app(fn (_, xReg) => addRealConflict(retReg, GenReg xReg)) results

                |   RaiseExceptionPacket{ packetReg } =>
                        (* This wasn't needed previously because we always pushed the registers
                           across an exception. *)
                        reserveRegister(regNo packetReg, GenReg X0)
                
                |   BeginHandler { packetReg, ...} =>
                        reserveRegister(regNo packetReg, GenReg X0)
                
                |   FunctionCall { dests, regArgs, saveRegs, ...} =>
                        (* This is only needed if we are saving the registers rather
                           than marking them as "must push". *)
                    let
                        val () = checkUnsaved saveRegs
                        val () = List.app(fn (PReg pr, r) => reserveRegister(pr, GenReg r)) dests
                        (* The argument registers also conflict.  In order to execute this call we need to load
                           the arguments into specific registers so we can't use them for values that we want after
                           the call. *)
                        val toReserve = X30 :: List.map #2 regArgs
                    in
                        List.app(fn i => List.app(fn r => addRealConflict(i, GenReg r)) toReserve)
                            (setToList passThrough @ setToList afterRemoveDests)
                    end

                    (* We can't use X30 as the result because it's needed for the return addr if we have to GC. *)
                |   AllocateMemoryFixed{dest=PReg destReg, saveRegs, ...} => (addRealConflict(destReg, GenReg X30); checkUnsaved saveRegs)
                |   AllocateMemoryVariable{dest=PReg destReg, saveRegs, ...} => (addRealConflict(destReg, GenReg X30); checkUnsaved saveRegs)
                |   BoxLarge{dest=PReg destReg, saveRegs, ...} => (addRealConflict(destReg, GenReg X30); checkUnsaved saveRegs)
                |   BoxTagFloat{dest=PReg destReg, saveRegs, ...} => (addRealConflict(destReg, GenReg X30) (* Could exclude floats on native addr. *); checkUnsaved saveRegs)
                |   JumpLoop{checkInterrupt=SOME saveRegs, ...} => checkUnsaved saveRegs

                |   _ => ()
        in
            ()
        end
        
        (* Process the block. *)
        fun conflictsForBlock(ExtendedBasicBlock{block, passThrough, exports, ...}) =
        let
            (* We need to establish conflicts between all the registers active at
               the end of the block since they may not be established elsewhere.
               This isn't necessary for an unconditional branch since the
               same registers will be included in the block that is the target
               of the branch, possibly along with others.  However if this is
               a conditional or indexed branch we may have different sets at
               each of the targets and we have to ensure that all the registers
               differ. *)
            val united = union(exports, passThrough)
            val () = addConflictsTo(setToList united, united)

            val () = List.app (conflictsForInstr passThrough) block
        in
            ()
        end

        val () = Vector.app conflictsForBlock blocks

        (* Hint values.  The idea of hints is that by using a hinted register
           we may avoid an unnecessary move instruction.  realHints is set when
           a pseudo-register is going to be loaded from a specific register
           e.g. a register argument, or moved into one e.g. X0 for the
           function result.
           friends is set to the other pReg that may be associated
           with the pReg.  Typically this is where we have a merge
           register that we move some value into. *)
        val realHints = Array.array(maxPRegs, NONE: reg option)

        (* Sources and destinations.  These indicate the registers that are
           the sources and destinations of the indexing register and are used
           as hints.  If a register has been allocated for a source or destination
           we may be able to reuse it.  *)
        val sourceRegs = Array.array(maxPRegs, []: int list)
        and destinationRegs = Array.array(maxPRegs, []: int list)
        local
            (* Real hints.  If this is the source of a value e.g. a function argument in a register,
               we'll use it directly.  If, though, this is the result of a function and we want the
               result to end up in a specific register we want to propagate it to any pReg that
               moves its value into this. *)
            fun addRealHint(r, reg) =
                case Array.sub(realHints, r) of
                    SOME _ => ()
                |   NONE =>
                    (
                        (* Add to this pReg *)
                        Array.update(realHints, r, SOME reg);
                        (* and to any other pReg that moves here. *)
                        List.app(fn r => addRealHint(r, reg)) (Array.sub(sourceRegs, r))
                    )

            fun addSourceAndDestinationHint{src, dst} =
            let
                val conflicts = Array.sub(regConflicts, src)
            in
                (* If they conflict we can't add them. *)
                if member(dst, conflicts)
                then ()
                else
                let
                    val currentDests = Array.sub(destinationRegs, src)
                    val currentSources = Array.sub(sourceRegs, dst)
                in
                    (* Add the destination for this source i.e. the registers we move
                       this source into. *)
                    if List.exists(fn i => i=dst) currentDests
                    then ()
                    else Array.update(destinationRegs, src, dst :: currentDests);

                    (* Add the source to the list of sources for this destination.
                       A merge register may have several sources, a different one
                       for each path. If the destination has a real hint we want to
                       propagate that back.  That isn't needed for the destinations
                       because we allocate the registers from the start forward. *)
                    if List.exists(fn i => i=src) currentSources
                    then ()
                    else
                    let
                        val sources = src :: currentSources
                        val () = Array.update(sourceRegs, dst, sources)
                    in
                        case Array.sub(realHints, dst) of
                            NONE => ()
                        |   SOME real => List.app(fn r => addRealHint(r, real)) sources
                    end
                end
            end

            (* Add the hints to steer the register allocation.  The idea is to avoid moves between
               registers by getting values into the appropriate register in advance.
               We don't actually need to add real hints where the real register is providing
               the value, e.g. BeginFunction, because the allocation process will take care of
               that. *)
            fun addHints{instr=MoveRegister{source=PReg sreg, dest=PReg dreg, ...}, ...} =
                    addSourceAndDestinationHint {src=sreg, dst=dreg}

            |   addHints{instr=BitFieldInsert{destAsSource=PReg dsReg, dest=PReg dReg, ...}, ...} =
                    (* The "destAsSource" is the destination if some bits are retained. *)
                    addSourceAndDestinationHint {src=dsReg, dst=dReg}

            |   addHints{instr=ReturnResultFromFunction { results, fpResults, ... }, ...} =
                (
                    List.app(fn(PReg pr, r) => addRealHint(pr, GenReg r)) results;
                    List.app(fn(PReg pr, r) => addRealHint(pr, FPReg r)) fpResults
                )

            |   addHints{instr=JumpLoop{regArgs, ...}, ...} =
                let
                    fun addRegArg {src=ArgInReg(PReg argReg), dst=PReg resReg} =
                            addSourceAndDestinationHint {dst=resReg, src=argReg}
                    |   addRegArg {src=ArgOnStack _, ...} = ()
                in
                    List.app addRegArg regArgs
                end

            |   addHints{instr=BeginFunction{regArgs, fpRegArgs, ...}, ...} =
                (
                    List.app (fn (PReg pr, reg) => addRealHint(pr, GenReg reg)) regArgs;
                    List.app (fn (PReg pr, reg) => addRealHint(pr, FPReg reg)) fpRegArgs
                )

            |   addHints{instr=TailRecursiveCall{regArgs, fpRegArgs, ...}, ...} =
                let
                    fun setHint(ArgInReg(PReg pr), reg) = addRealHint(pr, GenReg reg) | setHint _ = ()
                in
                    List.app setHint regArgs;
                    List.app (fn(PReg pr, reg) => addRealHint(pr, FPReg reg)) fpRegArgs
                end
        
            |   addHints{instr=FunctionCall{regArgs, dests, fpRegArgs, fpDests, ...}, ...} =
                let
                    fun setHint(ArgInReg(PReg pr), reg) = addRealHint(pr, GenReg reg) | setHint _ = ()
                in
                    List.app(fn(PReg pr, r) => addRealHint(pr, GenReg r)) dests;
                    List.app(fn(PReg pr, r) => addRealHint(pr, FPReg r)) fpDests;
                    List.app setHint regArgs;
                    List.app (fn(PReg pr, reg) => addRealHint(pr, FPReg reg)) fpRegArgs
                end

                (* Exception packets are in X0 *)
            |   addHints{instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...} = addRealHint(preg, GenReg X0)
            |   addHints{instr=BeginHandler{ packetReg=PReg preg }, ...} = addRealHint(preg, GenReg X0)

            |   addHints{instr=UnaryFloatingPt{source=PReg sreg, dest=PReg dreg, fpOp=MoveFloat}, ...} =
                    addSourceAndDestinationHint {src=sreg, dst=dreg}

            |   addHints{instr=UnaryFloatingPt{source=PReg sreg, dest=PReg dreg, fpOp=MoveDouble}, ...} =
                    addSourceAndDestinationHint {src=sreg, dst=dreg}

            |   addHints _ = ()
        in
            val () = Vector.app(fn ExtendedBasicBlock { block, ...} => List.app addHints block) blocks        
        end

        val failures = ref []: intSet list ref

        (* Turn the abstract icode into a concrete version by allocating the registers. *)
        local

            fun asGenReg(GenReg reg) = reg | asGenReg _ = raise InternalError "asGenReg"
            and asFPReg(FPReg reg) = reg | asFPReg _ = raise InternalError "asFPReg"

            datatype cacheItem =
                CacheStack of {cached: stackLocn, cachedIn: xReg }             (* A value loaded from the stack. *)
            |   CacheAbsAddress of {cached: preg, cachedIn: xReg }             (* 32-in-64: An absolute address from an object ID *)
            |   CacheAbsAddrOnStack of {cached: stackLocn, cachedIn: xReg }    (* 32-in-64: An absolute address from an object loaded from the stack. *)
            |   CacheLargeUnbox of {cached: preg, cachedIn: xReg }             (* A LargeWord.word value has been unboxed *)
            |   CacheRealUnbox of {cached: preg, cachedIn: vReg, floatSize: floatSize } (* A double or float has been unboxed. *)

            fun regCachedIn(CacheStack{cachedIn, ...}) = GenReg cachedIn
            |   regCachedIn(CacheAbsAddress{cachedIn, ...}) = GenReg cachedIn
            |   regCachedIn(CacheAbsAddrOnStack{cachedIn, ...}) = GenReg cachedIn
            |   regCachedIn(CacheLargeUnbox{cachedIn, ...}) = GenReg cachedIn
            |   regCachedIn(CacheRealUnbox{cachedIn, ...}) = FPReg cachedIn

            val allocatedRegs = Array.array(maxPRegs, NONE: reg option)

            (* Find a real register for a preg.
               1.  If a register is already allocated use that.
               2.  Try the "preferred" register if one has been given.
               3.  Try the realHints value if there is one.
               4.  See if there is a "friend" that has an appropriate register
               5.  Look at all the registers and find one. *)
            fun findRegister(r, pref, regSet, cache: cacheItem list) =
            case Array.sub(allocatedRegs, r) of
                SOME reg => reg

            |   NONE =>
                let
                    val conflicts = Array.sub(regConflicts, r)
                    and realConflicts = Array.sub(regRealConflicts, r)
                    (* Find the registers we've already allocated that may conflict. *)
                    val conflictingRegs =
                        List.mapPartial(fn i => Array.sub(allocatedRegs, i)) (setToList conflicts) @
                            realConflicts

                    fun isFree aReg = not (List.exists(fn i => i=aReg) conflictingRegs)

                    fun tryAReg NONE = NONE
                    |   tryAReg (somePref as SOME prefReg) =
                            if isFree prefReg
                            then (Array.update(allocatedRegs, r, somePref); somePref)
                            else NONE

                    (* Search the sources and destinations to see if a register has
                       already been allocated or there is a hint. *)
                    fun findAFriend([], [], _) = NONE

                    |   findAFriend(aDest :: otherDests, sources, alreadySeen) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, aDest) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, aDest))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add the destinations of the destinations to the list
                                       if they don't conflict and haven't been seen. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) alreadySeen) andalso not(member(f, conflicts))
                                    val fOfF = List.filter newFriend (Array.sub(destinationRegs, aDest))
                                in
                                    findAFriend(otherDests @ fOfF, sources, aDest :: alreadySeen)
                                end
                        end

                    |   findAFriend([], aSrc :: otherSrcs, alreadySeen) =
                        let
                            val possReg =
                                case Array.sub(allocatedRegs, aSrc) of
                                    v as SOME _ => tryAReg v
                                |   NONE => tryAReg(Array.sub(realHints, aSrc))
                        in
                            case possReg of
                                reg as SOME _ => reg
                            |   NONE =>
                                let
                                    (* Add the sources of the sources to the list
                                       if they don't conflict and haven't been seen. *)
                                    fun newFriend f =
                                        not(List.exists (fn n => n=f) alreadySeen) andalso not(member(f, conflicts))
                                    val fOfF = List.filter newFriend (Array.sub(sourceRegs, aSrc))
                                in
                                    findAFriend([], otherSrcs @ fOfF, aSrc :: alreadySeen)
                                end
                        end
                in
                    case tryAReg pref of
                        SOME r => r
                    |   NONE =>
                        (
                            case tryAReg (Array.sub(realHints, r)) of
                                SOME r => r
                            |   NONE =>
                                (
                                    case findAFriend(Array.sub(destinationRegs, r), Array.sub(sourceRegs, r), []) of
                                        SOME r => r
                                        (* Look through the registers to find one that's free.  First try excluding
                                           the cache registers. *)
                                    |   NONE =>
                                        let
                                            (* First try filtering all the cache registers to see if we can find a
                                               register.  If not see if it works by freeing one. *)
                                            fun filterCache(filteredRegset, []) = List.find isFree filteredRegset

                                            |   filterCache(filteredRegset, cacheItem :: cache) =
                                                let
                                                    val cReg = regCachedIn cacheItem
                                                in
                                                    case filterCache(List.filter(fn r => r <> cReg) filteredRegset, cache) of
                                                        NONE => if isFree cReg then SOME cReg else NONE
                                                    |   result => result
                                                end

                                           val pick =
                                                case filterCache(regSet, cache) of
                                                    SOME reg => reg
                                                |   NONE =>
                                                    (
                                                        (* This failed.  We're going to have to spill something. *)
                                                        failures := conflicts :: ! failures;
                                                        hd regSet (* Return something to allow this pass to complete *)
                                                    )
                                            val () = Array.update(allocatedRegs, r, SOME pick)
                                        in
                                            pick
                                        end
                                )
                        )
                end

            (* Cache hints: Try to use the same register for values that can be cached.  This increases
               the chances that we will be able to retain the cache when we merge different branches. *)
            val cacheHints = Array.array(maxPRegs, NONE: reg option)

            (* Remove any reference to newly allocated registers from the cache.  Also used after
               block move and comparison that modify registers *)
            fun pruneCache(reg: reg, cache) = List.filter(fn c => regCachedIn c <> reg) cache

            (* Return the cache registers that contain valid addresses.  N.B. We must only
               save registers that contain properly tagged values together, on compact 32-bit,
               with absolute addresses.  All registers are saved across a GC but only these
               registers are updated. *)
            local
                fun hasAddress(CacheStack{cachedIn, ...}) = SOME cachedIn
                |   hasAddress(CacheAbsAddress{cachedIn, ...}) = SOME cachedIn
                |   hasAddress(CacheAbsAddrOnStack{cachedIn, ...}) = SOME cachedIn
                |   hasAddress(CacheLargeUnbox _) = NONE (* The value was untagged. *)
                |   hasAddress(CacheRealUnbox _) = NONE (* Untagged value in a FP register. *)
            in
                val cachedAddressRegs = List.mapPartial hasAddress
            end

            (* Merge the cache states *)
            fun mergeCacheStates ([]: cacheItem list list) = []: cacheItem list
            |   mergeCacheStates [single] = single
        
            |   mergeCacheStates (many as first :: rest) =
                let
                    (* Generally we will either be unable to merge and have an empty cache or
                       will have just one or two entries. *)
                    (* Find the shortest.  If it's empty we're done. *)
                    fun findShortest(_, [], _) = []
                    |   findShortest(_, shortest, []) = shortest
                
                    |   findShortest(len, shortest, hd::tl) =
                        let
                            val hdLen = List.length hd
                        in
                            if hdLen < len then findShortest(hdLen, hd, tl) else findShortest(len, shortest, tl)
                        end
                    val shortest = findShortest(List.length first, first, rest)
                
                    (* Find the item we're caching for.  If it is in a different register we
                       can't use it. *)
                    fun findItem search (hd::tl) = search = hd orelse findItem search tl
                    |   findItem _ [] = false
                
                    (* It's present if it's in all the sources. *)
                    fun present search = List.all(findItem search) many
                
                    val filtered =
                        List.foldl (fn (search, l) => if present search then search :: l else l) [] shortest
                in
                    filtered
                end

            fun allocateNewDestination(PReg r, pref, regSet, cacheList) =
                case Array.sub(allocatedRegs, r) of
                    SOME reg =>
                        (
                            case Vector.sub(regProps, r) of
                                RegPropMultiple => (reg, pruneCache(reg, cacheList)) (* This is allowed for merge registers *)
                            |   _ => raise InternalError "Register defined at multiple points"
                        )
                |   NONE =>
                    let
                        val reg = findRegister(r, pref, regSet, cacheList)
                    in
                        (reg, pruneCache(reg, cacheList))
                    end

            fun allocateGenReg(r, hint, cache) =
            let
                val (reg, newCache) = allocateNewDestination(r, hint, generalRegisters, cache)
            in
                (asGenReg reg, newCache)
            end
            and allocateFPReg(r, hint, cache) =
            let
                val (reg, newCache) = allocateNewDestination(r, hint, floatingPtRegisters, cache)
            in
                (asFPReg reg, newCache)
            end

            and allocateGenRegOrZero(ZeroReg, _, cache) = (XZero, cache)
            |   allocateGenRegOrZero(SomeReg reg, hint, cache) = allocateGenReg(reg, hint, cache)

            fun getAllocatedGenReg(PReg r) =
                case Array.sub(allocatedRegs, r) of SOME(GenReg reg) => reg | _ => raise InternalError "getAllocatedGenReg"
            and getAllocatedFPReg(PReg r) =
                case Array.sub(allocatedRegs, r) of SOME(FPReg reg) => reg | _ => raise InternalError "getAllocatedFPReg"

            fun getAllocatedGenRegOrZero ZeroReg = XZero
            |   getAllocatedGenRegOrZero (SomeReg reg) = getAllocatedGenReg reg

            fun getAllocatedArg(ArgInReg reg) = ArgInReg(getAllocatedGenReg reg)
            |   getAllocatedArg(ArgOnStack stackLoc) = ArgOnStack stackLoc

            fun getSaveRegs{gcUpdate, noUpdate} =
            let
                fun checkX30(PReg r) =
                    case Array.sub(allocatedRegs, r) of
                        SOME(GenReg reg) => if reg = X30 then raise InternalError "X30 in noupdate set" else ()
                    |   _ => () (* May be a floating point reg or unallocated. *)
                val _ = List.app checkX30 noUpdate
            in
                List.map getAllocatedGenReg gcUpdate
            end

            (* Reversed cons and append to make the code easier to read. *)
            infix 5 <::> <@>
            fun tl <::> hd = hd :: tl
            (*and snd <@> fst = fst @ snd*)

            fun absToConcrete([], context, code) = (context, code)

            |   absToConcrete({instr=MoveRegister{ source, dest}, ...} :: rest, cache, code) =
                let
                    (* Try to use the register we've allocated for the source as the destination so
                       that we can eliminate this instruction altogether. *)
                    val sourceReg = getAllocatedGenReg source
                    val (destReg, newCache) = allocateGenReg(dest, SOME(GenReg sourceReg), cache)
                in
                    if sourceReg = destReg
                    then absToConcrete(rest, newCache, code)
                    else absToConcrete(rest, newCache, code <::> MoveRegister { source=sourceReg, dest=destReg})
                end

            |   absToConcrete({instr=LoadNonAddressConstant { dest, source}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> LoadNonAddressConstant { dest=destReg, source=source})
                end

            |   absToConcrete({instr=LoadFPConstant { dest, source, floatSize }, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateFPReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> LoadFPConstant { dest=destReg, source=source, floatSize=floatSize})
                end

            |   absToConcrete({instr=LoadAddressConstant { dest, source}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> LoadAddressConstant { dest=destReg, source=source})
                end

            |   absToConcrete({instr=LoadWithConstantOffset { base, dest, byteOffset, loadType}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        LoadWithConstantOffset { base=getAllocatedGenReg base, dest=destReg, byteOffset=byteOffset, loadType=loadType})
                end

            |   absToConcrete({instr=LoadFPWithConstantOffset { base, dest, byteOffset, floatSize}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateFPReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        LoadFPWithConstantOffset { base=getAllocatedGenReg base, dest=destReg, byteOffset=byteOffset, floatSize=floatSize})
                end

            |   absToConcrete({instr=LoadWithIndexedOffset { base, dest, index, loadType, signExtendIndex}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        LoadWithIndexedOffset { base=getAllocatedGenReg base, dest=destReg, index=getAllocatedGenReg index,
                                                loadType=loadType, signExtendIndex=signExtendIndex})
                end

            |   absToConcrete({instr=LoadFPWithIndexedOffset { base, dest, index, floatSize, signExtendIndex}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateFPReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        LoadFPWithIndexedOffset { base=getAllocatedGenReg base, dest=destReg, index=getAllocatedGenReg index,
                                                  floatSize=floatSize, signExtendIndex=signExtendIndex})
                end

            |   absToConcrete({instr=GetThreadId { dest}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> GetThreadId { dest=destReg})
                end

            |   absToConcrete({instr=ObjectIndexAddressToAbsolute { source as PReg srcNo, dest=destOiA as PReg doia}, kill, ...}
                                    :: rest, cache, code) =
                let
                    (* See if this is in the cache and use it if it is.  If this is the last reference to this
                       source entry we don't want it in the cache any longer. *)
                    val killThis = member(srcNo, kill)
                    val (newCode, dReg, newCache, next) =
                        case List.find(fn (CacheAbsAddress{cached, ...}) => cached=source | _ => false) cache of
                            SOME (CacheAbsAddress{cachedIn, ...}) =>
                            let
                                (* Try to use the cache register as the destination if we can. *)
                                val (destReg, newCache) = allocateNewDestination(destOiA, SOME(GenReg cachedIn), generalRegisters, cache)
                                val dReg = asGenReg destReg
                            in
                                if dReg = cachedIn
                                then (code, dReg, newCache, rest) (* We will have pruned this since it's the destination. *)
                                else (code <::> MoveRegister { source=cachedIn, dest=dReg}, dReg,
                                        if killThis then pruneCache(GenReg cachedIn, newCache) else newCache, rest)
                            end

                        |   _ =>
                                (* If this is the last reference and the next instruction is loading with a zero
                                   offset we can use an indexed load and avoid converting to an absolute address.
                                   If this is not the last reference it's likely that we're loading another field
                                   so it's probably better to convert the object index and cache it.  We might
                                   manage to use a load-pair instruction.  N.B. This only works with a shift of 2 *)
                            (
                                case (killThis, rest) of
                                    (true, {instr=LoadWithConstantOffset{ byteOffset=0, loadType=Load32, base, dest=destLoad, ... },
                                            kill=killLoad, ...} :: next) =>
                                    if base = destOiA (* of objectindex *) andalso member(doia, killLoad) andalso
                                            (case archType of ArchC32 0w2 => true | _ => false)
                                    then
                                    let
                                        val (destReg, newCache) = allocateGenReg(destLoad, NONE, cache)
                                    in
                                        (code <::> LoadWithIndexedOffset{ base=X24(*X_Base32in64*), dest=destReg,
                                                        index=getAllocatedGenReg source, loadType=Load32, signExtendIndex=false },
                                                        destReg, newCache, next)
                                    end
                                    else
                                    let
                                        val (destReg, newCache) = allocateGenReg(destOiA, Array.sub(cacheHints, srcNo), cache)
                                    in
                                        (code <::> ObjectIndexAddressToAbsolute { source=getAllocatedGenReg source, dest=destReg},
                                         destReg, newCache, rest)
                                    end
                                    
                                |   _ =>
                                    let
                                        val (destReg, newCache) = allocateGenReg(destOiA, Array.sub(cacheHints, srcNo), cache)
                                    in
                                        (code <::> ObjectIndexAddressToAbsolute { source=getAllocatedGenReg source, dest=destReg},
                                         destReg, newCache, rest)
                                    end
                            )
                    val () = if killThis then () else Array.update(cacheHints, srcNo, SOME(GenReg dReg))
                in
                    absToConcrete(next, if killThis then newCache else CacheAbsAddress{cached=source, cachedIn=dReg} :: newCache, newCode)
                end

            |   absToConcrete({instr=AbsoluteToObjectIndex { source, dest}, ...} :: rest, cache, code) =
                let
                    (* Don't make an entry in the cache for this; it won't be used. *)
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> AbsoluteToObjectIndex { source=getAllocatedGenReg source, dest=destReg})
                end

            |   absToConcrete({instr=AllocateMemoryFixed { bytesRequired, dest, saveRegs }, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                    val filteredCache = pruneCache(GenReg X30, newCache)
                    val saved = cachedAddressRegs filteredCache @ getSaveRegs saveRegs
                in
                    absToConcrete(rest, filteredCache, code <::>
                        AllocateMemoryFixed { dest=destReg, bytesRequired=bytesRequired, saveRegs={gcUpdate=saved, noUpdate=[]}})
                end

            |   absToConcrete({instr=AllocateMemoryVariable{size, dest, saveRegs}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                    val filteredCache = pruneCache(GenReg X30, newCache)
                    val saved = cachedAddressRegs filteredCache @ getSaveRegs saveRegs
                in
                    absToConcrete(rest, filteredCache, code <::>
                        AllocateMemoryVariable{size=getAllocatedGenReg size, dest=destReg, saveRegs={gcUpdate=saved, noUpdate=[]}})
                end

            |   absToConcrete({instr=InitialiseMem{size, addr, init}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> InitialiseMem{size=getAllocatedGenReg size, addr=getAllocatedGenReg addr, init=getAllocatedGenReg init})

            |   absToConcrete({instr=BeginLoop, ...} :: rest, context, code) = absToConcrete(rest, context, code <::> BeginLoop)

            |   absToConcrete({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...} :: rest, context, code) =
                let
                    fun getStackArg{src, wordOffset, stackloc} =
                            {src=getAllocatedArg src, wordOffset=wordOffset, stackloc=stackloc}
                    and getRegArg{src, dst} = {src=getAllocatedArg src, dst=getAllocatedGenReg dst}
                    val saves =
                        case checkInterrupt of NONE => NONE | SOME regs => SOME{gcUpdate=getSaveRegs regs, noUpdate=[]}
                in
                    absToConcrete(rest, context, code <::> JumpLoop{ regArgs=map getRegArg regArgs, stackArgs=map getStackArg stackArgs,
                              checkInterrupt=saves})
                end

            |   absToConcrete({instr=StoreWithConstantOffset { base, source, byteOffset, loadType}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreWithConstantOffset{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, byteOffset=byteOffset, loadType=loadType})

            |   absToConcrete({instr=StoreFPWithConstantOffset { base, source, byteOffset, floatSize}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreFPWithConstantOffset{ base=getAllocatedGenReg base, source=getAllocatedFPReg source, byteOffset=byteOffset, floatSize=floatSize})

            |   absToConcrete({instr=StoreWithIndexedOffset { base, source, index, loadType, signExtendIndex}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreWithIndexedOffset{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, index=getAllocatedGenReg index,
                                                loadType=loadType, signExtendIndex=signExtendIndex})

            |   absToConcrete({instr=StoreFPWithIndexedOffset { base, source, index, floatSize, signExtendIndex}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        StoreFPWithIndexedOffset{ base=getAllocatedGenReg base, source=getAllocatedFPReg source, index=getAllocatedGenReg index,
                                                  floatSize=floatSize, signExtendIndex=signExtendIndex})

            |   absToConcrete({instr=AddSubImmediate{ source, dest, ccRef, immed, isAdd, length }, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenRegOrZero(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        AddSubImmediate { source=getAllocatedGenReg source, dest=destReg, ccRef=ccRef,
                                                 immed=immed, isAdd=isAdd, length=length})
                end

            |   absToConcrete({instr=AddSubRegister{ base, shifted, dest, ccRef, isAdd, length, shift}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenRegOrZero(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        AddSubRegister { base=getAllocatedGenReg base, shifted=getAllocatedGenReg shifted,
                                     dest=destReg, ccRef=ccRef,
                                     isAdd=isAdd, length=length, shift=shift})
                end

            |   absToConcrete({instr=LogicalImmediate{ source, dest, ccRef, immed, logOp, length }, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenRegOrZero(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        LogicalImmediate { source=getAllocatedGenReg source, dest=destReg, ccRef=ccRef,
                                                  immed=immed, logOp=logOp, length=length})
                end

            |   absToConcrete({instr=LogicalRegister{ base, shifted, dest, ccRef, logOp, length, shift}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenRegOrZero(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        LogicalRegister { base=getAllocatedGenReg base, shifted=getAllocatedGenReg shifted,
                                      dest=destReg, ccRef=ccRef,
                                      logOp=logOp, length=length, shift=shift})
                end

            |   absToConcrete({instr=ShiftRegister{ direction, dest, source, shift, opSize}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        ShiftRegister { source=getAllocatedGenReg source, shift=getAllocatedGenReg shift, dest=destReg,
                                               direction=direction, opSize=opSize})
                end

            |   absToConcrete({instr=CountLeadingZeroBits{ dest, source, opSize}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        CountLeadingZeroBits { source=getAllocatedGenReg source, dest=destReg, opSize=opSize})
                end

            |   absToConcrete({instr=Multiplication{ kind, dest, sourceA, sourceM, sourceN }, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        Multiplication { kind=kind, sourceA=getAllocatedGenRegOrZero sourceA, sourceM=getAllocatedGenReg sourceM,
                                     sourceN=getAllocatedGenReg sourceN, dest=destReg})
                end

            |   absToConcrete({instr=Division{ isSigned, dest, dividend, divisor, opSize }, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        Division { isSigned=isSigned, dividend=getAllocatedGenReg dividend, divisor=getAllocatedGenReg divisor,
                               dest=destReg, opSize=opSize})
                end

            |   absToConcrete({instr=BeginFunction {regArgs, fpRegArgs, stackArgs}, ...} :: rest, _, code) =
                let
                    (* Allocate the register arguments.  At this point all the registers are free and the cache is empty.
                       However we may have a "real conflict" that means that the allocated register is different.  e.g.
                       we need this argument some time after an arbitrary precision operation that may call a
                       function. *)
                    fun allocReg(src, dst) =
                    let
                        val (destReg, _) = allocateNewDestination(src, SOME(GenReg dst), generalRegisters, [])
                    in
                        (asGenReg destReg, dst)
                    end
                    fun allocFPReg(src, dst) =
                    let
                        val (destReg, _) = allocateNewDestination(src, SOME(FPReg dst), floatingPtRegisters, [])
                    in
                        (asFPReg destReg, dst)
                    end
                in
                    absToConcrete(rest, [], code <::>
                        BeginFunction {regArgs=map allocReg regArgs, fpRegArgs=map allocFPReg fpRegArgs, stackArgs=stackArgs})
                end

            |   absToConcrete({instr=FunctionCall{callKind, regArgs, stackArgs, dests, fpRegArgs, fpDests, containers, saveRegs, ...}, ...} :: rest, _, code) =
                let
                    fun getRegArg(src, dst) = (getAllocatedArg src, dst)
                    and getFPRegArg(src, dst) = (getAllocatedFPReg src, dst)
                    fun getResult allocReg (preg, reg) =
                    let
                        (* We empty the cache at this point. *)
                        val (newReg, _) = allocReg(preg, NONE, [])
                    in
                        (newReg, reg)
                    end
                in
                    absToConcrete(rest, [] (* Empty after a function call. *), code <::>
                        FunctionCall{ callKind=callKind, regArgs=map getRegArg regArgs, stackArgs=map getAllocatedArg stackArgs,
                                  dests=map (getResult allocateGenReg) dests, fpRegArgs=map getFPRegArg fpRegArgs,
                                  fpDests=map (getResult allocateFPReg) fpDests, saveRegs={gcUpdate=getSaveRegs saveRegs, noUpdate=[]}, containers=containers})
                end

            |   absToConcrete({instr=TailRecursiveCall{callKind, regArgs, stackArgs, fpRegArgs, stackAdjust, currStackSize}, ...} :: rest, context, code) =
                let
                    fun getRegArg(src, dst) = (getAllocatedArg src, dst)
                    and getStackArg{src, stack} = {src=getAllocatedArg src, stack=stack}
                    and getFPRegArg(src, dst) = (getAllocatedFPReg src, dst)
                in
                    absToConcrete(rest, context, code <::>
                        TailRecursiveCall{ callKind=callKind, regArgs=map getRegArg regArgs, fpRegArgs=map getFPRegArg fpRegArgs,
                            stackArgs=map getStackArg stackArgs, stackAdjust=stackAdjust, currStackSize=currStackSize})
                end

            |   absToConcrete({instr=ReturnResultFromFunction{results, fpResults, returnReg, numStackArgs}, ...} :: rest, context, code) =
                let
                    fun getResult(preg, reg) = (getAllocatedGenReg preg, reg)
                    and getFPResult(preg, reg) = (getAllocatedFPReg preg, reg)
                in
                    absToConcrete(rest, context, code <::>
                        ReturnResultFromFunction{results=map getResult results, fpResults=map getFPResult fpResults,
                                                 returnReg=getAllocatedGenReg returnReg, numStackArgs=numStackArgs})
                end

            |   absToConcrete({instr=RaiseExceptionPacket{packetReg}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> RaiseExceptionPacket{packetReg=getAllocatedGenReg packetReg})

            |   absToConcrete({instr=PushToStack{ source, container as StackLoc{size, rno}, copies }, ...} :: rest, cache, code) =
                let
                    val srcReg = getAllocatedGenReg source
                    val newCache =
                        if size = 1 then CacheStack{cached=container, cachedIn=srcReg} :: cache else cache
                    val () = Array.update(cacheHints, rno, SOME(GenReg srcReg))
                in
                    absToConcrete(rest, newCache, code <::> PushToStack{source=srcReg, container=container, copies=copies})
                end

            |   absToConcrete({instr=LoadStack{ dest=destLoad, container as StackLoc{rno, ...} , field=0, wordOffset}, kill, ...} :: 
                              (restPlusOia as {instr=ObjectIndexAddressToAbsolute { source as PReg srcNo, dest=destOia}, kill=killOia, ...} :: rest), cache, code) =
                (* If a preg has been pushed to the stack every subsequent reference will be via the stack.  If we want to
                   be able to cache object index to absolute addresses for them we have to recognise this combination. *)
                (* They could be unrelated in which case process the LoadStack and then the ObjectIndex...
                        It seems there are also rare circumstances(??) where the result of the load is not
                        killed and so would have to be preserved. *)
                if destLoad = source andalso member(srcNo, killOia)
                then
                let
                    val killThis = member(rno, kill) (* Is it the last reference to the stack entry? *)

                    val (newCode, destReg, newCache) =
                        case List.find(fn CacheAbsAddrOnStack{cached, ...} => cached=container | _ => false) cache of
                            SOME (CacheAbsAddrOnStack{cachedIn, ...}) =>
                            let
                                (* Try to use the cache register as the destination if we can. *)
                                val (destReg, newCache) = allocateNewDestination(destOia, SOME(GenReg cachedIn), generalRegisters, cache)
                                val dReg = asGenReg destReg
                            in
                                if dReg = cachedIn
                                then (code, dReg, newCache) (* We will have pruned this since it's the destination. *)
                                else (code <::> MoveRegister { source=cachedIn, dest=dReg}, dReg,
                                        if killThis then pruneCache(GenReg cachedIn, newCache) else newCache)
                            end

                        |   _ => (* It's not in the cache - load it which could be cached. *)
                            let
                                val (cachePostLoad, loadCode) =
                                    processLoadStack(destLoad, container, wordOffset, kill, cache, code)
                                val (destReg, cachePlusOia) = allocateGenReg(destOia, Array.sub(cacheHints, rno), cachePostLoad)
                            in
                                (loadCode <::> ObjectIndexAddressToAbsolute { source=getAllocatedGenReg source, dest=destReg},
                                 destReg, cachePlusOia)
                            end
                    val () = if killThis then () else Array.update(cacheHints, rno, SOME(GenReg destReg))
                in
                    absToConcrete(rest, if killThis then newCache else CacheAbsAddrOnStack{cached=container, cachedIn=destReg} :: newCache, newCode)
                end
                else (* Can't combine these. *)
                let
                    val (newCache, newCode) = processLoadStack(destLoad, container, wordOffset, kill, cache, code)
                in
                    absToConcrete(restPlusOia, newCache, newCode)
                end

            |   absToConcrete({instr=LoadStack{ dest, container, wordOffset, field=0, ...}, kill, ...} :: rest, cache, code) =
                let
                    val (newCache, newCode) = processLoadStack(dest, container, wordOffset, kill, cache, code)
                in
                    absToConcrete(rest, newCache, newCode)
                end

            |   absToConcrete({instr=LoadStack{ dest, container, field, wordOffset}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> LoadStack{ dest=destReg, container=container, field=field, wordOffset=wordOffset })
                end

            |   absToConcrete({instr=StoreToStack{source, container, field, stackOffset}, ...} :: rest, cache, code) =
                (* We may have cached the original push that cleared the container.  We could cache this since it now
                   contains the entry but it's probably better to deal with multiple results at a higher level. *)
                let
                    val sReg = getAllocatedGenReg source
                    val newCache =
                        List.filter(
                            fn  CacheStack{cached, ...} => cached <> container
                            |   CacheAbsAddrOnStack{cached, ...} => cached <> container | _ => true) cache
                in
                    absToConcrete(rest, newCache, code <::>
                        StoreToStack{source=sReg, container=container, field=field, stackOffset=stackOffset})
                end

            |   absToConcrete({instr=ContainerAddress{ dest, container, stackOffset}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        ContainerAddress{ dest=destReg, container=container, stackOffset=stackOffset })
                end

            |   absToConcrete({instr=ResetStackPtr {numWords}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> ResetStackPtr {numWords=numWords})

            |   absToConcrete({instr=TagValue{source, dest, isSigned, opSize}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        TagValue{source=getAllocatedGenReg source, dest=destReg, isSigned=isSigned, opSize=opSize})
                end

            |   absToConcrete({instr=UntagValue{source, dest, isSigned, opSize, ...}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        UntagValue{source=getAllocatedGenReg source, dest=destReg, isSigned=isSigned, opSize=opSize})
                end

            |   absToConcrete({instr=BoxLarge{source, dest, saveRegs, ...}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                    val filteredCache = pruneCache(GenReg X30, newCache)
                    val saved = cachedAddressRegs filteredCache @ getSaveRegs saveRegs
                in
                    absToConcrete(rest, filteredCache, code <::>
                        BoxLarge{source=getAllocatedGenReg source, dest=destReg, saveRegs={gcUpdate=saved, noUpdate=[]}})
                end

            |   absToConcrete({instr=UnboxLarge{source as PReg rno, dest}, kill, ...} :: rest, cache, code) =
                (* Unboxing a large word.  We cache this in case we need to unbox it again. *)
                let
                    val killThis = member(rno, kill)
                in
                    case List.find(fn CacheLargeUnbox{cached, ...} => cached=source | _ => false) cache of
                        SOME(CacheLargeUnbox{cachedIn, ...}) =>
                        let
                            val (destReg, newCache) = allocateNewDestination(dest, SOME(GenReg cachedIn), generalRegisters, cache)
                            val dReg = asGenReg destReg
                        in
                            absToConcrete(rest,
                                if killThis then pruneCache(GenReg cachedIn, newCache) else newCache,
                                    code <::> MoveRegister { source=cachedIn, dest=dReg})
                        end
                   |    _ =>
                        let
                            val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                        in
                            absToConcrete(rest, if killThis then newCache else CacheLargeUnbox{cached=source, cachedIn=destReg} :: newCache,
                                code <::> UnboxLarge{source=getAllocatedGenReg source, dest=destReg})
                        end
                end

            |   absToConcrete({instr=BoxTagFloat{floatSize, source, dest, saveRegs}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                    val filteredCache = pruneCache(GenReg X30, newCache)
                    val saved = cachedAddressRegs filteredCache @ getSaveRegs saveRegs
                in
                    absToConcrete(rest, filteredCache, code <::>
                        BoxTagFloat{floatSize=floatSize, source=getAllocatedFPReg source, dest=destReg, saveRegs={gcUpdate=saved, noUpdate=[]}})
                end

            |   absToConcrete({instr=UnboxTagFloat{floatSize, source as PReg rno, dest}, kill, ...} :: rest, cache, code) =
                (* Unboxing a large word.  We cache this in case we need to unbox it again. *)
                let
                    val killThis = member(rno, kill)
                in
                    case List.find(fn CacheRealUnbox{cached=s,floatSize=f, ...}=> s=source andalso f=floatSize | _ => false) cache of
                        SOME (CacheRealUnbox{cachedIn, ...}) =>
                        let
                            val (destReg, newCache) = allocateNewDestination(dest, SOME (FPReg cachedIn), floatingPtRegisters, cache)
                            val dReg = asFPReg destReg
                        in
                            absToConcrete(rest,
                                if killThis then pruneCache(FPReg cachedIn, newCache) else newCache,
                                    code <::> UnaryFloatingPt { source=cachedIn, dest=dReg, fpOp=case floatSize of Float32=>MoveFloat | Double64=>MoveDouble})
                        end
                   |    _ =>
                        let
                            val (destReg, newCache) = allocateFPReg(dest, NONE, cache)
                        in
                            absToConcrete(rest, if killThis then newCache else CacheRealUnbox{cached=source, floatSize=floatSize, cachedIn=destReg} :: newCache,
                                code <::> UnboxTagFloat{floatSize=floatSize, source=getAllocatedGenReg source, dest=destReg})
                        end
                end

            |   absToConcrete({instr=LoadAcquire { base, dest, loadType}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> LoadAcquire { base=getAllocatedGenReg base, dest=destReg, loadType=loadType})
                end

            |   absToConcrete({instr=StoreRelease { base, source, loadType}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> StoreRelease{ base=getAllocatedGenReg base, source=getAllocatedGenReg source, loadType=loadType})

            |   absToConcrete({instr=BitFieldShift{source, dest, isSigned, length, immr, imms}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> BitFieldShift { source=getAllocatedGenReg source, dest=destReg, isSigned=isSigned,
                                    immr=immr, imms=imms, length=length})
                end

            |   absToConcrete({instr=BitFieldInsert{source, destAsSource, dest, length, immr, imms}, ...} :: rest, cache, code) =
                let
                    val destAsSourceReg = getAllocatedGenReg destAsSource
                    val (destReg, newCache) = allocateNewDestination(dest, SOME(GenReg destAsSourceReg), generalRegisters, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        BitFieldInsert { source=getAllocatedGenReg source, destAsSource=destAsSourceReg, dest=asGenReg destReg,
                                     immr=immr, imms=imms, length=length})
                end

            |   absToConcrete({instr=IndexedCaseOperation{testReg}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> IndexedCaseOperation{testReg=getAllocatedGenReg testReg})

            |   absToConcrete({instr=PushExceptionHandler, ...} :: rest, context, code) = absToConcrete(rest, context, code <::> PushExceptionHandler)

            |   absToConcrete({instr=PopExceptionHandler, ...} :: rest, context, code) = absToConcrete(rest, context, code <::> PopExceptionHandler)

            |   absToConcrete({instr=BeginHandler{packetReg}, ...} :: rest, _, code) =
                let
                    (* The cache is undefined at the start of a handler. *)
                    val (destReg, newCache) = allocateGenReg(packetReg, NONE, [])
                in
                    absToConcrete(rest, newCache, code <::> BeginHandler{packetReg=destReg})
                end

            |   absToConcrete({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, ...} :: rest, cache, code) =
                let
                    (* This instruction modifies these registers so they must be removed from the cache *)
                    val vec1Reg = getAllocatedGenReg vec1Addr
                    and vec2Reg = getAllocatedGenReg vec2Addr
                    and lenReg = getAllocatedGenReg length
                    val newCache = pruneCache(GenReg vec1Reg, pruneCache(GenReg vec2Reg, pruneCache(GenReg lenReg, cache)))
                in
                    absToConcrete(rest, newCache, code <::> CompareByteVectors{vec1Addr=vec1Reg, vec2Addr=vec2Reg, length=lenReg, ccRef=ccRef})
                end

            |   absToConcrete({instr=BlockMove{srcAddr, destAddr, length, isByteMove}, ...} :: rest, cache, code) =
                let
                    (* This instruction modifies these registers so they must be removed from the cache *)
                    val srcAReg = getAllocatedGenReg srcAddr
                    and dstAReg = getAllocatedGenReg destAddr
                    and lenReg = getAllocatedGenReg length
                    val newCache = pruneCache(GenReg srcAReg, pruneCache(GenReg dstAReg, pruneCache(GenReg lenReg, cache)))
                in
                    absToConcrete(rest, newCache, code <::> BlockMove{srcAddr=srcAReg, destAddr=dstAReg, length=lenReg, isByteMove=isByteMove})
                end

            |   absToConcrete({instr=AddSubXSP{source, dest, isAdd}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenRegOrZero(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> AddSubXSP { source=getAllocatedGenReg source, dest=destReg, isAdd=isAdd})
                end

            |   absToConcrete({instr=TouchValue{source}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::> TouchValue { source=getAllocatedGenReg source})

            |   absToConcrete({instr=LoadAcquireExclusive{ base, dest }, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> LoadAcquireExclusive { base=getAllocatedGenReg base, dest=destReg})
                end

            |   absToConcrete({instr=StoreReleaseExclusive{ base, source, result }, ...} :: rest, cache, code) =
                let
                    val (resultReg, newCache) = allocateGenReg(result, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> StoreReleaseExclusive{ base=getAllocatedGenReg base, source=getAllocatedGenRegOrZero source,
                                           result=resultReg})
                end

            |   absToConcrete({instr=MemoryBarrier, ...} :: rest, cache, code) = absToConcrete(rest, cache, code <::> MemoryBarrier)

            |   absToConcrete({instr=ConvertIntToFloat{ source, dest, srcSize, destSize}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateFPReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        ConvertIntToFloat{ source=getAllocatedGenReg source, dest=destReg, srcSize=srcSize, destSize=destSize})
                end

            |   absToConcrete({instr=ConvertFloatToInt{ source, dest, srcSize, destSize, rounding}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        ConvertFloatToInt{ source=getAllocatedFPReg source, dest=destReg, srcSize=srcSize,
                                       destSize=destSize, rounding=rounding})
                end

            |   absToConcrete({instr=UnaryFloatingPt{ source, dest, fpOp=MoveFloat}, ...} :: rest, cache, code) =
                (* Register-register moves can be eliminated if the same resgister is used. *)
                let
                    val srcReg = getAllocatedFPReg source
                    val (destReg, newCache) = allocateFPReg(dest, SOME(FPReg srcReg), cache)
                in
                    absToConcrete(rest, newCache,
                        if destReg = srcReg then code else code <::> UnaryFloatingPt{ source=srcReg, dest=destReg, fpOp=MoveFloat})
                end

            |   absToConcrete({instr=UnaryFloatingPt{ source, dest, fpOp=MoveDouble}, ...} :: rest, cache, code) =
                let
                    val srcReg = getAllocatedFPReg source
                    val (destReg, newCache) = allocateFPReg(dest, SOME(FPReg srcReg), cache)
                in
                    absToConcrete(rest, newCache,
                        if destReg = srcReg then code else code <::> UnaryFloatingPt{ source=srcReg, dest=destReg, fpOp=MoveDouble})
                end

            |   absToConcrete({instr=UnaryFloatingPt{ source, dest, fpOp}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateFPReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> UnaryFloatingPt{ source=getAllocatedFPReg source, dest=destReg, fpOp=fpOp})
                end

            |   absToConcrete({instr=BinaryFloatingPoint{ arg1, arg2, dest, fpOp, opSize}, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateFPReg(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::> BinaryFloatingPoint{ arg1=getAllocatedFPReg arg1, arg2=getAllocatedFPReg arg2,
                                         dest=destReg, fpOp=fpOp, opSize=opSize})
                end

            |   absToConcrete({instr=CompareFloatingPoint{ arg1, arg2, opSize, ccRef}, ...} :: rest, context, code) =
                    absToConcrete(rest, context, code <::>
                        CompareFloatingPoint{ arg1=getAllocatedFPReg arg1, arg2=getAllocatedFPReg arg2, opSize=opSize, ccRef=ccRef})

            |   absToConcrete({instr=CPUYield, ...} :: rest, cache, code) = absToConcrete(rest, cache, code <::> CPUYield)

            |   absToConcrete({instr=AtomicOperation{ base, source, dest, atOp }, ...} :: rest, cache, code) =
                let
                    val (destReg, newCache) = allocateGenRegOrZero(dest, NONE, cache)
                in
                    absToConcrete(rest, newCache, code <::>
                        AtomicOperation{ base=getAllocatedGenReg base, source=getAllocatedGenRegOrZero source, dest=destReg, atOp=atOp })
                end

            (* LoadStack. *)
            and processLoadStack(dest, container as StackLoc{rno, ...}, wordOffset, kill, cache, code) =
            let
                (* See if this is in the cache and use it if it is.  If this is the last reference to this
                   stack entry we don't want it in the cache any longer. *)
                val killThis = member(rno, kill)

                val (newCode, destReg: xReg, newCache) =
                    case List.find(fn CacheStack{cached=c, ...} => c=container | _ => false) cache of
                        SOME (CacheStack{cachedIn, ...}) =>
                        let
                            val (destReg, newCache) = allocateNewDestination(dest, SOME(GenReg cachedIn), generalRegisters, cache)
                            val dReg = asGenReg destReg
                        in
                            if dReg = cachedIn
                            then (code, dReg, newCache) (* We will have pruned this since it's the destination. *)
                            else (code <::> MoveRegister { source=cachedIn, dest=dReg}, dReg,
                                    if killThis then pruneCache(GenReg cachedIn, newCache) else newCache)
                        end

                    |   _ =>
                        let
                            val (destReg, newCache) = allocateGenReg(dest, Array.sub(cacheHints, rno), cache)
                        in
                            (code <::> LoadStack{ dest=destReg, container=container, field=0, wordOffset=wordOffset },
                             destReg, newCache)
                        end
                val () = if killThis then () else Array.update(cacheHints, rno, SOME(GenReg destReg))
            in
                (if killThis then newCache else CacheStack{cached=container, cachedIn=destReg} :: newCache, newCode)
            end

        in
            fun concreteBlock(ExtendedBasicBlock{ block, ...}, inputCache) =
            let
                val (cache, code) = absToConcrete(block, inputCache, [])
            in
                {cache=cache, code=List.rev code}
            end
            
            val mergeCacheStates = mergeCacheStates
        end

        val numBlocks = Vector.length blocks
        (* The results.  The cache state is initialised to empty so that if we
           have a loop we will end up with an empty input cache. *)
        val resultArray = Array.array(numBlocks, {code=[], cache=[]})

        (* Process the blocks in execution order so that normally we will be able to
           propagate the cache states.  If we have a loop the input cache state will
           be empty because the output cache state for an unprocessed block is empty. *)
        (* Get the blocks that are inputs for each one. *)
        local
            val blockRefs = Array.array(numBlocks, [])

            (* The successors of this block but only including handlers in SetHandler. *)
            fun directSuccessors ExitCode = []
            |   directSuccessors(IndexedBr cases) = cases
            |   directSuccessors(Unconditional dest) = [dest]
            |   directSuccessors(Conditional {trueJump, falseJump, ...}) = [falseJump, trueJump]
            |   directSuccessors(SetHandler { handler, continue }) = [handler, continue]
            |   directSuccessors(UnconditionalHandle _) = []
            |   directSuccessors(ConditionalHandle { continue, ...}) = [continue]
            
            fun setReferences fromBlock =
            let
                val ExtendedBasicBlock{ flow, ...} = Vector.sub(blocks, fromBlock)
                val refs = directSuccessors flow
                fun setRefs toBlock =
                let
                    val oldRefs = Array.sub(blockRefs, toBlock)
                in
                    Array.update(blockRefs, toBlock, fromBlock :: oldRefs);
                    if null oldRefs
                    then setReferences toBlock
                    else ()
                end
            in
                List.app setRefs refs
            end

            val () = setReferences 0
        in
            val directSuccessors = directSuccessors
            val blockRefs = blockRefs
        end

        val processed = Array.array(numBlocks, false)
        fun haveProcessed n = Array.sub(processed, n)
           
        fun processBlocks (toDo: int list) =
            case List.filter (fn n => not(haveProcessed n)) toDo of
                [] => ()
            |   stillToDo as head :: _ =>
                let
                    (* Try to find a block all of whose predecessors have been processed.  That
                       increases the chances that we will have cached items. *)
                    fun available dest = List.all haveProcessed (Array.sub(blockRefs, dest))
                    val blockNo =
                        case List.find available stillToDo of
                            SOME c => c
                        |   NONE => head
                    val thisBlock as ExtendedBasicBlock { flow, ...} = Vector.sub(blocks, blockNo)
                    (* Get the input cache state.  Take the list of output caches of everything that
                       jumps here and produce the intersection. *)
                    val inputCacheList =
                        List.map (fn n => #cache(Array.sub(resultArray, n))) (Array.sub(blockRefs, blockNo))
                    val inputCache = mergeCacheStates inputCacheList
                    (* Process this block and add it to the results. *)
                    val () = Array.update(processed, blockNo, true)
                    val () = Array.update(resultArray, blockNo, concreteBlock(thisBlock, inputCache))

                    (* Add the successors but with handlers only included in SetHandler. *)
                    val addSet = directSuccessors flow
                in
                    processBlocks(addSet @ stillToDo)
                end
    in
        processBlocks [0];
        (* If the failures list is empty we succeeded. *)
        case !failures of
            [] =>  (* Return the allocation vector.  We may have unused registers, *)
                AllocateSuccess(
                    Vector.mapi(fn (i, ExtendedBasicBlock{ flow, ...}) =>
                        BasicBlock{block= #code(Array.sub(resultArray, i)), flow=flow}) blocks )
            (* Else we'll have to spill something. *)
        |   l => AllocateFailure l
    end

    val nGenRegs = List.length generalRegisters

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and ('genReg, 'optGenReg, 'fpReg) basicBlock = ('genReg, 'optGenReg, 'fpReg) basicBlock
        and regProperty         = regProperty
        and reg                 = reg
        and xReg                = xReg
        and vReg                = vReg
        and allocateResult      = allocateResult
    end

end;
