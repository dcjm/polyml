(*
    Copyright David C. J. Matthews 2016-17

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

functor X86PushRegisters(
    structure ICODE: ICodeSig
    structure INTSET: INTSETSIG
    structure IDENTIFY: X86IDENTIFYREFSSIG
    sharing ICODE.Sharing = IDENTIFY.Sharing = INTSET
) : X86PUSHREGISTERSIG
=
struct
    open ICODE
    open INTSET
    open IDENTIFY
    
    (* Curried subscript functions *)
    fun asub a i = Array.sub(a, i)
    and vsub v i = Vector.sub(v, i)
    
    exception InternalError = Misc.InternalError

    (* Each preg in the input is mapped to either a new preg or the stack. *)
    datatype pregMapType = Unset | ToPReg of preg | ToStack of int * stackLocn

    (* The stack contains both entries in the input code and entries added here.
       It is really used to ensure that the stack at run time is the same size
       at the start of a block whichever block has jumped to it. *)
    datatype stackEntry =
        NewEntry of {pregNo: int} (* pregNo is the original preg that has been pushed here. *)
    |   OriginalEntry of { stackLoc: stackLocn }
    |   HandlerEntry

    fun addRegisterPushes{code: extendedBasicBlock vector, pushVec: bool vector, pregProps, firstPass} =
    let
        val maxPRegs = Vector.length pregProps
        val numberOfBlocks = Vector.length code
        (* Output registers and properties. *)
        val pregCounter = ref 0
        val pregPropList = ref []
        val pregMap = Array.array(maxPRegs, Unset)
        
        (* Cache registers. *)
        datatype cacheType =
            CacheStack of { rno: int }(* Original preg or stack loc. *)
        |   CacheMemory of { rno: int, offset: int } (* Base offset *)
            (* CacheTagged is used if we tag a value to see if we can use the
               original untagged value somewhere. *)
        |   CacheTagged of { rno: int, isSigned: bool }

        local
            (* The number of active cache entries is likely to be small and is
               at most proportional to the number of instructions in the block.
               Any function call will clear it.
               For memory entries we need to know if the value is tagged and
               what kind of move we're using.
               Stack entries always will be tagged and MoveWord. *)
            val cache: {cacheFor: cacheType, cacheReg: preg, isTagged: bool, kind: moveKind } list ref = ref []
           
            fun isStack n {cacheFor=CacheStack{rno}, ...} = rno = n
            |   isStack _ _ = false
            
            fun isMemory (r, off) {cacheFor=CacheMemory{rno, offset}, ...} = rno = r andalso offset=off
            |   isMemory _ _ = false
            
            fun isTagCache(r, s) {cacheFor=CacheTagged{rno, isSigned}, ...} = rno = r andalso s = isSigned
            |   isTagCache _ _ = false
            
            fun findCache f = List.find f (! cache)
            
            fun removeCache f = cache := List.filter (not o f) (! cache)
        in
            fun clearCache() = cache := []
             
            fun findCachedStack n = Option.map (#cacheReg) (findCache (isStack n))
            and findCachedMemory (r, off) =
                Option.map (fn {cacheReg, isTagged, kind, ...} => (cacheReg, isTagged, kind)) (findCache(isMemory (r, off)))
            and findCachedTagged (r, s) = Option.map #cacheReg (findCache(isTagCache (r, s)))
            
            fun removeStackCache n = removeCache (isStack n)
            and removeMemoryCache (r, off) = removeCache (isMemory (r, off))
            and removeTagCache (r, s) = removeCache (isTagCache (r, s))
            
            fun clearMemoryCache() =
                cache := List.filter(fn {cacheFor=CacheMemory _,...} => false | _ => true) (!cache)

            fun setStackCache(n, new) =
                (
                    removeStackCache n;
                    cache := {cacheFor=CacheStack{rno=n}, cacheReg=new, isTagged=true, kind=MoveWord} :: ! cache
                )
            
            and setMemoryCache(r, off, new, isTagged, kind) =
                (
                    removeMemoryCache (r, off);
                    cache := {cacheFor=CacheMemory{rno=r, offset=off}, cacheReg=new, isTagged=isTagged, kind=kind} :: ! cache
                )
            
            and setTagCache(r, s, new) =
                (
                    removeTagCache (r, s);
                    cache := {cacheFor=CacheTagged{rno=r, isSigned=s}, cacheReg=new, isTagged=true, kind=MoveWord} :: ! cache
                )

            fun getCache () = ! cache
            
            (* Merge the cache states *)
            fun setCommonCacheState [] = clearCache() 
            |   setCommonCacheState [single] = cache := single
            
            |   setCommonCacheState (many as first :: rest) =
                let
                    (* Generally we will either be unable to merge and have an empty cache or
                       will have just one or two entries. *)
                    (* Find the shortest.  If it's empty we're done. *)
                    fun findShortest(_, [], _) = []
                    |   findShortest(_, shortest, []) = shortest
                    
                    |   findShortest(len, shortest, hd::tl) =
                        let
                            val hdLen = length hd
                        in
                            if hdLen < len then findShortest(hdLen, hd, tl) else findShortest(len, shortest, tl)
                        end
                    val shortest = findShortest(length first, first, rest)
                    
                    (* Find the item we're caching for.  If it is in a different register we
                       can't use it. *)
                    fun findItem search (hd::tl) =
                        if #cacheFor hd = #cacheFor search then #cacheReg hd = #cacheReg search
                        else findItem search tl
                    |   findItem _ [] = false
                    
                    (* It's present if it's in all the sources. *)
                    fun present search = List.all(findItem search) many
                    
                    val filtered =
                        List.foldl (fn (search, l) => if present search then search :: l else l) [] shortest
                in
                    cache := filtered
                end
        end

        val maxStack = ref 0
        (* The stack size we've assumed for the block.  Also indicates if
           a block has already been processed. *)
        val inputStackSizes =
            Array.array(numberOfBlocks, NONE: {expectedInput:int, reqCC: bool} option)
        (* The result of processing a block. *)
        val blockOutput = Array.array(numberOfBlocks, {code=[], cache=[], stackCount=0})
        
        (* Extra blocks to adjust the stack are added here. *)
        val extraBlocks: basicBlock list ref = ref []
        val blockCounter = ref numberOfBlocks
        
        (* Get the blocks that are inputs for each one. *)
        local
            val blockRefs = Array.array(numberOfBlocks, [])
            
            fun setReferences fromBlock =
            let
                val ExtendedBasicBlock{ flow, ...} = vsub code fromBlock
                val refs = successorBlocks flow
                fun setRefs toBlock =
                let
                    val oldRefs = asub blockRefs toBlock
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
            val blockRefs = blockRefs
        end

        (* Recursive scan of the blocks.  For each block we produce an input and output state.
           The input state is the output state of the predecessor i.e. some block that jumps to
           this, but with any entries removed that are not used in this block.  It is then
           necessary to match the input state, if necessary by adding extra blocks that just
           do the matching. *)
        local
            val haveProcessed = isSome o asub inputStackSizes
        
            fun processBlocks toDo =
            case List.filter (fn (n, _) => not(haveProcessed n)) toDo of
                [] => () (* Nothing left to do *)

            |   stillToDo as head :: _ =>
                let
                    (* Try to find a block all of whose predecessors have been processed.  That
                       increases the chances that we will have cached items. *)
                    fun available(dest, _) = List.all haveProcessed (Array.sub(blockRefs, dest))
                    val (blockNo, lastOutputState) =
                        case List.find available stillToDo of
                            SOME c => c
                        |   NONE => head
                
                    (* This is the first time we've come to this block.  *)
                    val ExtendedBasicBlock{ block, flow, imports, passThrough, inCCState, initialStacks, ...} = vsub code blockNo
                    val requiresCC = isSome inCCState

                    (* Remove any items from the input state that are no longer needed for
                       this block.  They could be local to the previous block or needed by
                       a different successor. *)
                    fun removeItems(result as {stack=[], stackCount=0}) = result
                
                    |   removeItems{stack=[], ...} = raise InternalError "removeItems - stack size"

                    |   removeItems (thisStack as {stack=NewEntry{pregNo} :: rest, stackCount}) =
                        if member(pregNo, imports) orelse member(pregNo, passThrough)
                        then thisStack
                        else removeItems{stack=rest, stackCount=stackCount-1}

                    |   removeItems (thisStack as {stack=OriginalEntry{stackLoc=StackLoc{rno, size}, ...} :: rest, stackCount}) =
                        if member(rno, initialStacks)
                        then thisStack
                        else removeItems{stack=rest, stackCount=stackCount-size}

                    |   removeItems result = result

                    val {stackCount=newSp, stack=newStack} = removeItems lastOutputState
                    
                    (* References to hold the current stack count (number of words on the stack)
                       and the list of items on the stack.  The list is not used directly to map
                       stack addresses.  Instead it is used to match the stack at the beginning
                       and end of a block. *)
                    val stackCount = ref newSp
                    val stack = ref newStack
                    (* Items from the stack that have been marked as deleted but not yet
                       removed.  We only remove items from the top of the stack to avoid
                       quadratic behaviour with a very deep stack. *)
                    val deletedItems = ref []
                    
                    (* Save the stack size in case we come by a different route. *)
                    val () = Array.update(inputStackSizes, blockNo, SOME{expectedInput=newSp, reqCC=requiresCC})
                    
                    fun pushItemToStack item =
                    let
                        val size =
                            case item of
                                NewEntry _ => 1
                            |   OriginalEntry{stackLoc=StackLoc{size, ...}, ...} => size
                            |   HandlerEntry => 2
                    in
                        stackCount := ! stackCount+size;
                        stack := item :: ! stack;
                        maxStack := Int.max(!maxStack, !stackCount)
                    end
 
                    fun newPReg propKind =
                    let
                        val regNo = !pregCounter before pregCounter := !pregCounter + 1
                        val () = pregPropList := propKind :: !pregPropList
                    in
                        PReg regNo
                    end
        
                    and newStackLoc size =
                    let
                        val regNo = !pregCounter before pregCounter := !pregCounter + 1
                        val () = pregPropList := RegPropStack size :: !pregPropList
                    in
                        StackLoc{size=size, rno=regNo}
                    end
        
                    (* Map a source register.  This always loads the argument. *)
                    fun mapSrcRegEx(PReg n) =
                        case Array.sub(pregMap, n) of
                            Unset => raise InternalError "mapSrcReg - unset"
                        |   ToPReg preg => (preg, [], [])
                        |   ToStack(stackLoc, container as StackLoc{size, ...}) =>
                            let
                                (* Make a new untagged register. That will prevent us pushing it if
                                   we have to spill registers. *)
                                val newReg = newPReg RegPropUntagged
                                val sourceCache = findCachedStack n
                                val stackSource =
                                    StackLocation{wordOffset= !stackCount-stackLoc-size, container=container, field=0, cache=sourceCache}
                                (* Because this is in a register we can copy it to a cache register. *)
                                val newCacheReg = newPReg RegPropCacheTagged
                                val () = setStackCache(n, newCacheReg)
                            in
                                (newReg,
                                    [LoadArgument{source=stackSource, dest=newReg, kind=MoveWord}],
                                    [CopyToCache{source=newReg, dest=newCacheReg, kind=MoveWord}])
                            end

                    fun mapSrcReg srcReg =
                    let
                        val (newReg, codePre, codePost) = mapSrcRegEx srcReg
                    in
                        (newReg, codePost @ codePre)
                    end

                    fun mapDestReg(PReg n) =
                    let
                        val currentLocation = Array.sub(pregMap, n)
                        val kind = Vector.sub(pregProps, n)
                    in
                        if Vector.sub(pushVec, n)
                        then
                        let
                            (* This should not have been seen before. *)
                            val _ = case currentLocation of Unset => () | _ => raise InternalError "mapDestReg - already set"
                            val newReg = newPReg kind
                            val newContainer = newStackLoc 1
                            val () = Array.update(pregMap, n, ToStack (!stackCount, newContainer))
                            val () = pushItemToStack(NewEntry{pregNo=n})
                        in
                            (newReg, [PushValue{arg=RegisterArgument newReg, container=newContainer}])
                        end
                        else
                        let
                        (* See if we already have a number for it.  We may encounter the same preg
                           as a destination when returning the result from a conditional in which
                           case we have to use the same number.  We shouldn't have pushed it. *)
                            val newReg =
                                case currentLocation of
                                    Unset =>
                                    let
                                        val newReg = newPReg kind
                                        val () = Array.update(pregMap, n, ToPReg newReg)
                                    in
                                        newReg
                                    end
                                |   ToPReg preg => preg
                                |   ToStack _ => raise InternalError "mapDestReg - already on stack"
                        in
                            (newReg, [])
                        end
                    end
        
                    (* A work register must be a normal register. *)
                    fun mapWorkReg(PReg n) =
                    let
                        val currentLocation = Array.sub(pregMap, n)
                        val _ = Vector.sub(pushVec, n) andalso raise InternalError "mapWorkReg - MustPush"
                    in
                        case currentLocation of
                            Unset =>
                            let
                                val kind = Vector.sub(pregProps, n)
                                val newReg = newPReg kind
                                val () = Array.update(pregMap, n, ToPReg newReg)
                            in
                                newReg
                            end
                        |   ToPReg preg => preg
                        |   ToStack _ => raise InternalError "mapWorkReg - on stack"
                
                    end

                    fun mapIndexEx(NoMemIndex) = (NoMemIndex, [], [])
                    |   mapIndexEx(MemIndex1 r) =
                            let val (sreg, c1, c2) = mapSrcRegEx r in (MemIndex1 sreg, c1, c2) end
                    |   mapIndexEx(MemIndex2 r) =
                            let val (sreg, c1, c2) = mapSrcRegEx r in (MemIndex2 sreg, c1, c2) end
                    |   mapIndexEx(MemIndex4 r) =
                            let val (sreg, c1, c2) = mapSrcRegEx r in (MemIndex4 sreg, c1, c2) end
                    |   mapIndexEx(MemIndex8 r) =
                            let val (sreg, c1, c2) = mapSrcRegEx r in (MemIndex8 sreg, c1, c2) end

                    fun mapIndex index =
                    let
                        val (newIndex, codePre, codePost) = mapIndexEx index
                    in
                        (newIndex, codePost @ codePre)
                    end

                    (* Adjust a stack offset from the old state to the new state. *)
                    fun mapContainerAndStack(StackLoc{rno, size}, field) =
                    let
                        val (newStackAddr, newContainer) =
                        case Array.sub(pregMap, rno) of
                            Unset => raise InternalError "mapContainer - unset"
                        |   ToPReg _ => raise InternalError "mapContainer - ToPReg"
                        |   ToStack stackContainer => stackContainer

                        val newOffset = !stackCount-(newStackAddr+size) + field
                    in
                        (newOffset, newContainer)
                    end
        
                    (* Add an entry for an existing stack entry. *)
                    fun mapDestContainer(StackLoc{rno, size}, locn) =
                    (
                        case Array.sub(pregMap, rno) of
                            Unset =>
                            let
                                val newContainer = newStackLoc size
                                val () = Array.update(pregMap, rno, ToStack(locn, newContainer))
                            in
                                newContainer
                            end
                        |   _ => raise InternalError "mapDestContainer: already set"
                    )

                    fun mapSourceEx(RegisterArgument(PReg r)) =
                        (
                            case Array.sub(pregMap, r) of
                                Unset => raise InternalError "mapSource - unset"
                            |   ToPReg preg => (RegisterArgument preg, [], [])
                            |   ToStack(stackLoc, container as StackLoc{size, ...}) =>
                                let
                                    val sourceCache = findCachedStack r
                                    val stackLoc =
                                        StackLocation{wordOffset= !stackCount-stackLoc-size, container=container, field=0, cache=sourceCache}
                                    (* If this is cached we need to make a new cache register and copy it there. *)
                                    val cacheCode =
                                        case sourceCache of
                                            NONE => []
                                        |   SOME cacheR =>
                                            let
                                                val newCacheReg = newPReg RegPropCacheTagged
                                                val () = setStackCache(r, newCacheReg)
                                            in
                                                [CopyToCache{source=cacheR, dest=newCacheReg, kind=MoveWord}]
                                            end
                                in
                                    (stackLoc, [], cacheCode)
                                end
                        )

                    |   mapSourceEx(a as AddressConstant _) = (a, [], [])
                    |   mapSourceEx(i as IntegerConstant _) = (i, [], [])

                    |   mapSourceEx(MemoryLocation{base as PReg breg, offset, index=NoMemIndex, cache, ...}) =
                        let
                            val (baseReg, baseCodePre, baseCodePost) = mapSrcRegEx base
                            (* We can cache this if it is the first pass or if we have previously
                               cached it and we haven't marked it as pushed. *)
                            val newCache =
                                case cache of
                                    NONE => if firstPass then findCachedMemory(breg, offset) else NONE
                                |   SOME (PReg c) =>
                                    if Vector.sub(pushVec, c)
                                    then NONE (* We had marked this as to be pushed - we can't use a cache here. *)
                                    else findCachedMemory(breg, offset)
                            val memLoc =
                                MemoryLocation{base=baseReg, offset=offset, index=NoMemIndex, cache=Option.map #1 newCache}
                            val cacheCode =
                                case newCache of
                                    NONE => (removeMemoryCache(breg, offset); [])
                                |   SOME (oldCacheReg, isTagged, kind) =>
                                    let
                                        (* Set the cache kind.  If this is the first pass we will have a
                                           general or untagged register. *)
                                        val cacheKind = if isTagged then RegPropCacheTagged else RegPropCacheUntagged
                                        val newCacheReg = newPReg cacheKind
                                        val () = setMemoryCache(breg, offset, newCacheReg, isTagged, kind)
                                    in
                                        [CopyToCache{source=oldCacheReg, dest=newCacheReg, kind=kind}]
                                    end
                        in
                            (memLoc, baseCodePre, baseCodePost @ cacheCode)
                        end

                    |   mapSourceEx(MemoryLocation{base, offset, index, ...}) =
                        let
                            val (baseReg, baseCodePre, baseCodePost) = mapSrcRegEx base
                            val (indexValue, indexCodePre, indexCodePost) = mapIndexEx index
                        in
                            (MemoryLocation{base=baseReg, offset=offset, index=indexValue, cache=NONE}, baseCodePre @ indexCodePre,
                                baseCodePost @ indexCodePost)
                        end

                    |   mapSourceEx(StackLocation{container as StackLoc{rno, ...}, field, cache, ...}) =
                        let
                            val (newOffset, newContainer) = mapContainerAndStack(container, field)
                            (* Was the item previously cached?  If it wasn't or the cache reg has been marked
                               as "must push" we can't use a cache. *)
                            val newCache =
                                case cache of
                                    NONE => NONE
                                |   SOME (PReg c) =>
                                    if Vector.sub(pushVec, c)
                                    then NONE (* We had marked this as to be pushed - we can't use a cache here. *)
                                    else findCachedStack rno
                            val stackLoc =
                                StackLocation{wordOffset=newOffset, container=newContainer, field=field, cache=newCache}
                            val cacheCode =
                                case newCache of
                                    NONE => (removeStackCache rno; [])
                                |   SOME oldCacheReg =>
                                    let
                                        val newCacheReg = newPReg RegPropCacheTagged
                                        val () = setStackCache(rno, newCacheReg)
                                    in
                                        [CopyToCache{source=oldCacheReg, dest=newCacheReg, kind=MoveWord}]
                                    end
                        in
                            (stackLoc, [], cacheCode)
                        end
                    
                    |   mapSourceEx(ContainerAddr{container, ...}) =
                        let
                            val (newOffset, newContainer) = mapContainerAndStack(container, 0)
                        in
                            (ContainerAddr{container=newContainer, stackOffset=newOffset}, [], [])
                        end

                    fun mapSource src =
                    let
                        val (sourceVal, sourceCodePre, sourceCodePost) = mapSourceEx src
                    in
                        (sourceVal, sourceCodePost @ sourceCodePre)
                    end

                    (* Force a load of the source into a register if it is on the stack.
                       This is used in cases where a register or literal is allowed but not
                       a memory location. If we do load it we can cache the register. *)
                    fun mapAndLoad(source as RegisterArgument(PReg r)) =
                        let
                            val (sourceVal, sourceCodePre, sourceCodePost) = mapSourceEx source
                        in
                            case sourceVal of
                                stack as StackLocation _ =>
                                let
                                    val newReg = newPReg RegPropUntagged
                                    val newCacheReg = newPReg RegPropCacheTagged
                                    val _ = setStackCache(r, newCacheReg)
                                in
                                    (RegisterArgument newReg,
                                        CopyToCache{source=newReg, dest=newCacheReg, kind=MoveWord} :: sourceCodePost @
                                            LoadArgument{source=stack, dest=newReg, kind=MoveWord} :: sourceCodePre)
                                end
                            |   _ => (sourceVal, sourceCodePost @ sourceCodePre)
                        end

                    |   mapAndLoad(StackLocation _) = raise InternalError "mapAndLoad - already a stack loc"
                    |   mapAndLoad(MemoryLocation _) = raise InternalError "mapAndLoad - already a mem loc"
                    |   mapAndLoad source = mapSource source
        
                    (* Rewrite the code, replacing any registers that need to be pushed with references to
                       the stack.  The result is built up in reverse order and then reversed. *)
                    fun pushRegisters({instr=LoadArgument{source, dest=PReg dReg, kind}, ...}, code) =
                        if Vector.sub(pushVec, dReg)
                        then (* We're going to push this. *)
                        let
                            val (sourceVal, sourceCode) = mapSource source
                            (* If we have to push the value we don't have to first load it into a register. *)
                            val _ = case Array.sub(pregMap, dReg) of Unset => () | _ => raise InternalError "LoadArgument - already set"
                            val container = newStackLoc 1
                            val () = Array.update(pregMap, dReg, ToStack(! stackCount, container))
                            val () = pushItemToStack(NewEntry{pregNo=dReg})
                        in
                            PushValue{arg=sourceVal, container=container} :: sourceCode @ code
                        end
                        else (* We're not going to push this. *)
                        let
                            val (sourceVal, sourceCodePre, sourceCodePost) = mapSourceEx source
                            val dKind = Vector.sub(pregProps, dReg)
                            val destReg =
                                case Array.sub(pregMap, dReg) of
                                    Unset =>
                                    let
                                        val newReg = newPReg dKind
                                        val () = Array.update(pregMap, dReg, ToPReg newReg)
                                    in
                                        newReg
                                    end
                                |   ToPReg preg => preg
                                |   ToStack _ => raise InternalError "LoadArgument - already on stack"
                            
                            (* Can we cache this? . *)
                            val cacheCode =
                                case source of
                                    MemoryLocation{base=PReg breg, offset, index=NoMemIndex, ...} =>
                                    let
                                        (* The cache kind must match the kind of register we're loading.
                                           If the value is untagged it must not be marked to be examined
                                           by the GC if we allocate anything.
                                           The move kind has to be suitable for a register to register move. *)
                                        val moveKind =
                                            case kind of
                                                MoveWord => MoveWord
                                            |   MoveByte => MoveWord
                                            |   Move16Bit => MoveWord
                                            |   Move32Bit => MoveWord
                                            |   MoveFloat => MoveDouble
                                            |   MoveDouble => MoveDouble
                                        val (cacheType, isTagged) =
                                            case dKind of
                                                RegPropGeneral => (RegPropCacheTagged, true)
                                            |   RegPropUntagged => (RegPropCacheUntagged, false)
                                            |   _ => raise InternalError "cacheKind"
                                        val newCacheReg = newPReg cacheType
                                        val _ = setMemoryCache(breg, offset, newCacheReg, isTagged, moveKind)
                                    in 
                                        [CopyToCache{source=destReg, dest=newCacheReg, kind=moveKind}]
                                    end
                                |   _ => []

                            val destCode = LoadArgument{source=sourceVal, dest=destReg, kind=kind}
                        in
                            cacheCode @ sourceCodePost @ destCode :: sourceCodePre @ code
                        end

                    |   pushRegisters({instr=StoreArgument{source, offset, base, index, kind, isMutable}, ...}, code) =
                        let
                            val (loadedSource, sourceCode) = mapAndLoad source
                            (* We can't have a memory-memory store so we have to load the source if it's now on the stack. *)
                            val (baseReg, baseCode) = mapSrcReg(base)
                            val (indexValue, indexCode) = mapIndex(index)
                            (* If we're assigning to a mutable we can no longer rely on
                               the memory cache.  Clear it completely in that case although
                               we could be more selective. *)
                            val () = if isMutable then clearMemoryCache() else ()
                        in
                            StoreArgument{source=loadedSource, base=baseReg, offset=offset, index=indexValue, kind=kind, isMutable=isMutable} ::
                                    indexCode @ baseCode @ sourceCode @ code
                        end

                    |   pushRegisters({instr=LoadMemReg { offset, dest}, ...}, code) =
                        let
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ LoadMemReg { offset=offset, dest=destVal} :: code
                        end

                    |   pushRegisters({instr=BeginFunction {regArgs, stackArgs}, ...}, code) =
                        let
                            (* Create a new container list.  The offsets begin at -numArgs. *)
                            fun newContainers(src :: srcs, offset) =
                                let
                                    val newContainer = mapDestContainer(src, offset)
                                in
                                    newContainer :: newContainers(srcs, offset+1)
                                end
                            |   newContainers _ = []

                            val newStackArgs = newContainers(stackArgs, ~ (List.length stackArgs))
                            (* Push any registers that need to be pushed. *)
                            fun pushReg((preg, rreg), (others, code)) =
                            let
                                val (newReg, newCode) = mapDestReg(preg)
                            in
                                ((newReg, rreg) :: others, newCode @ code)
                            end
                            val (newRegArgs, pushCode) = List.foldl pushReg ([], []) regArgs
                        in
                            pushCode @ BeginFunction {regArgs=newRegArgs, stackArgs=newStackArgs} :: code
                        end

                    |   pushRegisters({instr=FunctionCall{callKind, regArgs, stackArgs, dest}, ...}, code) =
                        let
                            (* It's possible that this could lead to having to spill registers in order
                               to load others.  Leave that problem for the moment. *)
                            fun loadStackArg (arg, (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) = mapSource arg
                            in
                                (loadCode @ otherLoads, argVal :: otherArgs)
                            end
                            val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs

                            fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) = mapSource arg
                            in
                                (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                            end
                            val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs
                            val (destVal, destCode) = mapDestReg dest
                            (* Now clear the cache table. *)
                            val () = clearCache()
                        in
                            destCode @
                                    FunctionCall{ callKind=callKind, regArgs=newRegArgs, stackArgs=newStackArgs, dest=destVal} ::
                                        regArgLoads @ stackArgLoads @ code
                        end

                    |   pushRegisters({instr=TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, workReg, ...}, ...}, code) =
                        let
                            val newWorkReg = mapWorkReg workReg
                            val newStackOffset = !stackCount
                            
                            fun loadStackArg ({src, stack}, (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) =
                                    case mapSource src of
                                        (source as StackLocation{wordOffset, ...}, loadCode) =>
                                            (* If we're leaving it in its old location or we're pushing it
                                               above the current top we're ok.  We're also ok if
                                               we're moving it from a somewhere above the last argument.
                                               Otherwise we have to load it.
                                               It goes into a normal tagged register which may mean that it
                                               could be pushed onto the stack in a subsequent pass. *)
                                            if wordOffset = stack+newStackOffset orelse stack+newStackOffset < 0
                                                orelse newStackOffset-wordOffset > ~ stackAdjust
                                            then (source, loadCode)
                                            else
                                            let
                                                val preg = newPReg RegPropGeneral
                                            in
                                                (RegisterArgument preg,
                                                    LoadArgument{source=source, dest=preg, kind=MoveWord} :: loadCode)
                                            end
                                    |   argCode => argCode
                            in
                                (loadCode @ otherLoads, {src=argVal, stack=stack} :: otherArgs)
                            end
                            val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                
                            fun loadRegArg ((arg, reg), (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) = mapSource arg
                            in
                                (loadCode @ otherLoads, (argVal, reg) :: otherArgs)
                            end
                            val (regArgLoads, newRegArgs) = List.foldr loadRegArg ([], []) regArgs

                        in
                            TailRecursiveCall{ callKind=callKind, regArgs=newRegArgs,
                                    stackArgs=newStackArgs, stackAdjust=stackAdjust, currStackSize=newStackOffset,
                                    workReg=newWorkReg} ::
                                        regArgLoads @ stackArgLoads @ code
                        end

                    |   pushRegisters({instr=AllocateMemoryOperation{size, flags, dest, ...}, ...}, code) =
                        let
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ AllocateMemoryOperation{size=size, flags=flags, dest=destVal, saveRegs=[]} :: code
                        end

                    |   pushRegisters({instr=AllocateMemoryVariable{size, dest, ...}, ...}, code) =
                        let
                            val (sizeVal, sizeCode) = mapSrcReg size
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ AllocateMemoryVariable{size=sizeVal, dest=destVal, saveRegs=[]} :: sizeCode @ code
                        end

                    |   pushRegisters({instr=InitialiseMem{size, addr, init}, ...}, code) =
                        let
                            val (sizeVal, sizeCode) = mapSrcReg size
                            val (addrVal, addrCode) = mapSrcReg addr
                            val (initVal, initCode) = mapSrcReg init
                        in
                            InitialiseMem{size=sizeVal, addr=addrVal, init=initVal} :: initCode @ addrCode @ sizeCode @ code
                        end

                    |   pushRegisters({instr=InitialisationComplete, ...}, code) = InitialisationComplete :: code

                    |   pushRegisters({instr=JumpLoop{regArgs, stackArgs, checkInterrupt, workReg}, ...}, code) =
                        let
                            (* Normally JumpLoop will be the last item in a block but it is possible that we've
                               added a reset-stack after it. *)
                            fun getValues [] = ([], [], [])
                            |   getValues ((source, PReg n) :: rest) =
                                let
                                    val (otherRegArgs, otherStackArgs, otherCode) = getValues rest
                                in
                                    case Array.sub(pregMap, n) of
                                        ToPReg lReg =>
                                        let
                                            val (sourceVal, sourceCode) = mapSource source
                                        in
                                            ((sourceVal, lReg) :: otherRegArgs, otherStackArgs, sourceCode @ otherCode)
                                        end
                                    |   ToStack(stackLoc, stackC as StackLoc{size, ...}) =>
                                        let
                                            val (sourceVal, sourceCode) = mapSource source
                                            val stackOff = !stackCount - stackLoc - size
                                        in
                                            (otherRegArgs, (sourceVal, stackOff, stackC) :: otherStackArgs, sourceCode @ otherCode)
                                        end
                                    |   Unset => (* Drop it.  It's never used. Probably a unit argument. *)
                                            (otherRegArgs, otherStackArgs, otherCode)
                                end
                            val (newRegArguments, newStackArgs, sourceCode) = getValues regArgs
                            fun loadStackArg((source, _, destC), (otherLoads, otherArgs)) =
                            let
                                val (sourceVal, sourceCode) = mapSource source
                                val (newOffset, newContainer) = mapContainerAndStack(destC, 0)
                            in
                                (sourceCode @ otherLoads, (sourceVal, newOffset, newContainer) :: otherArgs)
                            end
                
                            val (stackArgLoads, oldStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                            val check = case checkInterrupt of NONE => NONE | SOME _ => SOME []
                            (* Map the work reg if it exists already but get a new one if
                               we now have stack args. *)
                            val newWorkReg =
                                case (workReg, newStackArgs) of
                                    (SOME r, _) => SOME(mapWorkReg r)
                                |   (NONE, []) => NONE
                                |   _ => SOME(newPReg RegPropGeneral)
                        in
                            JumpLoop{ regArgs=newRegArguments, stackArgs=oldStackArgs @ newStackArgs, checkInterrupt=check, workReg=newWorkReg} ::
                                     sourceCode @ stackArgLoads @ code
                        end

                    |   pushRegisters({instr=RaiseExceptionPacket{packetReg}, ...}, code) =
                        let
                            val (packetVal, packetCode) = mapSrcReg packetReg
                        in
                            RaiseExceptionPacket{packetReg=packetVal} :: packetCode @ code
                        end

                    |   pushRegisters({instr=ReserveContainer{size, container}, ...}, code) =
                        let
                            val newContainer = mapDestContainer(container, !stackCount)
                            val () = pushItemToStack(OriginalEntry{stackLoc=container})
                        in
                            ReserveContainer{size=size, container=newContainer} :: code
                        end

                    |   pushRegisters({instr=IndexedCaseOperation{testReg, workReg}, ...}, code) =
                        let
                            val (srcVal, srcCode) = mapSrcReg(testReg)
                            val newWorkReg = mapWorkReg workReg
                        in
                            (* This is an unconditional branch. *)
                            IndexedCaseOperation{testReg=srcVal, workReg=newWorkReg} :: srcCode @ code
                        end

                    |   pushRegisters({instr=LockMutable{addr}, ...}, code) =
                        let
                            val (addrVal, addrCode) = mapSrcReg(addr)
                        in
                            LockMutable{addr=addrVal} :: addrCode @ code
                        end
 
                    |   pushRegisters({instr=WordComparison{arg1, arg2, ccRef}, ...}, code) =
                        let
                            val (loadedOp1, op1Code) = mapSrcReg arg1
                            val (op2Val, op2Code) = mapSource arg2
                        in
                            WordComparison{arg1=loadedOp1, arg2=op2Val, ccRef=ccRef} :: op2Code @ op1Code @ code
                        end

                    |   pushRegisters({instr=PushExceptionHandler{workReg}, ...}, code) =
                        let
                            val newWorkReg = mapWorkReg workReg
                            (* Add a handler entry to the stack. *)
                            val () = pushItemToStack HandlerEntry
                        in
                            PushExceptionHandler{workReg=newWorkReg} :: code
                        end

                    |   pushRegisters({instr=PopExceptionHandler{workReg, ...}, ...}, code) =
                        let
                            val newWorkReg = mapWorkReg workReg
                            (* Appears at the end of the block whose exceptions are being handled.  Delete the
                               handler and anything above it. *)
                            (* Get the state after removing the handler. *)
                            fun popContext ([], _) = raise InternalError "pushRegisters - pop handler"
                            |   popContext (HandlerEntry :: tl, new) = (tl, new-2)
                            |   popContext (OriginalEntry{stackLoc=StackLoc{size, ...}, ...} :: tl, new) = popContext(tl, new-size)
                            |   popContext (NewEntry _ :: tl, new) = popContext(tl, new-1)
                            val (newStack, nnCount) = popContext(!stack, !stackCount)
                            val () = stack := newStack
                            val oldStackPtr = ! stackCount
                            val () = stackCount := nnCount
                            (* Reset the stack to just above the two words of the handler. *)
                            val resetCode =
                                if oldStackPtr <> nnCount+2
                                then [ResetStackPtr{numWords=oldStackPtr-nnCount-2, preserveCC=false}]
                                else []
                        in
                            PopExceptionHandler{workReg=newWorkReg} :: resetCode @ code
                        end

                    |   pushRegisters({instr=BeginHandler{packetReg, workReg, ...}, ...}, code) =
                        let
                            (* Clear the cache.  This may not be necessary if we are only handling
                               locally generated exceptions but keep it for the moment. *)
                            val () = clearCache()
                            (* Start of a handler.  The top active entry should be the handler. *)
                            val () =
                                case !stack of
                                    HandlerEntry :: tl => stack := tl
                                |   _ => raise InternalError "pushRegisters: BeginHandler"
                            val () = stackCount := !stackCount - 2
                            val newWorkReg = mapWorkReg workReg
                            val (pktReg, pktCode) = mapDestReg(packetReg)
                        in
                            pktCode @ BeginHandler{packetReg=pktReg, workReg=newWorkReg} :: code
                        end

                    |   pushRegisters({instr=ReturnResultFromFunction{resultReg, numStackArgs}, ...}, code) =
                        let
                            val (resultValue, loadResult) = mapSrcReg resultReg
                            val resetCode =
                                if !stackCount = 0 then [] else [ResetStackPtr{numWords= !stackCount, preserveCC=false}]
                        in
                            ReturnResultFromFunction{resultReg=resultValue, numStackArgs=numStackArgs} :: resetCode @ loadResult @ code
                        end

                    |   pushRegisters({instr=ArithmeticFunction{oper, resultReg, operand1, operand2, ccRef}, ...}, code) =
                        let
                            val (loadedOp1, op1Code) = mapSrcReg operand1
                            val (op2Val, op2Code) = mapSource operand2
                            val (destVal, destCode) = mapDestReg resultReg
                        in
                            destCode @ ArithmeticFunction{oper=oper, resultReg=destVal, operand1=loadedOp1, operand2=op2Val, ccRef=ccRef} ::
                                    op2Code @ op1Code @ code
                        end

                    |   pushRegisters({instr=TestTagBit{arg, ccRef}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource arg
                        in
                            TestTagBit{arg=sourceVal, ccRef=ccRef} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=PushValue{arg, container, ...}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource arg
                            (* This was a push from a previous pass.  Treat as a container of size 1. *)
                            val newContainer = mapDestContainer(container, !stackCount)
                            val () = pushItemToStack(OriginalEntry{stackLoc=container})
                        in
                            PushValue{arg=sourceVal, container=newContainer} :: sourceCode @ code
                        end
                    
                    |   pushRegisters({instr=CopyToCache _, ...}, code) = code
                            (* This was added on a previous pass.  Discard it.  If we are going to cache this again we'll
                               add new CopyToCache instructions. *)

                    |   pushRegisters({instr=ResetStackPtr _, ...}, code) = code
                            (* Added in a previous pass - discard it. *)

                    |   pushRegisters({instr=StoreToStack{source, container, field, ...}, ...}, code) =
                        let
                            val (loadedSource, sourceCode) = mapAndLoad source
                            (* We can't have a memory-memory store so we have to load the source if it's now on the stack. *)
                            val (newOffset, newContainer) = mapContainerAndStack(container, field)
                        in
                            StoreToStack{source=loadedSource, container=newContainer, field=field, stackOffset=newOffset} ::
                                    sourceCode @ code
                        end

                    |   pushRegisters({instr=TagValue{source, dest as PReg dReg, isSigned}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                            val _ = setTagCache(dReg, isSigned, sourceVal)
                        in
                            destCode @ TagValue{source=sourceVal, dest=destVal, isSigned=isSigned} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=UntagValue{source as PReg srcReg, dest, isSigned, ...}, ...}, code) =
                        let
                            val (loadedSource, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                            val cache = findCachedTagged(srcReg, isSigned)
                        in
                            destCode @ UntagValue{source=loadedSource, dest=destVal, isSigned=isSigned, cache=cache} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=LoadEffectiveAddress{base, offset, index, dest}, ...}, code) =
                        let
                            val (baseVal, baseCode) =
                                case base of
                                    NONE => (NONE, [])
                                |   SOME bReg =>
                                        let val (newBReg, regCode) = mapSrcReg(bReg) in (SOME newBReg, regCode) end
                            val (indexVal, indexCode) = mapIndex index
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ LoadEffectiveAddress{base=baseVal, offset=offset, index=indexVal, dest=destVal} :: indexCode @ baseCode @ code
                        end

                    |   pushRegisters({instr=ShiftOperation{shift, resultReg, operand, shiftAmount, ccRef}, ...}, code) =
                        let
                            val (opVal, opCode) = mapSrcReg operand
                            val (shiftVal, shiftCode) = mapSource shiftAmount
                            val (destVal, destCode) = mapDestReg resultReg
                        in
                            destCode @ ShiftOperation{shift=shift, resultReg=destVal, operand=opVal, shiftAmount=shiftVal, ccRef=ccRef} ::
                                    shiftCode @ opCode @ code
                        end

                    |   pushRegisters({instr=Multiplication{resultReg, operand1, operand2, ccRef}, ...}, code) =
                        let
                            val (op1Val, op1Code) = mapSrcReg operand1
                            val (op2Val, op2Code) = mapSource operand2
                            val (destVal, destCode) = mapDestReg resultReg
                        in
                            destCode @ Multiplication{resultReg=destVal, operand1=op1Val, operand2=op2Val, ccRef=ccRef} :: op2Code @ op1Code @ code
                        end

                    |   pushRegisters({instr=Division{isSigned, dividend, divisor, quotient, remainder}, ...}, code) =
                        let
                            val (dividendVal, dividendCode) = mapSrcReg dividend
                            val (divisorVal, divisorCode) = mapSource divisor
                            val (quotVal, quotCode) = mapDestReg quotient
                            val (remVal, remCode) = mapDestReg remainder
                        in
                            remCode @ quotCode @
                                    Division{isSigned=isSigned, dividend=dividendVal, divisor=divisorVal, quotient=quotVal, remainder=remVal} ::
                                        divisorCode @ dividendCode @ code
                        end

                    |   pushRegisters({instr=AtomicExchangeAndAdd{base, source}, ...}, code) =
                        let
                            val (baseVal, baseCode) = mapSrcReg(base)
                            val (sourceVal, sourceCode) = mapSrcReg source
                            (* The "source" is also a result and must be in a register.  It's an untagged reg
                               so it shouldn't have been marked as to be pushed. *)
                            val _ = case sourceCode of [] => () | _ => raise InternalError "pushRegisters - AtomicExchangeAndAdd"
                        in
                            AtomicExchangeAndAdd{base=baseVal, source=sourceVal} :: baseCode @ code
                        end

                    |   pushRegisters({instr=BoxValue{boxKind, source, dest as PReg dReg, ...}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                            (* We can cache the boxed value except if this is an X87 box.
                               We can't cache X87 values because there's effectively only one register
                               and this box instruction uses FSTP (store and POP). *)
                            val cacheCode =
                                if Vector.sub(pushVec, dReg) orelse boxKind = BoxX87 then []
                                else
                                let
                                    val newCacheReg = newPReg RegPropCacheUntagged
                                    val moveKind = case boxKind of BoxLargeWord => MoveWord | BoxX87 => MoveDouble | BoxSSE2 => MoveDouble
                                    val _ = setMemoryCache(dReg, 0, newCacheReg, true, moveKind)
                                in
                                    [CopyToCache{source=sourceVal, dest=newCacheReg, kind=moveKind}]
                                end
                        in
                            cacheCode @ destCode @ BoxValue{boxKind=boxKind, source=sourceVal, dest=destVal, saveRegs=[]} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, ...}, code) =
                        let
                            val (vec1Val, vec1Code) = mapSrcReg vec1Addr
                            val (vec2Val, vec2Code) = mapSrcReg vec2Addr
                            val (lengthVal, lengthCode) = mapSrcReg length
                        in
                            CompareByteVectors{vec1Addr=vec1Val, vec2Addr=vec2Val, length=lengthVal, ccRef=ccRef} ::
                                    lengthCode @ vec2Code @ vec1Code @ code
                        end

                    |   pushRegisters({instr=BlockMove{srcAddr, destAddr, length, isByteMove}, ...}, code) =
                        let
                            val (srcVal, srcCode) = mapSrcReg srcAddr
                            val (destVal, destCode) = mapSrcReg destAddr
                            val (lengthVal, lengthCode) = mapSrcReg length
                            (* For safety clear the memory cache here.  That may not be necessary. *)
                            val () = clearMemoryCache()
                        in
                            BlockMove{srcAddr=srcVal, destAddr=destVal, length=lengthVal, isByteMove=isByteMove} ::
                                    lengthCode @ destCode @ srcCode @ code
                        end

                    |   pushRegisters({instr=X87Compare{arg1, arg2, ccRef}, ...}, code) =
                        let
                            val (arg1Val, arg1Code) = mapSrcReg arg1
                            val (arg2Val, arg2Code) = mapSource arg2
                        in
                            X87Compare{arg1=arg1Val, arg2=arg2Val, ccRef=ccRef} :: arg2Code @ arg1Code @ code
                        end

                    |   pushRegisters({instr=SSE2Compare{arg1, arg2, ccRef}, ...}, code) =
                        let
                            val (arg1Val, arg1Code) = mapSrcReg arg1
                            val (arg2Val, arg2Code) = mapSource arg2
                        in
                            SSE2Compare{arg1=arg1Val, arg2=arg2Val, ccRef=ccRef} :: arg2Code @ arg1Code @ code
                        end

                    |   pushRegisters({instr=X87FPGetCondition{dest, ccRef}, ...}, code) =
                        let
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ X87FPGetCondition{dest=destVal, ccRef=ccRef} :: code
                        end

                    |   pushRegisters({instr=X87FPArith{opc, resultReg, arg1, arg2}, ...}, code) =
                        let
                            val (arg1Val, arg1Code) = mapSrcReg arg1
                            val (arg2Val, arg2Code) = mapSource arg2
                            val (destVal, destCode) = mapDestReg resultReg
                        in
                            destCode @ X87FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                                    arg2Code @ arg1Code @ code
                        end
    
                    |   pushRegisters({instr=X87FPUnaryOps{fpOp, dest, source}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ X87FPUnaryOps{fpOp=fpOp, dest=destVal, source=sourceVal} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=X87Float{dest, source}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ X87Float{dest=destVal, source=sourceVal} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=SSE2Float{dest, source}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSource source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ SSE2Float{dest=destVal, source=sourceVal} :: sourceCode @ code
                        end
    
                    |   pushRegisters({instr=SSE2FPArith{opc, resultReg, arg1, arg2}, ...}, code) =
                        let
                            val (arg1Val, arg1Code) = mapSrcReg arg1
                            val (arg2Val, arg2Code) = mapSource arg2
                            val (destVal, destCode) = mapDestReg resultReg
                        in
                            destCode @ SSE2FPArith{opc=opc, resultReg=destVal, arg1=arg1Val, arg2=arg2Val} ::
                                    arg2Code @ arg1Code @ code
                        end
        

                    (* Find the common cache state. *)
                    val () = setCommonCacheState(List.map (#cache o asub blockOutput) (asub blockRefs blockNo))

                    local
                        fun doPush(instr as {kill, ...}, code) =
                        let
                            val newCode = pushRegisters(instr, code)
                            (* Can we pop the stack? *)
                            val stackReset =
                                case setToList kill of
                                    [] => []
                                |   killList =>
                                    let
                                        (* See if any of the kill items are at the top of the stack.
                                           If they are we can pop them and perhaps items we've
                                           previously marked for deletion but not been able to pop. *)
                                        val oldStack = !stackCount
                                        fun checkAndAdd(r, output) =
                                            case Array.sub(pregMap, r) of
                                                ToStack(stackLoc, StackLoc{size, ...}) =>
                                                    if stackLoc < 0
                                                    then r :: output (* We can have arguments and return address. *)
                                                    else if !stackCount = stackLoc+size
                                                    then
                                                    (
                                                        stack := tl (!stack);
                                                        stackCount := stackLoc;
                                                        output
                                                    )
                                                    else r :: output
                                            |   _ => r :: output
                                        val toAdd = List.foldl checkAndAdd [] killList
                                        fun reprocess list =
                                        let
                                            val prevStack = !stackCount
                                            val outlist = List.foldl checkAndAdd [] list
                                        in
                                            if !stackCount = prevStack
                                            then list
                                            else reprocess outlist
                                        end
                                        val () =
                                            if !stackCount = oldStack
                                            then deletedItems := toAdd @ !deletedItems
                                            else deletedItems := reprocess(toAdd @ !deletedItems)
                                    in
                                        if !stackCount = oldStack then []
                                        else [ResetStackPtr{numWords=oldStack - !stackCount, preserveCC=true (* In case*)}]
                                    end
                        in
                            stackReset @ newCode
                        end
                    in
                        val codeResult = List.foldl doPush [] block
                        val outputCount = ! stackCount
                        val results = {code=codeResult, cache=getCache(), stackCount= outputCount}
                        val stateResult = { stackCount= outputCount, stack= !stack }
                        
                        val () = Array.update(blockOutput, blockNo,  results)
                    end

                    val addSet =
                        case flow of
                            ExitCode => []
                        |   IndexedBr cases => cases
                        |   Unconditional dest => [dest]
                        |   Conditional {trueJump, falseJump, ...} => [falseJump, trueJump]
                        |   SetHandler { handler, continue } => [handler, continue]
                        |   UnconditionalHandle _ => []
                        |   ConditionalHandle { continue, ...} => [continue]
                        
                    val addItems = List.map(fn m => (m, stateResult)) addSet
                in
                    processBlocks(addItems @ stillToDo)
                end
        in
            val () = processBlocks([(0, {stack=[], stackCount=0})])
        end


        (* Put together the result code and blocks. *)
        local
            fun createBlock blockNo =
            (* Skip unreferenced blocks apart from block 0. *)
            if blockNo <> 0 andalso null (asub blockRefs blockNo)
            then BasicBlock{block=[], flow=ExitCode}
            else
            let
                val ExtendedBasicBlock{ flow, ...} = vsub code blockNo
                val {code=codeResult, stackCount=outputCount, ...} = asub blockOutput blockNo
                
                (* Process the successor.  If we need a stack adjustment this will require
                   an adjustment block.  TODO: We could put a pre-adjustment if we only have one
                   branch to this block. *)
                fun matchStacks targetBlock =
                let
                    (* Process the destination.  If it hasn't been processed. *)
                    val {expectedInput, ...} = valOf (asub inputStackSizes targetBlock)
                in
                    if expectedInput = outputCount
                    then targetBlock
                    else
                    let
                        val _ = outputCount > expectedInput orelse raise InternalError "adjustStack"
                        val adjustCode = [ResetStackPtr{numWords=outputCount-expectedInput, preserveCC=true (* For the moment *)}]
                        val newBlock = BasicBlock{block=adjustCode, flow=Unconditional targetBlock}
                        val newBlockNo = !blockCounter before blockCounter := !blockCounter+1
                        val () = extraBlocks := newBlock :: !extraBlocks
                    in
                        newBlockNo
                    end
                end
                
                val (finalCode, newFlow) =
                    case flow of
                        ExitCode => (codeResult, ExitCode)
                    
                    |   Unconditional m =>
                        let
                            (* Process the block.  Since we're making an unconditional jump
                               we can include any stack adjustment needed to match the
                               destination in here.  In particular this includes loops. *)
                            val {expectedInput, reqCC} = valOf (asub inputStackSizes m)
                            val resultCode =
                                if expectedInput = outputCount
                                then codeResult
                                else ResetStackPtr{numWords=outputCount-expectedInput, preserveCC=reqCC} :: codeResult
                        in
                            (resultCode, Unconditional m)
                        end

                        (* For any of these, if we need to adjust the stack we have to add an
                           adjustment block. *)
                    |   Conditional {trueJump, falseJump, ccRef, condition} =>
                        (codeResult,
                            Conditional{trueJump=matchStacks trueJump, falseJump=matchStacks falseJump,
                                ccRef=ccRef, condition=condition})
                    
                    |   SetHandler{ handler, continue } =>
                        (codeResult, SetHandler{ handler=matchStacks handler, continue=matchStacks continue})
                    
                    |   IndexedBr cases => (codeResult, IndexedBr(map matchStacks cases))
                    
                    |   u as UnconditionalHandle _ => (codeResult, u)
                    
                    |   c as ConditionalHandle{ continue, ... } =>
                        let
                            (* As for unconditional branch *)
                            val {expectedInput, reqCC} = valOf (asub inputStackSizes continue)
                            val resultCode =
                                if expectedInput = outputCount
                                then codeResult
                                else ResetStackPtr{numWords=outputCount-expectedInput, preserveCC=reqCC} :: codeResult
                        in
                            (resultCode, c)
                        end
             in
                BasicBlock{block=List.rev finalCode, flow=newFlow}
            end
        in
            val resultBlocks = Vector.tabulate(numberOfBlocks, createBlock)
        end
        
        (* Add any extra blocks to the result. *)
        val finalResult =
            case !extraBlocks of
                [] => resultBlocks
            |   blocks => Vector.concat[resultBlocks, Vector.fromList(List.rev blocks)]
        
        val pregProperties = Vector.fromList(List.rev(! pregPropList))
    in
        {code=finalResult, pregProps=pregProperties, maxStack= !maxStack}
    end

    structure Sharing =
    struct
        type x86ICode = x86ICode
        and preg = preg
        and intSet = intSet
        and extendedBasicBlock = extendedBasicBlock
        and basicBlock = basicBlock
        and regProperty = regProperty
    end
end;
