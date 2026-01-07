(*
    Copyright David C. J. Matthews 2021-3

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

functor Arm64PushRegisters(
    structure Arm64ICode: ARM64ICODE
    structure IntSet: INTSET
    structure Identify: ARM64IDENTIFYREFERENCES
    sharing Arm64ICode.Sharing = Identify.Sharing = IntSet
) : ARM64PUSHREGISTERS
=
struct
    open Arm64ICode
    open IntSet
    open Identify
    
    type basicBlockAbstract = (preg, pregOrZero, preg) basicBlock
    
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


    fun addRegisterPushes{code: extendedBasicBlock vector, pushVec: bool vector, pregProps, firstPass=_} =
    let
        val maxPRegs = Vector.length pregProps
        val numberOfBlocks = Vector.length code
        (* Output registers and properties. *)
        val pregCounter = ref 0
        val pregPropList = ref []
        val pregMap = Array.array(maxPRegs, Unset)
 
        val maxStack = ref 0
        (* The stack size we've assumed for the block.  Also indicates if
           a block has already been processed. *)
        val inputStackSizes = Array.array(numberOfBlocks, NONE)
        (* The result of processing a block. *)
        val blockOutput = Array.array(numberOfBlocks, {code=[], stackCount=0})
        
        (* Extra blocks to adjust the stack are added here. *)
        val extraBlocks: basicBlockAbstract list ref = ref []
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
                       increases the chances that we will have cached items.
                       TODO: This is no longer necessary since we don't do any caching here
                       now so could may be simplified. *)
                    fun available(dest, _) = List.all haveProcessed (Array.sub(blockRefs, dest))
                    val (blockNo, lastOutputState) =
                        case List.find available stillToDo of
                            SOME c => c
                        |   NONE => head
                
                    (* This is the first time we've come to this block.  *)
                    val ExtendedBasicBlock{ block, flow, imports, passThrough, loopRegs, initialStacks, ...} = vsub code blockNo

                    (* Remove any items from the input state that are no longer needed for
                       this block.  They could be local to the previous block or needed by
                       a different successor.  Although the values in loopRegs are not
                       required the stack space is so that they can be updated. *)
                    fun removeItems(result as {stack=[], stackCount=0}) = result
                
                    |   removeItems{stack=[], ...} = raise InternalError "removeItems - stack size"

                    |   removeItems (thisStack as {stack=NewEntry{pregNo} :: rest, stackCount}) =
                        if member(pregNo, imports) orelse member(pregNo, passThrough) orelse member(pregNo, loopRegs)
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
                    val () = Array.update(inputStackSizes, blockNo, SOME newSp)
                    
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
                    fun mapSrcReg(PReg n) =
                        case Array.sub(pregMap, n) of
                            Unset => raise InternalError "mapSrcReg - unset"
                        |   ToPReg preg => (preg, [])
                        |   ToStack(stackLoc, container as StackLoc{size, ...}) =>
                            let
                                (* Make a new untagged register. That will prevent us pushing it if
                                   we have to spill registers. *)
                                val newReg = newPReg RegPropUntagged
                            in
                                (newReg,
                                    [LoadStack{wordOffset= !stackCount-stackLoc-size, container=container,
                                               field=0, dest=newReg}])
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
                            (newReg, [PushToStack{source= newReg, container=newContainer, copies=1}])
                        end
                        else
                        let
                        (* See if we already have a number for it.  We may encounter the same preg
                           as a destination when returning the result from a conditional in which
                           case we have to use the same number.  We shouldn't have pushed it. *)
                            val newReg =
                                case (currentLocation, kind) of
                                    (Unset, _) =>
                                    let
                                        val newReg = newPReg kind
                                        val () = Array.update(pregMap, n, ToPReg newReg)
                                    in
                                        newReg
                                    end
                                |   (ToPReg preg, RegPropMultiple) => preg
                                |   _ => raise InternalError "mapDestReg - multiply defined non-merge reg"
                        in
                            (newReg, [])
                        end
                    end
                    
                    (* Optional destination for arithmetic and logical ops. *)
                    fun mapOptDest ZeroReg = (ZeroReg, [])
                    |   mapOptDest (SomeReg destReg) =
                        let val (destVal, destCode) = mapDestReg destReg in (SomeReg destVal, destCode) end

                    fun mapOptSrc ZeroReg = (ZeroReg, [])
                    |   mapOptSrc (SomeReg srcReg) =
                        let val (srcVal, srcCode) = mapSrcReg srcReg in (SomeReg srcVal, srcCode) end

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

                    (* Map a function argument which could be a register or a stack entry.  A register
                       entry could have been pushed. *)
                    fun mapArgument(ArgInReg (PReg r)) =
                        (
                            case Array.sub(pregMap, r) of
                                Unset => raise InternalError "mapSource - unset"
                            |   ToPReg preg => ArgInReg preg
                            |   ToStack(stackLoc, container as StackLoc{size, ...}) =>
                                ArgOnStack{wordOffset= !stackCount-stackLoc-size, container=container, field=0}
                        )

                    |   mapArgument(ArgOnStack{container, field, ...}) =
                            let
                                val (newOffset, newContainer) = mapContainerAndStack(container, field)
                            in
                                ArgOnStack{container=newContainer, wordOffset=newOffset, field=field}
                            end

                    (* Rewrite the code, replacing any registers that need to be pushed with references to
                       the stack.  The result is built up in reverse order and then reversed. *)
                    fun pushRegisters({instr=MoveRegister{ source, dest as PReg dReg }, ...}, code) =
                        if Vector.sub(pushVec, dReg)
                        then (* We're going to push this. *)
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                            (* TODO: Since we're pushing it we don't need to move it first. *)
                        in
                            destCode @ MoveRegister { source=sourceVal, dest=destVal} :: sourceCode @ code
                        end
                        else
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ MoveRegister { source=sourceVal, dest=destVal} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=LoadNonAddressConstant { dest, source}, ...}, code) =
                        let
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ LoadNonAddressConstant { dest=destVal, source=source} :: code
                        end

                    |   pushRegisters({instr=LoadFPConstant { dest, source, floatSize}, ...}, code) =
                        let
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ LoadFPConstant { dest=destVal, source=source, floatSize=floatSize} :: code
                        end

                    |   pushRegisters({instr=LoadAddressConstant { dest, source}, ...}, code) =
                        let
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ LoadAddressConstant { dest=destVal, source=source} :: code
                        end

                    |   pushRegisters({instr=LoadWithConstantOffset { base, dest, byteOffset, loadType}, ...}, code) =
                        let
                            val (baseVal, baseCode) = mapSrcReg base
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @
                                LoadWithConstantOffset { base=baseVal, dest=destVal, byteOffset=byteOffset, loadType=loadType} ::
                                    baseCode @ code
                        end

                    |   pushRegisters({instr=LoadFPWithConstantOffset { base, dest, byteOffset, floatSize}, ...}, code) =
                        let
                            val (baseVal, baseCode) = mapSrcReg base
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @
                                LoadFPWithConstantOffset { base=baseVal, dest=destVal, byteOffset=byteOffset, floatSize=floatSize} ::
                                    baseCode @ code
                        end

                    |   pushRegisters({instr=LoadWithIndexedOffset { base, dest, index, loadType, signExtendIndex}, ...}, code) =
                        let
                            val (baseVal, baseCode) = mapSrcReg base
                            val (indexVal, indexCode) = mapSrcReg index
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @
                                LoadWithIndexedOffset { base=baseVal, dest=destVal, index=indexVal,
                                                        loadType=loadType, signExtendIndex=signExtendIndex} ::
                                    indexCode @ baseCode @ code
                        end

                    |   pushRegisters({instr=LoadFPWithIndexedOffset { base, dest, index, floatSize, signExtendIndex}, ...}, code) =
                        let
                            val (baseVal, baseCode) = mapSrcReg base
                            val (indexVal, indexCode) = mapSrcReg index
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @
                                LoadFPWithIndexedOffset { base=baseVal, dest=destVal, index=indexVal,
                                                          floatSize=floatSize, signExtendIndex=signExtendIndex} ::
                                    indexCode @ baseCode @ code
                        end

                    |   pushRegisters({instr=GetThreadId { dest}, ...}, code) =
                        let
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ GetThreadId { dest=destVal} :: code
                        end

                    |   pushRegisters({instr=ObjectIndexAddressToAbsolute { source, dest}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ ObjectIndexAddressToAbsolute { source=sourceVal, dest=destVal} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=AbsoluteToObjectIndex { source, dest}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ AbsoluteToObjectIndex { source=sourceVal, dest=destVal} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=AllocateMemoryFixed { bytesRequired, dest, ...}, ...}, code) =
                        let
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ AllocateMemoryFixed { dest=destVal, bytesRequired=bytesRequired, saveRegs={gcUpdate=[], noUpdate=[]}} :: code
                        end

                    |   pushRegisters({instr=AllocateMemoryVariable{size, dest, ...}, ...}, code) =
                        let
                            val (sizeVal, sizeCode) = mapSrcReg size
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ AllocateMemoryVariable{size=sizeVal, dest=destVal, saveRegs={gcUpdate=[], noUpdate=[]}} :: sizeCode @ code
                        end

                    |   pushRegisters({instr=InitialiseMem{size, addr, init}, ...}, code) =
                        let
                            val (sizeVal, sizeCode) = mapSrcReg size
                            val (addrVal, addrCode) = mapSrcReg addr
                            val (initVal, initCode) = mapSrcReg init
                        in
                            InitialiseMem{size=sizeVal, addr=addrVal, init=initVal} :: initCode @ addrCode @ sizeCode @ code
                        end

                    |   pushRegisters({instr=BeginLoop, ...}, code) = BeginLoop :: code

                    |   pushRegisters({instr=JumpLoop{regArgs, stackArgs, checkInterrupt}, ...}, code) =
                        let
                            (* Normally JumpLoop will be the last item in a block but it is possible that we've
                               added a reset-stack after it. *)
                            fun getValues [] = ([], [])
                            |   getValues ({src=source, dst=PReg n} :: rest) =
                                let
                                    val (otherRegArgs, otherStackArgs) = getValues rest
                                in
                                    case Array.sub(pregMap, n) of
                                        ToPReg lReg => ({src=mapArgument source, dst=lReg} :: otherRegArgs, otherStackArgs)

                                    |   ToStack(stackloc, stackC as StackLoc{size, ...}) =>
                                        let
                                            val sourceVal = mapArgument source
                                            val stackOff = !stackCount - stackloc - size
                                        in
                                            (otherRegArgs, {src=sourceVal, wordOffset=stackOff, stackloc=stackC} :: otherStackArgs)
                                        end

                                    |   Unset =>
                                        (* Drop it.  It's never used. This can happen if we are folding a
                                           function over a list such that it always returns the last value and
                                           then discard the result of the fold. *)
                                            (otherRegArgs, otherStackArgs)
                                end
                            val (newRegArguments, newStackArgs) = getValues regArgs
                            fun loadStackArg({src=source, stackloc=destC, ...}, otherArgs) =
                            let
                                val sourceVal = mapArgument source
                                val (newOffset, newContainer) = mapContainerAndStack(destC, 0)
                            in
                                {src=sourceVal, wordOffset=newOffset, stackloc=newContainer} :: otherArgs
                            end
                
                            val oldStackArgs = List.foldr loadStackArg [] stackArgs
                            val check = case checkInterrupt of NONE => NONE | SOME _ => SOME {gcUpdate=[], noUpdate=[]}
                        in
                            JumpLoop{ regArgs=newRegArguments, stackArgs=oldStackArgs @ newStackArgs, checkInterrupt=check} :: code
                        end

                    |   pushRegisters({instr=StoreWithConstantOffset { base, source, byteOffset, loadType}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (baseVal, baseCode) = mapSrcReg base 
                        in
                            StoreWithConstantOffset{ base=baseVal, source=sourceVal, byteOffset=byteOffset, loadType=loadType} ::
                                baseCode @ sourceCode @ code
                        end

                    |   pushRegisters({instr=StoreFPWithConstantOffset { base, source, byteOffset, floatSize}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (baseVal, baseCode) = mapSrcReg base 
                        in
                            StoreFPWithConstantOffset{ base=baseVal, source=sourceVal, byteOffset=byteOffset, floatSize=floatSize} ::
                                baseCode @ sourceCode @ code
                        end

                    |   pushRegisters({instr=StoreWithIndexedOffset { base, source, index, loadType, signExtendIndex}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (baseVal, baseCode) = mapSrcReg base 
                            val (indexVal, indexCode) = mapSrcReg index 
                        in
                            StoreWithIndexedOffset{ base=baseVal, source=sourceVal, index=indexVal,
                                                    loadType=loadType, signExtendIndex=signExtendIndex} ::
                                indexCode @ baseCode @ sourceCode @ code
                        end

                    |   pushRegisters({instr=StoreFPWithIndexedOffset { base, source, index, floatSize, signExtendIndex}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (baseVal, baseCode) = mapSrcReg base 
                            val (indexVal, indexCode) = mapSrcReg index 
                        in
                            StoreFPWithIndexedOffset{ base=baseVal, source=sourceVal, index=indexVal,
                                                      floatSize=floatSize, signExtendIndex=signExtendIndex} ::
                                indexCode @ baseCode @ sourceCode @ code
                        end

                    |   pushRegisters({instr=AddSubImmediate{ source, dest, ccRef, immed, isAdd, length }, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapOptDest dest
                        in
                            destCode @ AddSubImmediate { source=sourceVal, dest=destVal, ccRef=ccRef,
                                                         immed=immed, isAdd=isAdd, length=length} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=AddSubRegister{ base, shifted, dest, ccRef, isAdd, length, shift}, ...}, code) =
                        let
                            val (op1Val, op1Code) = mapSrcReg base
                            val (op2Val, op2Code) = mapSrcReg shifted
                            val (destVal, destCode) = mapOptDest dest
                        in
                            destCode @ AddSubRegister { base=op1Val, shifted=op2Val, dest=destVal, ccRef=ccRef,
                                                         isAdd=isAdd, length=length, shift=shift} :: op2Code @ op1Code @ code
                        end

                    |   pushRegisters({instr=LogicalImmediate{ source, dest, ccRef, immed, logOp, length }, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapOptDest dest
                        in
                            destCode @ LogicalImmediate { source=sourceVal, dest=destVal, ccRef=ccRef,
                                                          immed=immed, logOp=logOp, length=length} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=LogicalRegister{ base, shifted, dest, ccRef, logOp, length, shift}, ...}, code) =
                        let
                            val (op1Val, op1Code) = mapSrcReg base
                            val (op2Val, op2Code) = mapSrcReg shifted
                            val (destVal, destCode) = mapOptDest dest
                        in
                            destCode @ LogicalRegister { base=op1Val, shifted=op2Val, dest=destVal, ccRef=ccRef,
                                                         logOp=logOp, length=length, shift=shift} :: op2Code @ op1Code @ code
                        end

                    |   pushRegisters({instr=ShiftRegister{ direction, dest, source, shift, opSize}, ...}, code) =
                        let
                            val (srcVal, op1Code) = mapSrcReg source
                            val (shiftVal, op2Code) = mapSrcReg shift
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ ShiftRegister { source=srcVal, shift=shiftVal, dest=destVal,
                                                       direction=direction, opSize=opSize} :: op2Code @ op1Code @ code
                        end

                    |   pushRegisters({instr=CountLeadingZeroBits{ dest, source, opSize}, ...}, code) =
                        let
                            val (srcVal, op1Code) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ CountLeadingZeroBits { source=srcVal, dest=destVal, opSize=opSize} :: op1Code @ code
                        end

                    |   pushRegisters({instr=Multiplication{ kind, dest, sourceA, sourceM, sourceN }, ...}, code) =
                        let
                            val (srcAVal, srcACode) = mapOptSrc sourceA
                            val (srcMVal, srcMCode) = mapSrcReg sourceM
                            val (srcNVal, srcNCode) = mapSrcReg sourceN
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ Multiplication { kind=kind, sourceA=srcAVal, sourceM=srcMVal, sourceN=srcNVal, dest=destVal} ::
                                srcNCode @ srcMCode @ srcACode @ code
                        end

                    |   pushRegisters({instr=Division{ isSigned, dest, dividend, divisor, opSize }, ...}, code) =
                        let
                            val (dividendVal, dividendCode) = mapSrcReg dividend
                            val (divisorVal, divisorCode) = mapSrcReg divisor
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ Division { isSigned=isSigned, dividend=dividendVal, divisor=divisorVal, dest=destVal,
                                                       opSize=opSize} :: divisorCode @ dividendCode @ code
                        end

                    |   pushRegisters({instr=BeginFunction {regArgs, fpRegArgs, stackArgs}, ...}, code) =
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
                                val (newReg, newCode) = mapDestReg preg
                            in
                                ((newReg, rreg) :: others, newCode @ code)
                            end
                            val (newRegArgs, pushCode) = List.foldl pushReg ([], []) regArgs
                            val (newFPRegArgs, pushFPCode) = List.foldl pushReg ([], []) fpRegArgs
                        in
                            pushFPCode @ pushCode @ BeginFunction {regArgs=newRegArgs, fpRegArgs=newFPRegArgs, stackArgs=newStackArgs} :: code
                        end

                    |   pushRegisters({instr=FunctionCall{callKind, regArgs, stackArgs, dests, fpRegArgs, fpDests, containers, ...}, ...}, code) =
                        let
                            (* It's possible that this could lead to having to spill registers in order
                               to load others.  Leave that problem for the moment. *)
                            fun loadStackArg (arg, otherArgs) =
                            let
                                val argVal = mapArgument arg
                            in
                                argVal :: otherArgs
                            end
                            val newStackArgs = List.foldr loadStackArg [] stackArgs

                            fun loadRegArg ((arg, reg), otherArgs) =
                            let
                                val argVal = mapArgument arg
                            in
                                (argVal, reg) :: otherArgs
                            end
                            val newRegArgs = List.foldr loadRegArg [] regArgs

                            fun loadFPRegArg ((PReg n, reg), otherArgs) =
                            let
                                (* FP regs are untagged registers and should never be pushed. *)
                                val argVal =
                                    case Array.sub(pregMap, n) of
                                        Unset => raise InternalError "mapSrcReg - unset"
                                    |   ToPReg preg => preg
                                    |   ToStack _ => raise InternalError "loadFPRegArg: on stack"
                            in
                                (argVal, reg) :: otherArgs
                            end
                            val newFPRegArgs = List.foldr loadFPRegArg [] fpRegArgs

                            (* Push any result registers that need to be pushed. *)
                            fun pushResults((preg, rreg), (others, code)) =
                            let
                                val (newReg, newCode) = mapDestReg preg
                            in
                                ((newReg, rreg) :: others, newCode @ code)
                            end
                            val (destVals, destCode) = List.foldl pushResults ([], []) dests
                            val (destFPVals, destFPCode) = List.foldl pushResults ([], []) fpDests

                            val newContainers = List.map(fn c => #2(mapContainerAndStack(c, 0))) containers
                            (* Stack arguments are pushed in addition to anything on the stack. *)
                            val () = maxStack := Int.max(!maxStack, !stackCount + List.length newStackArgs)
                        in
                            destFPCode @ destCode @ FunctionCall{ callKind=callKind, regArgs=newRegArgs, stackArgs=newStackArgs,
                                                  dests=destVals, fpRegArgs=newFPRegArgs, fpDests=destFPVals, saveRegs={gcUpdate=[], noUpdate=[]}, containers=newContainers} :: code
                        end

                    |   pushRegisters({instr=TailRecursiveCall{callKind, regArgs, stackArgs, stackAdjust, fpRegArgs, ...}, ...}, code) =
                        let
                            val newStackOffset = !stackCount
                            
                            fun loadStackArg ({src, stack}, (otherLoads, otherArgs)) =
                            let
                                val (argVal, loadCode) =
                                    case mapArgument src of
                                        (source as ArgOnStack{wordOffset, container, field}) =>
                                            (* If we're leaving it in its old location or we're pushing it
                                               above the current top we're ok.  We're also ok if
                                               we're moving it from a somewhere above the last argument.
                                               Otherwise we have to load it.
                                               It goes into a normal tagged register which may mean that it
                                               could be pushed onto the stack in a subsequent pass. *)
                                            if wordOffset = stack+newStackOffset orelse stack+newStackOffset < 0
                                                orelse newStackOffset-wordOffset > ~ stackAdjust
                                            then (source, [])
                                            else
                                            let
                                                val preg = newPReg RegPropGeneral
                                            in
                                                (ArgInReg preg,
                                                    [LoadStack{wordOffset=wordOffset, container=container, field=field, dest=preg}])
                                            end
                                    |   argCode => (argCode, [])
                            in
                                (loadCode @ otherLoads, {src=argVal, stack=stack} :: otherArgs)
                            end
                            val (stackArgLoads, newStackArgs) = List.foldr loadStackArg ([], []) stackArgs
                
                            fun loadRegArg ((arg, reg), otherArgs) =
                            let
                                val argVal = mapArgument arg
                            in
                                (argVal, reg) :: otherArgs
                            end
                            val newRegArgs = List.foldr loadRegArg [] regArgs

                            fun loadFPRegArg ((PReg n, reg), otherArgs) =
                            let
                                (* FP regs are untagged registers and should never be pushed. *)
                                val argVal =
                                    case Array.sub(pregMap, n) of
                                        Unset => raise InternalError "mapSrcReg - unset"
                                    |   ToPReg preg => preg
                                    |   ToStack _ => raise InternalError "loadFPRegArg: on stack"
                            in
                                (argVal, reg) :: otherArgs
                            end
                            val newFPRegArgs = List.foldr loadFPRegArg [] fpRegArgs
                            (* Stack arguments replace existing arguments but could grow the stack. *)
                            val () = maxStack := Int.max(!maxStack, List.length newStackArgs)
                        in
                            TailRecursiveCall{ callKind=callKind, regArgs=newRegArgs, fpRegArgs=newFPRegArgs,
                                    stackArgs=newStackArgs, stackAdjust=stackAdjust, currStackSize=newStackOffset} ::
                                        stackArgLoads @ code
                        end

                    |   pushRegisters({instr=ReturnResultFromFunction{results, fpResults, returnReg, numStackArgs}, ...}, code) =
                        let
                            fun getResults((preg, rreg), (others, code)) =
                            let
                                val (newReg, newCode) = mapSrcReg preg
                            in
                                ((newReg, rreg) :: others, newCode @ code)
                            end
                            val (resultValues, loadResults) = List.foldr getResults ([], []) results
                            val (fpResultValues, loadFPResults) = List.foldr getResults ([], []) fpResults

                            val (returnValue, loadReturn) = mapSrcReg returnReg
                            val resetCode =
                                if !stackCount = 0 then [] else [ResetStackPtr{numWords= !stackCount}]
                        in
                            ReturnResultFromFunction{results=resultValues, fpResults=fpResultValues, returnReg=returnValue, numStackArgs=numStackArgs} ::
                                resetCode @ loadReturn @ loadFPResults @ loadResults @ code
                        end

                    |   pushRegisters({instr=RaiseExceptionPacket{packetReg}, ...}, code) =
                        let
                            val (packetVal, packetCode) = mapSrcReg packetReg
                        in
                            RaiseExceptionPacket{packetReg=packetVal} :: packetCode @ code
                        end

                    |   pushRegisters({instr=PushToStack{ source, container, copies }, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            (* This was a push from a previous pass.  Treat as a container of size "copies". *)
                            val newContainer = mapDestContainer(container, !stackCount)
                            val () = pushItemToStack(OriginalEntry{stackLoc=container})
                        in
                            PushToStack{source=sourceVal, container=newContainer, copies=copies} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=LoadStack{ dest, container, field, ... }, ...}, code) =
                        let
                            val (newOffset, newContainer) = mapContainerAndStack(container, field)
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ LoadStack{ dest=destVal, container=newContainer, field=field,
                                                  wordOffset=newOffset } :: code
                        end

                    |   pushRegisters({instr=StoreToStack{source, container, field, ...}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (newOffset, newContainer) = mapContainerAndStack(container, field)
                        in
                            StoreToStack{source=sourceVal, container=newContainer, field=field, stackOffset=newOffset} ::
                                    sourceCode @ code
                        end

                    |   pushRegisters({instr=ContainerAddress{ dest, container, ... }, ...}, code) =
                        let
                            val (newOffset, newContainer) = mapContainerAndStack(container, 0)
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ ContainerAddress{ dest=destVal, container=newContainer, stackOffset=newOffset } :: code
                        end

                    |   pushRegisters({instr=ResetStackPtr _, ...}, code) = code
                            (* Added in a previous pass - discard it. *)

                    |   pushRegisters({instr=TagValue{source, dest, isSigned, opSize}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ TagValue{source=sourceVal, dest=destVal, isSigned=isSigned, opSize=opSize} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=UntagValue{source, dest, isSigned, opSize, ...}, ...}, code) =
                        let
                            val (loadedSource, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ UntagValue{source=loadedSource, dest=destVal, isSigned=isSigned, opSize=opSize} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=BoxLarge{source, dest, ...}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ BoxLarge{source=sourceVal, dest=destVal, saveRegs={gcUpdate=[], noUpdate=[]}} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=UnboxLarge{source, dest, ...}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ UnboxLarge{source=sourceVal, dest=destVal} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=BoxTagFloat{floatSize, source, dest, ...}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ BoxTagFloat{floatSize=floatSize, source=sourceVal, dest=destVal, saveRegs={gcUpdate=[], noUpdate=[]}} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=UnboxTagFloat{floatSize, source, dest, ...}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ UnboxTagFloat{floatSize=floatSize, source=sourceVal, dest=destVal} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=LoadAcquire { base, dest, loadType}, ...}, code) =
                        let
                            val (baseVal, baseCode) = mapSrcReg base
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ LoadAcquire { base=baseVal, dest=destVal, loadType=loadType} :: baseCode @ code
                        end

                    |   pushRegisters({instr=StoreRelease { base, source, loadType}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (baseVal, baseCode) = mapSrcReg base 
                        in
                            StoreRelease{ base=baseVal, source=sourceVal, loadType=loadType} :: baseCode @ sourceCode @ code
                        end

                    |   pushRegisters({instr=BitFieldShift{source, dest, isSigned, length, immr, imms}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @
                                BitFieldShift { source=sourceVal, dest=destVal, isSigned=isSigned, immr=immr, imms=imms, length=length} ::
                                    sourceCode @ code
                        end

                    |   pushRegisters({instr=BitFieldInsert{source, destAsSource, dest, length, immr, imms}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destSrcVal, destSrcCode) = mapSrcReg destAsSource
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @
                                BitFieldInsert { source=sourceVal, destAsSource=destSrcVal, dest=destVal, immr=immr, imms=imms, length=length} ::
                                    destSrcCode @ sourceCode @ code
                        end

                    |   pushRegisters({instr=IndexedCaseOperation{testReg}, ...}, code) =
                        let
                            val (testVal, testCode) = mapSrcReg testReg
                        in
                            IndexedCaseOperation{testReg=testVal} :: testCode @ code
                        end

                    |   pushRegisters({instr=PushExceptionHandler, ...}, code) =
                        let
                            (* Add a handler entry to the stack. *)
                            val () = pushItemToStack HandlerEntry
                        in
                            PushExceptionHandler :: code
                        end

                    |   pushRegisters({instr=PopExceptionHandler, ...}, code) =
                        let
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
                                then [ResetStackPtr{numWords=oldStackPtr-nnCount-2}]
                                else []
                        in
                            PopExceptionHandler :: resetCode @ code
                        end

                    |   pushRegisters({instr=BeginHandler{packetReg}, ...}, code) =
                        let
                            (* Start of a handler.  The top active entry should be the handler. *)
                            val () =
                                case !stack of
                                    HandlerEntry :: tl => stack := tl
                                |   _ => raise InternalError "pushRegisters: BeginHandler"
                            val () = stackCount := !stackCount - 2
                            val (packetVal, packetCode) = mapDestReg packetReg
                        in
                            packetCode @ BeginHandler{packetReg=packetVal} :: code
                        end

                    |   pushRegisters({instr=CompareByteVectors{vec1Addr, vec2Addr, length, ccRef}, ...}, code) =
                        let
                            val (vec1Val, vec1Code) = mapSrcReg vec1Addr
                            val (vec2Val, vec2Code) = mapSrcReg vec2Addr
                            val (lenVal, lenCode) = mapSrcReg length
                        in
                            CompareByteVectors{vec1Addr=vec1Val, vec2Addr=vec2Val, length=lenVal, ccRef=ccRef} ::
                                lenCode @ vec2Code @ vec1Code @ code
                        end

                    |   pushRegisters({instr=BlockMove{srcAddr, destAddr, length, isByteMove}, ...}, code) =
                        let
                            val (srcVal, srcCode) = mapSrcReg srcAddr
                            val (destVal, destCode) = mapSrcReg destAddr
                            val (lenVal, lenCode) = mapSrcReg length
                        in
                            BlockMove{srcAddr=srcVal, destAddr=destVal, length=lenVal, isByteMove=isByteMove} ::
                                lenCode @ destCode @ srcCode @ code
                        end


                    |   pushRegisters({instr=AddSubXSP{source, dest, isAdd}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapOptDest dest
                        in
                            destCode @ AddSubXSP { source=sourceVal, dest=destVal, isAdd=isAdd} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=TouchValue{source, ...}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                        in
                            TouchValue { source=sourceVal} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=LoadAcquireExclusive{ base, dest }, ...}, code) =
                        let
                            val (baseVal, baseCode) = mapSrcReg base
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ LoadAcquireExclusive { base=baseVal, dest=destVal} :: baseCode @ code
                        end

                    |   pushRegisters({instr=StoreReleaseExclusive{ base, source, result }, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapOptSrc source
                            val (baseVal, baseCode) = mapSrcReg base 
                            val (resVal, resCode) = mapDestReg result
                        in
                            resCode @ StoreReleaseExclusive{ base=baseVal, source=sourceVal, result=resVal} :: baseCode @ sourceCode @ code
                        end

                    |   pushRegisters({instr=MemoryBarrier, ...}, code) = MemoryBarrier :: code

                    |   pushRegisters({instr=ConvertIntToFloat{ source, dest, srcSize, destSize}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @
                                ConvertIntToFloat{ source=sourceVal, dest=destVal, srcSize=srcSize, destSize=destSize} ::
                                sourceCode @ code
                        end

                    |   pushRegisters({instr=ConvertFloatToInt{ source, dest, srcSize, destSize, rounding}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @
                                ConvertFloatToInt{ source=sourceVal, dest=destVal, srcSize=srcSize, destSize=destSize, rounding=rounding} ::
                                sourceCode @ code
                        end

                    |   pushRegisters({instr=UnaryFloatingPt{ source, dest, fpOp}, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapSrcReg source
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ UnaryFloatingPt{ source=sourceVal, dest=destVal, fpOp=fpOp} :: sourceCode @ code
                        end

                    |   pushRegisters({instr=BinaryFloatingPoint{ arg1, arg2, dest, fpOp, opSize}, ...}, code) =
                        let
                            val (arg1Val, arg1Code) = mapSrcReg arg1
                            val (arg2Val, arg2Code) = mapSrcReg arg2
                            val (destVal, destCode) = mapDestReg dest
                        in
                            destCode @ BinaryFloatingPoint{ arg1=arg1Val, arg2=arg2Val, dest=destVal, fpOp=fpOp, opSize=opSize} ::
                                arg2Code @ arg1Code @ code
                        end

                    |   pushRegisters({instr=CompareFloatingPoint{ arg1, arg2, opSize, ccRef}, ...}, code) =
                        let
                            val (arg1Val, arg1Code) = mapSrcReg arg1
                            val (arg2Val, arg2Code) = mapSrcReg arg2
                        in
                            CompareFloatingPoint{ arg1=arg1Val, arg2=arg2Val, opSize=opSize, ccRef=ccRef} ::
                                arg2Code @ arg1Code @ code
                        end

                    |   pushRegisters({instr=CPUYield, ...}, code) = CPUYield :: code

                    |   pushRegisters({instr=AtomicOperation{ base, source, dest, atOp }, ...}, code) =
                        let
                            val (sourceVal, sourceCode) = mapOptSrc source
                            val (baseVal, baseCode) = mapSrcReg base
                            val (destVal, destCode) = mapOptDest dest
                        in
                            destCode @ AtomicOperation{ base=baseVal, source=sourceVal, dest=destVal, atOp=atOp } :: baseCode @ sourceCode @ code
                        end

                    local
                        fun doPush(instr as {kill, ...}, code) =
                        let
                            val newCode = pushRegisters(instr, code)
                            (* Can we pop the stack? *)
                            val stackReset =
                                case setToList (minus(kill, loopRegs)) of
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
                                        val _ = oldStack >= !stackCount orelse raise InternalError "negative stack offset"
                                    in
                                        if !stackCount = oldStack then []
                                        else [ResetStackPtr{numWords=oldStack - !stackCount}]
                                    end
                        in
                            stackReset @ newCode
                        end
                    in
                        val codeResult = List.foldl doPush [] block
                        val outputCount = ! stackCount
                        val results = {code=codeResult, stackCount= outputCount}
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
                    val expectedInput = valOf (asub inputStackSizes targetBlock)
                in
                    if expectedInput = outputCount
                    then targetBlock
                    else
                    let
                        val _ = outputCount > expectedInput orelse raise InternalError "adjustStack"
                        val adjustCode = [ResetStackPtr{numWords=outputCount-expectedInput}]
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
                            val expectedInput = valOf (asub inputStackSizes m)
                            val _ = outputCount >= expectedInput orelse raise InternalError "negative reset"
                            val resultCode =
                                if expectedInput = outputCount
                                then codeResult
                                else ResetStackPtr{numWords=outputCount-expectedInput} :: codeResult
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
                            val expectedInput = valOf (asub inputStackSizes continue)
                            val _ = outputCount >= expectedInput orelse raise InternalError "negative reset"
                            val resultCode =
                                if expectedInput = outputCount
                                then codeResult
                                else ResetStackPtr{numWords=outputCount-expectedInput} :: codeResult
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
        type extendedBasicBlock = extendedBasicBlock
        and ('genReg, 'optGenReg, 'fpReg) basicBlock =  ('genReg, 'optGenReg, 'fpReg) basicBlock
        and regProperty = regProperty
        and preg = preg
        and pregOrZero = pregOrZero
    end
end;
