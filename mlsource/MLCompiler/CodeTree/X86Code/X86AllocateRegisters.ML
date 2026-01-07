(*
    Copyright David C. J. Matthews 2016-23

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

functor X86AllocateRegisters(
    structure X86ICode: X86ICODE
    structure Identify: X86IDENTIFYREFERENCES
    structure ConflictSets: X86GETCONFLICTSET
    structure IntSet: INTSET
    
    sharing X86ICode.Sharing = Identify.Sharing = ConflictSets.Sharing = IntSet
): X86ALLOCATEREGISTERS =
struct
    open X86ICode
    open Identify
    open ConflictSets
    open IntSet

    open Address
    
    exception InternalError = Misc.InternalError
    
    local
        val regs =
            case targetArch of
                Native32Bit         => [edi, esi, edx, ecx, ebx, eax]
            |   Native64Bit         => [edi, esi, edx, ecx, ebx, eax, r14, r13, r12, r11, r10, r9, r8]
            |   ObjectId32Bit2Word  => [edi, esi, edx, ecx, eax, r14, r13, r12, r11, r10, r9, r8]
            |   ObjectId32Bit4Word  => [edi, esi, edx, ecx, eax, r14, r13, r12, r11, r10, r9, r8]
    in
        val generalRegisters = List.map GenReg regs
        val nGenRegs = List.length generalRegisters
    end
    
    val floatingPtRegisters =
        case fpMode of
            (* XMM0-5 are the only volatile SSE2 registers in Windows X64. *)
            FPModeSSE2 => List.map XMMReg [xmm5, xmm4, xmm3, xmm2, xmm1, xmm0]
            (* We can't include fp7 because we need one spare. *)
            (* For the moment we only have FP0 here.  There are problems with using the
               others because we need to ensure the stack is empty if we call any
               non-ML function and we don't currently manage it properly. *)
        |   FPModeX87 => List.map FPReg [fp0(*, fp1, fp2, fp3, fp4, fp5, fp6*)]

    datatype allocateResult =
        AllocateSuccess of reg vector
    |   AllocateFailure of intSet list
    
    fun allocateRegisters{blocks, regStates, regProps, ...} =
    let
        val maxPRegs = Vector.length regStates
        and numBlocks = Vector.length blocks
        
        (* Hint values.  The idea of hints is that by using a hinted register
           we may avoid an unnecessary move instruction.  realHints is set when
           a pseudo-register is going to be loaded from a specific register
           e.g. a register argument, or moved into one e.g. ecx for a shift.
           friends is set to the other pseudo-registers that may be associated
           with the pseudo-register.  E.g. the argument and destination of
           an arithmetic operation where choosing the same register for
           each may avoid a move. *)
        val realHints = Array.array(maxPRegs, NONE: reg option)

        (* Sources and destinations.  These indicate the registers that are
           the sources and destinations of the indexing register and are used
           as hints.  If a register has been allocated for a source or destination
           we may be able to reuse it.  *)
        val sourceRegs = Array.array(maxPRegs, []: int list)
        and destinationRegs = Array.array(maxPRegs, []: int list)

        local
            (* Turn cached locations into register arguments. *)
            fun decache(StackLocation{cache=SOME r, ...}) = RegisterArgument r
            |   decache(MemoryLocation{cache=SOME r, ...}) = RegisterArgument r
            |   decache arg = arg

            fun addRealHint(r, reg) =
                case Array.sub(realHints, r) of
                    NONE => Array.update(realHints, r, SOME reg)
                |   SOME _ => ()
        
            fun addSourceAndDestinationHint{src, dst} =
            let
                val {conflicts, ...} = Vector.sub(regStates, src)
            in
                (* If they conflict we can't add them. *)
                if member(dst, conflicts)
                then ()
                else
                let
                    val currentDests = Array.sub(destinationRegs, src)
                    val currentSources = Array.sub(sourceRegs, dst)
                in
                    if List.exists(fn i => i=dst) currentDests
                    then ()
                    else Array.update(destinationRegs, src, dst :: currentDests);
                    if List.exists(fn i => i=src) currentSources
                    then ()
                    else Array.update(sourceRegs, dst, src :: currentSources)
                end
            end
        
        in
            (* Add the hints to steer the register allocation.  The idea is to avoid moves between
               registers by getting values into the appropriate register in advance. *)
            fun addHints{instr=LoadArgument{source, dest=PReg dreg, ...}, ...} =
                (
                    case decache source of
                        RegisterArgument(PReg sreg) => addSourceAndDestinationHint {src=sreg, dst=dreg}
                    |   _ => ()
                )

            |   addHints{instr=StoreArgument{ source, kind, ... }, ...} =
                (
                    case (decache source, kind, targetArch) of
                        (* Special case for byte register on X86/32 *)
                        (RegisterArgument(PReg sReg), MoveByte, Native32Bit) => addRealHint(sReg, GenReg ecx)
                    |   _ => ()
                )
        
            |   addHints{instr=BeginFunction{regArgs, ...}, ...} =
                    List.app (fn (PReg pr, reg) => addRealHint(pr, reg)) regArgs

            |   addHints{instr=TailRecursiveCall{regArgs, ...}, ...} =
                    List.app (fn (arg, reg) => case decache arg of RegisterArgument(PReg pr) => addRealHint(pr, reg) | _ => ()) regArgs
        
            |   addHints{instr=FunctionCall{regArgs, results, ...}, ...} =
                (
                    List.app (fn (PReg pr, reg) => addRealHint(pr, reg)) results;
                    List.app (fn (arg, reg) => case decache arg of RegisterArgument(PReg pr) => addRealHint(pr, reg) | _ => ()) regArgs
                )
        
            |   addHints{instr=InitialiseMem{size=PReg sReg, addr=PReg aReg, init=PReg iReg}, ...} =
                    (addRealHint(aReg, GenReg edi); addRealHint(iReg, GenReg eax); addRealHint(sReg, GenReg ecx))

            |   addHints{instr=JumpLoop{regArgs, ...}, ...} =
                let
                    fun addRegArg (arg, PReg resReg) =
                        case decache arg of
                            RegisterArgument(PReg argReg) => addSourceAndDestinationHint {dst=resReg, src=argReg}
                        |   _ => ()
                in
                    List.app addRegArg regArgs
                end

            |   addHints{instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...} = addRealHint(preg, GenReg eax)
        
            |   addHints{instr=BeginHandler{packetReg=PReg pReg, workReg=_}, ...} =
                    (* The exception packet is in rax. *) addRealHint(pReg, GenReg eax)
        
            |   addHints{instr=ReturnResultFromFunction { results, ... }, ...} =
                    List.app(fn(PReg resReg, realReg) => addRealHint(resReg, realReg)) results
        
            |   addHints{instr=ArithmeticFunction{oper=SUB, resultReg=PReg resReg, operand1=PReg op1Reg, ...}, ...} =
                    (* Can only be one way round. *)
                    addSourceAndDestinationHint {dst=resReg, src=op1Reg}

            |   addHints{instr=ArithmeticFunction{resultReg=PReg resReg, operand1=PReg op1Reg, operand2, ...}, ...} =
                (
                    addSourceAndDestinationHint {dst=resReg, src=op1Reg};
                    case decache operand2 of
                        RegisterArgument(PReg op2Reg) =>
                            addSourceAndDestinationHint {dst=resReg, src=op2Reg}
                    |   _ => ()
                )
         
            |   addHints{instr=CopyToCache{source=PReg sreg, dest=PReg dreg, ...}, ...} =
                    addSourceAndDestinationHint {src=sreg, dst=dreg}
        
            |   addHints{instr=UntagValue{source=PReg sReg, dest=PReg dReg, ...}, ...} =
                    addSourceAndDestinationHint{src=sReg, dst=dReg}

            |   addHints{instr=ShiftOperation{resultReg=PReg resReg, operand=PReg operReg, shiftAmount=IntegerConstant _, ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=operReg}
        
            |   addHints{instr=ShiftOperation{resultReg=PReg resReg, operand=PReg operReg,
                                            shiftAmount=RegisterArgument(PReg shiftReg), ...}, ...} =
                    (addSourceAndDestinationHint{dst=resReg, src=operReg}; addRealHint(shiftReg, GenReg ecx))

            |   addHints{instr=Multiplication{resultReg=PReg resReg, operand1=PReg op1Reg, operand2, ...}, ...} =
                (
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg};
                    case decache operand2 of
                        RegisterArgument(PReg op2Reg) =>
                            addSourceAndDestinationHint {dst=resReg, src=op2Reg}
                    |   _ => ()
                )
            
            |   addHints{instr=Division{dividend=PReg regDivid, quotient=PReg regQuot, remainder=PReg regRem, ...}, ...} =
                    (addRealHint(regDivid, GenReg eax); addRealHint(regQuot, GenReg eax); addRealHint(regRem, GenReg edx))

            |   addHints{instr=CompareByteVectors{vec1Addr=PReg v1Reg, vec2Addr=PReg v2Reg, length=PReg lReg, ...}, ...} =
                    (addRealHint(v1Reg, GenReg esi); addRealHint(v2Reg, GenReg edi); addRealHint(lReg, GenReg ecx))

            |   addHints{instr=BlockMove{srcAddr=PReg sReg, destAddr=PReg dReg, length=PReg lReg, ...}, ...} =
                    (addRealHint(sReg, GenReg esi); addRealHint(dReg, GenReg edi); addRealHint(lReg, GenReg ecx))
        
            |   addHints{instr=X87FPGetCondition{dest=PReg dReg, ...}, ...} = addRealHint(dReg, GenReg eax)
        
            |   addHints{instr=X87FPArith{resultReg=PReg resReg, arg1=PReg op1Reg, ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg}
        
            |   addHints{instr=X87FPUnaryOps{dest=PReg resReg, source=PReg op1Reg, ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg}
        
            |   addHints{instr=SSE2FPBinary{resultReg=PReg resReg, arg1=PReg op1Reg, ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg}
        
            |   addHints{instr=LongArbArithmetic{oper=SUB, resultReg=PReg resReg, operand1=PReg op1Reg, ...}, ...} =
                    (* Can only be one way round. *)
                    addSourceAndDestinationHint {dst=resReg, src=op1Reg}

            |   addHints{instr=AtomicExchangeAndAdd{resultReg=PReg resReg, source=PReg op1Reg, ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg}

            |   addHints{instr=AtomicExchange{resultReg=PReg resReg, source=PReg op1Reg, ...}, ...} =
                    addSourceAndDestinationHint{dst=resReg, src=op1Reg}

            |   addHints{instr=AtomicCompareAndExchange{resultReg=PReg resReg, compare=PReg cmpReg, ...}, ...} =
                    (addRealHint(resReg, GenReg eax); addRealHint(cmpReg, GenReg eax))

            |   addHints _ = ()
        
        end

        val allocatedRegs = Array.array(maxPRegs, NONE: reg option)
        val failures = ref []: intSet list ref

        (* Find a real register for a preg.
           1.  If a register is already allocated use that.
           2.  Try the "preferred" register if one has been given.
           3.  Try the realHints value if there is one.
           4.  See if there is a "friend" that has an appropriate register
           5.  Look at all the registers and find one. *)
        fun findRegister(r, pref, regSet) =
        case Array.sub(allocatedRegs, r) of
            SOME reg => reg
        |   NONE =>
            let
                val {conflicts, realConflicts, ...} = Vector.sub(regStates, r)
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

                fun findAReg [] =
                    (
                        (* This failed.  We're going to have to spill something. *)
                        failures := conflicts :: ! failures;
                        hd regSet (* Return a register to satisfy everything. *)
                    )
                |   findAReg (reg::regs) =
                        if isFree reg then (Array.update(allocatedRegs, r, SOME reg); reg)
                        else findAReg regs


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

                (* See if there is a friend that has a register already or a
                   hint.  Friends are registers that don't conflict and can
                   possibly avoid an extra move. *) 
(*                    fun findAFriend([], _) = NONE
                |   findAFriend(friend :: tail, old) =
                    let
                        val possReg =
                            case Array.sub(allocatedRegs, friend) of
                                v as SOME _ => tryAReg v
                            |   NONE => tryAReg(Array.sub(realHints, friend))
                    in
                        case possReg of
                            reg as SOME _ => reg
                        |   NONE =>
                            let
                                (* Add a friend of a friend to the list if we haven't already
                                   seen it and it doesn't conflict. *)
                                fun newFriend f =
                                    not(List.exists (fn n => n=f) old) andalso
                                        not(List.exists (fn n => n=f) conflicts)
                                val fOfF = List.filter newFriend (Array.sub(friends, friend))
                            in
                                findAFriend(tail @ fOfF, friend :: old)
                            end
                    end*)
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
                                    (* Look through the registers to find one that's free. *)
                                |   NONE => findAReg regSet
                            )
                    )
            end
            
        fun allocateRegister args = ignore(findRegister args)

        fun allocateGeneralReg r = allocateRegister(r, NONE, generalRegisters)
        and allocateFloatReg r = allocateRegister(r, NONE, floatingPtRegisters)

        fun allocateArgument(RegisterArgument(PReg r), regSet) = allocateRegister(r, NONE, regSet)
        |   allocateArgument(MemoryLocation{base=PReg bReg, index, cache=NONE, ...}, _) = (allocateGeneralReg bReg; allocateArgIndex index)
            (* Unfortunately we still have to allocate a register for the base even if we're going to use the cache.
               That's because the conflict sets are based on the assumption that the registers are allocated at the
               last occurrence (first when working from the end back) and it uses getInstructionRegisters which in turn
               uses argRegs which returns both the base and the cache.  GetConflictSets could use a different version
               but we also have to take account of save registers in e.g. AllocateMemoryOperation.  If we
               don't allocate a register because it's not needed at some point it shouldn't be allocated
               for the save set. *)
        |   allocateArgument(MemoryLocation{cache=SOME(PReg r), base=PReg bReg, index, ...}, regSet) =
                (allocateGeneralReg bReg; allocateArgIndex index; allocateRegister(r, NONE, regSet))
        |   allocateArgument(StackLocation{cache=SOME(PReg r), ...}, regSet) = allocateRegister(r, NONE, regSet)
        |   allocateArgument _ = ()
    
        and allocateArgGeneral arg = allocateArgument(arg, generalRegisters)
        and allocateArgFloat arg = allocateArgument(arg, floatingPtRegisters)

        and allocateArgIndex NoMemIndex = ()
        |   allocateArgIndex(MemIndex1(PReg r)) = allocateGeneralReg r
        |   allocateArgIndex(MemIndex2(PReg r)) = allocateGeneralReg r
        |   allocateArgIndex(MemIndex4(PReg r)) = allocateGeneralReg r
        |   allocateArgIndex(MemIndex8(PReg r)) = allocateGeneralReg r
        |   allocateArgIndex ObjectIndex2 = ()
        |   allocateArgIndex ObjectIndex4 = ()

        (* Return the register part of a cached item.  We must still, unfortunately, ensure that a register is
           allocated for base registers because we're assuming that a register is allocated on the last
           occurrence and this might be it. *)
        fun decache(StackLocation{cache=SOME r, ...}) = RegisterArgument r
        |   decache(MemoryLocation{cache=SOME r, base=PReg bReg, ...}) =
                (allocateGeneralReg bReg; RegisterArgument r)
        |   decache arg = arg

        val allocateFindRegister = findRegister
    
        fun registerAllocate({instr=LoadArgument{source, dest=PReg dreg, kind}, ...}) =
            let
                val regSet =
                    case kind of
                        MoveFloat => floatingPtRegisters
                    |   MoveDouble => floatingPtRegisters
                    |   _ => generalRegisters
                val realDestReg = findRegister(dreg, NONE, regSet)
            in
                (* We previously used decache here but that has the disadvantage that it
                   may allocate the destination register as the base register resulting in
                   it not being available as the cache register. *)
                case source of
                    RegisterArgument(PReg sreg) => allocateRegister(sreg, SOME realDestReg, regSet)
                
                |   StackLocation{cache=SOME(PReg sreg), ...} =>
                        allocateRegister(sreg, SOME realDestReg, regSet)

                |   MemoryLocation{cache=SOME(PReg sreg), base=PReg bReg, ...} =>
                    (
                        (* Cached source.  Allocate this first. *)
                        allocateRegister(sreg, SOME realDestReg, regSet);
                        (* We need to allocate a register but do it afterwards. *)
                        allocateGeneralReg bReg
                    )
                
                |   source => allocateArgument(source, regSet)
            end

        |   registerAllocate({instr=StoreArgument{ source, base=PReg bReg, index, kind, ... }, ...}) =
            (
                case (decache source, kind) of
                    (RegisterArgument(PReg sReg), MoveByte) =>
                    if targetArch <> Native32Bit
                    then (allocateArgGeneral source; allocateGeneralReg bReg; allocateArgIndex index)
                    else
                    (* This is complicated on X86/32.  We can't use edi or esi for the store registers.  Instead
                       we reserve ecx (see special case in "identify") and use that if we have to. *)
                    (
                        allocateRegister(sReg, SOME(GenReg ecx), generalRegisters);
                        allocateGeneralReg bReg; allocateArgIndex index
                    )
                
                |   _ =>
                    let
                        val regSet =
                            case kind of MoveFloat => floatingPtRegisters | MoveDouble => floatingPtRegisters | _ => generalRegisters
                    in
                        allocateArgument(source, regSet);
                        allocateGeneralReg bReg;
                        allocateArgIndex index
                    end
            )

        |   registerAllocate{instr=LoadMemReg { dest=PReg pr, ...}, ...} = allocateGeneralReg pr

        |   registerAllocate{instr=StoreMemReg { source=PReg pr, ...}, ...} = allocateGeneralReg pr

        |   registerAllocate{instr=BeginFunction _, ...} = ()
            (* Any registers that are referenced will have been allocated real registers. *)

        |   registerAllocate({instr=TailRecursiveCall{regArgs=oRegArgs, stackArgs=oStackArgs, workReg=PReg wReg, ...}, ...}) =
            let
                val regArgs = List.map (fn (arg, reg) => (decache arg, reg)) oRegArgs
                and stackArgs = List.map(fn {src, stack } => {src=decache src, stack=stack}) oStackArgs
                fun allocateRegArg(argReg, GenReg _) = allocateArgGeneral argReg
                |   allocateRegArg(argReg, XMMReg _) = allocateArgument(argReg, floatingPtRegisters)
                |   allocateRegArg(_, FPReg _) = raise InternalError "allocateRegArg" (* Never used. *)
            in
                allocateGeneralReg wReg;
                List.app (allocateArgGeneral o #src) stackArgs;
                (* We've already hinted the arguments. *)
                List.app allocateRegArg regArgs
            end

        |   registerAllocate({instr=FunctionCall{regArgs=oRegArgs, stackArgs=oStackArgs, results, saveRegs, ...}, ...}) =
            let
                val regArgs = List.map (fn (arg, reg) => (decache arg, reg)) oRegArgs
                and stackArgs = List.map decache oStackArgs
                fun allocateRegArg(argReg, GenReg _) = allocateArgGeneral argReg
                |   allocateRegArg(argReg, XMMReg _) = allocateArgument(argReg, floatingPtRegisters)
                |   allocateRegArg(_, FPReg _) = raise InternalError "allocateRegArg" (* Never used. *)
            in
                List.app(fn (PReg r) => allocateGeneralReg r) saveRegs;
                (* Try to use the destination register but don't force it.  We may have already
                   allocated a different register in which case we'll have to move the result there. *)
                List.app(fn (PReg dReg, realDest) =>
                    allocateRegister(dReg, SOME realDest,
                            case realDest of GenReg _ => generalRegisters | _ => floatingPtRegisters)) results;
                List.app allocateArgGeneral stackArgs;
                (* We've already hinted the arguments. *)
                List.app allocateRegArg regArgs
            end

        |   registerAllocate({instr=AllocateMemoryOperation{ dest=PReg dReg, saveRegs, ...}, ...}) =
            (
                List.app(fn (PReg r) => allocateGeneralReg r) saveRegs;
                allocateGeneralReg dReg
            )

        |   registerAllocate({instr=AllocateMemoryVariable{size=PReg sReg, dest=PReg dReg, saveRegs}, ...}) =
            (
                List.app(fn (PReg r) => allocateGeneralReg r) saveRegs;
                allocateGeneralReg dReg;
                allocateGeneralReg sReg
            )

        |   registerAllocate({instr=InitialiseMem{size=PReg sReg, addr=PReg aReg, init=PReg iReg}, ...}) =
            (
                (* We are going to use rep stosl/q to set the memory.
                   That requires the length to be in ecx, the initialiser to be in eax and
                   the destination to be edi. *)
                allocateRegister(aReg, SOME(GenReg edi), generalRegisters);
                allocateRegister(iReg, SOME(GenReg eax), generalRegisters);
                allocateRegister(sReg, SOME(GenReg ecx), generalRegisters)
            )
            
        |   registerAllocate{instr=InitialisationComplete, ...} = ()

        |   registerAllocate{instr=BeginLoop, ...} = ()

        |   registerAllocate({instr=JumpLoop{regArgs, stackArgs, checkInterrupt, workReg}, ...}) =
            (
                case workReg of SOME(PReg r) => allocateGeneralReg r | NONE => ();
                List.app (fn (src, _, _) => allocateArgGeneral src) stackArgs;
                List.app (fn (a, PReg r) => (allocateArgGeneral a; allocateGeneralReg r)) regArgs;
                case checkInterrupt of SOME regs => List.app(fn PReg r => allocateGeneralReg r) regs | NONE => ()
            )

        |   registerAllocate({instr=RaiseExceptionPacket{ packetReg=PReg preg }, ...}) =
                (* The argument must be put into rax. *)
                allocateRegister(preg, SOME(GenReg eax), generalRegisters)

        |   registerAllocate{instr=ReserveContainer _, ...} = ()

        |   registerAllocate({instr=IndexedCaseOperation{testReg=PReg tReg, workReg=PReg wReg}, ...}) =
            (
                allocateRegister(tReg, NONE, generalRegisters);
                allocateRegister(wReg, NONE, generalRegisters)
            )

        |   registerAllocate({instr=LockMutable{addr=PReg pr}, ...}) = allocateRegister(pr, NONE, generalRegisters)

        |   registerAllocate({instr=WordComparison{ arg1=PReg arg1Reg, arg2, ... }, ...}) =
            (
                allocateRegister(arg1Reg, NONE, generalRegisters);
                allocateArgGeneral arg2
            )

        |   registerAllocate({instr=CompareLiteral{ arg1, ... }, ...}) = allocateArgGeneral arg1

        |   registerAllocate({instr=CompareByteMem{ arg1={base=PReg bReg, index, ...}, ...}, ...}) =
                (allocateGeneralReg bReg; allocateArgIndex index)

            (* Set up an exception handler. *)
        |   registerAllocate({instr=PushExceptionHandler{workReg=PReg hReg}, ...}) = allocateGeneralReg hReg

            (* Pop an exception handler at the end of a handled section.  Executed if no exception has been raised.
               This removes items from the stack. *)
        |   registerAllocate({instr=PopExceptionHandler{workReg=PReg wReg, ...}, ...}) = allocateGeneralReg wReg

            (* Start of a handler.  Sets the address associated with PushExceptionHandler and
               provides a register for the packet.*) 
        |   registerAllocate({instr=BeginHandler{packetReg=PReg pReg, workReg=PReg wReg}, ...}) =
            (
                (* The exception packet is in rax. *)
                allocateRegister(pReg, SOME(GenReg eax), generalRegisters);
                allocateGeneralReg wReg
            )

        |   registerAllocate({instr=ReturnResultFromFunction { results, ... }, ...}) =
                List.app(fn (PReg resReg, realReg) => 
                    allocateRegister(resReg, SOME realReg, [realReg] (* It MUST be in this register *))) results

        |   registerAllocate{instr=ArithmeticFunction{oper=SUB, resultReg=PReg resReg, operand1=PReg op1Reg,
                                            operand2, ...}, ...} =
            (* Subtraction  - Unlike the other arithmetic operations we can't put the second
               argument into the result register and then do the operation. *)
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                (* Try to put the argument into the same register as the result. *)
            in
                allocateRegister(op1Reg, SOME realDestReg, generalRegisters);
                allocateArgGeneral operand2
            end

        |   registerAllocate({instr=ArithmeticFunction{resultReg=PReg resReg, operand1=PReg op1Reg, operand2, ...}, ...}) =
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val () = allocateRegister(op1Reg, SOME realDestReg, generalRegisters)
            in
                case decache operand2 of
                    RegisterArgument(PReg op2Reg) =>
                    (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
                       we can try to put either argument into the result reg and then do the operation on the other arg. *)
                        allocateRegister(op2Reg, SOME realDestReg, generalRegisters)
                |   operand2 => allocateArgGeneral operand2
            end

        |   registerAllocate({instr=TestTagBit{arg, ...}, ...}) = allocateArgGeneral arg

        |   registerAllocate({instr=PushValue {arg, ...}, ...}) = allocateArgGeneral arg

        |   registerAllocate({instr=CopyToCache{source=PReg sreg, dest=PReg dreg, kind}, ...}) =
            let
                val regSet =
                    case kind of
                        MoveFloat => floatingPtRegisters
                    |   MoveDouble => floatingPtRegisters
                    |   _ => generalRegisters
                val realDestReg = findRegister(dreg, NONE, regSet)
            in
                (* Get the source register using the current destination as a preference. *)
                allocateRegister(sreg, SOME realDestReg, regSet)
            end
            
        |   registerAllocate({instr=ResetStackPtr _, ...}) = ()

        |   registerAllocate({instr=StoreToStack{ source, ... }, ...}) = allocateArgument(source, generalRegisters)

        |   registerAllocate({instr=TagValue{source=PReg srcReg, dest=PReg dReg, ...}, ...}) =
            (
                (* Since we're using LEA to tag there's no cost to using a different reg. *)
                allocateRegister(dReg, NONE, generalRegisters);
                allocateRegister(srcReg, NONE, generalRegisters)
            )

        |   registerAllocate({instr=UntagValue{source=PReg sReg, dest=PReg dReg, cache, ...}, ...}) =
            let
                val regResult = findRegister(dReg, NONE, generalRegisters)
                val () =
                    case cache of
                        SOME(PReg cReg) => allocateRegister(cReg, SOME regResult, generalRegisters)
                    |   NONE => ()
            in
                allocateRegister(sReg, SOME regResult, generalRegisters)
            end

        |   registerAllocate({instr=LoadEffectiveAddress{base, index, dest=PReg dReg, ...}, ...}) =
            (
                allocateGeneralReg dReg;
                case base of SOME(PReg br) => allocateGeneralReg br | _ => ();
                allocateArgIndex index
            )

        |   registerAllocate({instr=ShiftOperation{resultReg=PReg resReg, operand=PReg operReg, shiftAmount=IntegerConstant _, ...}, ...}) =
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
            in
                allocateRegister(operReg, SOME realDestReg, generalRegisters)
            end

        |   registerAllocate({instr=ShiftOperation{resultReg=PReg resReg, operand=PReg operReg,
                                        shiftAmount=RegisterArgument(PReg shiftReg), ...}, ...}) =
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
            in
                allocateRegister(shiftReg, SOME(GenReg ecx), generalRegisters);
                allocateRegister(operReg, SOME realDestReg, generalRegisters)
            end

        |   registerAllocate{instr=ShiftOperation _, ...} = raise InternalError "registerAllocate - ShiftOperation"

        |   registerAllocate({instr=
                Multiplication{resultReg=PReg resReg, operand1=PReg op1Reg,
                               operand2, ...}, ...}) =
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val () = allocateRegister(op1Reg, SOME realDestReg, generalRegisters)
            in
                case decache operand2 of
                    RegisterArgument(PReg op2Reg) =>
                        (* Treat exactly the same as ArithmeticFunction. *)
                        allocateRegister(op2Reg, SOME realDestReg, generalRegisters)
                |   operand2 => allocateArgGeneral operand2
            end

        |   registerAllocate({instr=Division{dividend=PReg regDivid, divisor, quotient=PReg regQuot,
                                  remainder=PReg regRem, ...}, ...}) =
            (
                (* Division is specific as to the registers.  The dividend must be eax, quotient is
                   eax and the remainder is edx. *)
                allocateRegister(regDivid, SOME(GenReg eax), generalRegisters);
                allocateRegister(regQuot, SOME(GenReg eax), generalRegisters);
                allocateRegister(regRem, SOME(GenReg edx), generalRegisters);
                allocateArgGeneral divisor
            )

        |   registerAllocate({instr=AtomicExchangeAndAdd{base=PReg bReg, source=PReg sReg, resultReg=PReg rReg}, ...}) =
                (allocateGeneralReg sReg; allocateGeneralReg bReg; allocateGeneralReg rReg)

        |   registerAllocate({instr=AtomicExchange{base=PReg bReg, source=PReg sReg, resultReg=PReg rReg}, ...}) =
                (allocateGeneralReg sReg; allocateGeneralReg bReg; allocateGeneralReg rReg)

        |   registerAllocate({instr=AtomicCompareAndExchange{base=PReg bReg, compare=PReg cReg,
                                                             toStore=PReg tsReg, resultReg=PReg rReg, ...}, ...}) =
            (
                (* The value we're comparing with and the result are both RAX. *)
                allocateRegister(cReg, SOME(GenReg eax), generalRegisters);
                allocateRegister(rReg, SOME(GenReg eax), generalRegisters);
                allocateGeneralReg tsReg; allocateGeneralReg bReg
            )

        |   registerAllocate({instr=BoxValue{boxKind, source=PReg sReg, dest=PReg dReg, saveRegs}, ...}) =
            (
                List.app(fn (PReg r) => allocateGeneralReg r) saveRegs;
                case boxKind of
                    BoxLargeWord => allocateGeneralReg sReg
                |   BoxX87Double => allocateFloatReg sReg
                |   BoxX87Float => allocateFloatReg sReg
                |   BoxSSE2Float => allocateFloatReg sReg
                |   BoxSSE2Double => allocateFloatReg sReg;
                allocateGeneralReg dReg
            )

        |   registerAllocate({instr=CompareByteVectors{vec1Addr=PReg v1Reg, vec2Addr=PReg v2Reg, length=PReg lReg, ...}, ...}) =
            (
                allocateRegister(v1Reg, SOME(GenReg esi), generalRegisters);
                allocateRegister(v2Reg, SOME(GenReg edi), generalRegisters);
                allocateRegister(lReg, SOME(GenReg ecx), generalRegisters)
            )

        |   registerAllocate({instr=BlockMove{srcAddr=PReg sReg, destAddr=PReg dReg, length=PReg lReg, ...}, ...}) =
            (
                allocateRegister(sReg, SOME(GenReg esi), generalRegisters);
                allocateRegister(dReg, SOME(GenReg edi), generalRegisters);
                allocateRegister(lReg, SOME(GenReg ecx), generalRegisters)
            )

        |   registerAllocate{instr=X87Compare{arg1=PReg arg1Reg, arg2, ...}, ...} =
                (allocateRegister(arg1Reg, NONE, floatingPtRegisters); allocateArgFloat arg2)

        |   registerAllocate{instr=SSE2Compare{arg1=PReg arg1Reg, arg2, ...}, ...} =
                (allocateRegister(arg1Reg, NONE, floatingPtRegisters); allocateArgFloat arg2)

        |   registerAllocate({instr=X87FPGetCondition{dest=PReg dReg, ...}, ...}) =
                (* We can only use RAX here. *)
                allocateRegister(dReg, SOME(GenReg eax), generalRegisters)

        |   registerAllocate({instr=X87FPArith{resultReg=PReg resReg, arg1=PReg op1Reg, arg2, ...}, ...}) =
            let
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
            in
                allocateRegister(op1Reg, SOME realDestReg, floatingPtRegisters);
                allocateArgFloat arg2
            end

        |   registerAllocate({instr=X87FPUnaryOps{dest=PReg resReg, source=PReg op1Reg, ...}, ...}) =
            let
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
            in
                allocateRegister(op1Reg, SOME realDestReg, floatingPtRegisters)
            end

        |   registerAllocate({instr=X87Float{dest=PReg resReg, source}, ...}) =
                (allocateArgGeneral source; allocateRegister(resReg, NONE, floatingPtRegisters))

        |   registerAllocate({instr=SSE2IntToReal{dest=PReg resReg, source, ...}, ...}) =
                (allocateArgGeneral source; allocateRegister(resReg, NONE, floatingPtRegisters))

        |   registerAllocate({instr=SSE2FPUnary{resultReg=PReg resReg, source, ...}, ...}) =
            (
                allocateRegister(resReg, NONE, floatingPtRegisters);
                allocateArgFloat source
            )

        |   registerAllocate({instr=SSE2FPBinary{resultReg=PReg resReg, arg1=PReg op1Reg, arg2, ...}, ...}) =
            let
                val realDestReg = findRegister(resReg, NONE, floatingPtRegisters)
            in
                allocateRegister(op1Reg, SOME realDestReg, floatingPtRegisters);
                allocateArgFloat arg2
            end

        |   registerAllocate({instr=TagFloat{dest=PReg resReg, source=PReg sReg, ...}, ...}) =
            (
                allocateRegister(resReg, NONE, generalRegisters);
                allocateRegister(sReg, NONE, floatingPtRegisters)
            )

        |   registerAllocate({instr=UntagFloat{source, dest=PReg dReg, cache, ...}, ...}) =
            let
                val regResult = findRegister(dReg, NONE, floatingPtRegisters)
                val () =
                    case cache of
                        SOME(PReg cReg) => allocateRegister(cReg, SOME regResult, floatingPtRegisters)
                    |   NONE => ()
            in
                allocateArgGeneral source
            end

        |   registerAllocate({instr=GetSSE2ControlReg{dest=PReg destReg}, ...}) =
                allocateRegister(destReg, NONE, generalRegisters)

        |   registerAllocate({instr=SetSSE2ControlReg{source=PReg srcReg}, ...}) =
                allocateRegister(srcReg, NONE, generalRegisters)

        |   registerAllocate({instr=GetX87ControlReg{dest=PReg destReg}, ...}) =
                allocateRegister(destReg, NONE, generalRegisters)

        |   registerAllocate({instr=SetX87ControlReg{source=PReg srcReg}, ...}) =
                allocateRegister(srcReg, NONE, generalRegisters)

        |   registerAllocate({instr=X87RealToInt{source=PReg srcReg, dest=PReg destReg}, ...}) =
            (
                allocateRegister(srcReg, NONE, floatingPtRegisters);
                allocateRegister(destReg, NONE, generalRegisters)
            )

        |   registerAllocate({instr=SSE2RealToInt{source, dest=PReg destReg, ...}, ...}) =
            (
                allocateRegister(destReg, NONE, generalRegisters);
                allocateArgFloat source
            )

        |   registerAllocate({instr=SignExtend32To64{source, dest=PReg destReg, ...}, ...}) =
            (
                allocateRegister(destReg, NONE, generalRegisters);
                allocateArgGeneral source
            )

        |   registerAllocate({instr=TouchArgument{source=PReg srcReg}, ...}) =
                allocateRegister(srcReg, NONE, generalRegisters)
        
        |   registerAllocate({instr=PauseCPU, ...}) = ()

        |   registerAllocate({instr=BitScanReverse{source, dest=PReg destReg, ...}, ...}) =
            (
                allocateRegister(destReg, NONE, generalRegisters);
                allocateArgGeneral source
            )

        |   registerAllocate({instr=LongArbComparison{ arg1=PReg arg1Reg, arg2, saveRegs, ... }, ...}) =
            (
                List.app(fn (PReg r) => allocateGeneralReg r) saveRegs;
                allocateRegister(arg1Reg, NONE, generalRegisters);
                allocateArgGeneral arg2
            )

        |   registerAllocate{instr=LongArbArithmetic{oper=SUB, resultReg=PReg resReg, operand1=PReg op1Reg,
                                            operand2, saveRegs, ...}, ...} =
            (* Subtraction  - As with the unemulated case, we can only do this one way round. *)
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                (* Try to put the argument into the same register as the result. *)
            in
                List.app(fn (PReg r) => allocateGeneralReg r) saveRegs;
                allocateRegister(op1Reg, SOME realDestReg, generalRegisters);
                allocateArgGeneral operand2
            end

        |   registerAllocate({instr=LongArbArithmetic{resultReg=PReg resReg, operand1=PReg op1Reg, operand2, saveRegs, ...}, ...}) =
            let
                val realDestReg = findRegister(resReg, NONE, generalRegisters)
                val () = allocateRegister(op1Reg, SOME realDestReg, generalRegisters)
                val () = List.app(fn (PReg r) => allocateGeneralReg r) saveRegs;
            in
                case decache operand2 of
                    RegisterArgument(PReg op2Reg) =>
                    (* Arithmetic operation with both arguments as registers.  These operations are all symmetric so
                       we can try to put either argument into the result reg and then do the operation on the other arg. *)
                        allocateRegister(op2Reg, SOME realDestReg, generalRegisters)
                |   operand2 => allocateArgGeneral operand2
            end

        (* Depth-first scan. *)
        val visited = Array.array(numBlocks, false)

        fun processBlocks blockNo =
        if Array.sub(visited, blockNo)
        then ()  (* Done or currently being done. *)
        else
        let
            val () = Array.update(visited, blockNo, true)
            val ExtendedBasicBlock { flow, block, passThrough, exports, ...} =
                Vector.sub(blocks, blockNo)
            (* Add the hints for this block before the actual allocation of registers. *)
            val _ = List.app addHints block
            val () =
                (* Process the dependencies first. *)
                case flow of
                    ExitCode => ()
                |   Unconditional m => processBlocks m
                |   Conditional {trueJump, falseJump, ...} =>
                        (processBlocks trueJump; processBlocks falseJump)
                |   IndexedBr cases => List.app processBlocks cases
                |   SetHandler{ handler, continue } =>
                        (processBlocks handler; processBlocks continue)
                |   UnconditionalHandle _ => ()
                |   ConditionalHandle { continue, ...} => processBlocks continue
            (* Now this block. *)
            local
                (* We assume that anything used later will have been allocated a register.
                   This is generally true except for a loop where the use may occur earlier. *)
                val exported = setToList passThrough @ setToList exports
                fun findAReg r =
                    case Vector.sub(regProps, r) of
                        RegPropStack _ => ()
                    |   _ => ignore(allocateFindRegister(r, NONE, generalRegisters))
            in
                val () = List.app findAReg exported
            end
        in
            List.foldr(fn (c, ()) => registerAllocate c) () block
        end

    in
        processBlocks 0;
        (* If the failures list is empty we succeeded. *)
        case !failures of
            [] =>  (* Return the allocation vector.  If a register isn't used replace it with rax. *)
                AllocateSuccess(Vector.tabulate(maxPRegs, fn i => getOpt(Array.sub(allocatedRegs, i), GenReg eax)))
            (* Else we'll have to spill something. *)
        |   l => AllocateFailure l
    end

    structure Sharing =
    struct
        type intSet             = intSet
        and extendedBasicBlock  = extendedBasicBlock
        and regProperty         = regProperty
        and reg                 = reg
        and allocateResult      = allocateResult
    end

end;
