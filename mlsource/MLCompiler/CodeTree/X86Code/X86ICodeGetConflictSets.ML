(*
    Copyright (c) 2016-18, 2021-2 David C.J. Matthews

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

functor X86ICodeGetConflictSets(
    structure X86ICode: X86ICODE
    structure IntSet: INTSET
    structure Identify: X86IDENTIFYREFERENCES
    sharing X86ICode.Sharing = Identify.Sharing = IntSet
): X86GETCONFLICTSET =
struct
    open X86ICode
    open IntSet
    open Identify

    type conflictState =
    {
        conflicts: intSet, realConflicts: reg list
    }
    
    type triple = {instr: x86ICode, current: intSet, active: intSet}

    exception InternalError = Misc.InternalError
    
    (* Get the conflict sets.  This code was originally part of identifyRegisterState and
       was split off. *)
    fun getConflictStates (blocks: extendedBasicBlock vector, maxPRegs) =
    let
        (* Other registers that conflict with this i.e. cannot share the same
           real register. *)
        val regConflicts = Array.array(maxPRegs, emptySet)
        (* Real registers that cannot be used for this because they are needed for
           an instruction e.g. shift or block move, that requires these. *)
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
                fun reserveAReg r =
                let
                    val absConflicts = Array.sub(regConflicts, r)
                    fun addConflict i =
                        if isInset i reserveFor then () else addRealConflict (i, reg)
                in
                    List.app addConflict (setToList absConflicts)
                end
            in
                List.app reserveAReg reserveFor
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
                   a source.  The exceptions are AllocateMemoryVariable and BoxValues
                   which can only store the size or the contents after the memory has
                   been allocated.  We also have to make sure that the test and
                   work registers are different in IndexedCase. *)
                val postInstruction =
                    case instr of
                        AllocateMemoryVariable _ => destRegNos @ sourceRegNos
                    |   BoxValue _ => destRegNos @ sourceRegNos
                    |   IndexedCaseOperation _ => destRegNos @ sourceRegNos
                    |   ArithmeticFunction{oper=SUB, operand2, ...} =>
                            (* Special case for subtraction - we can't use the same register for the result and
                               the second operand. *) destRegNos @ map regNo (argRegs operand2)
                    |   LongArbArithmetic{oper=SUB, operand2, ...} => destRegNos @ map regNo (argRegs operand2)
                    |   ArithmeticFunction{operand2 as MemoryLocation _, ...} =>
                        (* If operand1 is not in the destination register we will move it
                           there before the instruction.  That means that we must not have the
                           destination register as either a base or index register.  *)
                            destRegNos @ map regNo (argRegs operand2)
                    |   LongArbArithmetic{operand2 as MemoryLocation _, ...} => destRegNos @ map regNo (argRegs operand2)
                    |   Multiplication{operand2 as MemoryLocation _, ...} =>
                        (* Likewise for multiplication.  *)
                            destRegNos @ map regNo (argRegs operand2)
                    |   AtomicExchangeAndAdd{base, ...} =>
                        (* and for atomic operations. The base is always a memory address. *)
                            regNo base :: destRegNos
                    |   AtomicExchange{base, ...} => regNo base :: destRegNos
                    |   AtomicCompareAndExchange{base, ...} => regNo base :: destRegNos
                            
                        (* TailRecursiveCall and JumpLoop may require a work register.
                           This is the only destination but if present it must be distinct
                           from the arguments. *)
                    |   TailRecursiveCall _ => destRegNos @ sourceRegNos
                    |   JumpLoop _ => destRegNos @ sourceRegNos
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
                val () = addConflictsTo(sourceRegNos, listToSet sourceRegNos)
                val () = addConflictsTo(sourceRegNos, afterRemoveDests);
                val () = addConflictsTo(sourceRegNos, passThrough)
            end
            
            (* I'm not sure if this is needed.  There was a check in the old code to ensure that
               different registers were used for loop variables even if they were actually unused.
               This may happen anyway. *)
            (* However, we need to ensure that if there is a work register it is distinct from any
               of the registers being used for arguments.  So at the least we need to add a
               conflict between these. *)
            val () =
                case instr of
                    JumpLoop{regArgs, workReg, ...} =>
                    let
                        val destRegs = List.foldl(fn ((_, PReg loopReg), dests) => loopReg :: dests) [] regArgs
                        val workDest = case workReg of SOME(PReg reg) => [reg] | NONE => []
                        val destRegs = destRegs @ workDest
                    in
                        addConflictsTo(destRegs, listToSet destRegs);
                        addConflictsTo(destRegs, current);
                        addConflictsTo(destRegs, passThrough)
                    end
                |   _ => ()

            (* Certain instructions are specific as to the real registers. *)
            val () =
                case instr of
                    (* Storing a byte value.  This is messy on X86/32 because we can't use edi or esi as the register
                       to store.  To get round this we reserve ecx as a possible register as with shifts.
                       We don't actually need to use this but it is available if necessary. *)
                    StoreArgument { source=RegisterArgument sReg, kind=MoveByte, ...} =>
                    if targetArch <> Native32Bit then ()
                    else reserveRegister([regNo sReg], GenReg ecx)

                |   InitialiseMem{size, addr, init} =>
                    (
                        (* We are going to use rep stosl/q to set the memory.
                           That requires the length to be in ecx, the initialiser to be in eax and
                           the address to be edi. *)
                        reserveRegister([regNo addr], GenReg edi);
                        reserveRegister([regNo init], GenReg eax);
                        reserveRegister([regNo size], GenReg ecx)
                    )

                |   ShiftOperation{shiftAmount=RegisterArgument shiftAmount, ...} =>
                    (
                        (* Shift with by amount specified in a register.  This must be ecx. *)
                        reserveRegister([regNo shiftAmount], GenReg ecx);
                        (* reserveRegister only sets a conflict between the args.  We need
                           to include the result because that will be allocated first. *)
                         List.app(fn r => addRealConflict (r, GenReg ecx)) destRegNos
                    )

                |   Division{dividend, quotient, remainder, ...} =>
                    (
                        (* Division is specific as to the registers.  The dividend must be eax, quotient is
                           eax and the remainder is edx.  The divisor must not be in either edx or eax because
                           we need to sign extend the dividend before the division. *)
                        reserveRegister([regNo quotient, regNo dividend], GenReg eax);
                        (* In addition, we need to register conflicts with the divisor,
                           at least for edx.  The remainder is a result and may well
                           not be in the conflict set with the divisor. *)
                        List.app(fn r => addRealConflict (r, GenReg edx)) sourceRegNos;
                        reserveRegister([regNo remainder], GenReg edx)
                    )

                |   CompareByteVectors{vec1Addr, vec2Addr, length, ...} =>
                    (
                        (* We have to use specific registers. *)
                        reserveRegister([regNo vec1Addr], GenReg esi);
                        reserveRegister([regNo vec2Addr], GenReg edi);
                        reserveRegister([regNo length], GenReg ecx)
                    )

                |   BlockMove{srcAddr, destAddr, length, ...} =>
                    (
                        (* We have to use specific registers. *)
                        reserveRegister([regNo srcAddr], GenReg esi);
                        reserveRegister([regNo destAddr], GenReg edi);
                        reserveRegister([regNo length], GenReg ecx)
                    )

                |   X87FPGetCondition{dest, ...} =>
                        (* This can only put the result in rax. *)
                        reserveRegister([regNo dest], GenReg eax)

                |   RaiseExceptionPacket{ packetReg } =>
                        (* This wasn't needed previously because we always pushed the registers
                           across an exception. *)
                        reserveRegister([regNo packetReg], GenReg eax)
                
                |   BeginHandler { packetReg, ...} =>
                        reserveRegister([regNo packetReg], GenReg eax)
                
                |   FunctionCall { results, regArgs, ...} =>
                        (* This is only needed if we are saving the registers rather
                           than marking them as "must push". *)
                    (
                        List.app (fn (dest, realDest) => reserveRegister([regNo dest], realDest)) results;
                        (* The argument registers also conflict.  In order to execute this call we need to load
                           the arguments into specific registers so we can't use them for values that we want after
                           the call.   We use regNo dest here because that will conflict with everything
                           immediately afterwards. *)
                        List.app(fn (_, r) =>
                            List.app(fn (dest, _) => reserveRegister([regNo dest], r)) results) regArgs
                    )

                |   TailRecursiveCall {workReg=PReg wReg, regArgs, ...} =>
                        (* Prevent the work reg from using any of the real register args. *)
                        List.app(fn (_, r) => addRealConflict (wReg, r)) regArgs

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
        
        val conflictState: conflictState vector =
            Vector.tabulate(maxPRegs,
                fn i => {
                    conflicts = Array.sub(regConflicts, i),
                    realConflicts = Array.sub(regRealConflicts, i)
                }
            )
    in
        conflictState
    end

    structure Sharing =
    struct
        type x86ICode = x86ICode
        and reg = reg
        and preg = preg
        and intSet = intSet
        and extendedBasicBlock = extendedBasicBlock
    end
end;
