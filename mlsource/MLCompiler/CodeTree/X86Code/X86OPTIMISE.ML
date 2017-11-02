(*
    Copyright David C. J. Matthews 2010, 2012, 2016-17

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

functor X86OPTIMISE(
    structure X86CODE: X86CODESIG
) :
sig
    type operation
    type code
    type operations = operation list
    type address = Address.address

    (* Optimise and code-generate. *)
    val generateCode: {code: code, ops: operations, labelCount: int} -> address

    structure Sharing:
    sig
        type operation = operation
        type code = code
    end
end =
struct
    open X86CODE
    exception InternalError = Misc.InternalError

    fun generateCode{code, ops, labelCount} =
    let
        open RegSet
        (* Print the pre-optimised code if required. *)
        val () = printLowLevelCode(ops, code)
    
        (* Optimise the code list by repeated scans up and down the list.
           Scan forward through the list reversing it as we go.  Then scan the
           reversed list and turn it back into the original order. *)
        fun forward([], list, rep) = reverse(list, [], rep)

        |   forward(ResetStack{numWords=count1, preserveCC=pres1} :: ResetStack{numWords=count2, preserveCC=pres2} :: tl, list, _) =
                (* Combine adjacent resets. *)
                forward(ResetStack{numWords=count1+count2, preserveCC=pres1 orelse pres2} :: tl, list, true)

        |   forward((a as ArithToGenReg{ opc=opA, output=outA, source=NonAddressConstArg constA }) ::
                    (b as ArithToGenReg{ opc=opB, output=outB, source=NonAddressConstArg constB }) :: tl, list, rep) =
            if outA = outB andalso (opA = ADD orelse opA = SUB) andalso (opB = ADD orelse opB = SUB)
            then
            let
                val (opc, result) =
                    case (opA, opB) of
                        (ADD, ADD) => (ADD, constA+constB)
                    |   (SUB, SUB) => (SUB, constA+constB)
                    |   (ADD, SUB) =>
                            if constA >= constB then (ADD, constA-constB)
                            else (SUB, constB-constA)
                    |   (SUB, ADD) =>
                            if constA >= constB then (SUB, constA-constB)
                            else (ADD, constB-constA)
                    |   _ => raise InternalError "forward: ArithRConst"
            in
                (* We could extract the case where the result is zero but that
                   doesn't seem to occur. *)
                forward(ArithToGenReg{ opc=opc, output=outA, source=NonAddressConstArg result } :: tl, list, true)
            end
            else forward(b :: tl, a :: list, rep)

        |   forward((mv as MoveToRegister{source=MemoryArg{base, offset, index=NoIndex}, output}) :: (reset as ResetStack{numWords=count, preserveCC}) :: tl,
                    list, rep) =
            (* If we have a load from the stack followed by a stack reset we may be able to use a pop.  Even if
               we can't we may be better to split the stack reset in case there's another load that could. *)
            if base = esp andalso offset < count * Address.wordSize
            then (* Can use a pop instruction. *)
            let
                val resetBefore = Int.min(offset div Address.wordSize, count)
            in
                if resetBefore = 0 (* So offset must be zero. *)
                then
                let
                    val _ = offset = 0 orelse raise InternalError "forward: offset non-zero"
                    val resetAfter = count - resetBefore - 1
                in
                    forward(if resetAfter = 0 then tl else ResetStack{numWords=resetAfter, preserveCC=preserveCC} :: tl,
                        PopR output :: list, true)
                end
                else forward(
                        MoveToRegister{
                            source=MemoryArg{base=base, offset=offset-resetBefore*Address.wordSize, index=NoIndex}, output=output} ::
                        (if count = resetBefore then tl else ResetStack{numWords=count - resetBefore, preserveCC=preserveCC} :: tl),
                        ResetStack{numWords=resetBefore, preserveCC=preserveCC} :: list, true)
            end
            else forward(reset::tl, mv::list, rep)

        |   forward(hd :: tl, list, rep) = forward(tl, hd :: list, rep)
        
        and reverse([], list, rep) = (list, rep)

            (* We store a result, then load it. *)
        |   reverse((l as FPLoadFromFPReg{source, lastRef}) ::
                    (s as FPStoreToFPReg{output, andPop=true}) :: tl, list, rep) =
            if source = output
            then if lastRef
            then (* We're not reusing the register so we don't need to store. *)
                reverse(tl, list, true)
            else (* We're reusing the register later.  Store it there but don't pop. *)
                reverse(FPStoreToFPReg{output=output, andPop=false} :: tl, list, true)
            else reverse(s :: tl, l :: list, rep)

            (* See if we can merge two allocations. *)
            (* Comment this out for the moment. *)
(*        |   reverse((l as AllocStore{size=aSize, output=aOut}) :: tl, list, rep) =
            let
                fun searchAlloc([], _, _, _) = []
                |   searchAlloc (AllocStore{size=bSize, output=bOut} :: tl, instrs, modRegs, true) =
                    (* We can merge this allocation unless the output register
                       has been modified in the meantime. *)
                    if inSet(bOut, modRegs)
                    then []
                    else (* Construct a new list with the allocation replaced by an
                            addition, the original instructions in between and the
                            first allocation now allocating the original space plus
                            space for the additional object and its length word. *)
                        LoadAddress{output=aOut, offset=(bSize+1) * Address.wordSize,
                                    base=SOME bOut, index=NoIndex} ::
                            List.filter (fn StoreInitialised => false | _ => true) (List.rev instrs) @
                            (AllocStore{size=aSize+bSize+1, output=bOut} :: tl)
                    (* Check the correct matching of allocation and completion. *)
                |   searchAlloc (AllocStore _ :: _, _, _, false) =
                        raise InternalError "AllocStore found but last allocation not complete"
                |   searchAlloc((s as StoreInitialised) :: tl, instrs, modRegs, false) =
                        searchAlloc(tl, s :: instrs, modRegs, true)
                |   searchAlloc(StoreInitialised :: _, _, _, true) =
                        raise InternalError "StoreInitialised found with no allocation"
                    (* For the moment we allow only a limited range of instructions here*)
                |   searchAlloc((s as StoreConstToMemory _) :: tl, instrs, modRegs, alloc) =
                        searchAlloc(tl, s :: instrs, modRegs, alloc)
                |   searchAlloc((s as StoreRegToMemory _) :: tl, instrs, modRegs, alloc) =
                        searchAlloc(tl, s :: instrs, modRegs, alloc)
                |   searchAlloc((s as StoreLongConstToMemory _) :: tl, instrs, modRegs, alloc) =
                        searchAlloc(tl, s :: instrs, modRegs, alloc)
                |   searchAlloc((s as ResetStack _) :: tl, instrs, modRegs, alloc) =
                        searchAlloc(tl, s :: instrs, modRegs, alloc)
                |   searchAlloc((s as LoadMemR{output, ...}) :: tl, instrs, modRegs, alloc) =
                        if output = aOut then []
                        else searchAlloc(tl, s :: instrs, regSetUnion(modRegs, singleton output), alloc)
                |   searchAlloc((s as MoveRR{output, ...}) :: tl, instrs, modRegs, alloc) =
                        if output = aOut then []
                        else searchAlloc(tl, s :: instrs, regSetUnion(modRegs, singleton output), alloc)                        
                    (* Anything else terminates the search. *)
                |   searchAlloc _ = []
            in
                case searchAlloc(tl, [], noRegisters, false) of
                    [] => reverse(tl, l :: list, rep)
                |   newTail => reverse(newTail, list, true)
            end
*)
        |   reverse(hd :: tl, list, rep) = reverse(tl, hd :: list, rep)

        (* Repeat scans through the code until there are no further changes. *)
        fun repeat ops =
            case forward(ops, [], false) of
                (list, false) => {ops=list, labelCount=labelCount}
            |   (list, true) => repeat list

        val {ops=finalOps, labelCount=finalLabelCount} =
            if lowLevelOptimise code
            then repeat ops
            else {ops=ops, labelCount=labelCount}
    in
        (* Pass on to the next stage. *)
        X86CODE.generateCode{ops=finalOps, labelCount=finalLabelCount, code=code}
    end

    structure Sharing =
    struct
        type operation = operation
        type code = code
    end
end;
