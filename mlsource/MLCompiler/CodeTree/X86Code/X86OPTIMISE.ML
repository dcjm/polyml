(*
    Copyright David C. J. Matthews 2010, 2012, 2016

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

    val optimise: code * operations -> operations

    structure Sharing:
    sig
        type operation = operation
        type code = code
    end
end =
struct
    open X86CODE
    exception InternalError = Misc.InternalError

    fun optimise(code, ops) =
    let
        open RegSet
        (* Print the pre-optimised code if required. *)
        val () = printLowLevelCode(ops, code)

        (* All these instructions can't be eliminated after a jump. *)
        fun labelInstruction(JumpLabel(Labels{uses=ref uses, ...})) = uses > 0
        |   labelInstruction(StartHandler _) = true
        |   labelInstruction _ = false

        (* If we remove a branch the use counts are reduced. *)
        fun eliminate(UncondBranch(Labels{uses, ...})) =
            (!uses > 0 orelse raise InternalError "eliminate"; uses := !uses-1)
        |   eliminate(ConditionalBranch{label=Labels{uses, ...}, ...}) =
            (!uses > 0 orelse raise InternalError "eliminate"; uses := !uses-1)
        |   eliminate _ = ()
    
        (* Optimise the code list by repeated scans up and down the list.
           Scan forward through the list reversing it as we go.  Then scan the
           reversed list and turn it back into the original order. *)
        fun forward([], list, rep) = reverse(list, [], rep)

            (* Eliminate dead instructions after RaiseException up to the next label. *)
        |   forward((u as RaiseException) :: next :: tl, list, rep) =
            if labelInstruction next
            then forward(next :: tl, u :: list, rep)
            else (eliminate next; forward(u :: tl, list, true))

            (* Eliminate Unconditional branches to the next instruction. *)
        |   forward((u as UncondBranch(Labels{forward=source, ...})) ::
                    (next as JumpLabel(Labels{forward=dest, ...})) :: tl, list, rep) =
            if source = dest
            then (eliminate u; forward(next :: tl, list, true))
            else forward(next :: tl, u :: list, rep)
           
            (* Eliminate dead instructions after UnconditionalBranch up to the next label. *)
        |   forward((u as UncondBranch _) :: next :: tl, list, rep) =
            if labelInstruction next
            then forward(next :: tl, u :: list, rep)
            else (eliminate next; forward(u :: tl, list, true))

            (* Eliminate unreferenced labels.  *)
        |   forward(JumpLabel(Labels{uses=ref 0, ...}) :: tl, list, _) =
                forward(tl, list, true)

            (* Branch chaining. If we have a label followed by an unconditional branch set the
               chain entry of the label to destination of the branch. *)
        |   forward((l as JumpLabel(Labels{chain as ref NONE, ...})) ::
                    (u as UncondBranch(uncondLab as Labels{chain=ref chainL, ...})) :: tl, list, _) =
            (
                if isSome chainL then chain := chainL else chain := SOME uncondLab;
                (* It's essential to include the branch in the output at this point.
                   Otherwise we could eliminate the branch if it happened to be followed
                   by its destination, set the use count of the destination label to
                   zero, eliminate that; all before we had updated the incoming
                   branches. *)
                forward(tl, u :: l :: list, true)
            )

            (* Reorder conditional and unconditional branches.  If we have a conditional branch
               followed by an unconditional branch followed by the destination of the conditional
               branch we can turn the test round. *)
        |   forward((c as ConditionalBranch{label=condDest, test, predict, ...}) ::
                    (u as UncondBranch uncondLab) ::
                    (d as JumpLabel(Labels{forward=nextLabel, ...})) :: tl, list, rep) =
            let
                (* See where the ultimate destination of the conditional branch is.
                   If it's already been forwarded we don't want to change that. *)
                fun follow(Labels{chain=ref(SOME c), ...}) = follow c
                |   follow c = c

                val Labels{forward=condRef, ...} = follow condDest

                fun reverseTest JE  = JNE
                |   reverseTest JNE = JE
                |   reverseTest JA  = JNA
                |   reverseTest JB  = JNB
                |   reverseTest JNA = JA
                |   reverseTest JNB = JB
                |   reverseTest JL  = JGE
                |   reverseTest JG  = JLE
                |   reverseTest JLE = JG
                |   reverseTest JGE = JL
                |   reverseTest JO  = JNO
                |   reverseTest JNO = JO
                |   reverseTest JP  = JNP
                |   reverseTest JNP = JP

                fun reversePrediction PredictNeutral = PredictNeutral
                |   reversePrediction PredictTaken = PredictNotTaken
                |   reversePrediction PredictNotTaken = PredictTaken
            in
                if condRef = nextLabel
                then
                (
                    eliminate c;
                    forward(d :: tl,
                        ConditionalBranch{label=uncondLab, test=reverseTest test, predict=reversePrediction predict} ::
                            list, true)
                )
                else forward(u :: d :: tl, c :: list, rep)
            end

        |   forward(ResetStack count1 :: ResetStack count2 :: tl, list, _) =
                (* Combine adjacent resets. *)
                forward(ResetStack(count1+count2) :: tl, list, true)

        |   forward((a as ArithToGenReg{ opc=opA, output=outA, source=ShortConstArg constA }) ::
                    (b as ArithToGenReg{ opc=opB, output=outB, source=ShortConstArg constB }) :: tl, list, rep) =
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
                forward(ArithToGenReg{ opc=opc, output=outA, source=ShortConstArg result } :: tl, list, true)
            end
            else forward(b :: tl, a :: list, rep)

        |   forward(hd :: tl, list, rep) = forward(tl, hd :: list, rep)
        
        and reverse([], list, rep) = (list, rep)

            (* Eliminate unreferenced labels.  *)
        |   reverse(JumpLabel(Labels{uses=ref 0, ...}) :: tl, list, _) =
                reverse(tl, list, true)

            (* Combine multiple labels by setting the earlier to point to the later.  This may
               subsequently eliminate the first label.  This simplifies branch chaining.
               This can only be done after we've checked that the later label is
               actually referenced. *)
        |   reverse((a as JumpLabel(aL as Labels{chain=ref chainA, ...})) ::
                    (b as JumpLabel(Labels{chain as ref NONE, ...})) :: tl,
                    list, _) =
            (
                if isSome chainA then chain := chainA else chain := SOME aL;
                reverse(b :: tl, a :: list, true)
            )

            (* Branch chaining.  If we have a branch to a destination that is chained
               we replace this by a branch to the ultimate destination.  This is done on
               the reverse pass because generally branches are forward and the extensions will
               have been added after the branches were seen on the forward pass. *)
        |   reverse(UncondBranch(Labels{chain=ref(SOME dest), uses=oldUses, ...}) :: tl, list, _) =
            let
                fun follow(Labels{chain=ref(SOME c), ...}) = follow c
                |   follow c = c
                val dest as Labels{uses=newUses, ...} = follow dest
            in
                oldUses := !oldUses - 1;
                ! newUses >= 1 orelse raise InternalError "UncondBranch1";
                newUses := !newUses + 1;
                ! oldUses >= 0 orelse raise InternalError "UncondBranch2";
                reverse(UncondBranch dest :: tl, list, true)
            end

        |   reverse(
                ConditionalBranch{
                    label=Labels{chain=ref(SOME dest), uses=oldUses, ...}, test, predict}
                    :: tl, list, _) =
            let
                fun follow(Labels{chain=ref(SOME c), ...}) = follow c
                |   follow c = c
                val dest as Labels{uses=newUses, ...} = follow dest
            in
                oldUses := !oldUses - 1;
                ! newUses >= 1 orelse raise InternalError "ConditionalBranch1";
                newUses := !newUses + 1;
                ! oldUses >= 0 orelse raise InternalError "ConditionalBranch2";
                reverse(ConditionalBranch{label=dest, test=test, predict=predict} :: tl, list, true)
            end

            (* Branch chaining with indexed branches.  We may be able to forward
               branch table entries to their new location. *)
        |   reverse(
                (i as IndexedCase{testReg, workReg, min, cases}) :: tl, list, rep) =
            let
                val changed = ref false
                fun forwardBranch(Labels{chain=ref(SOME dest), uses=oldUses, ...}) =
                let
                    fun follow(Labels{chain=ref(SOME c), ...}) = follow c
                    |   follow c = c
                    val dest as Labels{uses=newUses, ...} = follow dest
                in
                    oldUses := !oldUses - 1;
                    ! newUses >= 1 orelse raise InternalError "IndexedBranch1";
                    newUses := !newUses + 1;
                    ! oldUses >= 0 orelse raise InternalError "IndexedBranch2";
                    changed := true;
                    dest
                end
                |   forwardBranch lab = lab
                val newList = List.map forwardBranch cases
            in
                if ! changed
                then reverse(tl,
                        IndexedCase{testReg=testReg, workReg=workReg, min=min, cases=newList} :: list,
                        true)
                else reverse(tl, i :: list, rep)
            end

            (* If we free a floating point register after loading it onto the stack we want to
               propagate that information.  Otherwise push the RegisterStatusChange back up
               the list. *)
        |   reverse((r as FreeRegisters regs) :: (f as FPLoadFromFPReg{source, ...}) :: tl, list, _) =
            if inSet(FPReg source, regs)
            then
            let
                val left = regSetMinus(regs, singleton(FPReg source))
            in
                if left = noRegisters
                then reverse(FPLoadFromFPReg{source=source, lastRef=true} :: tl, list, true)
                else reverse(FPLoadFromFPReg{source=source, lastRef=true} ::
                             FreeRegisters left :: tl, list, true)
            end
            else reverse(f :: r :: tl, list, true) (* Push it back *)

        |   reverse((r as FreeRegisters regs) :: (f as FPStoreToFPReg{output, ...}) :: tl, list, rep) =
            let
                val fpo = FPReg output
            in
                if inSet(fpo, regs)
                then (* We're discarding this register without using it.  Why?
                        Split the sets but otherwise do nothing for the moment. *)
                let
                    val left = regSetMinus(regs, singleton fpo)
                in
                    if left = noRegisters
                    then reverse(f :: tl, FreeRegisters(singleton fpo) :: list, rep)
                    else reverse(f :: FreeRegisters left :: tl, FreeRegisters(singleton fpo) :: list, rep)
                end
                else (* We're saving into a different register.  Push back the Free. *)
                    reverse(f :: r :: tl, list, true) (* Push it back *)
            end

        |   reverse((r as FreeRegisters _) :: (f as FPArithConst _) :: tl, list, _) =
                reverse(f :: r :: tl, list, true) (* Push it back *)

        |   reverse((r as FreeRegisters _) :: (f as FPArithMemory _) :: tl, list, _) =
                reverse(f :: r :: tl, list, true) (* Push it back *)

(*  This isn't right.  The register may be being freed because its last use was in the
    test associated with the conditional but it could be because the register contains a
    value that is not used in the "fall through" branch but IS used in the branch we're
    going to.
        |   reverse((r as FreeRegisters _) :: (f as ConditionalBranch _) :: tl, list, _) =
                reverse(f :: r :: tl, list, true) (* Push it back *)
*)

        |   reverse((r as FreeRegisters _) :: (f as ArithToGenReg{source=ShortConstArg _, ...}) :: tl, list, _) =
                reverse(f :: r :: tl, list, true) (* Push it back *)

        |   reverse((r as FreeRegisters _) :: (f as FPStatusToEAX) :: tl, list, _) =
                reverse(f :: r :: tl, list, true) (* Push it back *)

        |   reverse((r as FreeRegisters _) :: (f as FPLoadFromConst _) :: tl, list, _) =
                reverse(f :: r :: tl, list, true) (* Push it back *)

        |   reverse((r as FreeRegisters _) :: (f as MoveToRegister{source=RegisterArg _, ...}) :: tl, list, _) =
                reverse(f :: r :: tl, list, true) (* Push it back *)

        |   reverse((r as FreeRegisters regs) :: (f as FPArithR{source, ...}) :: tl, list, rep) =
            let
                val fpr = FPReg source
            in
                if inSet(fpr, regs)
                then (* We're freeing the register after the arithmetic.  Split the sets but
                        otherwise do nothing. *)
                let
                    val left = regSetMinus(regs, singleton fpr)
                in
                    if left = noRegisters
                    then reverse(f :: tl, FreeRegisters(singleton fpr) :: list, rep)
                    else reverse(f :: FreeRegisters left :: tl, FreeRegisters(singleton fpr) :: list, rep)
                end
                else reverse(f :: r :: tl, list, true) (* Push it back *)
            end

            (* Merge multiple sets. *)
        |   reverse(FreeRegisters a :: FreeRegisters b :: tl, list, _) =
                reverse(FreeRegisters(RegSet.regSetUnion(a,b)) :: tl, list, true)

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
                (list, false) => list
            |   (list, true) => repeat list

    in
        if lowLevelOptimise code
        then repeat ops
        else ops
    end

    structure Sharing =
    struct
        type operation = operation
        type code = code
    end
end;
