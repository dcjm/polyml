(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(*
    Title:      Generate interpretable code for Poly system from the code tree.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* This generates byte-code that is interpreted by the run-time system.  It
   is now used as a fall-back to allow Poly/ML to run on non-X86 architectures.
   Early versions were used as a porting aid while a native code-generator
   was being developed and the "enter-int" instructions that were needed
   for that have been retained although they no longer actually generate code. *)
functor INTGCODE (

    structure CODECONS : INTCODECONSSIG

    structure MISC:
    sig
        exception InternalError of string;
    end;

    structure BACKENDTREE: BackendIntermediateCodeSig

) : GENCODESIG =

struct

    open CODECONS
    open Address
    open BACKENDTREE
    open MISC

    fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 1) n);

    val word0 = toMachineWord 0;
  
    val DummyValue : machineWord = word0; (* used as result of "raise e" etc. *)

    type caseForm =
        {
            cases   : (backendIC * word) list,
            test    : backendIC,
            caseType: caseType,
            default : backendIC
        }
   
    (* Where the result, if any, should go *)
    datatype whereto =
        NoResult     (* discard result *)
    |   ToStack     (* Need a result but it can stay on the pseudo-stack *);
  
    (* Are we at the end of the function. *)
    datatype tail =
        EndOfProc
    |   NotEnd

    (* Code generate a function or global declaration *)
    fun codegen (pt : backendIC, cvec : code, recReference: machineWord option, numOfArgs: int, parameters) : address =
    let
        val initTrans = 5 (* Initial size of tables. *)
    
        datatype decEntry =
            StackAddr of int
        |   Empty
    
        val decVec = StretchArray.stretchArray (initTrans, Empty)
    
        (* Count of number of items on the stack. *)
        val realstackptr = ref 1; (* The static-link/closure ptr is already there *)

        (* Exited - set to true if we have jumped out. *)
        val exited = ref false;

        (* Push a value onto the stack. For the moment this merely involves
           adding corresponding values to the real and pseudo-stacks. *)
        fun incsp () : unit = realstackptr := !realstackptr + 1;

        (* An entry has been removed from the stack. *)
        fun decsp ()  : unit= realstackptr := !realstackptr - 1;
 
        fun pushLocalStackValue addr = ( genLocal(!realstackptr + addr, cvec); incsp() )

        (* Loads a local, argument or closure value; translating local
           stack addresses to real stack offsets. *)
        fun locaddr(BICLoadArgument locn) = pushLocalStackValue (numOfArgs-locn)

        |   locaddr(BICLoadLocal locn) =
            (
                (* positive address - on the stack. *)
                    case StretchArray.sub (decVec, locn) of
                        StackAddr n => pushLocalStackValue (~ n)
                    |   _ => (* Should be on the stack, not a function. *)
                        raise InternalError "locaddr: bad stack address"
            )

        |   locaddr(BICLoadClosure locn) = (* closure-pointer relative *)
            (
                pushLocalStackValue ~1; (* The closure itself. *)
                genIndirect (locn+1, cvec) (* The value in the closure. +1 because first item is code addr. *)
            )

        |   locaddr BICLoadRecursive =
            (
                case recReference of
                    SOME codeAddr => ( pushConst (codeAddr, cvec); incsp () )
                |   NONE => pushLocalStackValue ~1 (* The closure itself - first value on the stack. *)
            )

   (* generates code from the tree *)
   fun gencde (pt : backendIC, whereto : whereto, tailKind : tail, loopAddr) : unit =
   let
     (* Save the stack pointer value here. We may want to reset the stack. *)
     val oldsp = !realstackptr;

        (* Load the address and index value for byte operations. *)
        fun genByteAddress{base, index, offset} =
        (
            gencde (base, ToStack, NotEnd, loopAddr);
            (* Because the index and offset are both byte counts we can just add
               them if we need both. *)
            case (index, offset) of
                (NONE, offset) => (pushConst (toMachineWord offset, cvec); incsp())
            |   (SOME indexVal, 0w0) => gencde (indexVal, ToStack, NotEnd, loopAddr)
            |   (SOME indexVal, offset) =>
                (
                    gencde (indexVal, ToStack, NotEnd, loopAddr);
                    pushConst (toMachineWord offset, cvec);
                    genOpcode(opcode_wordAdd, cvec)
                )
       )

     val () =
       case pt of
            BICEval evl => genEval (evl, tailKind)

        |   BICExtract ext =>
            (* This may just be being used to discard a value which isn't
              used on this branch. *)
                if whereto = NoResult then () else locaddr ext
       
        |   BICField {base, offset} =>
                (gencde (base, ToStack, NotEnd, loopAddr); genIndirect (offset, cvec))
       
        |   BICLambda lam => genProc (lam, false, fn () => ())
           
        |   BICConstnt(w, _) =>
            let
                val () = pushConst (w, cvec);
            in
                incsp ()
            end
  
        |   BICCond (testPart, thenPart, elsePart) =>
                genCond (testPart, thenPart, elsePart, whereto, tailKind, loopAddr)
  
        |   BICNewenv(decls, exp) =>
            let         
                (* Processes a list of entries. *)
            
                (* Mutually recursive declarations. May be either lambdas or constants. Recurse down
                   the list pushing the addresses of the closure vectors, then unwind the 
                   recursion and fill them in. *)
                fun genMutualDecs [] = ()

                |   genMutualDecs ({lambda, addr, ...} :: otherDecs) =
                        genProc (lambda, true,
                            fn() =>
                            (
                                StretchArray.update (decVec, addr, StackAddr (! realstackptr));
                                genMutualDecs (otherDecs)
                            ))

                fun codeDecls(BICRecDecs dl) = genMutualDecs dl

                |   codeDecls(BICDeclar{value=BICContainer size, addr, ...}) =
                    (
                        (* If this is a container we have to process it here otherwise it
                           will be removed in the stack adjustment code. *)
                        genContainer(size, cvec); (* Push the address of this container. *)
                        realstackptr := !realstackptr + size + 1; (* Pushes N words plus the address. *)
                        StretchArray.update (decVec, addr, StackAddr(!realstackptr))
                    )

                |   codeDecls(BICDeclar{value, addr, ...}) =
                    (
                        gencde (value, ToStack, NotEnd, loopAddr);
                        StretchArray.update (decVec, addr, StackAddr(!realstackptr))
                    )
                |   codeDecls(BICNullBinding exp) = gencde (exp, NoResult, NotEnd, loopAddr)
            in
                List.app codeDecls decls;
                gencde (exp, whereto, tailKind, loopAddr)
            end
          
        |   BICBeginLoop {loop=body, arguments} =>
            (* Execute the body which will contain at least one Loop instruction.
               There will also be path(s) which don't contain Loops and these
               will drop through. *)
            let
                val args = List.map #1 arguments
                (* Evaluate each of the arguments, pushing the result onto the stack. *)
                fun genLoopArg ({addr, value, ...}) =
                    (
                     gencde (value, ToStack, NotEnd, loopAddr);
                     StretchArray.update (decVec, addr, StackAddr (!realstackptr));
                     !realstackptr (* Return the posn on the stack. *)
                    )
                val argIndexList = map genLoopArg args;

                val startSp = ! realstackptr; (* Remember the current top of stack. *)
                val startLoop : addrs = ic cvec; (* Start of loop *)
            in
                (* Process the body, passing the jump-back address down for the Loop instruction(s). *)
                gencde (body, whereto, tailKind, SOME(startLoop, startSp, argIndexList))
                (* Leave the arguments on the stack.  They can be cleared later if needed. *)
            end

        |   BICLoop argList => (* Jump back to the enclosing BeginLoop. *)
            let
                val (startLoop, startSp, argIndexList) =
                    case loopAddr of
                        SOME l => l
                    |   NONE => raise InternalError "No BeginLoop for Loop instr"
                (* Evaluate the arguments.  First push them to the stack because evaluating
                   an argument may depend on the current value of others.  Only when we've
                   evaluated all of them can we overwrite the original argument positions. *)
                fun loadArgs ([], []) = !realstackptr - startSp (* The offset of all the args. *)
                  | loadArgs (arg:: argList, _ :: argIndexList) =
                    let
                        (* Evaluate all the arguments. *)
                        val () = gencde (arg, ToStack, NotEnd, NONE);
                        val argOffset = loadArgs(argList, argIndexList);
                    in
                        genSetStackVal(argOffset, cvec); (* Copy the arg over. *)
                        decsp(); (* The argument has now been popped. *)
                        argOffset
                    end
                  | loadArgs _ = raise InternalError "loadArgs: Mismatched arguments";

                val _: int = loadArgs(List.map #1 argList, argIndexList)
            in
                if !realstackptr <> startSp
                then resetStack (!realstackptr - startSp, false, cvec) (* Remove any local variables. *)
                else ();
            
                (* Jump back to the start of the loop. *)
                jumpback (startLoop, cvec)
            end
  
        |   BICRaise exp =>
            let
                val () = gencde (exp, ToStack, NotEnd, loopAddr)
                val () = genRaiseEx cvec;
            in
                exited := true
            end
  
        |   BICHandle {exp, handler} =>
            let
                (* Save old handler *)
                val () = genPushHandler cvec
                val () = incsp ()
                val genTag = putBranchInstruction (setHandler, cvec) before incsp()
                (* Code generate the body; "NotEnd" because we have to come back
                 to remove the handler; "ToStack" because delHandler needs
                 a result to carry down. *)
                val () = gencde (exp, ToStack, NotEnd, loopAddr)
      
                (* Now get out of the handler and restore the old one. *)
                val skipHandler = putBranchInstruction (delHandler, cvec)
          
                (* Now process the handler itself. First we have to reset the stack.
                   Note that we have to use "ToStack" again to be consistent with
                   the stack-handling in the body-part. If we actually wanted "NoResult",
                   the stack adjustment code at the end of gencde will take care
                   of this. This means that I don't want to do any clever "end-of-function"
                   optimisation either. SPF 6/1/97
                *)
                val () = realstackptr := oldsp
                val () = exited := false
                val () = fixup (genTag, cvec)
                (* If we were executing machine code we must re-enter the interpreter. *)
                val () = genEnterIntCatch cvec
                val () = gencde (handler, ToStack, NotEnd, loopAddr)
          
                (* Finally fix-up the jump around the handler *)
                val () = fixup (skipHandler, cvec)
            in
                exited := false
            end
  
        |   BICLdexc => (genLdexc cvec; incsp ())
  
        |   BICCase ({cases, caseType, test, default, ...}) =>
            let
                val () = gencde (test, ToStack, NotEnd, loopAddr)
                (* The exit jumps are chained together. *)
                val lastEndJump : labels ref = ref noJump;

                val (_, min: LargeInt.int, max:LargeInt.int) =
                    case caseType of
                        CaseTag max => (true, 0, Word.toLargeInt max)
                |   _ =>
                    let
                        val (_, aLabel) = hd cases
                        fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                        val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                    in
                        (false, Word.toLargeInt min, Word.toLargeInt max)
                    end

                val limit : int = Int.fromLarge(max - min)

                val () =
                    if min = 0 then ()
                    else
                    ( (* Subtract lower limit. *)
                        pushConst(toMachineWord min, cvec);
                        genOpcode(opcode_fixedSub, cvec) (* Is this fixed, word or arbitrary? *)
                    )

                val () = genCase (limit, cvec);
                val () = decsp ();
 
                (* Addresses are relative to the first entry in the vector. *)
                val startVec : addrs = ic cvec;

                (* Set each address in the vector to point to the default - they
                 will be overwritten by the actual address later. *)
                val defaultAddr : int = (limit + 1) * 2;

                val () = forLoop (fn (_ : int) => genWord (defaultAddr, cvec)) 0 limit;

                (* The default case, if any, follows the case statement. *)
                val () = gencde (default, whereto, tailKind, loopAddr);

                val () = exited := false;

                (* Now generate the code for each of the cases. *)
                fun genEachCase ([] : (backendIC * word) list) : unit = ()
                |   genEachCase ((body : backendIC, matches : word) :: otherCases) : unit =
                    let
                     (* First exit from the previous case or the default if
                        this is the first. *)
                        val () = 
                            lastEndJump := linkLabels
                                (!lastEndJump,
                                if !exited then noJump else putBranchInstruction(jump, cvec),
                                cvec)
                        (* Remove the result - the last case will leave it. *)
                        val () = case whereto of ToStack => decsp () | NoResult => ();
     
                        (* Now put the address of this code into the table if
                           an entry has not already been set. If it has the new
                           entry should be ignored. It is NOT an error (just
                           redundant). *)
                        local
                            val entryAddr : addrs = addrPlus(startVec, Int.fromLarge(Word.toLargeInt matches - min) * 2)
                        in
                            val () =
                                if getBytes(2, entryAddr, cvec) = defaultAddr
                                then putBytes(addrMinus(ic cvec, startVec), 2, entryAddr, cvec)
                                else ()
                        end

                        (* Generate code for this case *)
                        val () = exited := false;
                        val () = gencde (body, whereto, tailKind, loopAddr);
                    in
                        genEachCase otherCases
                    end; (* genEachCase *)
         
                val () = genEachCase cases
     
                (* Finally go down the list of exit labels pointing them to here. *)
                val () = fixup (!lastEndJump, cvec);
            in
                exited := false
            end
  
        |   BICTuple recList =>
            let
                val size = List.length recList
            in
                (* Move the fields into the vector. *)
                List.app(fn v => gencde (v, ToStack, NotEnd, loopAddr)) recList;
                genTuple (size, cvec);
                realstackptr := !realstackptr - (size - 1)
            end

        |   BICContainer _ => raise InternalError "BICContainer" (* This must be processed in codeDecls. *)

        |   BICSetContainer{container, tuple, filter} =>
            (* Copy the contents of a tuple into a container.  If the tuple is a
               Tuple instruction we can avoid generating the tuple and then
               unpacking it and simply copy the fields that make up the tuple
               directly into the container. *)
            (
                case tuple of
                    BICTuple cl =>
                        (* Simply set the container from the values. *)
                    let
                        (* Load the address of the container. *)
                        val _ = gencde (container, ToStack, NotEnd, loopAddr);
                        fun setValues([], _, _) = ()

                        |   setValues(v::tl, sourceOffset, destOffset) =
                            if sourceOffset < BoolVector.length filter andalso BoolVector.sub(filter, sourceOffset)
                            then
                            (
                                gencde (v, ToStack, NotEnd, loopAddr);
                                (* Move the entry into the container. This instruction
                                   pops the value to be moved but not the destination. *)
                                genMoveToVec(destOffset, cvec);
                                decsp();
                                setValues(tl, sourceOffset+1, destOffset+1)
                            )
                            else setValues(tl, sourceOffset+1, destOffset)
                    in
                        setValues(cl, 0, 0)
                        (* The container address is still on the stack. *)
                    end

                |   _ =>
                    let (* General case. *)
                        (* First the target tuple, then the container. *)
                        val () = gencde (tuple, ToStack, NotEnd, loopAddr)
                        val () = gencde (container, ToStack, NotEnd, loopAddr)
                        val last = BoolVector.foldli(fn (i, true, _) => i | (_, false, n) => n) ~1 filter

                        fun copy (sourceOffset, destOffset) =
                            if BoolVector.sub(filter, sourceOffset)
                            then
                            (
                                (* Duplicate the tuple address . *)
                                genLocal(1, cvec);
                                genIndirect(sourceOffset, cvec);
                                genMoveToVec(destOffset, cvec);
                                if sourceOffset = last
                                then ()
                                else copy (sourceOffset+1, destOffset+1)
                            )
                            else copy(sourceOffset+1, destOffset)
                    in
                        copy (0, 0)
                        (* The container and tuple addresses are still on the stack. *)
                    end
            )

        |   BICTagTest { test, tag, ... } =>
            (
                (* Convert this into a simple equality function. *)
                gencde (test, ToStack, NotEnd, loopAddr);
                pushConst (toMachineWord tag, cvec);
                genOpcode(opcode_equalWord, cvec)
            )

        |   BICBuiltIn0 { oper=BuiltIns.CurrentThreadId } =>
            (
                genOpcode(opcode_getThreadId, cvec);
                incsp()
            )

        |   BICBuiltIn1 { oper, arg1 } =>
            let
                open BuiltIns
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
            in
                case oper of
                    NotBoolean => genOpcode(opcode_notBoolean, cvec)
                |   IsTaggedValue => genOpcode(opcode_isTagged, cvec)
                |   MemoryCellLength => genOpcode(opcode_cellLength, cvec)
                |   MemoryCellFlags => genOpcode(opcode_cellFlags, cvec)
                |   ClearMutableFlag => genOpcode(opcode_clearMutable, cvec)
                |   StringLengthWord => genOpcode(opcode_stringLength, cvec)
                |   AtomicIncrement => genOpcode(opcode_atomicIncr, cvec)
                |   AtomicDecrement => genOpcode(opcode_atomicDecr, cvec)
                |   AtomicReset => genOpcode(opcode_atomicReset, cvec)
                |   LongWordToTagged => genOpcode(opcode_longWToTagged, cvec)
                |   SignedToLongWord => genOpcode(opcode_signedToLongW, cvec)
                |   UnsignedToLongWord => genOpcode(opcode_unsignedToLongW, cvec)
                |   RealAbs => genOpcode(opcode_realAbs, cvec)
                |   RealNeg => genOpcode(opcode_realNeg, cvec)
                |   FloatFixedInt => genOpcode(opcode_floatFixedInt, cvec)
            end

        |   BICBuiltIn2 { oper, arg1, arg2 } =>
            let
                open BuiltIns
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
            in
                case oper of
                    WordComparison{test=TestEqual, ...} => genOpcode(opcode_equalWord, cvec)
                |   WordComparison{test=TestLess, isSigned=true} => genOpcode(opcode_lessSigned, cvec)
                |   WordComparison{test=TestLessEqual, isSigned=true} => genOpcode(opcode_lessEqSigned, cvec)
                |   WordComparison{test=TestGreater, isSigned=true} => genOpcode(opcode_greaterSigned, cvec)
                |   WordComparison{test=TestGreaterEqual, isSigned=true} => genOpcode(opcode_greaterEqSigned, cvec)
                |   WordComparison{test=TestLess, isSigned=false} => genOpcode(opcode_lessUnsigned, cvec)
                |   WordComparison{test=TestLessEqual, isSigned=false} => genOpcode(opcode_lessEqUnsigned, cvec)
                |   WordComparison{test=TestGreater, isSigned=false} => genOpcode(opcode_greaterUnsigned, cvec)
                |   WordComparison{test=TestGreaterEqual, isSigned=false} => genOpcode(opcode_greaterEqUnsigned, cvec)

                |   FixedPrecisionArith ArithAdd => genOpcode(opcode_fixedAdd, cvec)
                |   FixedPrecisionArith ArithSub => genOpcode(opcode_fixedSub, cvec)
                |   FixedPrecisionArith ArithMult => genOpcode(opcode_fixedMult, cvec)
                |   FixedPrecisionArith ArithQuot => genOpcode(opcode_fixedQuot, cvec)
                |   FixedPrecisionArith ArithRem => genOpcode(opcode_fixedRem, cvec)
                |   FixedPrecisionArith ArithDiv => raise InternalError "TODO: FixedPrecisionArith ArithDiv"
                |   FixedPrecisionArith ArithMod => raise InternalError "TODO: FixedPrecisionArith ArithMod"

                |   WordArith ArithAdd => genOpcode(opcode_wordAdd, cvec)
                |   WordArith ArithSub => genOpcode(opcode_wordSub, cvec)
                |   WordArith ArithMult => genOpcode(opcode_wordMult, cvec)
                |   WordArith ArithDiv => genOpcode(opcode_wordDiv, cvec)
                |   WordArith ArithMod => genOpcode(opcode_wordMod, cvec)
                |   WordArith _ => raise InternalError "WordArith - unimplemented instruction"
                
                |   WordLogical LogicalAnd => genOpcode(opcode_wordAnd, cvec)
                |   WordLogical LogicalOr => genOpcode(opcode_wordOr, cvec)
                |   WordLogical LogicalXor => genOpcode(opcode_wordXor, cvec)

                |   WordShift ShiftLeft => genOpcode(opcode_wordShiftLeft, cvec)
                |   WordShift ShiftRightLogical => genOpcode(opcode_wordShiftRLog, cvec)
                |   WordShift ShiftRightArithmetic => genOpcode(opcode_wordShiftRArith, cvec)
                 
                |   SetStringLengthWord => genOpcode(opcode_setStringLength, cvec)
                |   AllocateByteMemory => genOpcode(opcode_allocByteMem, cvec)
                
                |   LargeWordComparison TestEqual => genOpcode(opcode_lgWordEqual, cvec)
                |   LargeWordComparison TestLess => genOpcode(opcode_lgWordLess, cvec)
                |   LargeWordComparison TestLessEqual => genOpcode(opcode_lgWordLessEq, cvec)
                |   LargeWordComparison TestGreater => genOpcode(opcode_lgWordGreater, cvec)
                |   LargeWordComparison TestGreaterEqual => genOpcode(opcode_lgWordGreaterEq, cvec)
                
                |   LargeWordArith ArithAdd => genOpcode(opcode_lgWordAdd, cvec)
                |   LargeWordArith ArithSub => genOpcode(opcode_lgWordSub, cvec)
                |   LargeWordArith ArithMult => genOpcode(opcode_lgWordMult, cvec)
                |   LargeWordArith ArithDiv => genOpcode(opcode_lgWordDiv, cvec)
                |   LargeWordArith ArithMod => genOpcode(opcode_lgWordMod, cvec)
                |   LargeWordArith _ => raise InternalError "LargeWordArith - unimplemented instruction"

                |   LargeWordLogical LogicalAnd => genOpcode(opcode_lgWordAnd, cvec)
                |   LargeWordLogical LogicalOr => genOpcode(opcode_lgWordOr, cvec)
                |   LargeWordLogical LogicalXor => genOpcode(opcode_lgWordXor, cvec)
                |   LargeWordShift ShiftLeft => genOpcode(opcode_lgWordShiftLeft, cvec)
                |   LargeWordShift ShiftRightLogical => genOpcode(opcode_lgWordShiftRLog, cvec)
                |   LargeWordShift ShiftRightArithmetic => genOpcode(opcode_lgWordShiftRArith, cvec)
                
                |   RealComparison TestEqual => genOpcode(opcode_realEqual, cvec)
                |   RealComparison TestLess => genOpcode(opcode_realLess, cvec)
                |   RealComparison TestLessEqual => genOpcode(opcode_realLessEq, cvec)
                |   RealComparison TestGreater => genOpcode(opcode_realGreater, cvec)
                |   RealComparison TestGreaterEqual => genOpcode(opcode_realGreaterEq, cvec)

                |   RealArith ArithAdd => genOpcode(opcode_realAdd, cvec)
                |   RealArith ArithSub => genOpcode(opcode_realSub, cvec)
                |   RealArith ArithMult => genOpcode(opcode_realMult, cvec)
                |   RealArith ArithDiv => genOpcode(opcode_realDiv, cvec)
                |   RealArith _ => raise InternalError "RealArith - unimplemented instruction"
                 ;
                decsp() (* Removes one item from the stack. *)
            end
            
        |   BICBuiltIn3 {
                oper=BuiltIns.AllocateWordMemory, arg1 as BICConstnt(length, _), arg2 as BICConstnt(flags, _), arg3 } =>
            if isShort length andalso toShort length = 0w1 andalso isShort flags andalso toShort flags = 0wx40
            then (* This is a very common case. *)
            (
                gencde (arg3, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_alloc_ref, cvec)
            )
            else
            let
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
                val () = gencde (arg3, ToStack, NotEnd, loopAddr)
            in
                genOpcode(opcode_allocWordMemory, cvec);
                decsp(); decsp()
            end

        |   BICBuiltIn3 { oper=BuiltIns.AllocateWordMemory, arg1, arg2, arg3 } =>
            let
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
                val () = gencde (arg3, ToStack, NotEnd, loopAddr)
            in
                genOpcode(opcode_allocWordMemory, cvec);
                decsp(); decsp()
            end

       |    BICLoadOperation { kind=LoadStoreMLWord _, address={base, index=NONE, offset}} =>
            (
                (* If the index is a constant, frequently zero, we can use indirection.
                   The offset is a byte count so has to be divided by the word size but
                   it should always be an exact multiple. *)
                gencde (base, ToStack, NotEnd, loopAddr);
                offset mod (Word.fromInt wordSize) = 0w0 orelse raise InternalError "gencde: BICLoadOperation - not word multiple";
                genIndirect (Word.toInt(offset div Word.fromInt wordSize), cvec)
            )

       |    BICLoadOperation { kind=LoadStoreMLWord _, address={base, index=SOME indexVal, offset}} =>
            let
                (* Variable index. *)
                val () = gencde (base, ToStack, NotEnd, loopAddr)
                val () = gencde (indexVal, ToStack, NotEnd, loopAddr)
                val () = (pushConst (toMachineWord offset, cvec); incsp())
            in
                genOpcode(opcode_loadMLWord, cvec);
                decsp(); decsp()
            end

       |    BICLoadOperation { kind=LoadStoreMLByte _, address} =>
            (
                genByteAddress address;
                genOpcode(opcode_loadMLByte, cvec);
                decsp()
            )

       |    BICLoadOperation { kind=LoadStoreC8, address} => raise InternalError "TODO: LoadStoreC8"

       |    BICLoadOperation { kind=LoadStoreC16, address} => raise InternalError "TODO: LoadStoreC16"

       |    BICLoadOperation { kind=LoadStoreC32, address} => raise InternalError "TODO: LoadStoreC32"

       |    BICLoadOperation { kind=LoadStoreC64, address} => raise InternalError "TODO: LoadStoreC64"

       |    BICLoadOperation { kind=LoadStoreCFloat, address} => raise InternalError "TODO: LoadStoreCFloat"

       |    BICLoadOperation { kind=LoadStoreCDouble, address} => raise InternalError "TODO: LoadStoreCDouble"

       |    BICStoreOperation { kind=LoadStoreMLWord _, address={base, index=NONE, offset}, value } =>
            let
                (* No index.  We could almost use move_to_vec here except that it leaves
                   the destination address on the stack instead of replacing it with "unit". *)
                val () = gencde (base, ToStack, NotEnd, loopAddr)
                val () = pushConst (toMachineWord 0, cvec)
                val () = incsp()
                val () = pushConst (toMachineWord offset, cvec)
                val () = incsp()
                val () = gencde (value, ToStack, NotEnd, loopAddr)
            in
                genOpcode(opcode_storeMLWord, cvec);
                decsp(); decsp(); decsp()
            end

       |    BICStoreOperation { kind=LoadStoreMLWord _, address={base, index=SOME indexVal, offset}, value } =>
            let
                (* Variable index *)
                val () = gencde (base, ToStack, NotEnd, loopAddr)
                val () = gencde (indexVal, ToStack, NotEnd, loopAddr)
                val () = pushConst (toMachineWord offset, cvec)
                val () = incsp()
                val () = gencde (value, ToStack, NotEnd, loopAddr)
            in
                genOpcode(opcode_storeMLWord, cvec);
                decsp(); decsp(); decsp()
            end

       |    BICStoreOperation { kind=LoadStoreMLByte _, address, value } =>
            (
                genByteAddress address;
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeMLByte, cvec);
                decsp(); decsp()
            )

       |    BICStoreOperation { kind=LoadStoreC8, address, value} => raise InternalError "TODO: StoreC8"

       |    BICStoreOperation { kind=LoadStoreC16, address, value} => raise InternalError "TODO: StoreC16"

       |    BICStoreOperation { kind=LoadStoreC32, address, value} => raise InternalError "TODO: StoreC32"

       |    BICStoreOperation { kind=LoadStoreC64, address, value} => raise InternalError "TODO: StoreC64"

       |    BICStoreOperation { kind=LoadStoreCFloat, address, value} => raise InternalError "TODO: StoreCFloat"

       |    BICStoreOperation { kind=LoadStoreCDouble, address, value} => raise InternalError "TODO: StoreCDouble"

        |   BICBlockOperation { kind=BlockOpMove{isByteMove=true}, sourceLeft, destRight, length } =>
            (
                genByteAddress sourceLeft;
                genByteAddress destRight;
                gencde (length, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_blockMoveByte, cvec);
                decsp(); decsp(); decsp(); decsp()
            )

        |   BICBlockOperation { kind=BlockOpMove{isByteMove=false}, sourceLeft, destRight, length } =>
            let
                (* This is more complicated - we can't just add the index and offset. *)
                fun loadAddress{base, index, offset} =
                (
                    gencde (base, ToStack, NotEnd, loopAddr);
                    case index of
                        NONE => (pushConst (toMachineWord 0, cvec); incsp())
                    |   SOME indexVal => gencde (indexVal, ToStack, NotEnd, loopAddr);
                    pushConst (toMachineWord offset, cvec); incsp()
                )
            in
                loadAddress sourceLeft;
                loadAddress destRight;
                gencde (length, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_blockMoveWord, cvec);
                decsp(); decsp(); decsp(); decsp(); decsp(); decsp()
            end

        |   BICBlockOperation { kind=BlockOpEqualByte, sourceLeft, destRight, length } =>
            (
                genByteAddress sourceLeft;
                genByteAddress destRight;
                gencde (length, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_blockEqualByte, cvec);
                decsp(); decsp(); decsp(); decsp()
            )

        |   BICBlockOperation { kind=BlockOpCompareByte, sourceLeft, destRight, length } =>
            (
                genByteAddress sourceLeft;
                genByteAddress destRight;
                gencde (length, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_blockCompareByte, cvec);
                decsp(); decsp(); decsp(); decsp()
            )

    in (* body of gencde *) 

      (* This ensures that there is precisely one item on the stack if
         whereto = ToStack and no items if whereto = NoResult. 
         There are two points to note carefully here:
           (1) Negative stack adjustments are legal if we have exited.
               This is because matchFailFn can cut the stack back too
               far for its immediately enclosing expression. This is
               harmless because the code actually exits that expression.
           (2) A stack adjustment of ~1 is legal if we're generating
               a declaration in "ToStack" mode, because not all declarations
               actually generate the dummy value that we expect. This
               used to be handled in resetStack itself, but it's more
               transparent to do it here. (In addition, there was a bug in
               resetStack - it accumulated the stack resets, but didn't
               correctly accumulate these "~1" dummy value pushes.)
               It's all much better now.
               SPF 9/1/97
     *)
        case whereto of
            ToStack =>
            let
                val newsp = oldsp + 1;
                val adjustment = !realstackptr - newsp

                val () =
                    if !exited orelse adjustment = 0
                    then ()
                    else if adjustment < ~1
                    then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
                    (* Hack for declarations that should push values, but don't *)
                    else if adjustment = ~1
                    then pushConst (DummyValue, cvec)
                    else resetStack (adjustment, true, cvec)
            in
                realstackptr := newsp
            end
          
        |   NoResult =>
            let
                val adjustment = !realstackptr - oldsp

                val () =
                    if !exited orelse adjustment = 0
                    then ()
                    else if adjustment < 0
                    then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
                    else resetStack (adjustment, false, cvec)
            in
                realstackptr := oldsp
            end
    end (* gencde *)

   (* doNext is only used for mutually recursive functions where a
     function may not be able to fill in its closure if it does not have
     all the remaining declarations. *)
    (* TODO: This always creates the closure on the heap even when makeClosure is false. *) 
   and genProc (lam: bicLambdaForm as { closure=[], ...}, mutualDecs, doNext: unit -> unit) : unit =
        let
            (* Create a one word item for the closure.  This is returned for recursive references
               and filled in with the address of the code when we've finished. *)
            val closure = Address.allocWordData(0w1, Word8.orb (F_mutable, F_words), Address.toMachineWord 0w0)
            val newCode : code = codeCreate(#name lam, parameters);

            (* Code-gen function. No non-local references. *)
             val res : address =
               codegen (#body lam, newCode, SOME(toMachineWord closure), List.length (#argTypes lam), parameters);
            val () = Address.assignWord(closure, 0w0, Address.toMachineWord res)
            val () = Address.lock closure

            val () = pushConst(toMachineWord closure, cvec);
            val () = incsp();
        in
            if mutualDecs then doNext () else ()
        end

    |   genProc (lam, mutualDecs, doNext) =
        let (* Full closure required. *)
            val newCode = codeCreate (#name lam, parameters)
            (* Code-gen function. *)
            val res = codegen (#body lam, newCode, NONE, List.length (#argTypes lam), parameters);
            val sizeOfClosure = List.length (#closure lam) + 1;
        in
            if mutualDecs
            then
            let (* Have to make the closure now and fill it in later. *)
                (* This previously used genGetStore which at one time was widely used. *)
                val () = pushConst(toMachineWord sizeOfClosure, cvec) (* Length *)
                val () = pushConst(toMachineWord F_mutable, cvec) (* Flags *)
                val () = pushConst(toMachineWord 0, cvec) (* Initialise to zero. *)
                val () = genOpcode(opcode_allocWordMemory, cvec) (* Allocate the memory. *)
                val () = incsp ()
           
                (* Put code address into closure *)
                val () = pushConst(toMachineWord res, cvec) 
                val () = genMoveToVec(0, cvec)
           
                val entryAddr : int = !realstackptr

                val () = doNext () (* Any mutually recursive functions. *)

                (* Push the address of the vector - If we have processed other
                   closures the vector will no longer be on the top of the stack. *)
                val () = pushLocalStackValue (~ entryAddr)

                (* Load items for the closure. *)
                fun loadItems ([], _) = ()
                |   loadItems (v :: vs, addr : int) =
                let
                    (* Generate an item and move it into the vector *)
                    val () = gencde (BICExtract v, ToStack, NotEnd, NONE)
                    val () = genMoveToVec(addr, cvec)
                    val () = decsp ()
                in
                    loadItems (vs, addr + 1)
                end
             
                val () = loadItems (#closure lam, 1)
                val () = genLock cvec (* Lock it. *)
           
                (* Remove the extra reference. *)
                val () = resetStack (1, false, cvec)
            in
                realstackptr := !realstackptr - 1
            end
         
            else
            let
                (* Put it on the stack. *)
                val () = pushConst (toMachineWord res, cvec)
                val () = incsp ()
                val () = List.app (fn pt => gencde (BICExtract pt, ToStack, NotEnd, NONE)) (#closure lam)
                val () = genTuple (sizeOfClosure, cvec)
            in
                realstackptr := !realstackptr - (sizeOfClosure - 1)
            end
        end

   and genCond (first: backendIC,
                second: backendIC,
                third: backendIC,
                whereto: whereto,
                tailKind: tail,
                loopAddr) : unit =
   let
     val () = gencde (first, ToStack, NotEnd, loopAddr)
     val toElse : labels = putBranchInstruction(jumpFalse, cvec)
     val () = decsp();
   in
        let
     val () = gencde (second, whereto, tailKind, loopAddr);
     (* Get rid of the result from the stack. If there is a result then the
        ``else-part'' will push it. *)
     val () = case whereto of ToStack => decsp () | NoResult => ();
     
     val thenExited : bool = !exited;
 
     val toExit : labels =
       if thenExited then noJump
       else putBranchInstruction (jump, cvec);
    
     (* start of "else part" *)
     val () = fixup (toElse, cvec);
     val () = exited := false;
     val () = gencde (third, whereto, tailKind, loopAddr);
 
     val elseExited : bool= !exited;
     
     val () = fixup (toExit, cvec);
       in
     exited := (thenExited andalso elseExited) (* Only exited if both sides did. *)
       end
   end (* genCond *)

    and genEval (eval, tailKind : tail) : unit =
    let
        val argList : backendIC list = List.map #1 (#argList eval)
        val argsToPass : int = List.length argList;

        (* Load arguments *)
        fun loadArgs [] = ()
        |   loadArgs (v :: vs) =
        let (* Push each expression onto the stack. *)
            val () = gencde(v, ToStack, NotEnd, NONE)
        in
            loadArgs vs
        end;

        (* Called after the args and the closure to call have been pushed
            onto the stack. *)
        fun callClosure () : unit =
            case tailKind of
                NotEnd => (* Normal call. *) genCallClosure cvec
         
            |   EndOfProc => (* Tail recursive call. *)
                let
                    (* Get the return address onto the top of the stack. *)
                    val () = pushLocalStackValue 0
           
                    (* Slide the return address, closure and args over the
                      old closure, return address and args, and reset the
                      stack. Then jump to the closure. *)
                    val () =
                        genTailCall(argsToPass + 2, !realstackptr - 1 + (numOfArgs - argsToPass), cvec);
                        (* It's "-1" not "-2", because we didn't bump the realstackptr
                           when we pushed the return address. SPF 3/1/97 *)
                in
                    exited := true
                end

        (* Have to guarantee that the expression to return the function
          is evaluated before the arguments. *)

        (* Returns true if evaluating it later is safe. *)
        fun safeToLeave (BICConstnt _) = true
        |   safeToLeave (BICLambda _) = true
        |   safeToLeave (BICExtract _) = true
        |   safeToLeave (BICField {base, ...}) = safeToLeave base
        |   safeToLeave _ = false

        val () =
            if (case argList of [] => true | _ => safeToLeave (#function eval))
            then
            let
                (* Can load the args first. *)
                val () = loadArgs argList
            in 
                gencde (#function eval, ToStack, NotEnd, NONE)
            end

            else
            let
                (* The expression for the function is too complicated to
                   risk leaving. It might have a side-effect and we must
                   ensure that any side-effects it has are done before the
                   arguments are loaded. *)
                val () = gencde(#function eval, ToStack, NotEnd, NONE);
                val () = loadArgs(argList);
                (* Load the function again. *)
                val () = genLocal(argsToPass, cvec);
            in
                incsp ()
            end

        val () = callClosure () (* Call the function. *)

        (* Make sure we interpret when we return from the call *)
        val () = genEnterIntCall (cvec, argsToPass)

    in (* body of genEval *)
        realstackptr := !realstackptr - argsToPass (* Args popped by caller. *)
    end

     
    (* Closure functions must start with ``enterIntProc''. This is actually a
       break-point instruction in the machine-code instruction set to make sure
       that the code is interpreted. It is a no-op if we are already
       interpreting. *)
    val () = genEnterIntProc (cvec, numOfArgs); (* SPF 23/6/95 *)

   (* Generate the function. *)
   (* Assume we always want a result. There is otherwise a problem if the
      called routine returns a result of type void (i.e. no result) but the
      caller wants a result (e.g. the identity function). *)
    val () = gencde (pt, ToStack, EndOfProc, NONE)

    val () = if !exited then () else genReturn (numOfArgs, cvec);
  in (* body of codegen *)
   (* Having code-generated the body of the function, it is copied
      into a new data segment. *)
   copyCode cvec
 end (* codegen *);

    fun gencodeLambda({ name, body, argTypes, ...}:bicLambdaForm, parameters, closure) =
    let
        (* make the code buffer for the new function. *)
        val newCode : code = codeCreate (name, parameters)
        (* This function must have no non-local references. *)
        val codeAddr =
            codegen (body, newCode, SOME(toMachineWord closure), List.length argTypes, parameters)
    in
        assignWord(closure, 0w0, toMachineWord codeAddr);
        lock closure
    end

    local
        val makeEntryPoint: string -> machineWord = Compat560.createEntryPointObject

        fun rtsCall makeCall (entryName: string, numOfArgs, debugArgs: Universal.universal list): address =
        let
            open Address
            val cvec = codeCreate (entryName, debugArgs)
            val closure = allocWordData(0w1, Word8.orb (F_mutable, F_words), toMachineWord 0w0)

            val entryPointAddr = makeEntryPoint entryName

            (* Each argument is at the same offset, essentially we're just shifting them *)
            fun genLocals 0 = ()
            |   genLocals n = (genLocal(numOfArgs +1, cvec); genLocals (n-1))
            val () = genLocals numOfArgs

            val () = pushConst(entryPointAddr, cvec)
            val () = makeCall(numOfArgs, cvec)
            val () = genReturn (numOfArgs, cvec)
        
            val codeAddr = copyCode cvec
        in
            assignWord(closure, 0w0, toMachineWord codeAddr);
            lock closure;
            closure
        end
    in
        structure Foreign = 
        struct

            val rtsCallFast = rtsCall genRTSCallFast
            and rtsCallFull = rtsCall genRTSCallFull
            
            fun rtsCallFastFloattoFloat(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastFloatFloat c) (entryName, 1, debugArgs)
            fun rtsCallFastGeneraltoFloat(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastGeneralFloat c) (entryName, 1, debugArgs)
        end
    end

end;

