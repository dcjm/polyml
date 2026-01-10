(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
        
    Further development copyright David C.J. Matthews 2016-18,2020-23

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
functor IntGCode (
    structure CodeCons : INTCODECONS
    structure BackendTree: BACKENDINTERMEDIATECODE
    structure CodeArray: CODEARRAY
    
    sharing CodeCons.Sharing = BackendTree.Sharing = CodeArray.Sharing

) : GENCODE =

struct

    open CodeCons
    open Address
    open BackendTree
    open Misc
    open CodeArray

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
    fun codegen (pt, cvec, resultClosure, numOfArgs, localCount, parameters) =
    let
        datatype decEntry =
            StackAddr of int
        |   Empty
    
        val decVec = Array.array (localCount, Empty)
    
        (* Count of number of items on the stack. *)
        val realstackptr = ref 1 (* The closure ptr is already there *)
        
        (* Maximum size of the stack. *)
        val maxStack = ref 1

        (* Push a value onto the stack. *)
        fun incsp () =
        (
            realstackptr := !realstackptr + 1;
            if !realstackptr > !maxStack
            then maxStack := !realstackptr
            else ()
        )

        (* An entry has been removed from the stack. *)
        fun decsp () = realstackptr := !realstackptr - 1;
 
        fun pushLocalStackValue addr = ( genLocal(!realstackptr + addr, cvec); incsp() )

        (* Loads a local, argument or closure value; translating local
           stack addresses to real stack offsets. *)
        fun locaddr(BICLoadArgument locn) = pushLocalStackValue (numOfArgs-locn)

        |   locaddr(BICLoadLocal locn) =
            (
                (* positive address - on the stack. *)
                    case Array.sub (decVec, locn) of
                        StackAddr n => pushLocalStackValue (~ n)
                    |   _ => (* Should be on the stack, not a function. *)
                        raise InternalError "locaddr: bad stack address"
            )

        |   locaddr(BICLoadClosure locn) = (* closure-pointer relative *)
            (
                genIndirectClosure{addr = !realstackptr-1, item=locn, code=cvec};
                incsp()
            )

        |   locaddr BICLoadRecursive =
                pushLocalStackValue ~1 (* The closure itself - first value on the stack. *)

   (* generates code from the tree *)
   fun gencde (pt : backendIC, whereto : whereto, tailKind : tail, loopAddr) : unit =
   let
     (* Save the stack pointer value here. We may want to reset the stack. *)
     val oldsp = !realstackptr;

        (* Operations on ML memory always have the base as an ML address.
           Word operations are always word aligned.  The higher level will
           have extracted any constant offset and scaled it if necessary.
           That's helpful for the X86 but not for the interpreter.  We
           have to turn them back into indexes. *)
        fun genMLAddress({base, index, offset}, scale) =
        (
            gencde (base, ToStack, NotEnd, loopAddr);
            offset mod scale = 0 orelse raise InternalError "genMLAddress";
            case (index, offset div scale) of
                (NONE, soffset) => (pushConst (toMachineWord soffset, cvec); incsp())
            |   (SOME indexVal, 0) => gencde (indexVal, ToStack, NotEnd, loopAddr)
            |   (SOME indexVal, soffset) =>
                (
                    gencde (indexVal, ToStack, NotEnd, loopAddr);
                    pushConst (toMachineWord soffset, cvec);
                    genOpcode(opcode_wordAdd, cvec)
                )
       )
       
       (* Load the address, index value and offset for non-byte operations.
          Because the offset has already been scaled by the size of the operand
          we have to load the index and offset separately. *)
       fun genCAddress{base, index, offset} =
        (
            gencde (base, ToStack, NotEnd, loopAddr);
            case index of
                NONE => (pushConst (toMachineWord 0, cvec); incsp())
            |   SOME indexVal => gencde (indexVal, ToStack, NotEnd, loopAddr);
            pushConst (toMachineWord offset, cvec); incsp()
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

        |   BICLoadContainer {base, offset} =>
                (gencde (base, ToStack, NotEnd, loopAddr); genIndirectContainer (offset, cvec))
       
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
                                Array.update (decVec, addr, StackAddr (! realstackptr));
                                genMutualDecs (otherDecs)
                            ))

                fun codeDecls(BICRecDecs dl) = genMutualDecs dl

                |   codeDecls(BICDecContainer{size, addr}) =
                    (
                        (* If this is a container we have to process it here otherwise it
                           will be removed in the stack adjustment code. *)
                        genContainer(size, cvec); (* Push the address of this container. *)
                        realstackptr := !realstackptr + size + 1; (* Pushes N words plus the address. *)
                        Array.update (decVec, addr, StackAddr(!realstackptr))
                    )

                |   codeDecls(BICDeclar{value, addr, ...}) =
                    (
                        gencde (value, ToStack, NotEnd, loopAddr);
                        Array.update (decVec, addr, StackAddr(!realstackptr))
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
                     Array.update (decVec, addr, StackAddr (!realstackptr));
                     !realstackptr (* Return the posn on the stack. *)
                    )
                val argIndexList = map genLoopArg args;

                val startSp = ! realstackptr; (* Remember the current top of stack. *)
                val startLoop = createLabel ()
                val () = setLabel(startLoop, cvec) (* Start of loop *)
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
                putBranchInstruction(JumpBack, startLoop, cvec)
            end
  
        |   BICRaise exp =>
            (
                gencde (exp, ToStack, NotEnd, loopAddr);
                genRaiseEx cvec
            )
  
        |   BICHandle {exp, handler, exPacketAddr} =>
            let
                (* Save old handler *)
                val () = genPushHandler cvec
                val () = incsp ()
                val handlerLabel = createLabel()
                val () = putBranchInstruction (SetHandler, handlerLabel, cvec)
                val () = incsp()
                (* Code generate the body; "NotEnd" because we have to come back
                 to remove the handler; "ToStack" because delHandler needs
                 a result to carry down. *)
                val () = gencde (exp, ToStack, NotEnd, loopAddr)
      
                (* Now get out of the handler and restore the old one. *)
                val () = genOpcode(opcode_deleteHandler, cvec)
                val skipHandler = createLabel()
                val () = putBranchInstruction (Jump, skipHandler, cvec)
          
                (* Now process the handler itself. First we have to reset the stack.
                   Note that we have to use "ToStack" again to be consistent with
                   the stack-handling in the body-part. If we actually wanted "NoResult",
                   the stack adjustment code at the end of gencde will take care
                   of this. This means that I don't want to do any clever "end-of-function"
                   optimisation either. SPF 6/1/97
                *)
                val () = realstackptr := oldsp
                val () = setLabel (handlerLabel, cvec)
                (* If we were executing machine code we must re-enter the interpreter. *)
                val () = genEnterIntCatch cvec
                (* Push the exception packet and set the address. *)
                val () = genLdexc cvec
                val () = incsp ()
                val () = Array.update (decVec, exPacketAddr, StackAddr(!realstackptr))
                val () = gencde (handler, ToStack, NotEnd, loopAddr)
                (* Have to remove the exception packet. *)
                val () = resetStack(1, true, cvec)
                val () = decsp()
          
                (* Finally fix-up the jump around the handler *)
                val () = setLabel (skipHandler, cvec)
            in
                ()
            end
  
        |   BICCase ({cases, test, default, firstIndex, ...}) =>
            let
                val () = gencde (test, ToStack, NotEnd, loopAddr)
                (* Label to jump to at the end of each case. *)
                val exitJump = createLabel()

                val () =
                    if firstIndex = 0w0 then ()
                    else
                    (   (* Subtract lower limit.  Don't check for overflow.  Instead
                           allow large value to wrap around and check in "case" instruction. *)
                        pushConst(toMachineWord firstIndex, cvec);
                        genOpcode(opcode_wordSub, cvec)
                    )

                (* Generate the case instruction followed by the table of jumps.  *)
                val nCases = List.length cases
                val caseLabels = genCase (nCases, cvec)
                val () = decsp ()

                (* The default case, if any, follows the case statement. *)
                (* If we have a jump to the default set it to jump here. *)
                local
                    fun fixDefault(NONE, defCase) = setLabel(defCase, cvec)
                    |   fixDefault(SOME _, _) = ()
                in
                    val () = ListPair.appEq fixDefault (cases, caseLabels)
                end
                val () = gencde (default, whereto, tailKind, loopAddr);

                fun genCases(SOME body, label) =
                    (
                        (* First exit from the previous case or the default if
                           this is the first. *)
                        putBranchInstruction(Jump, exitJump, cvec);
                        (* Remove the result - the last case will leave it. *)
                        case whereto of ToStack => decsp () | NoResult => ();
                        (* Fix up the jump to come here. *)
                        setLabel(label, cvec);
                        gencde (body, whereto, tailKind, loopAddr)
                    )
                |   genCases(NONE, _) = ()
                
                val () = ListPair.appEq genCases (cases, caseLabels)
     
                (* Finally set the exit jump to come here. *)
                val () = setLabel (exitJump, cvec)
            in
                ()
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
                                genMoveToContainer(destOffset, cvec);
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
                                genMoveToContainer(destOffset, cvec);
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
                gencde (test, ToStack, NotEnd, loopAddr);
                genEqualWordConst(tag, cvec)
            )

        |   BICNullary {oper=BuiltIns.GetCurrentThreadId} =>
            (
                genOpcode(opcode_getThreadId, cvec);
                incsp()
            )

        |   BICNullary {oper=BuiltIns.CPUPause} =>
            ( (* Do nothing.  It's really only a hint. *)
            )

        |   BICNullary {oper=BuiltIns.CreateMutex} =>
            (
                genOpcode(opcode_createMutex, cvec);
                incsp()
            )

        |   BICUnary { oper, arg1 } =>
            let
                open BuiltIns
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
            in
                case oper of
                    NotBoolean => genOpcode(opcode_notBoolean, cvec)
                |   IsTaggedValue => genIsTagged cvec
                |   MemoryCellLength => genOpcode(opcode_cellLength, cvec)
                |   MemoryCellFlags => genOpcode(opcode_cellFlags, cvec)
                |   ClearMutableFlag => genOpcode(opcode_clearMutable, cvec)
                |   LongWordToTagged => genOpcode(opcode_longWToTagged, cvec)
                |   SignedToLongWord => genOpcode(opcode_signedToLongW, cvec)
                |   UnsignedToLongWord => genOpcode(opcode_unsignedToLongW, cvec)
                |   RealAbs PrecDouble => genOpcode(opcode_realAbs, cvec)
                |   RealNeg PrecDouble => genOpcode(opcode_realNeg, cvec)
                |   RealFixedInt PrecDouble => genOpcode(opcode_fixedIntToReal, cvec)
                |   RealAbs PrecSingle => genOpcode(opcode_floatAbs, cvec)
                |   RealNeg PrecSingle => genOpcode(opcode_floatNeg, cvec)
                |   RealFixedInt PrecSingle => genOpcode(opcode_fixedIntToFloat, cvec)
                |   FloatToDouble => genOpcode(opcode_floatToReal, cvec)
                |   DoubleToFloat => genDoubleToFloat cvec
                |   RealToInt (PrecDouble, rnding) => genRealToInt(rnding, cvec)
                |   RealToInt (PrecSingle, rnding) => genFloatToInt(rnding, cvec)
                |   TouchAddress => resetStack(1, false, cvec) (* Discard this *)
                |   AllocCStack => genOpcode(opcode_allocCSpace, cvec)
                |   LockMutex => genOpcode(opcode_lockMutex, cvec)
                |   TryLockMutex => genOpcode(opcode_tryLockMutex, cvec)
                |   UnlockMutex => genOpcode(opcode_atomicReset, cvec)
                |   Log2Word => genOpcode(opcode_log2Word, cvec)
            end

        |   BICBinary { oper=BuiltIns.WordComparison{test=BuiltIns.TestEqual, ...}, arg1, arg2=BICConstnt(w, _) } =>
            let
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
            in
                genEqualWordConst(toShort w, cvec)
            end

        |   BICBinary { oper=BuiltIns.WordComparison{test=BuiltIns.TestEqual, ...}, arg1=BICConstnt(w, _), arg2 } =>
            let
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
            in
                genEqualWordConst(toShort w, cvec)
            end

        |   BICBinary { oper, arg1, arg2 } =>
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
                |   WordComparison{test=TestUnordered, ...} => raise InternalError "WordComparison: TestUnordered"

                |   PointerEq => genOpcode(opcode_equalWord, cvec)

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
                 
                |   AllocateByteMemory => genOpcode(opcode_allocByteMem, cvec)
                
                |   LargeWordComparison TestEqual => genOpcode(opcode_lgWordEqual, cvec)
                |   LargeWordComparison TestLess => genOpcode(opcode_lgWordLess, cvec)
                |   LargeWordComparison TestLessEqual => genOpcode(opcode_lgWordLessEq, cvec)
                |   LargeWordComparison TestGreater => genOpcode(opcode_lgWordGreater, cvec)
                |   LargeWordComparison TestGreaterEqual => genOpcode(opcode_lgWordGreaterEq, cvec)
                |   LargeWordComparison TestUnordered => raise InternalError "LargeWordComparison: TestUnordered"
                
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

                |   RealComparison (TestEqual, PrecDouble) => genOpcode(opcode_realEqual, cvec)
                |   RealComparison (TestLess, PrecDouble) => genOpcode(opcode_realLess, cvec)
                |   RealComparison (TestLessEqual, PrecDouble) => genOpcode(opcode_realLessEq, cvec)
                |   RealComparison (TestGreater, PrecDouble) => genOpcode(opcode_realGreater, cvec)
                |   RealComparison (TestGreaterEqual, PrecDouble) => genOpcode(opcode_realGreaterEq, cvec)
                |   RealComparison (TestUnordered, PrecDouble) => genOpcode(opcode_realUnordered, cvec)

                |   RealComparison (TestEqual, PrecSingle) => genOpcode(opcode_floatEqual, cvec)
                |   RealComparison (TestLess, PrecSingle) => genOpcode(opcode_floatLess, cvec)
                |   RealComparison (TestLessEqual, PrecSingle) => genOpcode(opcode_floatLessEq, cvec)
                |   RealComparison (TestGreater, PrecSingle) => genOpcode(opcode_floatGreater, cvec)
                |   RealComparison (TestGreaterEqual, PrecSingle) => genOpcode(opcode_floatGreaterEq, cvec)
                |   RealComparison (TestUnordered, PrecSingle) => genOpcode(opcode_floatUnordered, cvec)

                |   RealArith (ArithAdd, PrecDouble) => genOpcode(opcode_realAdd, cvec)
                |   RealArith (ArithSub, PrecDouble) => genOpcode(opcode_realSub, cvec)
                |   RealArith (ArithMult, PrecDouble) => genOpcode(opcode_realMult, cvec)
                |   RealArith (ArithDiv, PrecDouble) => genOpcode(opcode_realDiv, cvec)

                |   RealArith (ArithAdd, PrecSingle) => genOpcode(opcode_floatAdd, cvec)
                |   RealArith (ArithSub, PrecSingle) => genOpcode(opcode_floatSub, cvec)
                |   RealArith (ArithMult, PrecSingle) => genOpcode(opcode_floatMult, cvec)
                |   RealArith (ArithDiv, PrecSingle) => genOpcode(opcode_floatDiv, cvec)

                |   RealArith _ => raise InternalError "RealArith - unimplemented instruction"
                
                |   FreeCStack => genOpcode(opcode_freeCSpace, cvec)

                |   GeneralEquality => raise InternalError "gencde: GeneralEquality"
                    (* Should have been removed at a higher level. *)
                 ;
                decsp() (* Removes one item from the stack. *)
            end
            
        |   BICAllocateWordMemory {numWords as BICConstnt(length, _), flags as BICConstnt(flagByte, _), initial } =>
            if isShort length andalso toShort length = 0w1 andalso isShort flagByte andalso toShort flagByte = 0wx40
            then (* This is a very common case. *)
            (
                gencde (initial, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_alloc_ref, cvec)
            )
            else
            let
                val () = gencde (numWords, ToStack, NotEnd, loopAddr)
                val () = gencde (flags, ToStack, NotEnd, loopAddr)
                val () = gencde (initial, ToStack, NotEnd, loopAddr)
            in
                genOpcode(opcode_allocWordMemory, cvec);
                decsp(); decsp()
            end

        |   BICAllocateWordMemory { numWords, flags, initial } =>
            let
                val () = gencde (numWords, ToStack, NotEnd, loopAddr)
                val () = gencde (flags, ToStack, NotEnd, loopAddr)
                val () = gencde (initial, ToStack, NotEnd, loopAddr)
            in
                genOpcode(opcode_allocWordMemory, cvec);
                decsp(); decsp()
            end

        |   BICLoadOperation { kind=LoadStoreMLWord _, address={base, index=NONE, offset}} =>
            (
                (* If the index is a constant, frequently zero, we can use indirection.
                   The offset is a byte count so has to be divided by the word size but
                   it should always be an exact multiple. *)
                gencde (base, ToStack, NotEnd, loopAddr);
                offset mod Word.toInt wordSize = 0 orelse raise InternalError "gencde: BICLoadOperation - not word multiple";
                genIndirect (offset div Word.toInt wordSize, cvec)
            )

        |   BICLoadOperation { kind=LoadStoreMLWord _, address} =>
            (
                genMLAddress(address, Word.toInt wordSize);
                genOpcode(opcode_loadMLWord, cvec);
                decsp()
            )

        |   BICLoadOperation { kind=LoadStoreMLByte _, address} =>
            (
                genMLAddress(address, 1);
                genOpcode(opcode_loadMLByte, cvec);
                decsp()
            )

        |   BICLoadOperation { kind=LoadStoreC8, address} =>
            (
                genCAddress address;
                genOpcode(opcode_loadC8, cvec);
                decsp(); decsp()
            )

        |   BICLoadOperation { kind=LoadStoreC16, address} =>
            (
                genCAddress address;
                genOpcode(opcode_loadC16, cvec);
                decsp(); decsp()
            )

        |   BICLoadOperation { kind=LoadStoreC32, address} =>
            (
                genCAddress address;
                genOpcode(opcode_loadC32, cvec);
                decsp(); decsp()
            )

        |   BICLoadOperation { kind=LoadStoreC64, address} =>
            (
                genCAddress address;
                genOpcode(opcode_loadC64, cvec);
                decsp(); decsp()
            )

        |   BICLoadOperation { kind=LoadStoreCFloat, address} =>
            (
                genCAddress address;
                genOpcode(opcode_loadCFloat, cvec);
                decsp(); decsp()
            )

        |   BICLoadOperation { kind=LoadStoreCDouble, address} =>
            (
                genCAddress address;
                genOpcode(opcode_loadCDouble, cvec);
                decsp(); decsp()
            )

        |   BICLoadOperation { kind=LoadStoreUntaggedUnsigned, address} =>
            (
                genMLAddress(address, Word.toInt wordSize);
                genOpcode(opcode_loadUntagged, cvec);
                decsp()
            )

        |   BICLoadOperation { kind=LoadStorePolyWord _, address} =>
            (
                genMLAddress(address, Word.toInt wordSize);
                genOpcode(opcode_loadPolyWord, cvec);
                decsp()
            )

        |   BICLoadOperation { kind=LoadStoreNativeWord _, address} =>
            (
                genMLAddress(address, Word.toInt nativeWordSize);
                genOpcode(opcode_loadNativeWord, cvec);
                decsp()
            )

        |   BICStoreOperation { kind=LoadStoreMLWord _, address, value } =>
            (
                genMLAddress(address, Word.toInt wordSize);
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeMLWord, cvec);
                decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreMLByte _, address, value } =>
            (
                genMLAddress(address, 1);
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeMLByte, cvec);
                decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreC8, address, value} =>
            (
                genCAddress address;
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeC8, cvec);
                decsp(); decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreC16, address, value} =>
            (
                genCAddress address;
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeC16, cvec);
                decsp(); decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreC32, address, value} =>
            (
                genCAddress address;
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeC32, cvec);
                decsp(); decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreC64, address, value} =>
            (
                genCAddress address;
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeC64, cvec);
                decsp(); decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreCFloat, address, value} =>
            (
                genCAddress address;
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeCFloat, cvec);
                decsp(); decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreCDouble, address, value} =>
            (
                genCAddress address;
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeCDouble, cvec);
                decsp(); decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreUntaggedUnsigned, address, value} =>
            (
                genMLAddress(address, Word.toInt wordSize);
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeUntagged, cvec);
                decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStorePolyWord _, address, value} =>
            (
                genMLAddress(address, Word.toInt wordSize);
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storePolyWord, cvec);
                decsp(); decsp()
            )

        |   BICStoreOperation { kind=LoadStoreNativeWord _, address, value} =>
            (
                genMLAddress(address, Word.toInt nativeWordSize);
                gencde (value, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_storeNativeWord, cvec);
                decsp(); decsp()
            )

        |   BICBlockOperation { kind=BlockOpMove{isByteMove=true}, sourceLeft, destRight, length } =>
            (
                genMLAddress(sourceLeft, 1);
                genMLAddress(destRight, 1);
                gencde (length, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_blockMoveByte, cvec);
                decsp(); decsp(); decsp(); decsp()
            )

        |   BICBlockOperation { kind=BlockOpMove{isByteMove=false}, sourceLeft, destRight, length } =>
            (
                genMLAddress(sourceLeft, Word.toInt wordSize);
                genMLAddress(destRight, Word.toInt wordSize);
                gencde (length, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_blockMoveWord, cvec);
                decsp(); decsp(); decsp(); decsp()
            )

        |   BICBlockOperation { kind=BlockOpEqualByte, sourceLeft, destRight, length } =>
            (
                genMLAddress(sourceLeft, 1);
                genMLAddress(destRight, 1);
                gencde (length, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_blockEqualByte, cvec);
                decsp(); decsp(); decsp(); decsp()
            )

        |   BICBlockOperation { kind=BlockOpCompareByte, sourceLeft, destRight, length } =>
            (
                genMLAddress(sourceLeft, 1);
                genMLAddress(destRight, 1);
                gencde (length, ToStack, NotEnd, loopAddr);
                genOpcode(opcode_blockCompareByte, cvec);
                decsp(); decsp(); decsp(); decsp()
            )
       
       |    BICArbitrary { oper, arg1, arg2, ... } =>
            let
                open BuiltIns
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
            in
                case oper of
                    ArithAdd  => genOpcode(opcode_arbAdd, cvec)
                |   ArithSub  => genOpcode(opcode_arbSubtract, cvec)
                |   ArithMult => genOpcode(opcode_arbMultiply, cvec)
                |   _ => raise InternalError "Unknown arbitrary precision operation";
                decsp() (* Removes one item from the stack. *)
            end

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
                    if adjustment = 0
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
                    if adjustment = 0
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
   and genProc ({ closure=[], localCount, body, argTypes, name, ...}: bicLambdaForm, mutualDecs, doNext: unit -> unit) : unit =
        let
            (* Create a one word item for the closure.  This is returned for recursive references
               and filled in with the address of the code when we've finished. *)
            val closure = makeConstantClosure()
            val newCode : code = codeCreate(name, parameters);

            (* Code-gen function. No non-local references. *)
             val () =
               codegen (body, newCode, closure, List.length argTypes, localCount, parameters);
            val () = pushConst(closureAsAddress closure, cvec);
            val () = incsp();
        in
            if mutualDecs then doNext () else ()
        end

    |   genProc ({ localCount, body, name, argTypes, closure, ...}, mutualDecs, doNext) =
        let (* Full closure required. *)
            val resClosure = makeConstantClosure()
            val newCode = codeCreate (name, parameters)
            (* Code-gen function. *)
            val () = codegen (body, newCode, resClosure, List.length argTypes, localCount, parameters)
            val closureVars = List.length closure (* Size excluding the code address *)
        in
            if mutualDecs
            then
            let (* Have to make the closure now and fill it in later. *)
                val () = pushConst(toMachineWord resClosure, cvec)
                val () = genAllocMutableClosure(closureVars, cvec)
                val () = incsp ()
           
                val entryAddr : int = !realstackptr

                val () = doNext () (* Any mutually recursive functions. *)

                (* Push the address of the vector - If we have processed other
                   closures the vector will no longer be on the top of the stack. *)
                val () = pushLocalStackValue (~ entryAddr)

                (* Load items for the closure. *)
                fun loadItems ([], _) = ()
                |   loadItems (v :: vs, addr : int) =
                let
                    (* Generate an item and move it into the clsoure *)
                    val () = gencde (BICExtract v, ToStack, NotEnd, NONE)
                    (* The closure "address" excludes the code address. *)
                    val () = genMoveToMutClosure(addr, cvec)
                    val () = decsp ()
                in
                    loadItems (vs, addr + 1)
                end
             
                val () = loadItems (closure, 0)
                val () = genLock cvec (* Lock it. *)
           
                (* Remove the extra reference. *)
                val () = resetStack (1, false, cvec)
            in
                realstackptr := !realstackptr - 1
            end
         
            else
            let
                (* Put it on the stack. *)
                val () = pushConst (toMachineWord resClosure, cvec)
                val () = incsp ()
                val () = List.app (fn pt => gencde (BICExtract pt, ToStack, NotEnd, NONE)) closure
                val () = genClosure (closureVars, cvec)
            in
                realstackptr := !realstackptr - closureVars
            end
        end

    and genCond (testCode, thenCode, elseCode, whereto, tailKind, loopAddr) =
    let
        (* andalso and orelse are turned into conditionals with constants.
           Convert this into a series of tests. *)
        fun genTest(BICConstnt(w, _), jumpOn, targetLabel) =
            let
                val cVal = case toShort w of 0w0 => false | 0w1 => true | _ => raise InternalError "genTest"
            in
                if cVal = jumpOn
                then putBranchInstruction (Jump, targetLabel, cvec)
                else ()
            end

        |   genTest(BICUnary { oper=BuiltIns.NotBoolean, arg1 }, jumpOn, targetLabel) =
                genTest(arg1, not jumpOn, targetLabel)

        |   genTest(BICCond (testPart, thenPart, elsePart), jumpOn, targetLabel) =
            let
                val toElse = createLabel() and exitJump = createLabel()
            in
                genTest(testPart, false, toElse);
                genTest(thenPart, jumpOn, targetLabel);
                putBranchInstruction (Jump, exitJump, cvec);
                setLabel (toElse, cvec);
                genTest(elsePart, jumpOn, targetLabel);
                setLabel (exitJump, cvec)
            end

        |   genTest(testCode, jumpOn, targetLabel) =
            (
                gencde (testCode, ToStack, NotEnd, loopAddr);
                putBranchInstruction(if jumpOn then JumpTrue else JumpFalse, targetLabel, cvec);
                decsp() (* conditional branch pops a value. *)
            )

        val toElse = createLabel() and exitJump = createLabel()
        val () = genTest(testCode, false, toElse)
        val () = gencde (thenCode, whereto, tailKind, loopAddr)
        (* Get rid of the result from the stack. If there is a result then the
        ``else-part'' will push it. *)
        val () = case whereto of ToStack => decsp () | NoResult => ()

        val () = putBranchInstruction (Jump, exitJump, cvec)

        (* start of "else part" *)
        val () = setLabel (toElse, cvec)
        val () = gencde (elseCode, whereto, tailKind, loopAddr)
        val () = setLabel (exitJump, cvec)
    in
        ()
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
                    ()
                end

        (* Have to guarantee that the expression to return the function
          is evaluated before the arguments. *)

        (* Returns true if evaluating it later is safe. *)
        fun safeToLeave (BICConstnt _) = true
        |   safeToLeave (BICLambda _) = true
        |   safeToLeave (BICExtract _) = true
        |   safeToLeave (BICField {base, ...}) = safeToLeave base
        |   safeToLeave (BICLoadContainer {base, ...}) = safeToLeave base
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

   (* Generate the function. *)
   (* Assume we always want a result. There is otherwise a problem if the
      called routine returns a result of type void (i.e. no result) but the
      caller wants a result (e.g. the identity function). *)
    val () = gencde (pt, ToStack, EndOfProc, NONE)

    val () = genReturn (numOfArgs, cvec);
    in (* body of codegen *)
       (* Having code-generated the body of the function, it is copied
          into a new data segment. *)
        copyCode{code = cvec, maxStack = !maxStack, resultClosure=resultClosure, numberOfArguments=numOfArgs}
    end (* codegen *);

    fun gencodeLambda({ name, body, argTypes, localCount, ...}:bicLambdaForm, parameters, closure) =
    let
        (* make the code buffer for the new function. *)
        val newCode : code = codeCreate (name, parameters)
        (* This function must have no non-local references. *)
    in
        codegen (body, newCode, closure, List.length argTypes, localCount, parameters)
    end

    local
        val makeEntryPoint: string -> machineWord = RunCall.rtsCallFull1 "PolyCreateEntryPointObject"

        fun rtsCall makeCall (entryName: string, numOfArgs, debugArgs: Universal.universal list): machineWord =
        let
            open Address
            val cvec = codeCreate (entryName, debugArgs)
            
            val entryPointAddr = makeEntryPoint entryName

            (* Each argument is at the same offset, essentially we're just shifting them *)
            fun genLocals 0 = ()
            |   genLocals n = (genLocal(numOfArgs +1, cvec); genLocals (n-1))
            val () = genLocals numOfArgs

            val () = pushConst(entryPointAddr, cvec)
            val () = makeCall(numOfArgs, cvec)
            val () = genReturn (numOfArgs, cvec)
            val closure = makeConstantClosure()
        
            val () =
                copyCode{code=cvec, maxStack=numOfArgs+1, numberOfArguments=numOfArgs, resultClosure=closure}
        in
            closureAsAddress closure
        end
    in
        structure Foreign = 
        struct

            val rtsCallFast = rtsCall genRTSCallFast
            
            fun rtsCallFastRealtoReal(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastRealtoReal c) (entryName, 1, debugArgs)
            and rtsCallFastRealRealtoReal(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastRealRealtoReal c) (entryName, 2, debugArgs)
            and rtsCallFastGeneraltoReal(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastGeneraltoReal c) (entryName, 1, debugArgs)
            and rtsCallFastRealGeneraltoReal(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastRealGeneraltoReal c) (entryName, 2, debugArgs)
            
            fun rtsCallFastFloattoFloat(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastFloattoFloat c) (entryName, 1, debugArgs)
            and rtsCallFastFloatFloattoFloat(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastFloatFloattoFloat c) (entryName, 2, debugArgs)
            and rtsCallFastGeneraltoFloat(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastGeneraltoFloat c) (entryName, 1, debugArgs)
            and rtsCallFastFloatGeneraltoFloat(entryName, debugArgs) =
                rtsCall (fn (_, c) => genRTSCallFastFloatGeneraltoFloat c) (entryName, 2, debugArgs)
                
            
            type abi = int

            (* This must match the type in Foreign.LowLevel.  Once this is bootstrapped we could use that
               type but note that this is the type we use within the compiler and we build Foreign.LowLevel
               AFTER compiling this. *)
            datatype cTypeForm =
                CTypeFloatingPt | CTypePointer | CTypeSignedInt | CTypeUnsignedInt
            |   CTypeStruct of cType list | CTypeVoid
            withtype cType = { typeForm: cTypeForm, align: word, size: word }

            val abiList: unit -> (string * abi) list =
                RunCall.rtsCallFull0 "PolyInterpretedGetAbiList"

            type cif = Foreign.Memory.voidStar
            val createCIF: abi * cType * cType list -> cif=
                RunCall.rtsCallFull3 "PolyInterpretedCreateCIF"
            val callCFunction: cif * LargeWord.word * LargeWord.word * LargeWord.word -> unit =
                RunCall.rtsCallFull4 "PolyInterpretedCallFunction"
            
            (* foreignCall returns a function that actually calls the foreign function. *)
            fun foreignCall(abi, argTypes, resultType) =
            let
                val memocif = Foreign.Memory.memoise (fn () => createCIF(abi, resultType, argTypes)) ()
                val closure = makeConstantClosure()
                (* For compatibility with the native code version we have to
                   construct a function that takes three arguments rather than
                   a single triple. *)
                val bodyCode =
                    BICEval{function=BICConstnt(toMachineWord callCFunction, []),
                        argList=[
                            (BICTuple[
                                BICEval{
                                    function=BICConstnt(toMachineWord memocif, []),
                                    argList=[(BICConstnt(toMachineWord 0, []), GeneralType)], (* Unit. *)
                                    resultType=GeneralType
                                },
                                BICExtract(BICLoadArgument 0),
                                BICExtract(BICLoadArgument 2),
                                BICExtract(BICLoadArgument 1)], GeneralType)
                        ],
                        resultType=GeneralType}
                val lambdaCode =
                { body = bodyCode, name = "foreignCall", closure=[], argTypes=[GeneralType, GeneralType, GeneralType],
                  resultType = GeneralType, localCount=0}
                val () = gencodeLambda(lambdaCode, [], closure)
            in
                closureAsAddress closure
            end
            
            fun buildCallBack((*abi*) _, (*argTypes*) _, (*resultType*)_) =
            let
                fun buildClosure ((*mlFun*)_: LargeWord.word*LargeWord.word -> unit) =
                    (* The result is the SysWord.word holding the C function. *)
                    raise Foreign.Foreign "foreignCall not implemented"
            in
                Address.toMachineWord buildClosure
            end
        end
    end

    structure Sharing =
    struct
        open BackendTree.Sharing
        type closureRef = closureRef
    end
end;

