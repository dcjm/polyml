(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(*
    Title:      Generate interpretable code for Poly system from the code tree.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)

(* This code-generator is primarily intended as a porting aid from one
   machine-code to another. It is modelled on the VAX code-generator and has
   a similar structure. Note: It does not use the use-count information on
   declarations. *)
functor INTGCODE (

structure CODECONS : INTCODECONSSIG

structure MISC :
sig
  exception InternalError of string;
end;

structure BACKENDTREE: BackendIntermediateCodeSig

) : GENCODESIG =

let

(*****************************************************************************)
(*                  STRETCHARRAY                                             *)
(*****************************************************************************)
structure STRETCHARRAY :
sig
  type 'a stretchArray;
  
  val stretchArray : int * '_a -> '_a stretchArray;
  val update : '_a stretchArray * int * '_a -> unit;
  val sub    : 'a stretchArray * int -> 'a;
end = StretchArray;

in

(*****************************************************************************)
(*                  GCODE functor body                                       *)
(*****************************************************************************)
struct

  open CODECONS;
  open Address;
  open BACKENDTREE;
  open MISC;


  open RuntimeCalls; (* for POLY_SYS numbers *)

  (* gets a value from the run-time system; 
    usually this is a closure, but sometimes it's an int.  *)
  val ioOp : int -> machineWord = RunCall.run_call1 POLY_SYS_io_operation;

  (* minor HACKS *)
  fun forLoop f i n = if i > n then () else (f i; forLoop f (i + 1) n);
  fun apply _ [] = () | apply f (h::t) = (f h; apply f t);
      
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
  | ToStack     (* Need a result but it can stay on the pseudo-stack *);
  
  (* Are we at the end of the function. *)
  datatype tail =
    EndOfProc
  | NotEnd;

  datatype slValue =
    Address    of int  * int (* Address of an entry on the stack. *)
  | Recursive  of machineWord       (* A recursive reference to a closure. *)

  (* Code generate a function or global declaration *)
  fun codegen
       (pt : backendIC,
        cvec : code,
        loadStaticLink : int * int -> slValue,
        numOfArgs: int, parameters) : address =
  let
    val initTrans = 5; (* Initial size of tables. *)
    
    datatype decEntry =
      StackAddr of int
    | Empty;
    
    val decVec : decEntry STRETCHARRAY.stretchArray =
      STRETCHARRAY.stretchArray (initTrans, Empty);
    
    (* Count of number of items on the stack. *)
    val realstackptr = ref 1; (* The static-link/closure ptr is already there *)
 
    (* Exited - set to true if we have jumped out. *)
    val exited = ref false;
 
    (* Push a value onto the stack. For the moment this merely involves
       adding corresponding values to the real and pseudo-stacks. *)
    fun incsp () : unit = realstackptr := !realstackptr + 1;
 
    (* An entry has been removed from the stack. *)
    fun decsp ()  : unit= realstackptr := !realstackptr - 1;
 
    (* Pushes a local or non-local stack value. *)
    fun pushStackValue (addr : int, level : int) : unit =
    let
      val () =
        if level > 0
        then (* Non-local *)
          genNonLocal(!realstackptr, level, addr, cvec)
        else (* Locals and references to the closure. *)
           genLocal(!realstackptr + addr, cvec);
    in
      incsp ()
    end;


    (* Loads a local, argument or closure value; translating local
       stack addresses to real stack offsets. *)
        fun locaddr(BICLoadArgument locn) = pushStackValue (numOfArgs-locn, 0)
        |   locaddr(BICLoadLocal locn) =
            (
                (* positive address - on the stack. *)
                    case STRETCHARRAY.sub (decVec, locn) of
                        StackAddr n => pushStackValue (~ n, 0)

                    |   _ => (* Should be on the stack, not a function. *)
                        raise InternalError "locaddr: bad stack address"
            )
        | locaddr(BICLoadClosure locn) =
            ( (* closure-pointer relative *)
                case loadStaticLink (locn+1, 1) of
                    Recursive codeAddr => ( pushConst (codeAddr, cvec); incsp () )
                |   Address (addr, level) => pushStackValue (addr, level)
            )
        |   locaddr BICLoadRecursive =
            (
                case loadStaticLink (0, 1) of
                    Recursive codeAddr => ( pushConst (codeAddr, cvec); incsp () )
                |   Address (addr, level) => pushStackValue (addr, level)
            )

   (* generates code from the tree *)
   fun gencde (pt : backendIC, whereto : whereto, tailKind : tail, loopAddr) : unit =
   let
     (* Save the stack pointer value here. We may want to reset the stack. *)
     val oldsp = !realstackptr;

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
                                STRETCHARRAY.update (decVec, addr, StackAddr (! realstackptr));
                                genMutualDecs (otherDecs)
                            ))

                fun codeDecls(BICRecDecs dl) = genMutualDecs (dl)
                |   codeDecls(BICDeclar{value, addr, ...}) =
                    (
                        gencde (value, ToStack, NotEnd, loopAddr);
                        STRETCHARRAY.update (decVec, addr, StackAddr(!realstackptr))
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
                     STRETCHARRAY.update (decVec, addr, StackAddr (!realstackptr));
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
           type handler = labels;
           
           (* Save old handler *)
           val () = genPushHandler cvec;
           val () = incsp ();
  
            val genTag =
                putBranchInstruction (setHandler, cvec) before incsp()

            val handlerList : handler list = [genTag]

          (* Code generate the body; "NotEnd" because we have to come back
             to remove the handler; "ToStack" because delHandler needs
             a result to carry down. *)
          val () = gencde (exp, ToStack, NotEnd, loopAddr);
      
          (* Now get out of the handler and restore the old one. *)
          val skipHandler : labels = putBranchInstruction (delHandler, cvec);
          
          (* Now process the handler itself. First we have to reset the stack.
             Note that we have to use "ToStack" again to be consistent with
             the stack-handling in the body-part. If we actually wanted "NoResult",
             the stack adjustment code at the end of gencde will take care
             of this. This means that I don't want to do any clever "end-of-function"
             optimisation either. SPF 6/1/97
          *)
          val () = realstackptr := oldsp;
          val () = exited := false;
          val () = apply (fn handlerLab => fixup (handlerLab, cvec)) handlerList;
      (* If we were executing machine code we must re-enter the interpreter. *)
          val () = genEnterIntCatch cvec;
          val () = gencde (handler, ToStack, NotEnd, loopAddr);
          
          (* Finally fix-up the jump around the handler *)
          val () = fixup (skipHandler, cvec);
         in
           exited := false
         end
  
        |   BICLdexc =>
            let
                (* Get the name of the exception. *)
                val () = genLdexc cvec
            in
                incsp ()
            end
  
        |   BICCase (cas as {cases, caseType, ...}) =>
            let
                val numberOfCases = LargeInt.fromInt(List.length cases)
                val range =
                    case caseType of
                        CaseTag max => Word.toLargeInt max
                    |   _ =>
                        let
                            val (_, aLabel) = hd cases
                            fun foldCases((_, w), (min, max)) = (Word.min(w, min), Word.max(w, max))
                            val (min, max) = List.foldl foldCases (aLabel, aLabel) cases
                        in
                            Word.toLargeInt (max-min)
                        end
            in
                genDenseCase (cas, whereto, tailKind, loopAddr)
            end
  
        |   BICTuple recList =>
         let
           (* Move the fields into the vector. *)
           fun loadItems [] = ()
             | loadItems (v :: vs) =
             let
               val () = gencde (v, ToStack, NotEnd, loopAddr);
             in
               loadItems vs
             end;
             
           val size : int = List.length recList;
  
           val () = loadItems recList;
           val () = genTuple (size, cvec);
         in
           realstackptr := !realstackptr - (size - 1)
         end

            (* Containers are supposed to be implemented on the stack for efficiency.
               Unfortunately they cause problems in this code because we assume
               that if we generate a value to the stack it only occupies a single
               word.  Rather than change this code and risk breaking something I'm
               simply generating them as mutable tuples on the heap. *)
        |   BICContainer size =>
            (* Reserve a number of words on the stack for use as a tuple on the
               stack.  The result is the address of this space. *)
            (
            genContainer(size, cvec); (* Push the address of this container. *)
            incsp() (* Pushes a single word. *)
            )

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
            let
                (* Convert this into a simple equality function. *)
                val () = gencde (test, ToStack, NotEnd, loopAddr)
                val () = pushConst (toMachineWord tag, cvec)
            in
                genOpcode(opcode_equalWord, cvec)
            end

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
            end

        |   BICBuiltIn2 { oper=BuiltIns.LoadWord _, arg1, arg2 } =>
            let
                (* This is temporary.  The load instructions take three arguments:
                   base address, index and offset.  This does not currently use the
                   offset. *)
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
                val () = pushConst (toMachineWord 0, cvec)
                val () = incsp()
            in
                genOpcode(opcode_loadMLWord, cvec);
                decsp(); decsp()
            end

        |   BICBuiltIn2 { oper=BuiltIns.LoadByte _, arg1, arg2 } =>
            let
                (* This is temporary.  The load instructions take three arguments:
                   base address, index and offset.  This does not currently use the
                   offset. *)
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
                val () = pushConst (toMachineWord 0, cvec)
                val () = incsp()
            in
                genOpcode(opcode_loadMLByte, cvec);
                decsp(); decsp()
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
                |   WordComparison{test=BuiltIns.TestNotEqual, ...} => raise InternalError "TestNotEqual" (* Temporary. *)

                |   FixedPrecisionArith ArithAdd => genOpcode(opcode_fixedAdd, cvec)
                |   FixedPrecisionArith ArithSub => genOpcode(opcode_fixedSub, cvec)
                |   FixedPrecisionArith ArithMult => genOpcode(opcode_fixedMult, cvec)
                |   FixedPrecisionArith _ => raise InternalError "TODO: FixedPrecisionArith"

                |   WordArith ArithAdd => genOpcode(opcode_wordAdd, cvec)
                |   WordArith ArithSub => genOpcode(opcode_wordSub, cvec)
                |   WordArith ArithMult => genOpcode(opcode_wordMult, cvec)
                |   WordArith _ => raise InternalError "TODO: WordArith"
                
                |   LoadWord _ => raise InternalError "LoadWord"
                |   LoadByte _ => raise InternalError "LoadWord"
                ;
                decsp() (* Removes one item from the stack. *)
            end

        |   BICBuiltIn3 { oper=BuiltIns.StoreWord, arg1, arg2, arg3 } =>
            let
                (* This is temporary.  The store instructions take four arguments:
                   base address, index, offset and value to store.  This does not currently use the
                   offset. *)
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
                val () = pushConst (toMachineWord 0, cvec)
                val () = incsp()
                val () = gencde (arg3, ToStack, NotEnd, loopAddr)
            in
                genOpcode(opcode_storeMLWord, cvec);
                decsp(); decsp(); decsp()
            end

        |   BICBuiltIn3 { oper=BuiltIns.StoreByte, arg1, arg2, arg3 } =>
            let
                (* This is temporary.  The store instructions take four arguments:
                   base address, index, offset and value to store.  This does not currently use the
                   offset. *)
                val () = gencde (arg1, ToStack, NotEnd, loopAddr)
                val () = gencde (arg2, ToStack, NotEnd, loopAddr)
                val () = pushConst (toMachineWord 0, cvec)
                val () = incsp()
                val () = gencde (arg3, ToStack, NotEnd, loopAddr)
            in
                genOpcode(opcode_storeMLByte, cvec);
                decsp(); decsp(); decsp()
            end

        |   BICBuiltIn4 { oper, arg1, arg2, arg3, arg4 } => raise InternalError "intgcode: BICBuiltIn4"

        |   BICBuiltIn5 { oper, arg1, arg2, arg3, arg4, arg5 } => raise InternalError "intgcode: BICBuiltIn5"

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
          val adjustment = !realstackptr - newsp;

          val () =
            if !exited orelse adjustment = 0
              then ()
            else if adjustment < ~1
              then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
            (* Hack for declarations that should push values, but don't *)
            else if adjustment = ~1
              then pushConst (DummyValue, cvec)
            else
              resetStack (adjustment, true, cvec);
        in
          realstackptr := newsp
        end
          
      | NoResult =>
        let
          val adjustment = !realstackptr - oldsp;

          val () =
            if !exited orelse adjustment = 0
              then ()
            else if adjustment < 0
              then raise InternalError ("gencde: bad adjustment " ^ Int.toString adjustment)
            else
              resetStack (adjustment, false, cvec);
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
            val closure = Address.alloc(0w1, Word8.orb (F_mutable, F_words), Address.toMachineWord 0w0)
            val newCode : code = codeCreate(#name lam, parameters);
        
            (* The only global references are recursive ones (?) *)
            fun loadRecLink _ = Recursive(toMachineWord closure)
        
            (* Code-gen function. No non-local references. *)
             val res : address =
               codegen (#body lam, newCode, loadRecLink, List.length (#argTypes lam), parameters);
            val () = Address.assignWord(closure, 0w0, Address.toMachineWord res)
            val () = Address.lock closure

            val () = pushConst(toMachineWord closure, cvec);
            val () = incsp();
        in
            if mutualDecs then doNext () else ()
        end

    |   genProc (lam, mutualDecs: bool, doNext: unit -> unit) : unit =
      
        let (* Full closure required. *)
    (* If there is a closure we must fetch all non-local references
       from it. *)

    (* Loads the value from the closure. *)
    fun loadSl (addr : int, level : int) : slValue =
      if addr = 0
      then (* recursive - return address of closure. *)
        Address (~1, level - 1)
      else (* Return an entry in the closure. *)
        Address (addr, level);

        val newCode : code = codeCreate (#name lam, parameters);

        (* Code-gen function. *)
        val res : address =
          codegen (#body lam, newCode, loadSl, List.length (#argTypes lam), parameters);

        val sizeOfClosure = List.length (#closure lam) + 1;
      in
         if mutualDecs
         then let (* Have to make the closure now and fill it in later. *)
           val () = genGetStore (sizeOfClosure, cvec);
           val () = incsp ();
           
           (* Put code address into closure *)
           val () = pushConst(toMachineWord res, cvec); 
           val () = genMoveToVec(0, cvec);
           
           val entryAddr : int = !realstackptr;

           val () = doNext (); (* Any mutually recursive functions. *)

           (* Push the address of the vector - If we have processed other
              closures the vector will no longer be on the top of the stack. *)
           val () = pushStackValue (~ entryAddr, 0);

           (* Load items for the closure. *)
           fun loadItems ([], _) = ()
             | loadItems (v :: vs, addr : int) =
             let
               (* Generate an item and move it into the vector *)
               val () = gencde (BICExtract v, ToStack, NotEnd, NONE);
               val () = genMoveToVec(addr, cvec);
               val () = decsp ();
             in
               loadItems (vs, addr + 1)
             end;
             
           val () = loadItems (#closure lam, 1);
           val () = genLock cvec; (* Lock it. *)
           
           (* Remove the extra reference. *)
           val () = resetStack (1, false, cvec);
         in
           realstackptr := !realstackptr - 1
         end
         
         else let
           (* Put it on the stack. *)
           val () = pushConst (toMachineWord res, cvec);
           val () = incsp ();
           
           val () =
             apply (fn pt => gencde (BICExtract pt, ToStack, NotEnd, NONE)) (#closure lam);
                
           val () = genTuple (sizeOfClosure, cvec);
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
     val () = gencde (first, ToStack, NotEnd, loopAddr);
     val toElse : labels = putBranchInstruction(jumpFalse, cvec);
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
       | loadArgs (v :: vs) =
       let (* Push each expression onto the stack. *)
         val () = gencde(v, ToStack, NotEnd, NONE);
       in
         loadArgs vs
       end;

     (* Called after the args and the closure to call have been pushed
        onto the stack. *)
     fun callClosure () : unit =
       case tailKind of
         NotEnd => (* Normal call. *)
           genCallClosure cvec
         
       | EndOfProc => (* Tail recursive call. *)
         let
           (* Get the return address onto the top of the stack. *)
           val () = pushStackValue (0, 0);
           
           (* Slide the return address, closure and args over the
              old closure, return address and args, and reset the
              stack. Then jump to the closure. *)
           val () =
             genTailCall(argsToPass + 2, !realstackptr - 1 + (numOfArgs - argsToPass), cvec);
            (* It's "-1" not "-2", because we didn't bump the realstackptr
               when we pushed the return address. SPF 3/1/97 *)
         in
           exited := true
         end;
         
      val () =
    case #function eval of
       (* The function is being loaded from the stack or closure so it
          may be a static-link function. *)
      BICExtract ext =>
        let
            (* Since the function is on the stack there can be no side-effects
               in loading it. Can therefore load the arguments now. *)
            val () = loadArgs argList;
  
            val staticLinkValue =
                case ext of
                    BICLoadArgument addr => Address (numOfArgs - addr, 0)
                |   BICLoadLocal addr =>
                    (
                        (* Local - is it a function? *)
                        case STRETCHARRAY.sub (decVec, addr) of
                            StackAddr n => Address (~ n, 0)
                        |   Empty       => raise InternalError "staticLinkValue: missing decVec entry"
                    )
                |   BICLoadClosure addr => loadStaticLink (addr+1, 1)(* Non-local or recursive. *)
                |   BICLoadRecursive => loadStaticLink (0, 1)
       
            val () =
                case staticLinkValue of
                    Address (addr, level) => pushStackValue (addr, level)
                        (* recursive reference to a function. *)
                |   Recursive closureAddr => (pushConst(closureAddr, cvec); incsp())
        in
            callClosure ()
        end (* BICExtract *)
    
    | _ => (* The function is not being found by simply loading a value
          from the stack or the closure. *)
      let
        (* Must load the closure and call. *)
        (* Have to guarantee that the expression to return the function
          is evaluated before the arguments. *)
    
        (* Returns true if evaluating it later is safe. *)
        fun safeToLeave (node: backendIC) : bool =
          case node of
        BICConstnt _ => true
          | BICLambda _  => true
          | BICField {base, ...} => 
        (* Safe only if the expression (always a type) being indirected
           is. This is put in because calling functions in a type is a
           common occurence and should be made reasonably efficient. *)
           safeToLeave base
          | _        => false
    
        val () =
          if (case argList of [] => true | _ => safeToLeave (#function eval))
          then let
        (* Can load the args first. *)
        val () = loadArgs argList;
          in 
        gencde (#function eval, ToStack, NotEnd, NONE)
          end
          
          else let
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
     in
       (* Call the function. *)
       callClosure ()
     end; (* Not BICExtract *)

       (* Make sure we interpret when we return from the call *)
       val () = genEnterIntCall (cvec, argsToPass);

     in (* body of genEval *)
       realstackptr := !realstackptr - argsToPass (* Args popped by caller. *)
     end

   (* This is used for cases which are not sparse i.e. the values occupy
      a narrow enough range to make indexing efficient. *)
   and genDenseCase 
        (pt as { cases, ...}: caseForm,
         whereto: whereto,
         tailKind: tail,
         loopAddr) : unit =
   let     
     val () = gencde (#test pt, ToStack, NotEnd, loopAddr);

     (* The exit jumps are chained together. *)
     val lastEndJump : labels ref = ref noJump;

    val (_, min: LargeInt.int, max:LargeInt.int) =
        case #caseType pt of
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
       else let (* Subtract lower limit. *)
         val () = pushConst(toMachineWord min, cvec)
       in
         genOpcode(opcode_fixedSub, cvec) (* Is this fixed, word or arbitrary? *)
       end;

     val () = genCase (limit, cvec);
     val () = decsp ();
 
     (* Addresses are relative to the first entry in the vector. *)
     val startVec : addrs = ic cvec;
     
     (* Set each address in the vector to point to the default - they
         will be overwritten by the actual address later. *)
     val defaultAddr : int = (limit + 1) * 2;
     
     val () =
       forLoop (fn (_ : int) => genWord (defaultAddr, cvec)) 0 limit;

     (* The default case, if any, follows the case statement. *)
     val () = gencde (#default pt, whereto, tailKind, loopAddr);

     val () = exited := false;

     (* Now generate the code for each of the cases. *)
     fun genEachCase ([] : (backendIC * word) list) : unit = ()
       | genEachCase ((body : backendIC, matches : word) :: otherCases) : unit =
       let
     (* First exit from the previous case or the default if
        this is the first. *)
     val () = 
       lastEndJump :=
         linkLabels
           (!lastEndJump,
        if !exited then noJump else putBranchInstruction(jump, cvec),
        cvec);
        
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
         
     val () = genEachCase (#cases pt);
     
     (* Finally go down the list of exit labels pointing them to here. *)
     val () = fixup (!lastEndJump, cvec);
   in
     exited := false
   end (* genDenseCase *)
     
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

        (* The only global references are recursive ones (?) *)
        fun loadRecLink (_ : int, _ : int) : slValue = Recursive(toMachineWord closure)

        (* This function must have no non-local references. *)
        val codeAddr =
            codegen (body, newCode, loadRecLink, List.length argTypes, parameters)
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
            val closure = alloc(0w1, Word8.orb (F_mutable, F_words), toMachineWord 0w0)

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
        end
    end

end (* GCODE functor body *)

end; (* structure-level let *)
