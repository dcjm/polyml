(*
    Signature for built-in functions

    Copyright David C. J. Matthews 2016

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

signature BUILTINS =
sig
    datatype testConditions =
        TestEqual
    |   TestNotEqual
    |   TestLess
    |   TestLessEqual
    |   TestGreater
    |   TestGreaterEqual

    datatype arithmeticOperations =
        ArithAdd
    |   ArithSub
    |   ArithMult
    |   ArithQuot
    |   ArithRem

    datatype logicalOperations =
        LogicalAnd
    |   LogicalOr
    |   LogicalXor
    
    datatype shiftOperations =
        ShiftLeft
    |   ShiftRightLogical   (* Logical shift - zero added bits. *)
    |   ShiftRightArithmetic (* Arithmetic shift - add the sign bit. *)

    datatype builtIn0Ops =
        CurrentThreadId

    and builtIn1Ops =
        NotBoolean (* true => false; false => true - XOR *)
    |   IsTaggedValue (* Test the tag bit. *)
    |   MemoryCellLength (* Return the length of a memory cell (heap object) *)
    |   MemoryCellFlags (* Return the flags byte of a memory cell (heap object) *)
    |   ClearMutableFlag (* Remove the mutable flag from the flags byte *)
    |   StringLengthWord (* Load the length word from a string.  N.B. Doesn't check for single char strings. *)
    |   AtomicIncrement
    |   AtomicDecrement
    |   AtomicReset (* Set a value to (tagged) zero atomically. *)

    and builtIn2Ops =
        (* Compare two words and return the result.  This is used for both
           word values (isSigned=false) and fixed precision integer (isSigned=true).
           Tests for (in)equality can also be done on pointers in which case
           this is pointer equality. *)
        WordComparison of { test: testConditions, isSigned: bool }
        (* Fixed precision int operations.  These may raise Overflow. *)
    |   FixedPrecisionArith of arithmeticOperations
        (* Arithmetic operations on word values.  These do not raise Overflow. *)
    |   WordArith of arithmeticOperations
        (* Load a word at a specific offset in a heap object.  If this is immutable and the
           arguments are constants it can be folded at compile time since the result will
           never change. *)
    |   LoadWord of { isImmutable: bool }
    |   LoadByte of { isImmutable: bool }
    |   SetStringLengthWord (* Untag the second argument and store it at the address in the first. *)
    |   WordLogical of logicalOperations (* Logical operations on words. *)
    |   WordShift of shiftOperations (* Shift operations on words. *)

    and builtIn3Ops =
        StoreWord
    |   StoreByte

    and builtIn4Ops =
        Built4PlaceHolder

    and builtIn5Ops =
        Built5PlaceHolder
        
    val builtIn0Repr: builtIn0Ops -> string
    and builtIn1Repr: builtIn1Ops -> string
    and builtIn2Repr: builtIn2Ops -> string
    and builtIn3Repr: builtIn3Ops -> string
    and builtIn4Repr: builtIn4Ops -> string
    and builtIn5Repr: builtIn5Ops -> string
end;
