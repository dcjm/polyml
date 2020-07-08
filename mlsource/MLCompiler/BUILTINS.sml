(*
    Signature for built-in functions

    Copyright David C. J. Matthews 2016, 2018-20

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
        TestEqual (* No TestNotEqual because that is always generated with "not" *)
    |   TestLess
    |   TestLessEqual
    |   TestGreater
    |   TestGreaterEqual
    |   TestUnordered (* Reals only. *)

    datatype arithmeticOperations =
        ArithAdd
    |   ArithSub
    |   ArithMult
    |   ArithQuot
    |   ArithRem
    |   ArithDiv
    |   ArithMod

    datatype logicalOperations =
        LogicalAnd
    |   LogicalOr
    |   LogicalXor
    
    datatype shiftOperations =
        ShiftLeft
    |   ShiftRightLogical   (* Logical shift - zero added bits. *)
    |   ShiftRightArithmetic (* Arithmetic shift - add the sign bit. *)

    datatype unaryOps =
        NotBoolean (* true => false; false => true - XOR *)
    |   IsTaggedValue (* Test the tag bit. *)
    |   MemoryCellLength (* Return the length of a memory cell (heap object) *)
    |   MemoryCellFlags (* Return the flags byte of a memory cell (heap object) *)
    |   ClearMutableFlag (* Remove the mutable flag from the flags byte *)
    |   AtomicIncrement
    |   AtomicDecrement
    |   AtomicReset (* Set a value to (tagged) zero atomically. *)
    |   LongWordToTagged (* Convert a LargeWord.word to a Word.word or FixedInt.int. *)
    |   SignedToLongWord (* Convert a tagged value to a LargeWord with sign extension. *)
    |   UnsignedToLongWord (* Convert a tagged value to a LargeWord without sign extension. *)
    |   RealAbs of precision     (* Set the sign bit of a real to positive. *)
    |   RealNeg of precision     (* Invert the sign bit of a real. *)
    |   RealFixedInt of precision (* Convert an integer value into a real value. *)
    |   FloatToDouble (* Convert a single precision floating point value to double precision. *)
    |   DoubleToFloat of IEEEReal.rounding_mode option (* Convert a double precision floating point value to single precision. *)
    |   RealToInt of precision * IEEEReal.rounding_mode (* Convert a double or float to a fixed precision int. *)
    |   TouchAddress (* Ensures that the cell is reachable. *)

    and precision = PrecSingle | PrecDouble (* Single or double precision floating pt. *)

    and binaryOps =
        (* Compare two words and return the result.  This is used for both
           word values (isSigned=false) and fixed precision integer (isSigned=true).
           Values must be tagged and not pointers. *)
        WordComparison of { test: testConditions, isSigned: bool }
        (* Fixed precision int operations.  These may raise Overflow. *)
    |   FixedPrecisionArith of arithmeticOperations
        (* Arithmetic operations on word values.  These do not raise Overflow. *)
    |   WordArith of arithmeticOperations
        (* Load a word at a specific offset in a heap object.  If this is immutable and the
           arguments are constants it can be folded at compile time since the result will
           never change. *)
    |   WordLogical of logicalOperations (* Logical operations on words. *)
    |   WordShift of shiftOperations (* Shift operations on words. *)
         (* Allocate a heap cell for byte data.  The first argument is the number of words (not bytes)
            needed.  The second argument is the "flags" byte which must include F_bytes and F_mutable.
            The new cell is not initialised. *)
    |   AllocateByteMemory
        (* Operations on LargeWords.  These are 32/64 bit values that are "boxed". *)
    |   LargeWordComparison of testConditions
    |   LargeWordArith of arithmeticOperations
    |   LargeWordLogical of logicalOperations
    |   LargeWordShift of shiftOperations
    |   RealComparison of testConditions * precision
    |   RealArith of arithmeticOperations * precision
        (* Equality of values which could be pointers or tagged values.
           At the lowest level this is the same as WordComparison but
           if we try to use an indexed case there must be a check that the
           values are tagged. *)
    |   PointerEq
        (* Arbitrary precision operations using emulation. *)
    |   LongArbComparison of testConditions
    |   LongArbArith of arithmeticOperations
        
    val unaryRepr: unaryOps -> string
    and binaryRepr: binaryOps -> string
    and testRepr: testConditions -> string
    and arithRepr: arithmeticOperations -> string
end;
