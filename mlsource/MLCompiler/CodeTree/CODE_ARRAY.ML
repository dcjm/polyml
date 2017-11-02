(*
    Copyright (c) 2017 David C.J. Matthews
    
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation
    version 2.1 of the License.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

structure CODE_ARRAY :>
sig
    type machineWord = Address.machineWord
    and  address = Address.address
    type byteVec and codeVec

    val byteVecMake:            word -> byteVec
    val byteVecSet:             byteVec * word * Word8.word -> unit
    val byteVecToCodeVec:       byteVec -> codeVec
    
    val codeVecGet:             codeVec * word -> Word8.word
    val codeVecSet:             codeVec * word * Word8.word -> unit
    val codeVecAddr:            codeVec -> address
    val codeVecPutWord:         codeVec * word * machineWord -> unit
    
    datatype constantType = ConstAbsolute | ConstX86Relative
    
    val codeVecPutConstant:     codeVec * word * machineWord * constantType -> unit
    
    val codeVecLockAndGetExecutable: codeVec -> address
end = 
struct
    open Address
    open Misc

    datatype csegStatus =
        Bytes
    |   UnlockedCode

    type byteVec = address
    and codeVec = address

    val objLength: address -> word = length

    val F_mutable_bytes =  Word.fromLargeWord(Word8.toLargeWord(Word8.orb (F_mutable, F_bytes)))
    val wordSize = Word.fromInt Address.wordSize
    
    fun byteVecMake size =
    let
        val vec : address = RunCall.allocateByteMemory(size, F_mutable_bytes)
        (* allocateByteMemory does not clear the area.  We have to do that at least
           to ensure that the constant area is cleared before we copy it into a
           real code area.  In many cases we could get away with clearing less
           but for the moment this is the safest way. *)
        val byteLength = size * wordSize
        fun clear n =
            if n < byteLength then (assignByte(vec, n, 0w0); clear (n+0w1)) else ()
        val () = clear 0w0
    in
        vec
    end

    (* These use specific C functions to do the work. *)
    val byteVecToCodeVec = RunCall.rtsCallFull1 "PolyCopyByteVecToCode"
    and codeVecLockAndGetExecutable = RunCall.rtsCallFull1 "PolyLockMutableCode"

    (* Return the address of the segment. Used when putting in "self" addresses. *)
    fun codeVecAddr vector = vector
  
    (* Returns a value from the vector.  Used when printing code. *)
    fun codeVecGet (addr, byteIndex : word) : Word8.word =
    let
        val lengthWords = objLength addr
        val lengthBytes = wordSize * lengthWords
    in
        if byteIndex < lengthBytes
        then loadByte (addr, byteIndex)
        else raise Subscript
    end

    (* Set a byte. Used when setting the byte data. *)
    fun byteVecSet (addr, byteIndex, value: Word8.word) =
    let
        val lengthWords = objLength addr
        val lengthBytes = wordSize * lengthWords
    in
        if byteIndex < lengthBytes then assignByte (addr, byteIndex, value)
        else raise Subscript
    end

    (* Set a byte.  Used when fixing up code references. *)
    val codeVecSet = byteVecSet

    (* Puts in a word at a given word offset.  This can only be used when the
       word is on a word boundary. *)
    fun codeVecPutWord (addr, wordIndex, value:machineWord) : unit =
    let
        val lengthWords = objLength addr
    in
        if lengthWords <= wordIndex
        then raise Subscript
        else assignWord (addr, wordIndex, value)
    end
    
    datatype constantType = ConstAbsolute | ConstX86Relative

    local
        val setCodeConstantCall = RunCall.rtsCallFast4 "PolySetCodeConstant"
    in
        (* Store a constant into the code.  This must be used if the constant is
           not on a word boundary or if it needs special treatment. *)
        fun codeVecPutConstant (addr, byteIndex, value:machineWord, option: constantType) =
        let
            val lengthWords = objLength addr
            val lengthBytes = wordSize * lengthWords
            val optValue =
                case option of ConstAbsolute => 0w0 | ConstX86Relative => 0w1
        in
            if lengthBytes <= byteIndex
            then raise Subscript
            else setCodeConstantCall(addr, byteIndex, value, optValue)
        end
    end

end;
