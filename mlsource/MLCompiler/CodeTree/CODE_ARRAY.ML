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
    Title:  Code vector operations.
    Author:     Dave Matthews, Cambridge University Computer Laboratory
    Copyright   Cambridge University 1985
*)


(*
   This module constructs and operates on segments for machine code.
  "csegMake" constructs and returns a segment of the specified size with
  give flag codes.
  The segment can be read and written a byte at a time using the "csegGet"
  procedure, and the code can be entered using "csegCall" (this is only used
  if the code is an "early" procedure which must be called by the compiler
  or is a declaration or expression at the outer level. More usually
  "csegAddr" is used to return the address of the segment which is then
  compiled into another procedure as a constant.
  When a procedure has been compiled "csegLock" is called to remove the
  "mutable" bit. The segment then becomes a constant and cannot be
  changed.
  It is used in two ways. Initially the code is assembled into a segment which
  expands as necessary so that there is always room for the code. When the
  code has been completely assembled and we know how much there is, it is
  copied into a segment of the correct size. 
*)


structure CODE_ARRAY :
sig
    type machineWord
    type address
    type cseg

    val csegMake:          word  -> cseg
    val csegConvertToCode: cseg -> unit
    val csegGet:           cseg * word -> Word8.word
    val csegSet:           cseg * word * Word8.word -> unit
    val csegGetWord:       cseg * word -> machineWord
    val csegPutWord:       cseg * word * machineWord -> unit
    val csegCopySeg:       cseg * cseg * word * word -> unit
    val csegAddr:          cseg -> address
    val csegLockAndGetExecutable: cseg -> address
    val csegPutConstant:   cseg * word * machineWord * word -> unit
end = 
struct
    open Address
    open Misc

    datatype csegStatus =
        Bytes
    |   UnlockedCode

    type cseg = (csegStatus * address) ref

    val objLength: address -> word = length

    val expansionFactor = 0w3 (* Factor by which to increase size of segment *)

    val F_mutable_bytes =  Word.fromLargeWord(Word8.toLargeWord(Word8.orb (F_mutable, F_bytes)))
    val wordSize = Word.fromInt Address.wordSize
    
    fun allocateAndClear size =
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

    fun csegMake (size : word) : cseg = ref (Bytes, allocateAndClear size)

    fun csegConvertToCode (r as ref (Bytes, addr)) : unit =
        let
            (* Convert the byte data into code. *)
            val newAddr = Compat560.polySpecificGeneral (107, addr)
        in
            r := (UnlockedCode, newAddr)
        end
    | csegConvertToCode _ = 
       raise InternalError "csegConvertToCode: not a byte segment"

    fun csegLockAndGetExecutable (ref (UnlockedCode, addr)): address =
        (* Lock the code and return the executable address.  This currently just returns
           the original address. *)
        Compat560.polySpecificGeneral (106, addr)

    |   csegLockAndGetExecutable _ = raise InternalError "csegLockAndGetExecutable: not an unlocked code segment"
 
    fun csegAddr (ref (status, addr)) : address =
    if status = Bytes
    then raise InternalError "csegAddr: not a code segment"
    else addr
    
    (* Copy a segment.  This can only be called while the code is being constructed when it
       consists only of bytes.  Constants are not added to the vector at this stage. *)
    fun csegCopySeg (ref (Bytes, fromAddr), ref (Bytes, toAddr), lengthBytes : word, offsetWords : word) =
    let
        val fromLengthWords : word = objLength fromAddr
        val toLengthWords   : word = objLength toAddr
    
        val fromLengthBytes : word = wordSize * fromLengthWords
        val toLengthBytes   : word = wordSize * toLengthWords
        val offsetBytes     : word = wordSize * offsetWords

        val maxOffsetBytes = toLengthBytes - lengthBytes
    in
        (* Check the lengths are in the range. *)
        if fromLengthBytes < lengthBytes orelse maxOffsetBytes < offsetBytes
        then raise Subscript
        else RunCall.moveBytes(fromAddr, toAddr, 0w0, offsetBytes, lengthBytes)
    end

    |   csegCopySeg _ = raise InternalError "csegCopySeg: can only copy between byte segments"
  
    (* Returns a value from the vector. *)
    fun csegGet (ref (_, addr), byteIndex : word) : Word8.word =
    let
        val lengthWords = objLength addr
        val lengthBytes = wordSize * lengthWords
    in
        if byteIndex < lengthBytes
        then loadByte (addr, byteIndex)
        else raise Subscript
    end
       
    fun csegSet (r as (ref (status, addr)), byteIndex, value:Word8.word) : unit =
        let
            val lengthWords = objLength addr
            val lengthBytes = wordSize * lengthWords
        in
            if byteIndex < lengthBytes then assignByte (addr, byteIndex, value)
      
            else if status <> Bytes then raise InternalError "csegSet: can't extend code segment"
      
            else
            let (* length <= byteIndex; construct a new segment*)
                val oldSize = lengthWords (* size in words *)
                val expSize = expansionFactor * oldSize
      
                val wordIndex  = byteIndex div wordSize
                val minSize    = wordIndex + 0w1
      
                val newSize    = Word.max(expSize, minSize)
      
                (* create temporary codeseg (with larger byte-vector) *)
                val newVec = allocateAndClear newSize
                val newSeg = (Bytes, newVec)
            in
                (* copy the old code into the new codeseg *)
                csegCopySeg (r, ref newSeg, lengthBytes, 0w0);
      
                (* assign the byte to the new codeseg's byte-vector *)
                assignByte (newVec, byteIndex, value);

                (* update the old segment to point at the new byte-vector *)
                r := newSeg
            end
        end

    (* Puts in a word at a given word offset. Does not expand the segment. *)
    fun csegPutWord (ref (UnlockedCode, addr), wordIndex, value:machineWord) : unit =
    let
        val lengthWords = objLength addr
    in
        if lengthWords <= wordIndex
        then raise Subscript
        else assignWord (addr, wordIndex, value)
    end
    | csegPutWord _ = raise InternalError "csegPutWord: not an unlocked code segment"
  
    (* Gets a word at a given word offset. Does not expand the segment. *)
    fun csegGetWord (ref (status, addr), wordIndex) : machineWord =
    let
        val lengthWords = objLength addr
    in
        if status = Bytes 
        then raise InternalError "csegGetWord: can't load a a word from a byte segment"
        else if lengthWords <= wordIndex
        then raise Subscript
        else loadWord (addr, wordIndex)
    end

    (* csegPutConstant: store a constant into the code.  Exactly how this is
     done is architecture dependent: on the i386 it's simply a store, on
     other architectures the constant may have to be split between instructions.
     This means that it has to be done by the RTS so as to be certain that
     we don't have a garbage collection with an invalid address.  DCJM 2/1/01. *)
    fun csegPutConstant (ref (UnlockedCode, addr), byteIndex, value:machineWord, option: word) : unit =
    let
        val lengthWords = objLength addr
        val lengthBytes = wordSize * lengthWords
    in
        if lengthBytes <= byteIndex
        then raise Subscript
        else Compat560.setCodeConstant(addr, byteIndex, value, option)
    end
    | csegPutConstant _ = raise InternalError "csegPutConstant: not an unlocked code segment"

end; (* CODE_SEG *)
