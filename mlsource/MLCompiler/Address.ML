(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited
 
    Further development copyright David C.J. Matthews 2000-2017, 2026

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

signature AddressSig =
sig
    type machineWord
    type address
    type short = Word.word

    val stringOfWord: machineWord -> string

    val wordEq : machineWord * machineWord -> bool

    val isShort   : machineWord -> bool

    exception Cast of string
    val toMachineWord: 'a    -> machineWord
    val toShort:         machineWord -> Word.word
    val toAddress:       machineWord -> address

    val loadByte:   (address * Word.word) -> Word8.word 
    val loadWord:   (address * Word.word) -> machineWord

    val assignByte: (address * Word.word * Word8.word) -> unit
    val assignWord: (address * Word.word * machineWord)  -> unit

    val allocWordData: (short * Word8.word * machineWord) -> address

    val maxAllocation: word

    val lock:   address -> unit
    val length: address -> short
    val flags:  address -> Word8.word

    val wordSize: word and nativeWordSize: word and compact32UnitSize: word

    val F_words        : Word8.word
    val F_bytes        : Word8.word
    val F_closure      : Word8.word
    val F_code         : Word8.word
    val F_negative     : Word8.word
    val F_mutable      : Word8.word
    val F_gc           : Word8.word
    val F_noOverwrite  : Word8.word
    val F_weak         : Word8.word
    val F_profile      : Word8.word

    val isWords : address -> bool
    val isBytes : address -> bool
    val isCode  : address -> bool
    val isClosure: address -> bool
    val isMutable:address -> bool
end

structure Address :> AddressSig =

struct
    (* These want to be abstract. *)
    local
        structure M:> sig type machineWord and address end =
        struct 
            type machineWord = word (* a legal ML object (tag = 0 or 1) *)
            and address  = word (* a normal pointer (tag = 0) *)
        end
    in
        open M
    end

    (* This is the same as word *)
    type short    = word (* a 31/63-bit int  (tag = 1) *)

    (* pointer equality *)
    val wordEq: machineWord * machineWord -> bool = PolyML.pointerEq

    val unsafeCast : 'a -> 'b = RunCall.unsafeCast

    val isShort : machineWord->bool = RunCall.isShort

    (* The following cast is always safe *)
    val toMachineWord : 'a    -> machineWord = unsafeCast

    (* The following casts need checking *)
    exception Cast of string

    fun toAddress (w: machineWord) : address =
        if isShort w then raise Cast "toAddress" else unsafeCast w

    fun toShort (w: machineWord) : Word.word =
        if isShort w then unsafeCast w else raise Cast "toShort"

  
  (* Note: 
       assignByte should *not* be used with word-objects
     (we might copy half a pointer into the object,
      then call the garbage collector)
       
       loadWord should *not* be used with byte-objects
     (we might load something that's not a valid ML value,
      then call the garbage collector)
      
    Violating these assertions may corrupt the heap and cause unpredictable
    behaviour.
    
    It's safe to use assignWord with a byte-object or loadByte
    with a word-object but it may not do what you expect.
     
      Note that the offset for the
      "Word" functions is in words, whereas the offset for the
      "Byte" functions is in bytes.
  *)
  
    val loadByte:   address * Word.word -> Word8.word = RunCall.loadByte
    and loadWord:   address * Word.word -> machineWord  = RunCall.loadWord
    and assignByte: address * Word.word * Word8.word -> unit = RunCall.storeByte
    and assignWord: address * Word.word * machineWord -> unit = RunCall.storeWord
    and lock:       address -> unit = RunCall.clearMutableBit
    (* wordSize is the number of bytes in a Poly word. *)
    and wordSize:   word = RunCall.bytesPerWord
    and length:     address -> Word.word = RunCall.memoryCellLength
    and flags:      address -> Word8.word  = Word8.fromLargeWord o Word.toLargeWord o RunCall.memoryCellFlags
    
    (* The native word size is the number of bytes in an address.  This is the same as
       wordSize except in 32-in-64. *)
    val nativeWordSize = length(toAddress(toMachineWord(0w0:LargeWord.word))) * wordSize

    local
        val callGetAllocationSize = RunCall.rtsCallFast0 "PolyGetMaxAllocationSize"
    in
        val maxAllocation: word = callGetAllocationSize()
    end

    fun allocWordData(len: word, flags: Word8.word, initial: machineWord): address =
        (* Check that the size is within the acceptable range. *)
        if len >= maxAllocation
        then raise Size
        else RunCall.allocateWordMemory(len, Word.fromLargeWord(Word8.toLargeWord flags), initial)

    val F_words        : Word8.word = 0wx00 (* word object - contains pointers and/or tagged values. *)
    val F_bytes        : Word8.word = 0wx01 (* byte object (contains no pointers) *)
    val F_code         : Word8.word = 0wx02 (* code object (mixed bytes and words) *)
    val F_closure      : Word8.word = 0wx03 (* closure object.  This is only used in 32-in-64.  *)
    val F_noOverwrite  : Word8.word = 0wx08 (* don't overwrite when loading - mutables only. *)
    val F_negative     : Word8.word = 0wx10 (* sign bit for arbitrary precision ints (byte objects) *)
    val F_profile      : Word8.word = 0wx10 (* object has a profile pointer (word objects) *)
    val F_weak         : Word8.word = 0wx20 (* object contains weak references to option values. *)
    val F_mutable      : Word8.word = 0wx40 (* object is mutable *)
    val F_gc           : Word8.word = 0wx80 (* object is (pointer or depth) tombstone *)

    local
        val typeMask : Word8.word = 0wx03

        fun isType (t: Word8.word) (a: address):bool =  Word8.andb(flags a, typeMask) = t
    in
        val isWords = isType F_words
        val isBytes = isType F_bytes
        val isCode  = isType F_code
        val isClosure = isType F_closure

        (* The mutable flag may be used with any of the others. *)
        fun isMutable a = Word8.andb(flags a, F_mutable) = F_mutable
    end

    val functionName: machineWord -> string = RunCall.rtsCallFull1 "PolyGetFunctionName"

    fun stringOfWord w =
    if isShort w
    then "LIT" ^ Word.toString (unsafeCast w)

    else
    let
        val v = toAddress w
    in
        if isCode v
        then "CODE \"" ^ functionName w ^ "\""
        
        else if isBytes v
        then
        let
            val length = Int.min(Word.toInt(length v * wordSize), 16)
            val data = Word8Vector.tabulate(length, fn n => loadByte(v, Word.fromInt n))
        in
            "BYTE data" ^ String.toString(Byte.bytesToString data)
        end
        
        else if not(isMutable v) andalso isClosure v andalso Word.toInt(length v) >= 1
        then (* In 32-in-64 the first word of a closure is an absolute code address. *)
        (
            "FUN \"" ^ functionName w ^ "\"" (* Get the function name. *)
                handle Fail _ => "LIT <long word data>" (* May fail if it hasn't been set. *)
        )
    
        else if isWords v andalso Word.toInt(length(toAddress w)) >= 1
        then (* If it's the closure of a function try to print that. *)
            let
                val firstWord = loadWord(toAddress w, 0w0)
            in
                if not (isShort firstWord) andalso isCode(toAddress firstWord)
                then "FUN \"" ^ functionName firstWord ^ "\"" (* Get the function name. *)
                else "LIT <long word data>"
            end
            
        else "LIT <long word data>"
    end

    (* Heap unit size in Compact 32 bit mode.  Returns zero in native mode. *)
    local
        val getsize : unit->word = RunCall.rtsCallFast0 "PolyGetC32UnitSize"
    in
        val compact32UnitSize = getsize()
    end
end;

(* Add a print function for machineWord.  This is really only for
   the debugger but prevents addresses being printed as Word.word values. *)
local
    open PolyML Address
    fun printMachineWord _ _ w = PrettyString(stringOfWord w)
in
    val () = addPrettyPrinter printMachineWord
end;

