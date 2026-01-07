(*
    Title:      Standard Basis Library: Support functions
    Copyright   David C.J. Matthews 2000, 2015-20, 2023

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

(* We need to execute these calls BEFORE compiling LibrarySupport if
   we want them to be compiled in as constants. *)
structure MachineConstants =
struct
    local
        val isBigEndian: unit -> bool = RunCall.rtsCallFast1 "PolyIsBigEndian"
    in
        val bigEndian : bool = isBigEndian ()
    end
    
    val wordSize : word = RunCall.bytesPerWord
    
    (* This is the same as wordSize in native 32-bit and 64-bit but different in 32-in-64. *)
    val sysWordSize: word = RunCall.memoryCellLength(Word.toLargeWord 0w0) * wordSize
end;

structure LibrarySupport :>
sig
    eqtype address (* eqtype so we can compare vectors. *)
    structure CharArray:
        sig
        datatype array = Array of word*address
        end
    structure Word8Array:
        sig
        datatype array = Array of word*address
        eqtype vector
        val wVecLength: vector -> word
        end
    val w8vectorToString: Word8Array.vector -> string
    and w8vectorFromString: string -> Word8Array.vector
    val wordSize: word and sysWordSize: word
    val bigEndian: bool
    val allocString: word -> string (* Create a mutable string. *)
    val allocBytes: word -> address
    val isShortInt      : int -> bool
    val largeIntIsSmall: LargeInt.int -> bool
    val unsignedShortOrRaiseSubscript: int -> word
    val unsignedShortOrRaiseSize: int -> word
    val sizeAsWord      : string -> word
    val stringAsAddress : string -> address
    val w8vectorAsAddress : Word8Array.vector -> address
    val maxAllocation: word
    and maxString: word
    val emptyVector: word
    val quotRem: LargeInt.int*LargeInt.int -> LargeInt.int*LargeInt.int
    val getOSType: unit -> int
    eqtype syserror
    val syserrorToWord: syserror -> LargeWord.word
    val syserrorFromWord : LargeWord.word -> syserror
    exception SysErr of (string * syserror option)
    
    val onEntryList: (unit->unit) list ref (* This is picked up by InitialPolyML *)
    val addOnEntry: (unit->unit) -> unit
    val atExitList: (unit->unit) list ref (* This is picked up by OS.Process *)
    val addAtExit: (unit->unit) -> unit
    
    val volatileListRef: unit -> 'a list ref
    val volatileWordRef: unit -> word ref
    val volatileOptionRef: unit -> 'a option ref
    
    val log2Word: word -> word
end
=
struct
    (* An address is the address of a vector in memory. *)
    type address = Bootstrap.byteArray (* This forces pointer equality. *)

    local
        (* Add a pretty printer to avoid crashes during debugging. *)
        open PolyML
        fun prettyAddress _ _ (_: address) = PolyML.PrettyString "byteArray"
    in
        val () = addPrettyPrinter prettyAddress
    end

    (* This is always a short non-negative integer so can be cast as word or int. *)
    fun sizeAsWord(s: string): word = RunCall.loadUntagged(s, 0w0)

    (* Provide the implementation of CharArray.array, Word8Array.array
       and Word8Array.vector (= Word8Vector.vector) here so that they
       are available to the IO routines. *)
    structure CharArray =
    struct
        datatype array = Array of word*address
    end
    structure Word8Array =
    struct
        (* Using the Array constructor here does not add any overhead since it is compiled
           as an identity function. *)
        datatype array = Array of word*address

        (* The representation of Word8Vector.vector is the same as that of string.  We
           define it as "string" here so that it inherits the same equality function.
           The representation is assumed by the RTS. *)
        type vector = string
        val wVecLength: vector -> word = sizeAsWord
    end

    (* Identity functions to provide convertions. *)
    fun w8vectorToString s = s
    and w8vectorFromString s = s

    (* There are circumstances when we want to pass the address of a string where
       we expect an address. *)
    val stringAsAddress : string -> address = RunCall.unsafeCast
    val w8vectorAsAddress = stringAsAddress o w8vectorToString

    open MachineConstants;

    local
        val F_mutable_bytes : word = 0wx41
        (* This is put in by Initialise and filtered out later. *)
        val setLengthWord: string * word -> unit = fn (s, n) => RunCall.storeUntagged(s, 0w0, n)

        val callGetAllocationSize = RunCall.rtsCallFast0 "PolyGetMaxAllocationSize"
        val callGetMaxStringSize = RunCall.rtsCallFast0 "PolyGetMaxStringSize"
    in
        (* Get the maximum allocation size.  This is the maximum value that can
           fit in the length field of a segment. *)
        val maxAllocation = callGetAllocationSize()
        and maxString = callGetMaxStringSize()

        (* Check that we have a short int.  This is only necessary if
           int is arbitrary precision.  If int is fixed precision it will
           always be true. *)
        fun isShortInt(i: int): bool =
            not Bootstrap.intIsArbitraryPrecision orelse RunCall.isShort i

        (* Test whether a large int will fit in the short format. *)
        val largeIntIsSmall: LargeInt.int -> bool = RunCall.isShort

        fun unsignedShortOrRaiseSize (i: int): word =
            if isShortInt i andalso i >= 0
            then RunCall.unsafeCast i
            else raise Size

        fun unsignedShortOrRaiseSubscript (i: int): word =
            if isShortInt i andalso i >= 0
            then RunCall.unsafeCast i
            else raise Subscript

        fun allocBytes bytes : address =
        let
            val words : word =
                if bytes > maxString
                then raise Size
                (* The maximum string size is slightly smaller than the
                   maximum array size because strings have a length word.
                   It seems best to use the same maximum size for CharArray/Word8Array. *) 
                else (bytes + wordSize - 0w1) div wordSize
            val mem = RunCall.allocateByteMemory(words, F_mutable_bytes)
            (* Zero the last word. *)
            val () =
                if words = 0w0 then () else RunCall.storeUntagged(RunCall.unsafeCast mem, words-0w1, 0w0)
        in
            mem
        end

        (* Allocate store for the string and set the first word to contain
           the length and the rest zero. *)
        fun allocString charsW =
            let
                (* The space is the number of characters plus space for the length word
                   plus rounding. *)
                val words : word = (charsW + 0w2 * wordSize - 0w1) div wordSize
                val _ = words <= maxAllocation orelse raise Size
                val vec = RunCall.allocateByteMemory(words, F_mutable_bytes)
                (* Zero any extra bytes we've needed for rounding to a number of words.
                   This isn't essential but ensures that RTS sharing passes will
                   merge strings that are otherwise the same. *)
                val () = RunCall.storeUntagged(vec, words-0w1, 0w0)
            in
                (* Set the length word.  Since this is untagged we can't simply
                   use assign_word.*)
                setLengthWord(vec, charsW);
                vec
            end

        (* Volatile refs.  They are cleared to 0/nil/NONE in an exported or saved state
           and their current value is not written to a child state, unlike normal refs.
           They are used for things like mutexes, condition variables and the list of
           currently open streams which should always be reset. *)
        local
            fun volatileRef() : 'a ref = RunCall.allocateWordMemory(0w1, 0wx48, 0w0)
        in
            val volatileListRef = volatileRef
            and volatileWordRef = volatileRef
            and volatileOptionRef = volatileRef
        end
    end

    (* Create an empty vector.  This is used wherever we want an empty vector.
       It can't be 'a vector which is what we want because of the value restriction. *)
    val emptyVector: word = RunCall.allocateWordMemory(0w0, 0w0, 0w0)
    
    val quotRem = LargeInt.quotRem
    
    val getOSType: unit -> int = RunCall.rtsCallFast0 "PolyGetOSType"
    
    (* syserror is the same as SysWord.word and these are needed in Posix at least. *)
    type syserror = LargeWord.word
    fun syserrorToWord i = i
    and syserrorFromWord i = i
    
    exception SysErr = RunCall.SysErr
    
    (* The onEntry list.  PolyML.onEntry adds a mutex here. *)
    val onEntryList: (unit->unit) list ref = ref[]
    fun addOnEntry f = onEntryList := f :: !onEntryList
    
    (* The atExit list - This is a volatile since it should be reset
       at the start, unlike the onEntry list. *)
    val atExitList = volatileListRef()
    fun addAtExit f = atExitList := f :: !atExitList
    
    (* This is needed in IntInf so needs to be captured before LargeInt is redefined. *)
    val log2Word = LargeInt.log2Word
end;

