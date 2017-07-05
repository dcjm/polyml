(*
    Title:      Standard Basis Library: Support functions
    Copyright   David C.J. Matthews 2000, 2015-16

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
    val bigEndian : bool = RunCall.run_call0 RuntimeCalls.POLY_SYS_is_big_endian ();
    val wordSize : word = RunCall.bytesPerWord
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
        end
    val w8vectorToString: Word8Array.vector -> string
    and w8vectorFromString: string -> Word8Array.vector
    val wordSize: word
    val bigEndian: bool
    val allocString: word -> string (* Create a mutable string. *)
    val allocBytes: word -> address
    val isShortInt      : int -> bool
    val largeIntIsSmall: LargeInt.int -> bool
    val unsafeSubstring: string*word*word -> string
    val unsignedShortOrRaiseSubscript: int -> word
    val unsignedShortOrRaiseSize: int -> word
    val sizeAsWord      : string -> word
    val stringAsAddress : string -> address
    val w8vectorAsAddress : Word8Array.vector -> address
    val maxAllocation: word
    val noOverwriteRef: 'a -> 'a ref
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
    end

    (* Identity functions to provide convertions. *)
    fun w8vectorToString s = s
    and w8vectorFromString s = s

    (* There are circumstances when we want to pass the address of a string where
       we expect an address. *)
    val stringAsAddress : string -> address = RunCall.unsafeCast
    val w8vectorAsAddress = stringAsAddress o w8vectorToString

    open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
    open MachineConstants;

    local
        val F_mutable_bytes : word = 0wx41;

        val System_alloc: word*word*word->string  =
            RunCall.run_call3 POLY_SYS_alloc_store

        val System_lock: string -> unit =
            RunCall.run_call1 POLY_SYS_lockseg;

        val SetLengthWord: string * word -> unit =
            RunCall.run_call2 POLY_SYS_set_string_length
          
        val MemMove: string*word*string*word*word -> unit = 
            RunCall.run_call5 POLY_SYS_move_bytes
        val callProcessEnv = RunCall.rtsCallFull2 "PolyProcessEnvGeneral"
        val maxString = callProcessEnv (101, ())
        val charAsVec: char->string = RunCall.unsafeCast
    in
        (* Get the maximum allocation size.  This is the maximum value that can
           fit in the length field of a segment. *)
        val maxAllocation = callProcessEnv (100, ())

        (* This is always a short non-negative integer so can be cast as word or int. *)
        val sizeAsWord: string -> word = RunCall.run_call1 RuntimeCalls.POLY_SYS_string_length

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
            val System_alloc_array: word*word*word->address  =
                RunCall.run_call3 POLY_SYS_alloc_store
            val words : word =
                if bytes = 0w0
                then 0w1 (* Zero-sized objects are not allowed. *)
                else if bytes > maxString
                (* The maximum string size is slightly smaller than the
                   maximum array size because strings have a length word.
                   That means that System_alloc will not raise Size if "bytes"
                   size is between maxString and maxString+3. It seems best to
                   use the same maximum size for CharArray/Word8Array and
                   for String/Word8Vector so we need to check here. *) 
                then raise Size
                else (bytes + wordSize - 0w1) div wordSize
        in
            System_alloc_array(words, F_mutable_bytes, 0w0)
        end

        (* Allocate store for the string and set the first word to contain
           the length and the rest zero. *)
        fun allocString charsW =
            let
                (* The space is the number of characters plus space for the length word
                   plus rounding. *)
                val words : word = (charsW + 0w2 * wordSize - 0w1) div wordSize
                val () = if words >= maxAllocation then raise Size else ()
                (* We are relying on the allocator initialising the store
                   since we only copy as many bytes as we have in the string,
                   possibly leaving bytes in the last word unset.  Generally that
                   wouldn't be a problem, since we will use the string length word
                   to find out how many real characters there are, except in the
                   case of the structure equality function.  It uses the
                   segment length word and compares the whole of each word
                   so we must ensure that two equal strings are equal in every
                   WORD including any unused bytes at the end.
                   It might be faster if we didn't want to initialise every
                   byte to simply zero the last word of the segment. *)
                val vec = System_alloc(words, F_mutable_bytes, 0w0)
            in
                (* Set the length word.  Since this is untagged we can't simply
                   use assign_word.*)
                SetLengthWord(vec, charsW);
                vec
            end

        (* We want this in both String and Word8Array. *)
        fun unsafeSubstring(s: string, i: word, l: word) : string =
        let
            val baseLen = sizeAsWord s (* Length of base string. *)
        in
            if i = 0w0 andalso l = baseLen then s
            else if l = 0w0 then "" (* Empty string. *)
            else if l = 0w1 (* Result is a single character string (and s isn't). *)
            then charAsVec(RunCall.loadByte(s, i + wordSize))
            else
                let
                    (* Multiple character string. *)
                    val vec = allocString l
                in
                    MemMove(s, wordSize+i, vec, wordSize, l);
                    System_lock vec;
                    vec
                end
        end
        (* Create non-overwritable mutables for mutexes and condition variables.
           A non-overwritable mutable in the executable or a saved state is not
           overwritten when a saved state further down the hierarchy is loaded. 
           This is also used for imperative streams, really only so that stdIn
           works properly across SaveState.loadState calls. *)
        fun noOverwriteRef (a: 'a) : 'a ref =
            RunCall.unsafeCast(System_alloc(0w1, 0wx48, RunCall.unsafeCast a))
    end
end;

