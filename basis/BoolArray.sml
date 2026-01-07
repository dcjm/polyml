(*
    Title:      Standard Basis Library: BoolArray and BoolVector Structures
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005, 2016, 2022

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
local
    open LibrarySupport

    (* TODO: Use a single word for vectors of size <= number of bits in a word. *)
    (* We use int here for the length rather than word because the number of bits
       could be more than the maximum value of Word.word. *)
    datatype vector = Vector of int * Bootstrap.byteVector (* This has a byte-wise equality. *)
    and array = Array of int * Bootstrap.byteArray (* This has pointer equality. *)

    val wordSize : word = LibrarySupport.wordSize

    (* Casts between int and word. *)
    val intAsWord: int -> word = RunCall.unsafeCast
    and wordAsInt: word -> int = RunCall.unsafeCast

    val bitsPerWord = wordSize * 0w8

    (* Limit the size to Array.maxLen to avoid arithmetic overflow. *)
    val maxLen = Array.maxLen

    local
        val F_mutable_bytes = 0wx41
    in
        (* Allocate memory for a vector or an array. *)
        fun alloc (bits: int) =
        let
            val words : word =
                if bits < 0 orelse bits > maxLen
                then raise General.Size
                else (Word.fromInt bits + (bitsPerWord - 0w1)) div bitsPerWord
            val vec = RunCall.allocateByteMemory(words, F_mutable_bytes)
            val bytes = words * wordSize
            fun fill n =
                if n = bytes
                then ()
                else (RunCall.storeByte(vec, n, 0w0); fill(n+0w1))
            (* We will only set the bits that we actually use.  Unused bytes will be uninitialised.
               The equality function we're using tests all the bytes so we need to initialise them. *)
        in
            if bytes = 0w0 then () else fill(bytes - wordSize);
            vec
        end
    end

    val andb = Word.andb and orb = Word.orb and notb = Word.notb
    and << = Word.<< and >> = Word.>>;
    
    infix 9 sub
    infix 7 andb
    infix 6 orb
    infix 5 << >>


    (* Create a vector/array from a list.  Used as the basis of
       Array.fromList and Vector.fromList. *)
    fun fromList' (l : bool list) =
        let
            val length = List.length l
            (* Make a array initialised to zero. *)
            val vec = alloc length
            
            (* Accumulate the list elements into bytes and store
               them in the vector. *)
            fun init (byteno, acc, bit, []) =
                if bit = 0w1 then () else RunCall.storeByte(vec, byteno, acc)
              | init (byteno, acc, bit, a :: b) =
                let
                    val byte = if a then bit orb acc else acc
                in
                    if bit = 0wx80
                    then
                    (
                        RunCall.storeByte(vec, byteno, byte);
                        init(byteno+0w1, 0w0, 0w1, b)
                    )
                    else init(byteno, byte, bit << 0w1, b)
                end
        in
            init(0w0, 0w0, 0w1, l);
            (length, vec)
        end

    fun tabulate' (length: int, f : int->bool) =
    let
        val vec =
            if length >= 0 then alloc length else raise General.Size;

        (* Accumulate the bits into bytes and store into the array. *)
        fun init i byteNo bit acc =
        if i < length
        then
        let
            val byte = if f i then bit orb acc else acc
        in
            if bit = 0wx80
            then ( RunCall.storeByte(vec, byteNo, byte) ; init (i+1) (byteNo+0w1) 0w1 0w0 )
            else init (i+1) byteNo (bit << 0w1) byte
        end
        else if acc = 0w0
        then ()
        else (* Put in the last byte. *)
            RunCall.storeByte(vec, byteNo, acc)
    in
        init 0 0w0 0w1 0w0;
        (length, vec)
    end

    (* Internal function which subscripts the vector assuming that
       the index has already been checked for validity. *)
    fun uncheckedSub (v, i: int): bool =
        let
            val iW = Word.fromInt i
            val byte = RunCall.loadByte(v, iW >> 0w3)
            val mask = 0w1 << (iW andb 0w7)
        in
            byte andb mask <> 0w0
        end

    (* Move a set of bits from one vector of bytes to another.  The bits
       may not be on the same byte alignment.  Does not examine the
       destination so if dest_off is not byte aligned any bits required in
       the first byte must be passed in as src_in.  Returns any bits which
       do not exactly fit into a byte.  *)
    (* TODO: This only handles the case where the source starts at the beginning
       of the vector.  It is easy to modify it for the case where the source
       offset is a multiple of 8 but more difficult to handle the other cases. *)
    fun move_bits(src: Bootstrap.byteVector, dest: Bootstrap.byteVector, dest_off, len, last_bits) =
    let
        val dest_byte = intAsWord dest_off div 0w8 (* Byte offset *)
        val dest_bit = intAsWord dest_off - dest_byte*0w8 (* Bit offset *)

        fun do_move last byte len : word =
            if len >= 8
            then let
                (* Get the next byte and shift it up *)
                val newbyte = last orb (RunCall.loadByteFromImmutable(src, byte) << dest_bit)
            in
                (* Store the low-order 8 bits into the destination. *)
                RunCall.storeByte(dest, dest_byte+byte, newbyte);
                (* Shift the accumulator down by 8 bits and get ready for
                   the next byte. *)
                do_move (newbyte >> 0w8) (byte+0w1) (len-8)
            end
            else if len <= 0
            then last
            else (* 0 < len < 8 *)
            let
                (* Get the next byte and shift it up *)
                val nextsrc = RunCall.loadByteFromImmutable(src, byte);
                val newbyte: word = last orb (nextsrc << dest_bit)
                (* This assumes that any extra bits of the source are
                   zero. *)
            in
                if len + Word.toInt dest_bit >= 8
                then
                    (
                    (* Store the low-order 8 bits into the destination. *)
                    RunCall.storeByte(dest, dest_byte+byte, newbyte);
                    (* Shift the accumulator down by 8 bits and get ready for
                       the next byte. *)
                    do_move (newbyte >> 0w8) (byte+0w1) (len-8)
                    )
                else newbyte
            end
    in
        (* TODO: If dest_bit is zero we can simply move the bytes.  If len
           is not a multiple of 8 we may have to return the low-order bits. *)
        do_move last_bits 0w0 len
    end

in
    structure BoolVector: MONO_VECTOR =
    struct
        type vector = vector
        type elem = bool
        val maxLen = maxLen
        
        fun length(Vector(l, _)) = l
        
        fun op sub (Vector(l, v), i: int): bool =
            if i < 0 orelse i >= l then raise General.Subscript
            else uncheckedSub(v, i)
    
        (* Create a vector from a list.  Must lock the vector before
           returning it. *)
        fun fromList (l : elem list) : vector =
        let
            val (length, vec) = fromList' l
        in
            RunCall.clearMutableBit vec;
            Vector(length, vec)
        end
    
        fun tabulate (length: int, f : int->elem): vector =
        let
            val (length, vec) = tabulate' (length, f)
        in
            RunCall.clearMutableBit vec;
            Vector(length, vec)
        end
            
(*      fun map f (Vector(len, vec)) =
            let
                val new_vec = alloc len (* Destination vector. *)
                fun mapbyte b i acc max =
                    if i = max then acc
                    else if f ((b andb i) <> 0w0)
                    then mapbyte b (i<<0w1) (acc orb i) max
                    else mapbyte b (i<<0w1) acc max
                fun copy b l =
                    if l <= 0 then ()
                    else let
                        val byte = System_loadb(vec, b)
                        val res =
                            (* Map each byte to get the result.  Must not
                               apply the function beyond the last bit. *)
                            if l >= 8 then mapbyte byte 0w1 0w0 0wx100
                            else mapbyte byte 0w1 0w0 (0w1 << Word.fromInt l)
                    in
                        RunCall.storeByte(new_vec, b, res);
                        copy (b+0w1) (l-8)
                    end
            in
                copy 0w0 len;
                RunCall.clearMutableBit new_vec;
                Vector(len, new_vec)
            end*)

        fun mapi f (Vector(len, vec)) =
            let
                val new_vec = alloc len (* Destination vector. *)
                fun mapbyte b i acc max l =
                    if i = max then acc
                    else if f (len-l, ((b andb i) <> 0w0))
                    then mapbyte b (i<<0w1) (acc orb i) max (l-1)
                    else mapbyte b (i<<0w1) acc max (l-1)
                fun copy b l =
                    if l <= 0 then ()
                    else let
                        val byte = RunCall.loadByteFromImmutable(vec, b)
                        val res =
                            (* Map each byte to get the result.  Must not
                               apply the function beyond the last bit. *)
                            if l >= 8 then mapbyte byte 0w1 0w0 0wx100 l
                            else mapbyte byte 0w1 0w0 (0w1 << Word.fromInt l) l
                    in
                        RunCall.storeByte(new_vec, b, res);
                        copy (b+0w1) (l-8)
                    end
            in
                copy 0w0 len;
                RunCall.clearMutableBit new_vec;
                Vector(len, new_vec)
            end

        (* To save duplicating almost the same code just define map in terms of mapi. *)
        fun map f v = mapi (fn (_, x) => f x) v

        (* Return a copy of the vector with a particular entry replaced *)
        fun update (v as Vector(len, _), i, c) =
            if i < 0 orelse i >= len
            then raise Subscript
            else mapi (fn (j, s) => if j = i then c else s) v
    
        fun concat l =
        let
            (* Calculate the total length *)
            fun total [] i = i
              | total (Vector(len, _)::t) i = total t (i+len)
        
            val total_len = total l 0
        in
            let
                (* Allocate a new vector. *)
                val new_vec = alloc total_len
                (* Copy all the source vectors into the destination. *)
                fun copy_list (Vector(src_len, src_vec)::t) dest_off bits =
                    let
                        val next = move_bits(src_vec, new_vec, dest_off, src_len, bits)
                    in
                        copy_list t (dest_off+src_len) next
                    end
                 |  copy_list [] dest_off bits =
                    (* At the end of the lists store any extra in the last byte. *)
                    if bits = 0w0 then ()
                    else RunCall.storeByte(new_vec, intAsWord dest_off div 0w8, bits)
            in
                copy_list l 0 0w0;
                RunCall.clearMutableBit new_vec;
                Vector(total_len, new_vec)
            end
        end

        (* Create the other functions. *)
        structure VectorOps =
            VectorOperations(
                struct
                    type vector = vector and elem = elem
                    fun length(Vector(l, _)) = intAsWord l
                    fun unsafeSub (Vector(_, v), i) = uncheckedSub(v, wordAsInt i)
                    fun unsafeSet _ = raise Fail "Should not be called"
                end);
    
        open VectorOps;


        local
            (* Install the pretty printer for BoolVector.vector *)
            fun pretty(depth: FixedInt.int) _ (x: vector) =
                let
                    open PolyML
                    val last = length x - 1
                    fun put_elem (index, w, (l, d)) =
                        if d = 0 then ([PrettyString "...]"], d+1)
                        else if d < 0 then ([], d+1)
                        else
                        (
                        PrettyString(if w then "true" else "false") ::
                            (if index <> last then PrettyString "," :: PrettyBreak(1, 0) :: l else l),
                        d+1
                        )
                in
                    PrettyBlock(3, false, [],
                        PrettyString "fromList[" ::
                        (if depth <= 0 then [PrettyString "...]"]
                         else #1 (foldri put_elem ([PrettyString "]"], depth - FixedInt.fromInt last) x) )
                   )
                end
        in
            val () = PolyML.addPrettyPrinter pretty
        end
    
    end

    structure BoolArray: MONO_ARRAY =
    struct
        type array = array
        type elem = bool
        type vector = vector
        val maxLen = maxLen;

        fun length(Array(l, _)) = l
        
        (* Internal function for updating a bit assuming the bounds
           checks have already been done. *)
        fun uncheckedUpdate(v, i, new): unit =
        let
            val iW = Word.fromInt i
            val byteOffsetW = iW >> 0w3
            val byte = RunCall.loadByte(v, byteOffsetW);
            val mask = 0w1 << (iW andb 0w7)
            val newByte =
                if new then byte orb mask
                else byte andb (notb mask)
        in
            RunCall.storeByte(v, byteOffsetW, newByte)
        end

        fun array (len, ini) =
        let
            (* Create the uninitialised array. *)
            val vec = alloc len
            (* Set the bytes to all zeros or all ones.  Generally this will set
               more bits than we need but that doesn't matter. *)
            val initByte = if ini then 0wxff else 0wx00
            val bytes = (Word.fromInt len + 0w7) >> 0w3
            (* TODO: This should be set by a built-in. *)
            fun setBytes b =
                if b >= bytes then ()
                else (RunCall.storeByte(vec, b, initByte); setBytes (b+0w1))
            val () = setBytes 0w0
        in
            Array(len, vec)
        end
    
        fun op sub (Array(l, v), i: int): elem =
            if i < 0 orelse i >= l then raise General.Subscript
            else uncheckedSub(v, i)

        (* Exported update function. *)
        fun update (Array (l, v), i: int, new) : unit =
            if i < 0 orelse i >= l
            then raise General.Subscript
            else uncheckedUpdate(v, i, new)

        (* Create an array from a list. *)
        fun fromList (l : elem list) : array = Array(fromList' l)

        fun tabulate (length: int , f : int->elem): array =
            Array(tabulate'(length, f))

        fun vector(Array(len, vec)): vector =
            (* TODO: We may be able to handle special cases where the
               source and destination are aligned on the same bit offset.
               For the moment just take the simple approach. *)
            BoolVector.tabulate(len, fn j => uncheckedSub(vec, j))

        (* Copy one array into another. The arrays could be the same but in that case di must be zero. *)
        fun copy {src=Array (slen, s), dst=Array (dlen, d), di: int} =
            if di < 0 orelse di+slen > dlen
            then raise General.Subscript
            else (* TODO: Handle multiple bits where possible by using
               move_bits or a variant. *)
            let
            fun copyBits n =
                    if n >= slen then ()
                    else
                        (uncheckedUpdate(d, di+n, uncheckedSub(s, n));
                         copyBits(n+1))
            in
                copyBits 0
            end

(*      fun copy {src as Array (slen, s), dst as Array (dlen, d), di: int} =
            let
            in
                if di < 0 orelse di+slen > dlen
                then raise General.Subscript
                else if si < di
                then (* Moving up - Start from the end *)
                (* TODO: Handle multiple bits where possible by using
                   move_bits or a variant. *)
                let
                    fun copyBits n =
                        if n < 0 then ()
                        else
                            (uncheckedUpdate(d, di+n, uncheckedSub(s, si+n));
                             copyBits(n-1))
                in
                    copyBits (slen-1)
                end
                else (* Moving down. *)
                let
                    fun copyBits n =
                        if n >= slice_len then ()
                        else
                            (uncheckedUpdate(d, di+n, uncheckedSub(s, si+n));
                             copyBits(n+1))
                in
                    copyBits 0
                end
            end
*)  
        (* Copy a vector into an array. *)
        fun copyVec {src=Vector(slen, s), dst=Array (dlen, d), di: int} =
            let
                fun copyBits n =
                    if n >= slen then ()
                    else
                        (uncheckedUpdate(d, di+n, uncheckedSub(s, n));
                         copyBits(n+1))
            in
                if di < 0 orelse di+slen > dlen
                then raise General.Subscript
                else copyBits 0
            end

        (* Create the other functions. *)
        structure VectorOps =
            VectorOperations(
                struct
                    type vector = array and elem = elem
                    fun length(Array(l, _)) = intAsWord l
                    fun unsafeSub (Array(_, v), i) = uncheckedSub(v, wordAsInt i)
                    fun unsafeSet (Array(_, v), i, new) = uncheckedUpdate(v, wordAsInt i, new)
                end);
    
        open VectorOps;
    
        local
            (* Install the pretty printer for BoolArray.array *)
            (* We may have to do this outside the structure if we
               have opaque signature matching. *)
            fun pretty(depth: FixedInt.int) _ (x: array) =
                let
                    open PolyML
                    val last = length x - 1
                    fun put_elem (index, w, (l, d)) =
                        if d = 0 then ([PrettyString "...]"], d+1)
                        else if d < 0 then ([], d+1)
                        else
                        (
                        PrettyString(if w then "true" else "false") ::
                            (if index <> last then PrettyString "," :: PrettyBreak(1, 0) :: l else l),
                        d+1
                        )
                in
                    PrettyBlock(3, false, [],
                        PrettyString "fromList[" ::
                        (if depth <= 0 then [PrettyString "...]"]
                         else #1 (foldri put_elem ([PrettyString "]"], depth - FixedInt.fromInt last) x) )
                   )
                end
        in
            val () = PolyML.addPrettyPrinter pretty
        end
    end
end;
