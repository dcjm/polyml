(*
    Title:      Standard Basis Library: Array Structure
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005, 2015-16

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
    open RuntimeCalls
    type 'a array = 'a array (* Predeclared in the basis with special equality props. *)

    val System_move_words:
        word*int*word*int*int->unit = RunCall.run_call5 POLY_SYS_move_words
    val System_move_words_overlap:
        word*int*word*int*int->unit = RunCall.run_call5 POLY_SYS_move_words_overlap

    val arrayAsWord: 'a array -> word = RunCall.unsafeCast
    val intAsWord: int -> word = RunCall.unsafeCast

    (* Unsafe subscript and update functions used internally for cases
       where we've already checked the range. *)
    fun unsafeSub(v: 'a array, i: int): 'a = RunCall.loadWord(arrayAsWord v, intAsWord i)

    and unsafeUpdate(v: 'a array, i: int, new: 'a): unit =
        RunCall.storeWord (arrayAsWord v, intAsWord i, RunCall.unsafeCast new);

    val intAsWord: int -> word = RunCall.unsafeCast
    and wordAsInt: word -> int = RunCall.unsafeCast

    (* "vector" creates a vector from an array so the representation of a
       zero-length object is different.  Locking the resultant object turns
       into an immutable object and changes the equality function from pointer
       equality to value equality. *)
    fun makeVector(v: 'a array, start, length): 'a vector =
        if length = 0 then RunCall.unsafeCast LibrarySupport.emptyVector (* Special case for zero *)
        else (* The size must have already been checked. *)
        let
            (* Make a vector initialised to zero. *)
            val new_vec = RunCall.allocateWordMemory(Word.fromInt length, 0wx40, 0w0)
        in
            System_move_words(RunCall.unsafeCast v, start, new_vec, 0, length);
            RunCall.clearMutableBit new_vec;
            RunCall.unsafeCast new_vec
        end
in
structure Array: ARRAY =
struct
    type 'a array = 'a array
    type 'a vector = 'a Vector.vector
    
    val maxLen = RunCall.unsafeCast LibrarySupport.maxAllocation
    
    (* Internal function: Construct an array initialised to zero. *)
    fun alloc len =
        let
            val () = if len >= maxLen then raise General.Size else ()
            val vec = RunCall.allocateWordMemory(Word.fromInt len, 0wx40, 0w0)
        in
            RunCall.unsafeCast vec
        end
     
    fun array(len, a) =
        let
            val () = if len < 0 orelse len >= maxLen then raise General.Size else ()
            val vec = RunCall.allocateWordMemory(Word.fromInt len, 0wx40, RunCall.unsafeCast a)
        in
            RunCall.unsafeCast vec
        end

    val listLength = length; (* Pick this up from the prelude. *)
    fun length (vec: 'a array): int = wordAsInt(RunCall.memoryCellLength(arrayAsWord vec))
    
    fun op sub (vec: 'a array, i: int): 'a =
        if not (LibrarySupport.isShortInt i) orelse intAsWord i >= RunCall.memoryCellLength vec
        then raise General.Subscript
        else unsafeSub(vec, i)
 
    fun update (vec: 'a array, i: int, new: 'a) : unit =
        if not (LibrarySupport.isShortInt i) orelse intAsWord i >= RunCall.memoryCellLength vec
        then raise General.Subscript
        else RunCall.storeWord (arrayAsWord vec, intAsWord i, RunCall.unsafeCast new);

    (* Create an array from a list. *)
    fun fromList (l : 'a list) : 'a array =
        let
        val length = listLength l;
            
        (* Make a array initialised to zero. *)
        val vec = alloc length
        
        (* Copy the list elements into the array. *)
        fun init (v, i, a :: l) =
             (
             unsafeUpdate(v, i, a);
             init(v, i + 1, l)
             )
        |  init (_, _, []) = ();
        
    in
        init(vec, 0, l);
        vec
    end
        
    fun tabulate (length: int , f : int->'a): 'a array =
    let
        val vec =
            if length < 0 then raise General.Size
            else alloc length;
        (* Initialise it to the function values. *)
        fun init i = 
            if length <= i then ()
            else (unsafeUpdate(vec, i, f i); init(i+1))
    in
        init 0;
        vec
    end
    
    (* "vector" creates a vector from an array so the representation of a
       zero-length object is different.  Locking the resultant object turns
       into an immutable object and changes the equality function from pointer
       equality to value equality. *)
    fun vector (vec: 'a array): 'a vector = makeVector(vec, 0, length vec)
    
    (* Copy one array into another.  It's possible for the arrays
       to be the same but in that case di would have to be zero otherwise the length
       check would fail. *)
    fun copy {src: 'a array as s, dst: 'a array as d, di: int} =
        let
            val len = length src
        in
            if di < 0 orelse di+len > length dst
            then raise General.Subscript
            else System_move_words(RunCall.unsafeCast s, 0, RunCall.unsafeCast d, di, len)
        end

    (* Copy a vector into an array. *)
    fun copyVec {src: 'a vector, dst: 'a array as d, di: int} =
        let
            val len = Vector.length src
        in
            if di < 0 orelse di+len > length dst
            then raise General.Subscript
            else System_move_words(RunCall.unsafeCast src, 0, RunCall.unsafeCast d, di, len)
        end
        

    (* Create the other functions. *)
    structure VectorOps =
        PolyVectorOperations(
            struct
                type 'a vector = 'a array
                local val l = length in fun length(v: 'a array):word = intAsWord(l v) end
                local val u = unsafeSub in fun unsafeSub (v: 'a array, i: word) = u(v, wordAsInt i) end
                fun unsafeSet(v, i: word, e: 'a) = unsafeUpdate(v, wordAsInt i, e)
            end);

    open VectorOps;

    local
        (* Install the pretty printer for arrays *)
        (* We may have to do this outside the structure if we
           have opaque signature matching. *)
        fun pretty(depth: FixedInt.int)
                  (printElem: 'a * FixedInt.int -> PolyML.pretty)
                  (x: 'a array) =
            let
                open PolyML
                val last = length x - 1
                fun put_elem (index, w, (l, d)) =
                    if d = 0 then ([PrettyString "...]"], d+1)
                    else if d < 0 then ([], d+1)
                    else
                    (
                    printElem(w, d-1) ::
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

end (* Array *)

structure ArraySlice =
struct
    
    datatype 'a slice = Slice of { array: 'a Array.array,  start: int, length: int };
    
    fun length(Slice{length, ...}) = length
    
    fun op sub (Slice{array, start, length}, i: int): 'a =
        if i < 0 orelse i >= length then raise General.Subscript
        else unsafeSub(array, i+start)

    fun update (Slice{array, start, length}, i: int, new: 'a) : unit =
        if i < 0 orelse i >= length
        then raise General.Subscript
        else unsafeUpdate (array, i+start, new);

    (* Create a slice, checking the sizes so that the resulting slice is always valid. *)
    fun slice(vec: 'a array, i: int, NONE) =
    let
        val len = Array.length vec
    in
        if i >= 0 andalso i <= len
        then Slice{array=vec, start=i, length=len-i} (* Length is rest of array. *)
        else raise General.Subscript
    end
     |  slice(vec: 'a array, i: int, SOME l) =
    let
        val len = Array.length vec
    in
        if i >= 0 andalso l >= 0 andalso i+l <= len
        then Slice{array=vec, start=i, length=l} (* Length is as given. *)
        else raise General.Subscript
    end

    (* Slice from the whole array. *)
    fun full a = Slice{array=a, start=0, length=Array.length a}

    (* Slice from existing slice *)
    fun subslice(Slice{array, start, length}, i: int, NONE) =
        if i >= 0 andalso i <= length
        then Slice{array=array, start=i+start, length=length-i} (* Length is rest of array. *)
        else raise General.Subscript

     |  subslice(Slice{array, start, length}, i: int, SOME l) =
        if i >= 0 andalso l >= 0 andalso i+l <= length
        then Slice{array=array, start=i+start, length=l} (* Length is as given. *)
        else raise General.Subscript

    fun base(Slice{array, start, length}) = (array, start, length)

    fun vector (Slice{array, start, length}): 'a vector = makeVector(array, start, length)

    
    (* Copy one array into another.  It's possible for the arrays
       to be the same and for the source and destinations to overlap so we
       have to take care of that. *)
    fun copy {src = Slice{array=s, start=srcStart, length=srcLen}, dst, di: int} =
        if di < 0 orelse di+srcLen > Array.length dst
        then raise General.Subscript
        else System_move_words_overlap(RunCall.unsafeCast s, srcStart, RunCall.unsafeCast dst, di, srcLen)

    (* Copy a vector into an array. *)
    fun copyVec {src: 'a VectorSlice.slice, dst: 'a array as d, di: int} =
        let
            val (v, i, len) = VectorSlice.base src
        in
            if di < 0 orelse di+len > Array.length dst
            then raise General.Subscript
            else System_move_words(RunCall.unsafeCast v, i, RunCall.unsafeCast d, di, len)
        end

    fun isEmpty(Slice{length, ...}) = length = 0

    (* Return the first item of the slice and the rest of the slice. *)
    fun getItem(Slice{length=0, ...}) = NONE
     |  getItem(Slice{array, start, length}) =
            SOME(unsafeSub(array, start), Slice{array=array, start=start+1, length=length-1})

    (* Create the other functions. *)
    structure VectorOps =
        PolyVectorOperations(
            struct
                type 'a vector = 'a slice
                fun length(Slice{length, ...}) = intAsWord length
                local 
                    val u = unsafeSub
                in
                    fun unsafeSub (Slice{array, start, ...}, i: word) = u(array, wordAsInt i + start)
                end
                fun unsafeSet(Slice{array, start, ...}, i: word, e: 'a) = unsafeUpdate(array, wordAsInt i + start, e)
            end);

    open VectorOps;

end (* ArraySlice *)

end; (* Local in end *)
  
structure ArraySlice :> ARRAY_SLICE = ArraySlice;

local
    open ArraySlice

    (* Install the pretty printer for array slices *)
    (* We may have to do this outside the structure if we
       have opaque signature matching. *)
    fun pretty(depth: FixedInt.int)
              (printElem: 'a * FixedInt.int -> PolyML.pretty)
              (x: 'a slice) =
        let
            open PolyML
            val last = length x - 1
            fun put_elem (index, w, (l, d)) =
                if d = 0 then ([PrettyString "...]"], d+1)
                else if d < 0 then ([], d+1)
                else
                (
                printElem(w, d-1) ::
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
    val _ = PolyML.addPrettyPrinter pretty
end
