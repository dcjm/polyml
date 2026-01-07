(*
    Title:      Standard Basis Library: Vector Structure
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005, 2016

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
    (* Inherit the definition of vector in the initial environment.
       We have to declare vector in the initial environment in order
       for equality to work correctly. *)

    (* It would be simpler to be able to define these as functions
       to or from 'a vector but that gives error messages about free
       type variables. *)

    val vecAsWord: 'a vector -> word = RunCall.unsafeCast
    and wordAsVec: word -> 'a vector = RunCall.unsafeCast
    val intAsWord: int -> word = RunCall.unsafeCast
    and wordAsInt: word -> int = RunCall.unsafeCast

    (* All the arrays are initially created containing zeros and then initialised. *)
    fun alloc len = RunCall.allocateWordMemory(Word.fromInt len, 0wx40, 0w0)
    
    fun unsafeSub(v: 'a vector, i: int): 'a = RunCall.loadWord (vecAsWord v, intAsWord i)
    and unsafeUpdate(v: 'a vector, i: int, new: 'a): unit =
        RunCall.storeWord (vecAsWord v, intAsWord i, RunCall.unsafeCast new)
in

structure Vector: VECTOR =
struct
    (* N.B.  This implementation of vectors is implicitly used in
       Array.extract.  Don't change this implementation without also
       changing that. It's also used in the interface to the RTS in OS.Poll and Socket.select. *)
    type 'a vector = 'a vector

    (* The maximum size of a vector is the maximum object size we can allocate.
       This is one more than the maximum size of an array. *)
    val maxLen = RunCall.unsafeCast LibrarySupport.maxAllocation

    infix 9 sub (* For what it's worth *)
  
    (* Lock the arrays after they have been created.  All this does is
       switch off the "mutable" bit.  This does not prevent updating of
       itself, the signature does that by removing "update", but improves
       g.c. performance and causes equality to check for value equality
       not pointer equality. *)
    val listLength = length; (* Pick this up from the prelude. *)
 
    fun length v = wordAsInt(RunCall.memoryCellLength(vecAsWord v));

    fun op sub (vec:'a vector, i: int): 'a =
    let
        val v = vecAsWord vec
    in
        if not (LibrarySupport.isShortInt i) orelse intAsWord i >= RunCall.memoryCellLength v
        then raise General.Subscript
        else unsafeSub(vec, i)
    end
 
    (* Create a vector from a list.  We have to treat an empty list specially
       because we don't allow zero sized heap objects. *)
    fun fromList [] : 'a vector = wordAsVec LibrarySupport.emptyVector (* Must not try to lock it. *)
      | fromList (l : 'a list) : 'a vector =
        let
        val length = listLength l;
        val () = if length >= maxLen then raise General.Size else ()
            
        (* Make a vector initialised to zero. *)
        val vec = alloc length;
        
        (* Copy the list elements into the vector. *)
        fun init (v, i, a :: l) =
            (
            RunCall.storeWord(v, intAsWord i, RunCall.unsafeCast a);
            init(v, i + 1, l)
            )
        |  init (_, _, []) = ();
        
    in
        init(vec, 0, l);
        RunCall.clearMutableBit vec;
        wordAsVec vec
    end
        
    fun tabulate (0, _) : 'a vector = wordAsVec LibrarySupport.emptyVector (* Must not try to lock it. *)
     |  tabulate (length: int , f : int->'a): 'a vector =
    let
        val vec =
            if length > 0 andalso length < maxLen then alloc length else raise General.Size;
        (* Initialise it to the function values. *)
        fun init i = 
            if length <= i then ()
            else (RunCall.storeWord(vec, intAsWord i, RunCall.unsafeCast(f i)); init(i+1))
    in
        init 0;
        RunCall.clearMutableBit vec;
        wordAsVec vec
    end
    

    fun concat [] = wordAsVec LibrarySupport.emptyVector
     |  concat [v] = v (* Handle special cases to reduce copying. *)
     |  concat l =
    let
        (* Calculate the total length *)
        fun total [] i = i
          | total (h::t) i = total t (i+length h)
    
        val total_len = total l 0
    in
        if total_len = 0 then wordAsVec LibrarySupport.emptyVector
        else if total_len >= maxLen then raise General.Size
        else
        let
            (* Allocate a new vector. *)
            val new_vec = alloc total_len
                
            fun copy_list [] _ = ()
              | copy_list (h::t) j =
                let
                    val v = vecAsWord h
                    val src_len = length h
                in
                    RunCall.moveWords(v, new_vec, 0w0, Word.fromInt j, Word.fromInt src_len);
                    copy_list t (j+src_len)
                end
        in
            copy_list l 0;
            RunCall.clearMutableBit new_vec;
            wordAsVec new_vec
        end
    end
    
    
    fun map (f: 'a->'b) (vec: 'a vector): 'b vector =
    let
        val len = length vec
    in
        if len = 0 then wordAsVec LibrarySupport.emptyVector
        else
        let
            (* Allocate a new vector. *)
            val new_vec = alloc len
            val newResult = wordAsVec new_vec
                
            fun domap i =
                if i >= len then ()
                else (unsafeUpdate(newResult, i, f(unsafeSub(vec, i))); domap(i+1))
        in
            domap 0;
            RunCall.clearMutableBit new_vec;
            newResult
        end
    end

    fun mapi (f: int*'a->'b) (vec:'a vector): 'b vector =
    let
        val len = length vec
    in
        if len = 0 then wordAsVec LibrarySupport.emptyVector
        else
        let
            (* Allocate a new vector. *)
            val new_vec = alloc len
            val newResult = wordAsVec new_vec
                
            fun domap i =
                if i >= len then ()
                else (unsafeUpdate(newResult, i, f(i, unsafeSub(vec, i))); domap(i+1))
        in
            domap 0;
            RunCall.clearMutableBit new_vec;
            newResult
        end
    end
    
    (* Create a new vector with the ith element replaced by c *)
    fun update(v: 'a vector, i , c) =
        if i < 0 orelse i >= length v
        then raise Subscript
        else mapi (fn (j, s) => if j = i then c else s) v

    (* Create the other functions. *)
    structure VectorOps =
        PolyVectorOperations(
            struct
                type 'a vector = 'a vector
                fun length v = RunCall.memoryCellLength(vecAsWord v)
                local val u = unsafeSub in fun unsafeSub (v: 'a vector, i: word) = u(v, wordAsInt i) end
                fun unsafeSet _ = raise Fail "Should not be called"
            end);

    open VectorOps;

    local
        (* Install the pretty printer for vectors *)
        (* We may have to do this outside the structure if we
           have opaque signature matching. *)
        fun pretty(depth: FixedInt.int)
                  (printElem: 'a * FixedInt.int -> PolyML.pretty)
                  (x: 'a vector) =
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

end (* Vector *)

structure VectorSlice =
struct
    datatype 'a slice = Slice of { vector: 'a vector,  start: int, length: int };
    
    fun length(Slice{length, ...}) = length
    
    fun op sub (Slice{vector, start, length}, i: int): 'a =
        if i < 0 orelse i >= length then raise General.Subscript
        else unsafeSub(vector, i+start)
    
    (* Create a slice from a vector. *)
    fun slice(vec: 'a vector, i: int, NONE) =
        let
            val len = Vector.length vec
        in
            if i >= 0 andalso i <= len
            then Slice{vector=vec, start=i, length=len-i} (* Length is rest of vector. *)
            else raise General.Subscript
        end
     |  slice(vec: 'a vector, i: int, SOME l) =
        let
            val len = Vector.length vec
        in
            if i >= 0 andalso l >= 0 andalso i+l <= len
            then Slice{vector=vec, start=i, length=l} (* Length is as given. *)
            else raise General.Subscript
        end
        
    (* Slice from the whole vector. *)
    fun full v = Slice{vector=v, start=0, length=Vector.length v}

    (* Slice from existing slice *)
    fun subslice(Slice{vector, start, length}, i: int, NONE) =
        if i >= 0 andalso i <= length
        then Slice{vector=vector, start=i+start, length=length-i} (* Length is rest of array. *)
        else raise General.Subscript

     |  subslice(Slice{vector, start, length}, i: int, SOME l) =
        if i >= 0 andalso l >= 0 andalso i+l <= length
        then Slice{vector=vector, start=i+start, length=l} (* Length is as given. *)
        else raise General.Subscript
    
    fun vector(Slice{vector, start, length}) =
        if length = 0 then wordAsVec LibrarySupport.emptyVector (* Special case for zero *)
        else
        let
            (* Make a vector initialised to zero. *)
            val new_vec = alloc length
        in
            RunCall.moveWords(vecAsWord vector, new_vec, Word.fromInt start, 0w0, Word.fromInt length);
            RunCall.clearMutableBit new_vec;
            wordAsVec new_vec
        end

    fun base(Slice{vector, start, length}) = (vector, start, length)
    
    fun isEmpty(Slice{length, ...}) = length = 0

    (* Return the first item of the slice and the rest of the slice. *)
    fun getItem(Slice{length=0, ...}) = NONE
     |  getItem(Slice{vector, start, length}) =
            SOME(unsafeSub(vector, start), Slice{vector=vector, start=start+1, length=length-1})
            

    fun concat [] = wordAsVec LibrarySupport.emptyVector
     |  concat l =
    let
        (* Calculate the total length *)
        fun total [] i = i
          | total (h::t) i = total t (i+length h)
    
        val total_len = total l 0
    in
        if total_len = 0 then wordAsVec LibrarySupport.emptyVector
        else
        let
            (* Allocate a new vector. *)
            val new_vec = alloc total_len
                
            fun copy_list [] _ = ()
              | copy_list (Slice{vector, start, length}::t) j =
                (
                    RunCall.moveWords(vecAsWord vector, new_vec, Word.fromInt start, Word.fromInt j, Word.fromInt length);
                    copy_list t (j+length)
                )
        in
            copy_list l 0;
            RunCall.clearMutableBit new_vec;
            wordAsVec new_vec
        end
    end
    
    fun map (f: 'a->'b) (Slice{vector:'a Vector.vector, start, length}): 'b Vector.vector =
        if length = 0 then wordAsVec LibrarySupport.emptyVector
        else
        let
            (* Allocate a new vector. *)
            val new_vec = alloc length
            val newResult = wordAsVec new_vec
                
            fun domap i =
                if i >= length then ()
                else (unsafeUpdate(newResult, i, f(unsafeSub(vector, i+start))); domap(i+1))
        in
            domap 0;
            RunCall.clearMutableBit new_vec;
            newResult
        end

    fun mapi (f: int*'a->'b) (Slice{vector:'a Vector.vector, start, length}): 'b Vector.vector =
        if length = 0 then wordAsVec LibrarySupport.emptyVector
        else
        let
            (* Allocate a new vector. *)
            val new_vec = alloc length
            val newResult = wordAsVec new_vec
                
            fun domap i =
                if i >= length then ()
                else (unsafeUpdate(newResult, i, f(i, unsafeSub(vector, i+start))); domap(i+1))
        in
            domap 0;
            RunCall.clearMutableBit new_vec;
            newResult
        end


    (* Create the other functions. *)
    structure VectorOps =
        PolyVectorOperations(
            struct
                type 'a vector = 'a slice
                fun length(Slice{length, ...}) = intAsWord length
                val unsafeSub = fn (Slice{vector, start, ...}, i) => unsafeSub (vector, wordAsInt i + start)
                fun unsafeSet _ = raise Fail "Should not be called"
            end);

    open VectorOps;

end (* VectorSlice *)

end (* Local in end *);
  
structure VectorSlice :> VECTOR_SLICE = VectorSlice;

local
    open VectorSlice
    (* Install the pretty printer for vector slices *)
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
    val () = PolyML.addPrettyPrinter pretty
end
;


(* type 'a vector is available unqualified in the global basis. *)
val vector : 'a list -> 'a vector = Vector.fromList;
