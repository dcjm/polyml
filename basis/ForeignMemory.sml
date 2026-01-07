(*
    Title:      Foreign Function Interface: memory operations
    Author:     David Matthews
    Copyright   David Matthews 2015, 2017, 2019-20

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


structure ForeignMemory :>
    sig
        eqtype volatileRef
        val volatileRef: SysWord.word -> volatileRef
        val setVolatileRef: volatileRef * SysWord.word -> unit
        val getVolatileRef: volatileRef -> SysWord.word
        
        eqtype voidStar
        val voidStar2Sysword: voidStar -> SysWord.word
        val sysWord2VoidStar: SysWord.word -> voidStar
        val null: voidStar
        
        (* Helper functions to add a word value to an address.
           From 5.8.2 the word value is treated as signed. *)
        val ++ : voidStar * word -> voidStar
        val -- : voidStar * word -> voidStar
        
        (* Remember an address except across loads. *)
        val memoise: ('a -> voidStar) ->'a -> unit -> voidStar
        
        exception Memory

        (* malloc - allocate memory.  N.B. argument is the number of bytes.
           Raises Memory exception if it cannot allocate. *)
        val malloc: word -> voidStar
        (* free - free allocated memory. *)
        val free: voidStar -> unit

        (* alloca: allocate temporary memory on the C-stack and call the function.
           The memory is deallocated when the function returns or raises and exception. *)
        val alloca: word * (voidStar -> 'a) -> 'a

        (* Load and store a value.  From 5.8.2 the offset is
           treated as signed. *)
        val get8:  voidStar * Word.word -> Word8.word
        val get16: voidStar * Word.word -> Word.word
        val get32: voidStar * Word.word -> Word32.word
        val get64: voidStar * Word.word -> SysWord.word
        val set8:  voidStar * Word.word * Word8.word -> unit
        val set16: voidStar * Word.word * Word.word -> unit
        val set32: voidStar * Word.word * Word32.word -> unit
        val set64: voidStar * Word.word * SysWord.word -> unit

        val getFloat:   voidStar * Word.word -> real
        val getDouble:  voidStar * Word.word -> real
        val setFloat:   voidStar * Word.word * real -> unit
        val setDouble:  voidStar * Word.word * real -> unit

        val getAddress: voidStar * Word.word -> voidStar
        val setAddress: voidStar * Word.word * voidStar -> unit
    end
=
struct
    open ForeignConstants
    open ForeignMemory
    
    exception Foreign = Foreign.Foreign

    fun id x = x
    (* Internal utility function. *)
    fun alignUp(s, align) = Word.andb(s + align-0w1, ~ align)

    (* Both volatileRef and SysWord.word are the ADDRESSes of the actual value. *)
    type volatileRef = word ref

    val memMove: SysWord.word * SysWord.word * word * word* word -> unit = RunCall.moveBytes
   
    fun volatileRef init =
    let
        (* Allocate a single word marked as mutable, weak, no-overwrite, byte. *)
        (* A weak byte cell is cleared to zero when it is read in either from the
           executable or from a saved state.  Using the no-overwrite bit ensures
           that if it is contained in the executable it won't be changed by loading
           a saved state but there's a problem if it is contained in a parent state.
           Then loading a child state will clear it because we reload all the parents
           when we load a child. *)
        val v = RunCall.allocateByteMemory(sysWordSize div wordSize, 0wx69)
        (* Copy the SysWord into it. *)
        val () = memMove(init, RunCall.unsafeCast v, 0w0, 0w0, sysWordSize)
    in
        v
    end

    fun setVolatileRef(v, i) = memMove(i, RunCall.unsafeCast v, 0w0, 0w0, sysWordSize)

    fun getVolatileRef var =
    let
        (* Allocate a single word marked as mutable, byte. *)
        val v = RunCall.allocateByteMemory(sysWordSize div wordSize, 0wx41)
        val () = memMove(RunCall.unsafeCast var, v, 0w0, 0w0, sysWordSize)
        val () = RunCall.clearMutableBit v
    in
        v
    end

    type voidStar = SysWord.word
    val voidStar2Sysword = id and sysWord2VoidStar = id (* Exported conversions *)
    val null: voidStar = 0w0
        
    infix 6 ++ --
    (* Helper operations to add a constant to an address.
       These now treat the offset as signed so that adding ~1 is
       the same as subtracting 1. *)
    fun s ++ w = s + SysWord.fromLarge(Word.toLargeX w)
    and s -- w = s - SysWord.fromLarge(Word.toLargeX w)

    fun 'a memoise(f: 'a -> voidStar) (a: 'a) : unit -> voidStar =
    let
        (* Initialise to zero.  That means the function won't be
           executed until we actually want the result. *)
        val v = volatileRef 0w0
    in
        (* If we've reloaded the volatile ref it will have been reset to zero.
           We need to execute the function and set it. *)
        fn () => (case getVolatileRef v of 0w0 => let val r = f a in setVolatileRef(v, r); r end | r => r)
    end

    exception Memory

    (* Get and set addresses.  This is a bit messy because it has to compile on 64-bits as well as 32-bits. *)
    val getAddress: voidStar * Word.word -> voidStar =
        if sysWordSize = 0w4 then Word32.toLargeWord o get32 else get64
    val setAddress: voidStar * Word.word * voidStar -> unit =
        if sysWordSize = 0w4 then fn (s, i, v) => set32(s, i, Word32.fromLargeWord v) else set64

    local
        val systemMalloc: word -> voidStar = RunCall.rtsCallFull1 "PolyFFIMalloc"
        (*val systemFree: word -> unit = RunCall.rtsCallFast1 "PolyFFIFree"*)
        
        (* Simple malloc/free implementation to reduce the number of RTS calls needed. *)
        val lock = Thread.Mutex.mutex()
        (* It would be possible to chain the free list in the C memory
           itself.  For the moment we don't do that.
           The free list is the list of chunks ordered by increasing
           address.  That allows us to merge adjacent free blocks. *)
        val freeList: {address: SysWord.word, size: word} list ref = LibrarySupport.volatileListRef()

        (* Assume that if we align to the maximum of these we're all right. *)
        val maxAlign = Word.max(#align saDouble, Word.max(LibrarySupport.sysWordSize(*#align saPointer*), 0w8(*#align saSint64*)))
        (* We need a length word in each object we allocate but we need enough
           padding to align the result. *)
        val overhead = alignUp(sysWordSize, maxAlign)
        val chunkSize = 0w4096 (* Configure this. *)

        fun addFree(entry, []) = [entry]
        |   addFree(entry, this :: rest) =
            if #address entry < #address this
            then
            (
                if #address entry ++ #size entry = #address this
                then (* New entry is immediately before old one - merge. *)
                    {address= #address entry, size = #size entry + #size this } :: rest
                else entry :: this :: rest
            )
            else if #address this ++ #size this = #address entry
            then (* New entry is immediately after this - merge.  Continue because it could
                    also merge with an entry after this as well. *)
                addFree({address= #address this, size= #size entry + #size this}, rest)
            else this :: addFree(entry, rest) (* Search on. *)

        (* Find free space. *)
        fun findFree (_, []) = (NONE, [])
        |   findFree (space, (this as {size, address}) :: tl) =
            if space = size
            then (SOME address, tl)
            else if space < size
            then (SOME address, {size=size-space, address=address ++ space} :: tl)
            else
            let
                val (res, rest) = findFree(space, tl)
            in
                (res, this :: rest)
            end

        fun freeMem s =
        let
            val addr = s -- overhead
            val size = Word.fromLarge(SysWord.toLarge(getAddress(addr, 0w0)))
        in
            freeList := addFree({address=addr, size=size}, !freeList)
        end
        
        fun allocMem s =
        let
            val space = alignUp(s + overhead, maxAlign)
            val (found, newList) = findFree(space, !freeList)
        in
            case found of
                NONE =>
                let
                    (* Need more memory *)
                    val requestSpace = Word.max(chunkSize, space)
                    val newSpace = systemMalloc requestSpace
                    val _ = newSpace <> null orelse raise Memory
                in
                    (* Add the space to the free list in the appropriate place. *)
                    freeList := addFree({address=newSpace, size=requestSpace}, !freeList);
                    allocMem s (* Repeat - should succeed now. *)
                end
            |   SOME address =>
                let
                    val () = freeList := newList (* Update the free list *)
                    (* Store the length in the first word. *)
                    val () = setAddress(address, 0w0, SysWord.fromLarge(Word.toLarge space))
                in
                    address ++ overhead
                end
        end
    in
        val malloc: word -> voidStar = ThreadLib.protect lock allocMem
        fun free v = if v = null then () else ThreadLib.protect lock freeMem v

        (* Allocate space on the C stack.  This is faster than using malloc/free. *)
        fun alloca(length, f) =
        let
            (* This must be at least 16 byte aligned. *)
            val aligned = alignUp(length, Word.max(maxAlign, 0w16))
            val space = allocCStack aligned
        in
            f space before freeCStack(space, aligned)
                handle exn => (freeCStack(space, aligned); raise exn)
        end
    end
end;

