(*
    Title:      Standard Basis Library: StreamIO functor
    Copyright   David C.J. Matthews 2000, 2005, 2019, 2025

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

functor BasicStreamIO(
    structure PrimIO : PRIM_IO
    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
    structure VectorSlice: MONO_VECTOR_SLICE
    structure ArraySlice: MONO_ARRAY_SLICE
    sharing type PrimIO.elem = Vector.elem = Array.elem = VectorSlice.elem = ArraySlice.elem
    sharing type PrimIO.vector = Vector.vector = Array.vector = VectorSlice.vector = ArraySlice.vector
    sharing type PrimIO.array = Array.array = ArraySlice.array
    sharing type PrimIO.vector_slice = VectorSlice.slice = ArraySlice.vector_slice
    sharing type PrimIO.array_slice = ArraySlice.slice
    val someElem : PrimIO.elem
    ):
    sig
    include STREAM_IO
    (* Note: This is non-standard but enables us to define
       the derived BinIO and TextIO structures more efficiently. *)
    val outputVec: outstream * PrimIO.vector_slice -> unit
    end =
struct
    open IO
    type vector = Vector.vector
    type elem = PrimIO.elem
    datatype reader = datatype PrimIO.reader
    datatype writer = datatype PrimIO.writer
    type array = Array.array
    type pos = PrimIO.pos

    exception Interrupt = RunCall.Interrupt

    (* Called after any exception in the lower level reader or
       writer to map any exception other than Io into Io. *)
    fun mapToIo (io as Io _, _, _) = io
      | mapToIo (Interrupt, _, _) = Interrupt
      | mapToIo (nonIo, name, caller) =
            Io { name = name, function = caller, cause = nonIo }

    val emptyVec = Vector.fromList [] (* Represents end-of-stream. *)

    datatype instream =
        (* The usual state of a stream: We may have to read from the reader
           before we have any real data or we may have already read. *)
        Uncommitted of { state: streamState ref,
                         locker: Thread.Mutex.mutex }
        (* If we know we have unread input we can return this as the
           stream.  That allows part of the stream to be read without
           locking.  This is an optimisation. *)
      | Committed of
           { vec: vector, offset: int, rest: instream, startPos: pos option }

    and streamState =
        Truncated (* The stream has been closed or truncated. *)
    |   HaveRead of (* A vector has been read from the stream.  If the
                       vector has size zero this is treated as EOF.
                       startPos is the position when the vector was
                       read. *)
            {vec: vector, rest: streamState ref, startPos: pos option }
    |   ToRead of reader (* We have not yet closed or truncated the stream. *)


    (* Outstream. *)
    and outstream =
        OutStream of {
            wrtr: writer,
            buffType : IO.buffer_mode ref,
            buf: array,
            bufp: int ref,
            streamState: outstreamState ref,
            locker: Thread.Mutex.mutex
            }
    
    (* Stream state.
       OutStreamOpen means that attempts to write should proceed.
       OutStreamTerminated means the stream has been "terminated" i.e. buffers
       have been flushed and the writer has been extracted by getWriter.
       Any attempt to write at this level should fail.
       OutStreamClosed means that the writer's "close" function has been called.
       In addition the stream has been terminated.*)
    and outstreamState = OutStreamOpen | OutStreamTerminated | OutStreamClosed

    datatype out_pos = OutPos of outstream * pos

    (* Create a new stream from the vector and the reader. *)
    fun mkInstream (r, v: vector): instream =
    let
        val readRest =
           Uncommitted { state = ref (ToRead r), locker = Thread.Mutex.mutex() }
        (* If the vector is non-empty the first entry is as though the
           vector had been read otherwise it's just the reader. *)
    in
        if Vector.length v = 0
        then readRest
        else Committed { vec = v, offset = 0, rest = readRest, startPos = NONE }
    end

    local
        fun input' (ref (HaveRead {vec, rest, ...}), locker) =
            let
                (* TODO: If we have already read further on we could convert
                   these entries to Committed. *)
            in
                (vec, Uncommitted{ state = rest, locker = locker })
            end

          | input' (s as ref Truncated, locker) = (* Truncated: return end-of-stream *)
                   (emptyVec, Uncommitted{ state = s, locker = locker })

          | input' (state as
                       ref(readMore as ToRead (RD {chunkSize, readVec = SOME readVec, getPos, ...})),
                       locker) =
            let
                (* We've not yet read this.  Try reading from the reader. *)
                val startPos =
                   case getPos of SOME g => SOME(g()) | NONE => NONE
                val data = readVec chunkSize
                (* Create a reference to the reader which will be updated by
                   the next read.  The ref is shared between the existing stream
                   and the new one so reading on either adds to the same chain. *)
                val nextLink = ref readMore
                val nextChunk =
                    HaveRead {vec = data, rest = nextLink, startPos = startPos}
            in
                (* Extend the stream by adding this vector to the list of chunks read so far. *)
                state := nextChunk;
                (* Return a new stream which continues reading. *)
                (data, Uncommitted { state = nextLink, locker = locker })
            end

          | input' (ref(ToRead(RD{name, ...})), _) =
                (* readVec missing in reader. *)
                raise Io { name = name, function = "input", cause = BlockingNotSupported }

         fun inputNList' (ref (HaveRead {vec, rest, startPos}), locker, n) =
            let
                val vecLength = Vector.length vec
            in
                if vecLength = 0 (* End-of-stream: Return next in list. *)
                then ([vec], Uncommitted{ state = rest, locker = locker })
                else if n < vecLength
                then (* We can use what's already been read.  The stream we return allows us
                        to read the rest without blocking. *)
                    ([VectorSlice.vector(VectorSlice.slice(vec, 0, SOME n))],
                     Committed{ vec = vec, offset = n, startPos = startPos,
                         rest = Uncommitted{ state = rest, locker = locker}  })
                else if n = vecLength
                then (* Exactly uses up the buffer.  New stream state is the next entry. *)
                    ([vec], Uncommitted{ state = rest, locker = locker})
                else (* Have to get the next item *)
                let
                    val (nextVecs, nextStream) = inputNList' (rest, locker, n - vecLength)
                in
                    (vec :: nextVecs, nextStream)
                end
            end

         | inputNList' (s as ref Truncated, locker, _) =
                (* Truncated: return end-of-stream *)
                   ([emptyVec], Uncommitted{ state = s, locker = locker })
   
         | inputNList' (f, locker, n) = (* ToRead *)
             let
                 val (vec, f') = input' (f, locker)
             in
                 if Vector.length vec = 0
                 then ([vec], f') (* Truncated or end-of-file. *)
                 else inputNList' (f, locker, n) (* Reread *)
             end

    in
        fun input (Uncommitted { state, locker }) =
                LibraryIOSupport.protect locker input' (state, locker)

        |   input (Committed { vec, offset, rest, ... }) =
              (* This stream was produced from re-reading a stream that already
                 had data.  We can return the result without the overhead of locking. *)
                (VectorSlice.vector(VectorSlice.slice(vec, offset, NONE)), rest)

        fun inputNList (Uncommitted { state, locker }, n) =
                LibraryIOSupport.protect locker inputNList' (state, locker, n)

        |   inputNList (Committed { vec, offset, rest, startPos }, n) =
            let
                val vecLength = Vector.length vec
            in
                if vecLength = 0 (* End-of-stream: Return next in list. *)
                then ([vec], rest)
                else if n < vecLength - offset
                then (* We can use what's already been read.  Next entry is a committed
                        stream that returns the part we haven't yet used. *)
                    ([VectorSlice.vector(VectorSlice.slice(vec, offset, SOME n))],
                     Committed{ vec = vec, offset = offset+n, rest = rest, startPos = startPos })
                else if n = vecLength - offset
                then (* Exactly uses up the buffer.  New stream state is the next entry. *)
                    ([VectorSlice.vector(VectorSlice.slice(vec, offset, NONE))], rest)
                else (* Have to get the next item *)
                let
                    val (nextVecs, nextStream) = inputNList (rest, n - (vecLength - offset))
                in
                    (VectorSlice.vector(VectorSlice.slice(vec, offset, NONE)) :: nextVecs,
                     nextStream)
                end
            end

        fun inputN (f, n) =
        if n < 0
        then raise Size
        else if n = 0 (* Defined to return the empty vector and f *)
        then (emptyVec, f)
        else
        let
            val (vecs, f') = inputNList (f, n)
        in
            (Vector.concat vecs, f')
        end

        (* Read the whole of the remaining input until we get an EOF.
           The stream should then be positioned beyond that in case
           there are multiple EOFs *)
        fun inputAll f =
        let
            fun inputToEOF (l, f) =
            let
                val (s, f') = input f
            in
                if Vector.length s = 0
                then (l, f')
                else inputToEOF (s :: l, f')
            end
    
            val (all, f') = inputToEOF ([], f)
        in
            (Vector.concat(List.rev all), f')
        end

        (* Note a crucial difference between inputN and input1.  Because input1
           does not return a stream if it detects EOF it cannot advance beyond
           a temporary EOF in a stream. *)
        fun input1 (Committed { vec, offset, rest, startPos }) =
            let
                val vecSize = Vector.length vec
            in
                if vecSize = 0
                then NONE
                else if vecSize = offset+1
                then SOME(Vector.sub(vec, offset), rest)
                else SOME(Vector.sub(vec, offset),
                       Committed{ vec = vec, offset = offset+1, rest = rest, startPos = startPos })
            end

        |   input1 f =
            let
                val (s, f') = inputN (f, 1)
            in
                if Vector.length s = 0
                then NONE
                else SOME(Vector.sub(s, 0), f')
            end

    end

    local
        fun doClose (ref (HaveRead {rest, ...})) = doClose rest
          | doClose (ref Truncated) = ()
          | doClose (state as ref (ToRead (RD{close, name, ...}))) =
              (state := Truncated; close() handle exn => raise mapToIo(exn, name, "closeIn"))
    in
        fun closeIn (Uncommitted { state, locker }) = LibraryIOSupport.protect locker doClose state
          | closeIn (Committed { rest, ...}) = closeIn rest
    end

    local
        (* Return the reader. *) 
        fun getReader' (ref (HaveRead {rest, ...})) = getReader' rest
        |   getReader' (ref Truncated) =
                raise Io { name = "",  function = "getReader", cause = ClosedStream }
        |   getReader' (state as ref (ToRead reader)) =
                (state := Truncated; reader)
    in
        fun getReader'' (Uncommitted { state, locker }) =
                LibraryIOSupport.protect locker getReader' state
        |   getReader'' (Committed { rest, ... }) = getReader'' rest

        fun getReader f =
        let
            val reader = getReader'' f
            val (allInput, _) = inputAll f
        in
            (* Return the reader along with buffered input.  It's not clear
               what to do if there are EOFs in the stream.  The book says the
               result is the result of inputAll which takes everything up to the
               first EOF.  *)
            (reader, allInput)
        end
    end
    
    local
        (* Check that the stream is not terminated and then convert a file position
           plus a vector offset into a file position.  In particular, if the reader
           has converted CRNL into NL we don't have a simple relationship between
           elements and file offsets. *)
        fun findPosition'(startPos, offset, HaveRead {rest=ref rest, ...}) =
                findPosition'(startPos, offset, rest)
        |   findPosition'(_, _, Truncated) =
                raise Io { name = "",  function = "filePosIn", cause = ClosedStream }
        |   findPosition'(startPos, offset,
                ToRead (RD { getPos = SOME getPos, setPos = SOME setPos,
                             readVec = SOME readVec, ...})) =
                if offset = 0
                then startPos (* Easy *)
                else
                    (* When we read this vector we recorded the file position of
                       the beginning only.  To find the file position of the
                       particular element we actually need to read the portion of
                       the input up to that element and find out the file position
                       at that point. *)
                let
                    val savep = getPos() (* Save current position. *)
                    (* Move to the point where we read the vector. *)
                    val () = setPos startPos;
                    (* Call readVec until we have read the required number
                       of elements.  N.B.  Ganser & Reppy has a bug here.
                       There is no guarantee that readVec n will actually
                       return n elements so it's unsafe to assume that it
                       will move the file pointer by n elements. *)
                    fun doRead n =
                    let
                        val read = Vector.length(readVec n)
                    in
                        if read = n orelse read = 0 (* Error? *)
                        then ()
                        else doRead (n - read)
                    end
                    (* Read the offset number of elements. *)
                    val () = doRead offset;
                    (* Record the position after actually reading the elements. *)
                    val position = getPos();
                in
                    setPos savep; (* Restore. *)
                    position
                end
        |   findPosition'(_, _, ToRead _) =
                raise Io { name = "",  function = "filePosIn",
                           cause = RandomAccessNotSupported }

        fun findPosition(startPos, offset, Committed { rest, ... }) =
                findPosition(startPos, offset, rest)
        |   findPosition(startPos, offset, Uncommitted { state = ref state, locker }) =
                LibraryIOSupport.protect locker findPosition' (startPos, offset, state)

        fun filePosIn' (HaveRead {rest=ref rest, startPos = SOME startPos, ...}) =
                findPosition'(startPos, 0, rest)
        |   filePosIn' (HaveRead {startPos = NONE, ...}) =
                raise Io { name = "",  function = "filePosIn",
                           cause = RandomAccessNotSupported }
        |   filePosIn' Truncated =
                raise Io { name = "",  function = "filePosIn", cause = ClosedStream }
        |   filePosIn' (ToRead(RD { getPos = SOME getPos, ...})) = getPos()
        |   filePosIn' (ToRead _) =
                raise Io { name = "",  function = "filePosIn",
                           cause = RandomAccessNotSupported }

    in
        (* Find the first entry to get the position. *)
        fun filePosIn (Uncommitted { state = ref state, locker }) =
                LibraryIOSupport.protect locker filePosIn' state
        |   filePosIn (Committed { offset, rest, startPos = SOME startPos, ... }) =
                findPosition(startPos, offset, rest)
        |   filePosIn (Committed { startPos = NONE, ... }) =
              (* This can occur either because the reader doesn't support getPos or
                 because the position is within the initial vector passed to
                 mkInstream. *)
                raise Io { name = "",  function = "filePosIn",
                           cause = RandomAccessNotSupported }
    end
    
    local
        fun doCanInput' (ref (HaveRead {vec, rest, ...}), locker, n, k) =
            let
                val vecLength = Vector.length vec
            in
                if vecLength = 0
                then SOME k
                else if vecLength >= n
                then SOME (k+n)
                else doCanInput'(rest, locker, n-vecLength, k+vecLength)
            end
        
        |   doCanInput' (ref Truncated, _, _, k) = SOME k

        |   doCanInput' (state as
                            ref(readMore as ToRead (RD {chunkSize, readVecNB = SOME readVecNB, getPos, ...})),
                         locker, n, k) =
            let
                val startPos =
                   case getPos of SOME g => SOME(g()) | NONE => NONE
            in
               (* Read a block full.  This will avoid us creating lots of small items
                  in the list if there is actually plenty of input available. *)
               case readVecNB chunkSize of
                    NONE => (* Reading these would block but we may already have some input. *)
                        if k = 0 then NONE else SOME k
                |   SOME data =>
                    let (* We have to record this in the stream. *)
                        val nextLink = ref readMore
                        val nextChunk =
                            HaveRead {vec = data, rest = nextLink, startPos = startPos}
                    in
                        state := nextChunk;
                        (* Check whether this has satisfied the request. *)
                        doCanInput'(state, locker, n, k)
                    end
            end

        |   doCanInput' (ref(ToRead(RD {name, ...})), _, _, _) = 
                (* readVecNB missing in reader. *)
                raise Io { name = name, function = "canInput", cause = NonblockingNotSupported }

        fun doCanInput (Uncommitted { state, locker }, n, k) =
                LibraryIOSupport.protect locker doCanInput' (state, locker, n, k)
        |   doCanInput (Committed { vec, rest, ... }, n, k) =
            let
                val vecLength = Vector.length vec
            in
                if vecLength = 0
                then SOME k (* Reached EOF. *)
                else if vecLength >= n
                then SOME (k + n) (* Have already read enough. *)
                else doCanInput(rest, n-vecLength, k+vecLength)
            end
    in
        fun canInput(f, n) = if n < 0 then raise Size else doCanInput(f, n, 0)
    end


    (* Look for end-of-stream. Could be defined more directly
       but it probably isn't worth it. *)
    fun endOfStream f =
    let
        val (v, _) = input f
    in
        Vector.length v = 0
    end


    (* OUTPUT *)
    (* In order to be able to flush and close the streams when we exit
       we need to keep a list of the output streams. *)
    val ostreamLock = Thread.Mutex.mutex()
    (* We use a volatile ref so that the list is always reset
       at the start of a program. *)
    val outputStreamList: outstream list ref = LibrarySupport.volatileListRef()

    fun protectOut f (outs as OutStream{locker, ...}) = LibraryIOSupport.protect locker f outs

    fun mkOutstream'(wrtr as WR{chunkSize, ...}, buffMode) =
    let
        open Thread.Mutex
        val strm =
            OutStream{wrtr=wrtr,
                      buffType=ref buffMode,
                      buf=Array.array(chunkSize, someElem),
                      streamState=ref OutStreamOpen,
                      bufp=ref 0,
                      locker=Thread.Mutex.mutex()}
    in
        (* Add it to the list. *)
        outputStreamList := strm :: ! outputStreamList;
        strm
    end
    
    val mkOutstream = LibraryIOSupport.protect ostreamLock mkOutstream'

    fun getBufferMode(OutStream{buffType=ref b, ...}) = b

    local
        (* Flush anything from the buffer. *)
        fun flushOut'(OutStream{buf, bufp=bufp as ref endBuf,
                               wrtr=wrtr as WR{name, ...}, ...}) =
            if endBuf = 0 then () (* Nothing buffered *)
            else case wrtr of
                WR{writeArr=SOME wa, ...} =>
                let
                    fun flushBuff n =
                    let
                        val written =
                            wa(ArraySlice.slice(buf, n, SOME(endBuf-n)))
                            handle exn => raise mapToIo(exn, name, "flushOut")
                    in
                        if written+n = endBuf then ()
                        else flushBuff(written+n)
                    end
                in
                    (* Set the buffer to empty BEFORE writing anything.  If
                       we get an asynchronous interrupt (ctrl-C) we want to
                       lose data in preference to duplicating it. *)
                    bufp := 0;
                    flushBuff 0
                end
            |   _ =>
                raise Io { name = name, function = "flushOut",
                           cause = BlockingNotSupported }

        (* Terminate a stream either because it has been closed or
           because we have extracted the underlying writer. *)
        fun terminateStream'(f as OutStream{streamState as ref OutStreamOpen, ...}) =
            let
                (* outstream is not an equality type but we can get the
                   desired effect by comparing the streamState references for
                   equality (N.B. NOT their contents). *)
                fun removeThis(OutStream{streamState=streamState', ...}) =
                    streamState' <> streamState
                open Thread.Mutex
            in
                streamState := OutStreamTerminated;
                lock ostreamLock;
                outputStreamList := List.filter removeThis (!outputStreamList);
                unlock ostreamLock;
                flushOut' f
            end
        |   terminateStream' _ = () (* Nothing to do. *)
      
        (* Close the stream.  We must call the writer's close function only once
           unless the flushing fails.  In that case the stream is left open.  *)
        fun closeOut'(OutStream{streamState=ref OutStreamClosed, ...}) = ()

        |   closeOut'(f as OutStream{wrtr=WR{close, name, ...}, streamState, ...}) =
            (
                terminateStream' f;
                streamState := OutStreamClosed;
                close() handle exn => raise mapToIo(exn, name, "closeOut") (* Close the underlying writer. *)
            )

        (* Flush the stream, terminate it and return the underlying writer.
           According to the documentation this raises an exception if the stream
           is "closed" rather than "terminated" implying that it is possible to extract
           the writer more than once.  That's in contrast to getReader which is defined
           to raise an exception if the stream is closed or truncated. *)
        fun getWriter'(OutStream{wrtr=WR{name, ...}, streamState=ref OutStreamClosed, ...}) =
            (* Already closed. *)
                raise Io { name = name, function = "getWriter", cause = ClosedStream }
         |  getWriter'(f as OutStream{buffType, wrtr, ...}) =
            (
               terminateStream' f;
               (wrtr, !buffType)
            )

        (* Set the buffer mode, possibly flushing the buffer as it does. *)
        fun setBufferMode' newBuff (f as OutStream{buffType, bufp, ...}) =
        (* Question: What if the stream is terminated? *)
            (
            if newBuff = NO_BUF andalso !bufp <> 0
            then (* Flush pending output. *)
                (* Switching from block to line buffering does not flush. *)
                flushOut' f
            else ();
            buffType := newBuff
            )

        (* Internal function: Write a vector directly to the writer. It only
           returns when the vector has been completely written.
           "output" should work if the writer only provides writeArr so we
           may have to use that if writeVec isn't there. *)
        (* FOR TESTING: Put writeArr first. *)
        fun writeVec(OutStream{wrtr=WR{writeVec=SOME wv, name, ...}, ...}, v, i, len) =
        let
            fun forceOut p =
            let
                val written = wv(VectorSlice.slice(v, p+i, SOME(len-p)))
                    handle exn => raise mapToIo(exn, name, "output")
            in
                if written+p = len then ()
                else forceOut(written+p)
            end
        in
            forceOut 0
        end
        |  writeVec(OutStream{wrtr=WR{writeArr=SOME wa, name, ...}, ...}, v, i, len) =
        let
            val buffSize = 10
            val buff = Array.array(buffSize, someElem);
            fun forceOut p =
            let
                val toCopy = Int.min(len-p, buffSize)
                val () =
                   ArraySlice.copyVec{src=VectorSlice.slice(v, p+i, SOME toCopy), dst=buff, di=0}
                val written = wa(ArraySlice.slice(buff, 0, SOME toCopy))
                    handle exn => raise mapToIo(exn, name, "output")
            in
                if written+p = len then ()
                else forceOut(written+p)
            end
        in
            forceOut 0
        end       
        |   writeVec(OutStream{wrtr=WR{name, ...}, ...}, _, _, _) =
                raise Io { name = name, function = "output",
                           cause = BlockingNotSupported }
    
        (* Internal function. Write a vector to the stream using the start and
           length provided. *)
        fun outputVector (v, start, vecLen) (f as OutStream{streamState=ref OutStreamOpen, buffType, buf, bufp, ...})  =
        let
            val buffLen = Array.length buf

            fun arrayCopyVec{src: Vector.vector, si: int, len: int, dst: Array.array, di: int} =
                ArraySlice.copyVec{src=VectorSlice.slice(src, si, SOME len), dst=dst, di=di};
   
            fun addVecToBuff () =
                if vecLen < buffLen - !bufp
                then (* Room in the buffer. *)
                    (
                    arrayCopyVec{src=v, si=start, len=vecLen, dst=buf, di= !bufp};
                    bufp := !bufp + vecLen
                    )
                else
                let
                    val buffSpace = buffLen - !bufp
                in
                    (* Copy as much of the vector as will fit *)
                    arrayCopyVec{src=v, si=start, len=buffSpace, dst=buf, di= !bufp};
                    bufp := !bufp+buffSpace;
                    (* TODO: Flushing the buffer ensures that all the
                       buffer contents have been written.  We don't
                       actually need that, what we need is for enough
                       to have been written that we have space in the
                       buffer for the rest of the vector. *)
                    flushOut' f; (* Write it out. *)
                    (* Copy the rest of the vector. *)
                    arrayCopyVec{src=v, si=start+buffSpace, len=vecLen-buffSpace, dst=buf, di=0};
                    bufp := vecLen-buffSpace
                end (* addVecToBuff *)
        in
            if vecLen > buffLen
            then (* If the vector is too large to put in the buffer we're
                    going to have to write something out.  To reduce copying
                    we simply flush the buffer and write the vector directly. *)
                (flushOut' f; writeVec(f, v, start, vecLen))
            else (* Try copying to the buffer. *)
                if !buffType = IO.NO_BUF
                then (* Write it directly *) writeVec(f, v, start, vecLen)
                else (* Block or line buffering - add it to the buffer.
                        Line buffering is treated as block buffering on binary
                        streams and handled at the higher level for text streams. *)
                    addVecToBuff()
        end

            (* State was not open *)
        |   outputVector _ (OutStream{wrtr=WR{name, ...}, ...}) =
                raise Io { name = name, function = "output", cause = ClosedStream }
    
        (* This could be defined in terms of outputVector but this is
           likely to be much more efficient if we are buffering. *)
        fun output1' c (f as OutStream{streamState=ref OutStreamOpen, buffType, buf, bufp, ...}) =
            if !buffType = IO.NO_BUF
            then writeVec(f, Vector.fromList[c], 0, 1)
            else (* Line or block buffering. *)
            (
                Array.update(buf, !bufp, c);
                bufp := !bufp + 1;
                if !bufp = Array.length buf then flushOut' f else ()
            )

            (* State was not open *)
        |   output1' _ (OutStream{wrtr=WR{name, ...}, ...}) =
                raise Io { name = name, function = "output1", cause = ClosedStream }

        fun getPosOut'(f as OutStream{wrtr=WR{name, getPos=SOME getPos, ...}, ...}) =
            (
                flushOut' f;
                OutPos(f, getPos()) handle exn => raise mapToIo(exn, name, "getPosOut")
            )
                
        |   getPosOut'(OutStream{wrtr=WR{name, ...}, ...}) =
                raise Io { name = name, function = "getPosOut",
                           cause = RandomAccessNotSupported }
    
        fun setPosOut' p (f as OutStream{wrtr=WR{setPos=SOME setPos, ...}, ...}) =
            (
                flushOut' f;
                setPos p;
                f
            )
        |   setPosOut' _ (OutStream{wrtr=WR{name, ...}, ...}) =
                raise Io { name = name, function = "setPosOut",
                           cause = RandomAccessNotSupported }
    in
        fun output1(f, c) = protectOut (output1' c) f
        fun output(f, v) = protectOut (outputVector(v, 0, Vector.length v)) f
        val flushOut = protectOut flushOut'
        val closeOut = protectOut closeOut'
        val getWriter = protectOut getWriter'
        fun setBufferMode(f, n) = protectOut (setBufferMode' n) f
  
        (* Exported function to output part of a vector.  Non-standard. *)
        fun outputVec(f, slice) =
            let
                val (v, i, len) = VectorSlice.base slice
            in
                protectOut (outputVector(v, i, len)) f
            end

        val getPosOut = protectOut getPosOut'

        fun setPosOut(OutPos(f, p)) = protectOut (setPosOut' p) f
    end


    fun filePosOut(OutPos(_, p)) = p

    (* We need to set up a function to flush the streams when we
       exit.  This has to be set up for every session so we set up
       an entry function, which is persistent, to do it. *)
    local
        fun closeAll () =
        (* Close all the streams.  closeOut removes the streams
           from the list so we should end up with outputStreamList
           being nil. *)
            List.foldl (fn (s, ()) => closeOut s handle _ => ()) ()
                (! outputStreamList)

        fun doOnEntry () = OS.Process.atExit closeAll
    in
        val () = LibrarySupport.addOnEntry doOnEntry;
        val () = doOnEntry() (* Set it up for this session as well. *)
    end

    local
        open PolyML
        fun printWithName(s, name) =
            PolyML.PrettyString(String.concat[s, "-\"", String.toString name, "\""])

        fun prettyIn depth a (Committed { rest, ...}) =
                prettyIn depth a rest
        |   prettyIn _     _ (Uncommitted { state = ref streamState, ...}) =
            let
                fun prettyState Truncated =
                        PolyML.PrettyString("Instream-truncated")
                |   prettyState (HaveRead{ rest = ref rest, ...}) =
                        prettyState rest
                |   prettyState (ToRead(RD{name, ...})) =
                        printWithName("Instream", name)
            in
                prettyState streamState
            end

        fun prettyOut _ _ (OutStream { wrtr = WR { name, ...}, ...}) =
            printWithName("Outstream", name)
    in
        val () = addPrettyPrinter prettyIn
        val () = addPrettyPrinter prettyOut
    end
end;

(* Define the StreamIO functor in terms of BasicStreamIO to filter
   out outputVec. *)
(* This is non-standard.  According to G&R 2004 StreamIO does not take the slice structures as args. *)
functor StreamIO(
    structure PrimIO : PRIM_IO
    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
    structure VectorSlice: MONO_VECTOR_SLICE
    structure ArraySlice: MONO_ARRAY_SLICE
    sharing type PrimIO.elem = Vector.elem = Array.elem = VectorSlice.elem = ArraySlice.elem
    sharing type PrimIO.vector = Vector.vector = Array.vector = VectorSlice.vector = ArraySlice.vector
    sharing type PrimIO.array = Array.array = ArraySlice.array
    sharing type PrimIO.vector_slice = VectorSlice.slice = ArraySlice.vector_slice
    sharing type PrimIO.array_slice = ArraySlice.slice
    val someElem : PrimIO.elem
    ): STREAM_IO =
struct
    structure StreamIO =
        BasicStreamIO(structure PrimIO = PrimIO
                      and Vector = Vector
                      and Array = Array
                      and VectorSlice = VectorSlice
                      and ArraySlice = ArraySlice
                      val someElem = someElem)
    open StreamIO
end;
