(*
    Title:      Standard Basis Library: ImperativeIO functor
    Copyright   David C.J. Matthews 2000, 2015, 2020

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

(* This is also used in TextIO.  We need "protect". *)
functor BasicImperativeIO (
    structure StreamIO : STREAM_IO
    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
    sharing type StreamIO.elem = Vector.elem = Array.elem
    sharing type StreamIO.vector = Vector.vector = Array.vector
) (* No signature on the result *)
=
struct
    open IO

    structure StreamIO = StreamIO
    type vector = Vector.vector
    and  elem = StreamIO.elem

    datatype instream = InStream of {
        (* An imperative input stream is a reference to a lazy functional stream.
           It is an option ref because we use a volatile ref that is set to NONE
           if this is exported and re-imported. *)
        fStream: StreamIO.instream option ref,
        lock: Thread.Mutex.mutex
        }
    and outstream = OutStream of {
        (* An imperative output stream is a reference to the underlying stream.
           Unlike instream the underlying stream is also imperative but we need
           a reference here to allow us to redirect. As with instream
           this is a volatile ref. *)
        fStream: StreamIO.outstream option ref
        }
    (* We don't need a mutex for outstream assuming := and ! are atomic
       i.e. '!' returns either the previous value or the current one and
       not some intermediate value. *)

    (* Use no-overwrite refs for imperative streams.  This is really only needed for
       stdIn to make sure that when we call PolyML.SaveState.loadState we don't
       overwrite any unread input by the contents of the buffer when saveState
       was called. *)

    fun mkInstream (s : StreamIO.instream) : instream =
    let
        val r = LibrarySupport.volatileOptionRef ()
        val () = r := SOME s
    in
        InStream{fStream = r, lock = Thread.Mutex.mutex()}
    end
        
    fun protect (InStream{fStream, lock}) f =
        LibraryIOSupport.protect lock f fStream

    (* Get and set the underlying stream.  We have to interlock
       setInstream at least. *)
    fun getInstream s =
        protect s (
            fn ref (SOME stream) => stream | _ => raise Io { name = "",  function = "getInstream", cause = ClosedStream }
        ) 
    and setInstream(InStream{fStream, lock}, s) =
        LibraryIOSupport.protect lock (fn fStream => fStream := SOME s) fStream

    (* These are just wrappers for the underlying functional calls. *)
    fun input s = protect s 
           (fn fStream as ref(SOME stream) =>
            let
                val (v, f') = StreamIO.input stream
            in
                fStream := SOME f';
                v
            end
            |   _ => Vector.fromList[])

    (* We don't use StreamIO.input1 here because that never advances over
       a temporary EOF. *)
    fun input1 s = protect s
           (fn fStream as ref(SOME stream) =>
            let
                val (s, f') = StreamIO.inputN(stream, 1)
            in
                fStream := SOME f';
                if Vector.length s = 0 then NONE else SOME(Vector.sub(s, 0))
            end
            |   _ => NONE)

    fun inputN(InStream{fStream, lock}, n) =
        LibraryIOSupport.protect lock
           (fn fStream as ref(SOME stream) =>
            let
                val (v, f') = StreamIO.inputN(stream, n)
            in
                fStream := SOME f';
                v
            end
            |   _ => Vector.fromList[]) fStream

    fun inputAll s = protect s
           (fn fStream as ref(SOME stream) =>
            let
                val (v, f') = StreamIO.inputAll stream
            in
                fStream := SOME f';
                v
            end
            |   _ => Vector.fromList[])

    (* These next functions only query the stream and don't affect the
       fStream ref so don't really need interlocking.  If two threads
       call these functions simultaneously the result is non-deterministic
       anyway. *)
    fun canInput(InStream{fStream, lock}, n) =
        LibraryIOSupport.protect lock
           (fn ref(SOME stream) => StreamIO.canInput(stream, n) | _ => SOME 0) fStream

    and closeIn s =
        protect s (fn ref(SOME stream) => StreamIO.closeIn stream | _ => ())
    and endOfStream s =
        protect s (fn ref(SOME stream) => StreamIO.endOfStream stream | _ => true)

    fun lookahead s = protect s
        (fn ref(SOME stream) =>
            (
                case StreamIO.input1 stream of
                    NONE => NONE
                |   SOME(s, _) => SOME s
            )
        |   _ => NONE
        )

    (* These are simply wrappers. *)

    fun mkOutstream (s : StreamIO.outstream) : outstream =
    let
        val r = LibrarySupport.volatileOptionRef()
        val () = r := SOME s
    in
        OutStream{fStream = r}
    end

    fun getOutstream(OutStream{fStream = ref(SOME s)}) = s
    |   getOutstream _ = raise Io { name = "",  function = "getOutstream", cause = ClosedStream }
    and setOutstream(OutStream{fStream}, s) = fStream := SOME s

    fun output(out, v) = StreamIO.output(getOutstream out, v)
    and output1(out, c) = StreamIO.output1(getOutstream out, c)
    and flushOut out = StreamIO.flushOut(getOutstream out)
    and closeOut out = StreamIO.closeOut(getOutstream out)
    and getPosOut out = StreamIO.getPosOut(getOutstream out)

    fun setPosOut(OutStream{fStream}, p) = fStream := SOME(StreamIO.setPosOut p)
    
    (* Add pretty printers to hide the internals.  These just use the implementation streams. *)
    local
        open PolyML
        fun prettyIn depth _ (InStream{ fStream = ref(SOME s), ...}) =
            PolyML.prettyRepresentation(s, depth)
        |   prettyIn _ _ _ = PolyML.PrettyString("Instream-closed")
        fun prettyOut depth _ (OutStream { fStream = ref(SOME s), ...}) =
            PolyML.prettyRepresentation(s, depth)
        |   prettyOut _ _ _ = PolyML.PrettyString("Outstream-closed")
    in
        val () = addPrettyPrinter prettyIn
        val () = addPrettyPrinter prettyOut
    end
end;

(* General exported version with final signature. *)
functor ImperativeIO (
    structure StreamIO : STREAM_IO
    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
    sharing type StreamIO.elem = Vector.elem = Array.elem
    sharing type StreamIO.vector = Vector.vector = Array.vector
) : IMPERATIVE_IO
        where type StreamIO.elem = StreamIO.elem
        where type StreamIO.vector = StreamIO.vector
        where type StreamIO.instream = StreamIO.instream
        where type StreamIO.outstream = StreamIO.outstream
        where type StreamIO.out_pos = StreamIO.out_pos
        where type StreamIO.reader = StreamIO.reader
        where type StreamIO.writer = StreamIO.writer
        where type StreamIO.pos = StreamIO.pos
        =
    BasicImperativeIO(structure StreamIO = StreamIO and Vector = Vector and Array = Array);

