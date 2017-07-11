(*
    Title:      Rebuild the basis library: IO
    Copyright   David C.J. Matthews 2016

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

(* IO *)
(* This is much simpler if we ignore PrimIO. *)

useBasis "STREAM_IO.sml";
useBasis "IMPERATIVE_IO.sml";

signature TEXT_STREAM_IO =
sig
    include STREAM_IO
    where type vector = CharVector.vector
    where type elem = Char.char

    val inputLine : instream -> (string * instream) option
    val outputSubstr : outstream * Substring.substring -> unit
end;

signature TEXT_IO =
sig
    (* include IMPERATIVE_IO *)
    structure StreamIO : TEXT_STREAM_IO
        where type reader = TextPrimIO.reader
        where type writer = TextPrimIO.writer
        where type pos = TextPrimIO.pos

    type vector = StreamIO.vector
    type elem = StreamIO.elem

    type instream
    type outstream

    val input : instream -> vector
    val input1 : instream -> elem option
    val inputN : instream * int -> vector
    val inputAll : instream -> vector
    val canInput : instream * int -> int option
    val lookahead : instream -> elem option
    val closeIn : instream -> unit
    val endOfStream : instream -> bool
    val output : outstream * vector -> unit
    val output1 : outstream * elem -> unit
    val flushOut : outstream -> unit
    val closeOut : outstream -> unit
    val mkInstream : StreamIO.instream -> instream
    val getInstream : instream -> StreamIO.instream
    val setInstream : instream * StreamIO.instream -> unit
    val mkOutstream : StreamIO.outstream -> outstream
    val getOutstream : outstream -> StreamIO.outstream
    val setOutstream : outstream * StreamIO.outstream -> unit
    val getPosOut : outstream -> StreamIO.out_pos
    val setPosOut : outstream * StreamIO.out_pos -> unit
    (* End of include IMPERATIVE_IO *)

    val inputLine : instream -> string option
    val outputSubstr : outstream * Substring.substring -> unit
    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
    val openString : string -> instream

    val stdIn  : instream
    val stdOut : outstream
    val stdErr : outstream

    val print : string -> unit
    val scanStream : ((Char.char, StreamIO.instream) StringCvt.reader
                      -> ('a, StreamIO.instream) StringCvt.reader)
                      -> instream -> 'a option
end;

structure TextIO: TEXT_IO =
struct
    open TextIO
    val canInput =
        fn (s, n) => Option.map FixedInt.toLarge (canInput(s, FixedInt.fromLarge n))
    val inputN =
        fn (s, n) => inputN(s, FixedInt.fromLarge n)
    structure StreamIO =
    struct
        open StreamIO
        val canInput =
            fn (s, n) => Option.map FixedInt.toLarge (canInput(s, FixedInt.fromLarge n))
        val inputN =
            fn (s, n) => inputN(s, FixedInt.fromLarge n)
    end
end;

signature BIN_IO =
sig
    include IMPERATIVE_IO
       where type StreamIO.vector = Word8Vector.vector
       where type StreamIO.elem = Word8.word
       where type StreamIO.reader = BinPrimIO.reader
       where type StreamIO.writer = BinPrimIO.writer
       where type StreamIO.pos = BinPrimIO.pos

    val openIn  : string -> instream
    val openOut : string -> outstream
    val openAppend : string -> outstream
end;

structure BinIO: BIN_IO =
struct
    open BinIO
    val canInput =
        fn (s, n) => Option.map FixedInt.toLarge (canInput(s, FixedInt.fromLarge n))
    val inputN =
        fn (s, n) => inputN(s, FixedInt.fromLarge n)
    structure StreamIO =
    struct
        open StreamIO
        val canInput =
            fn (s, n) => Option.map FixedInt.toLarge (canInput(s, FixedInt.fromLarge n))
        val inputN =
            fn (s, n) => inputN(s, FixedInt.fromLarge n)
    end
end;

