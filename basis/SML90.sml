(*
    Title:      Standard Basis Library: SML90 Signature and Structure
    Author:     David Matthews
    Copyright   David Matthews 1999, 2016

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

(* This is intended for backwards compatibility only.  It should probably be withdrawn. *)

signature SML90 =
sig
    exception Abs
    exception Quot
    exception Prod
    exception Neg
    exception Sum
    exception Diff
    exception Floor
    exception Exp
    exception Sqrt
    exception Ln
    exception Ord
    exception Mod
    exception Io of string
    exception Interrupt
    val sqrt : real -> real
    val exp : real -> real
    val ln : real -> real
    val sin : real -> real
    val cos : real -> real
    val arctan : real -> real
    val ord : string -> int
    val chr : int -> string
    val explode : string -> string list
    val implode : string list -> string
    type instream
    type outstream
    val std_in  : instream
    val std_out : outstream
    val open_in : string -> instream
    val input : instream * int -> string
    val lookahead : instream -> string
    val close_in : instream -> unit
    val end_of_stream : instream -> bool
    val open_out : string -> outstream
    val output : outstream * string -> unit
    val close_out : outstream -> unit
    end;

structure SML90 :> SML90 =
struct
    exception Abs = Overflow and Quot = Overflow and Prod = Overflow
          and Neg = Overflow and Sum = Overflow and Diff = Overflow
          and Floor = Overflow and Exp = Overflow and Sqrt = Overflow
          and Ln = Overflow and Ord = Overflow and Mod = Div
          and Interrupt = RunCall.Interrupt

    exception Io of string

    fun sqrt x = if x < 0.0 then raise Sqrt else Real.Math.sqrt x

    fun exp x = let val r = Real.Math.exp x in if Real.isFinite r then r else raise Exp end

    fun ln x = if x < 0.0 then raise Ln else Real.Math.ln x

    val sin = Real.Math.sin and cos = Real.Math.cos and arctan = Real.Math.atan

    fun ord "" = raise Ord | ord s = Char.ord(String.sub(s, 0))

    fun chr i = str(Char.chr i)
    fun explode s = map String.str (String.explode s) 
    val implode = String.concat

    type instream = TextIO.instream and outstream = TextIO.outstream
    val std_in  : instream = TextIO.stdIn and std_out : outstream = TextIO.stdOut

    fun open_in s = TextIO.openIn s handle IO.Io _ => raise Io "Cannot open"
    and open_out s = TextIO.openOut s handle IO.Io _ => raise Io "Cannot open"

    fun input(str, i) = TextIO.inputN(str, i) handle IO.Io _ => raise Io "Cannot read"

    fun lookahead strm =
        case TextIO.lookahead strm of
            NONE => ""
        |   SOME ch => str ch

    val close_in : instream -> unit = TextIO.closeIn
    and close_out : outstream -> unit = TextIO.closeOut

    fun output(str, s) = TextIO.output(str, s) handle IO.Io _ => raise Io "Cannot output"

    val end_of_stream = TextIO.endOfStream
end;
