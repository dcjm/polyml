(*
    Title:      Standard Basis Library: Bool Structure
    Author:     David Matthews
    Copyright   David Matthews 1999, 2005

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(* G&R status: Minor change to signature.  Done. *)
signature BOOL =
  sig
    datatype bool = datatype bool
    val not : bool -> bool
    val fromString : string -> bool option
    val scan : (char, 'a) StringCvt.reader -> (bool, 'a) StringCvt.reader
    val toString : bool -> string
  end;

structure Bool : BOOL =
    struct
    open RuntimeCalls; (* for POLY_SYS and EXC numbers *)
    datatype bool = datatype bool

    val not: bool -> bool = RunCall.run_call1 POLY_SYS_not_bool;

    local
    val explode_true = Text.String.explode "true"
    and explode_false = Text.String.explode "false"
    in
    fun scan (getc: (char, 'a) StringCvt.reader) (str: 'a) : (bool * 'a) option =
        let
        (* Skip leading white space. *)
        val strm = StringCvt.skipWS getc str
        (* Test for a match between a reader and a list of lower case chars. *)
        fun matchNC _    strm [] = (strm, true )(* Reached end of list - succeeded *)
          | matchNC getc strm (ch::rest) =
                case getc strm of
                    NONE => (strm, false) (* Couldn't read it - fail. *)
                  | SOME(ch', strm') =>
                      if ch = Char.toLower ch' (* N.B. ch is already lower case. *)
                      then matchNC getc strm' rest
                      else (strm', false)
        in
            (* If it matches "true" or "false" we have a valid match,
               otherwise return NONE. *)
            case matchNC getc strm explode_true of
                (strm', true) => SOME(true, strm')
              | _ =>
                (
                case matchNC getc strm explode_false of
                    (strm', true) => SOME(false, strm')
                  | _ => NONE
                )
        end
    end
    
    (* Convert from a string. *)
    (* TODO: More efficient conversion? *)
    val fromString = StringCvt.scanString scan
    
    (* Convert to a string. *)
    fun toString true = "true"
      | toString false = "false"

    end;
