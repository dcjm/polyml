(*
    Title:      Standard Basis Library: IEEEReal Structure.
    Author:     David Matthews
    Copyright   David Matthews 2000, 2005

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

(* G&R 2004 status: updated. *)

structure IEEEReal: IEEE_REAL =
struct
    exception Unordered
    
    datatype real_order = LESS | EQUAL | GREATER | UNORDERED
    
    datatype float_class
       = NAN
       | INF
       | ZERO
       | NORMAL
       | SUBNORMAL
       
    datatype rounding_mode
       = TO_NEAREST
       | TO_NEGINF
       | TO_POSINF
       | TO_ZERO

    (* This is used for newly added functions in the Standard Basis. *)
    fun callReal (code: int) args =
        Compat560.realGeneral (code,args);

    fun setRoundingMode (r: rounding_mode) : unit =
    let
        (* Although the datatype values are almost certainly integers it's
           much safer to map them to known values here. *)
        val rv =
            case r of
                TO_NEAREST => 0
              | TO_NEGINF => 1
              | TO_POSINF => 2
              | TO_ZERO => 3
    in
        callReal 9 rv
    end;
    
    fun getRoundingMode () =
        case callReal 10 () of
            0 => TO_NEAREST
          | 1 => TO_NEGINF
          | 2 => TO_POSINF
          | _ => TO_ZERO

    type decimal_approx =
        { class : float_class, sign : bool, digits : int list, exp : int }

    local
        fun dodigits [] = ""
          | dodigits (a::b) = Int.toString a ^ dodigits b
    in
        fun toString {class, sign=true, digits, exp} = (* Sign bit set *)
                "~" ^ toString {class=class, sign=false, digits=digits, exp=exp}
          | toString {class=NAN, ...} = "nan"
          | toString {class=INF, ...} = "inf"
          | toString {class=ZERO, ...} = "0.0"
          | toString {digits, exp, ...} = (* NORMAL or SUBNORMAL *)
                "0." ^ dodigits digits ^
                    (if exp = 0 then "" else "E"^(Int.toString exp))
    end
    
    
    fun 'a scan (getc: (char, 'a) StringCvt.reader) (src: 'a) : (decimal_approx *'a) option =
    let
        fun checkString (src, match: substring) = 
            (* Check the string matches and return the rest of the
               input if it does. *)
            case Substring.getc match of
                NONE => (* Finished *) SOME src (* Return rest *)
              | SOME (ch, rest) =>
                (case getc src of
                    NONE => NONE
                 |  SOME (ch', src') =>
                        if ch = Char.toUpper ch'
                        then checkString(src', rest)
                        else NONE
                )

        (* Return a list of digits. *)
        fun getdigits inp src =
            case getc src of
                NONE => (List.rev inp, src)
              | SOME(ch, src') =>
                    if ch >= #"0" andalso ch <= #"9"
                    then getdigits ((Char.ord ch - Char.ord #"0") :: inp) src'
                    else (List.rev inp, src)

        (* Return the signed exponent.  If this doesn't represent a
           valid integer return NONE since we shouldn't take off the E. 
           Int.scan accepts and removes leading space but we don't allow
           space here so return NONE if we find any. *)
        fun getExponent src =
            case getc src of
                NONE => NONE
              | SOME(ch, _) =>
                if Char.isSpace ch
                then NONE
                else Int.scan StringCvt.DEC getc src

        fun readNumber sign (src: 'a): (decimal_approx *'a) option =
            case getc src of
                NONE => NONE
              | SOME (ch, _) =>
                if ch >= #"0" andalso ch <= #"9" orelse ch = #"."
                then (* Digits or decimal. *)
                let
                    (* Get the digits before the decimal point (if any) *)
                    val (intPart, src2) = getdigits [] src
                    (* Get the digits after the decimal point.  If there is a decimal
                       point with no digits after it we don't swallow the dp. *)
                    val (decimals, src3) =
                        case getc src2 of
                          SOME (#".", src3a) =>
                                (
                                    case getdigits [] src3a of
                                        ([], _) => ([], src2)
                                    |   (digs, s) => (digs, s)
                                )
                         |  _=> ([], src2)
                    (* Get the exponent, returning zero if it doesn't match. *)
                    val (exponent, src4) =
                        case getc src3 of
                            NONE => (0, src3)
                         |  SOME (ch, src4a) =>
                            if ch = #"e" orelse ch = #"E"
                            then (
                                case getExponent src4a of
                                    NONE => (0, src3)
                                |   SOME x => x
                            )
                            else (0, src3)
                    (* Trim leading zeros from the part before the decimal and
                       trailing zeros from the part after. *)
                    fun trimLeadingZeros [] = []
                     |  trimLeadingZeros (0 :: l) = trimLeadingZeros l
                     |  trimLeadingZeros l = l
                    val trimTrailingZeros = List.rev o trimLeadingZeros o List.rev
                    val leading = trimLeadingZeros intPart
                    val trailing = trimTrailingZeros decimals
                in
                    (* If both the leading and trailing parts are empty the number is zero,
                       except that if there were no digits at all we have a malformed number. *)
                    case (intPart, decimals, leading, trailing) of
                        ([], [], _, _) => NONE
                      | (_, _, [], []) =>
                            SOME ({class=ZERO, sign=sign, digits=[], exp=0}, src4)
                      | _ =>
                            SOME ({class=NORMAL, sign=sign,
                              digits=List.@(leading, trailing),
                              exp=exponent + List.length leading}, src4)
                end
                else ( (* Could be INFINITY, INF or NAN.  Check INFINITY before INF. *)
                    case checkString (src, Substring.full "INFINITY") of
                        SOME src' => SOME ({class=INF, sign=sign, digits=[], exp=0}, src')
                      | NONE => (
                          case checkString (src, Substring.full "INF") of
                            SOME src' => SOME ({class=INF, sign=sign, digits=[], exp=0}, src')
                          | NONE => (
                              case checkString (src, Substring.full "NAN") of
                                SOME src' => SOME ({class=NAN, sign=sign, digits=[], exp=0}, src')
                              | NONE => NONE
                              )
                         )
                    )
    in
        case getc src of
            NONE => NONE
          | SOME(ch, src') =>
                if Char.isSpace ch (* Skip white space. *)
                then scan getc src' (* Recurse *)
                else if ch = #"+"
                then readNumber false src' (* Skip it *)
                else if ch = #"-" orelse ch = #"~"
                then readNumber true src' (* Skip it and ignore sign *)
                else readNumber false src
    end (* scan *)

    fun fromString (s: string): decimal_approx option = StringCvt.scanString scan s

end;
