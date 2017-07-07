(*
    Title:      Standard Basis Library: String Signatures
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

(* Temporary definition for Char.scan. *)
structure StringCvt = struct type  ('a, 'b) reader = 'b -> ('a * 'b) option end;

signature CHAR =
sig
    eqtype  char
    eqtype  string
    
    val minChar : char
    val maxChar : char
    val maxOrd : int
    
    val ord : char -> int
    val chr : int -> char
    val succ : char -> char
    val pred : char -> char
    
    val < : (char * char) -> bool
    val <= : (char * char) -> bool
    val > : (char * char) -> bool
    val >= : (char * char) -> bool

    val compare : (char * char) -> General.order

    val contains : string -> char -> bool
    val notContains : string -> char -> bool

    val toLower : char -> char
    val toUpper : char -> char
    val isAlpha : char -> bool
    val isAlphaNum : char -> bool
    val isAscii : char -> bool
    val isCntrl : char -> bool
    val isDigit : char -> bool
    val isGraph : char -> bool
    val isHexDigit : char -> bool
    val isLower : char -> bool
    val isPrint : char -> bool
    val isSpace : char -> bool
    val isPunct : char -> bool
    val isUpper : char -> bool

    val fromString : String.string -> char option
    (* The argument to scan should be the global *)
    val scan : (Char.char, 'a) StringCvt.reader -> (char, 'a) StringCvt.reader
    val toString : char -> String.string
    val fromCString : String.string -> char option
    val toCString : char -> String.string
end;


signature STRING =
sig
    eqtype  string
    eqtype char

    val maxSize : int
    val size : string -> int
    val sub : (string * int) -> char
    val extract : (string * int * int option) -> string
    val substring : (string * int * int) -> string
    val concat : string list -> string
    val concatWith: string -> string list -> string
    val ^ : (string * string) -> string
    val str : char -> string
    val implode : char list -> string
    val explode : string -> char list
    val map : (char -> char) -> string -> string
    val translate : (char -> string) -> string -> string
    val tokens : (char -> bool) -> string -> string list
    val fields : (char -> bool) -> string -> string list
    val isPrefix : string -> string -> bool
    val isSubstring : string -> string -> bool
    val isSuffix : string -> string -> bool


    val compare : (string * string) -> General.order
    val collate : ((char * char) -> General.order) ->
                     (string * string) -> General.order
    val < : (string * string) -> bool
    val <= : (string * string) -> bool
    val > : (string * string) -> bool
    val >= : (string * string) -> bool

    val toString : string -> String.string
    val scan: (Char.char, 'a) StringCvt.reader -> (string, 'a) StringCvt.reader
    val fromString : String.string -> string option
    val toCString : string -> String.string
    val fromCString : String.string -> string option
end;

signature SUBSTRING =
sig
    type  substring
    eqtype char
    eqtype string
    val size : substring -> int
    val base : substring -> (string * int * int)
    val isEmpty : substring -> bool

    val sub : (substring * int) -> char
    val getc : substring -> (char * substring) option
    val first : substring -> char option
    
    val extract : (string * int * int option) -> substring
    val substring : (string * int * int) -> substring
    val slice : (substring * int * int option) -> substring
    val full: string -> substring
    val string : substring -> string
    
    val concat: substring list ->string
    val concatWith: string -> substring list ->string

    val explode : substring -> char list
    val translate : (char -> string) -> substring -> string
    val app : (char -> unit) -> substring -> unit
    val foldl : ((char * 'a) -> 'a) -> 'a -> substring -> 'a
    val foldr : ((char * 'a) -> 'a) -> 'a -> substring -> 'a
    val tokens : (char -> bool) -> substring -> substring list
    val fields : (char -> bool) -> substring -> substring list
    val isPrefix: string -> substring -> bool
    val isSubstring: string -> substring -> bool
    val isSuffix: string -> substring -> bool

    val compare : (substring * substring) -> General.order
    val collate : ((char * char) -> General.order) ->
                     (substring * substring) -> General.order

    val triml : int -> substring -> substring
    val trimr : int -> substring -> substring
    val splitl : (char -> bool) -> substring -> (substring * substring)
    val splitr : (char -> bool) -> substring -> (substring * substring)
    val splitAt : (substring * int) -> (substring * substring)
    val dropl : (char -> bool) -> substring -> substring
    val dropr : (char -> bool) -> substring -> substring
    val takel : (char -> bool) -> substring -> substring
    val taker : (char -> bool) -> substring -> substring
    val position : string -> substring -> (substring * substring)
    val span : (substring * substring) -> substring
end;

signature STRING_CVT =
sig
    datatype radix = BIN | OCT | DEC | HEX

    datatype realfmt
      = SCI of int option
      | FIX of int option
      | GEN of int option
      | EXACT

    type  ('a, 'b) reader = 'b -> ('a * 'b) option    

    val padLeft : char -> int -> string -> string
    val padRight : char -> int -> string -> string
    val splitl : (char -> bool) -> (char, 'a) reader ->'a -> (string * 'a)
    val takel : (char -> bool) -> (char, 'a) reader ->'a -> string
    val dropl : (char -> bool) -> (char, 'a) reader ->'a -> 'a
    val skipWS : (char, 'a) reader -> 'a -> 'a
    type  cs
    val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option

end;
