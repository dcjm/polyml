(*
    Title:      Standard Basis Library: Date Signature
    Copyright   David Matthews 2000, 2016

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

signature DATE =
sig
     datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
     datatype month = Jan | Feb | Mar | Apr | May | Jun | 
                      Jul | Aug | Sep | Oct | Nov | Dec
     type date
     exception Date
     val date : {
                    year : int,
                    month : month,
                    day : int,
                    hour : int,
                    minute : int,
                    second : int,
                    offset : Time.time option
                  } -> date
     val year    : date -> int
     val month   : date -> month
     val day     : date -> int
     val hour    : date -> int
     val minute  : date -> int
     val second  : date -> int
     val weekDay : date -> weekday
     val yearDay : date -> int
     val offset  : date -> Time.time option
     val isDst   : date -> bool option

     val localOffset : unit -> Time.time

     val fromTimeLocal : Time.time -> date
     val fromTimeUniv  : Time.time -> date

     val toTime : date -> Time.time

     val toString : date -> string
     val fmt      : string -> date -> string

     val fromString : string -> date option
     val scan       : (char, 'a) StringCvt.reader
                        -> 'a -> (date * 'a) option

     val compare : date * date -> General.order

end;
