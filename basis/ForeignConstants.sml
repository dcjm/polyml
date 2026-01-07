(*
    Title:      Foreign Function Interface: constants
    Author:     David Matthews
    Copyright   David Matthews 2015, 2016-17, 2019

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

(* This is defined separately so that the values are computed and
   available as compile-time constants for the Foreign structure. *)
structure ForeignConstants =
struct
    (* Assume that the alignment requirements for these types are the
       same as the size.  *)
    local
        val sizeFloat: word     = RunCall.rtsCallFast1 "PolySizeFloat" ()
        and sizeDouble: word    = RunCall.rtsCallFast1 "PolySizeDouble" ()
        and sizeShort: word     = RunCall.rtsCallFast1 "PolySizeShort" ()
        and sizeInt: word       = RunCall.rtsCallFast1 "PolySizeInt" ()
        and sizeLong: word      = RunCall.rtsCallFast1 "PolySizeLong" ()
        and sizeLonglong: word  = RunCall.rtsCallFast1 "PolySizeLonglong" ()
        and sizeSsize: word     = RunCall.rtsCallFast1 "PolySizeSsize" ()
        and sizeSize: word      = RunCall.rtsCallFast1 "PolySizeSize" ()
        and sizePtrdiff: word   = RunCall.rtsCallFast1 "PolySizePtrdiff" ()
        and sizeIntptr: word    = RunCall.rtsCallFast1 "PolySizeIntptr" ()
        and sizeUintptr: word   = RunCall.rtsCallFast1 "PolySizeUintptr" ()
    in
        val saFloat     = {size=sizeFloat, align=sizeFloat}
        and saDouble    = {size=sizeDouble, align=sizeDouble}
        and saShort     = {size=sizeShort, align=sizeShort}
        and saInt       = {size=sizeInt, align=sizeInt}
        and saLong      = {size=sizeLong, align=sizeLong}
        and saLonglong  = {size=sizeLonglong, align=sizeLonglong}
        and saSsize     = {size=sizeSsize, align=sizeSsize}
        and saSize      = {size=sizeSize, align=sizeSize}
        and saPtrdiff   = {size=sizePtrdiff, align=sizePtrdiff}
        and saIntptr    = {size=sizeIntptr, align=sizeIntptr}
        and saUintptr   = {size=sizeUintptr, align=sizeUintptr}
    end

    val bigEndian : bool = LibrarySupport.bigEndian
    and wordSize : word = RunCall.bytesPerWord
    and sysWordSize: word = LibrarySupport.sysWordSize
end;
