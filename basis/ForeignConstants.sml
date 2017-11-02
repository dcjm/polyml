(*
    Title:      Foreign Function Interface: constants
    Author:     David Matthews
    Copyright   David Matthews 2015, 2016-17

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
   available as constants for the Foreign structure. *)
structure ForeignConstants =
struct
    local
        val ffiGeneralCall = RunCall.rtsCallFull2 "PolyFFIGeneral"
        fun ffiGeneral(code: int, arg: 'a): 'b = RunCall.unsafeCast(ffiGeneralCall(RunCall.unsafeCast(code, arg)))
    in
    local
        fun getSizeAndAlign (n: int) =
        let
            val ffiType = ffiGeneral (52, n)
            val (size: word, align: word, _, _) = (* Just get the first two fields. *)
                ffiGeneral (53, ffiType)
        in
            {size=size, align=align}
        end
    in
        val saVoid      = getSizeAndAlign 0
        and saUint8     = getSizeAndAlign 1
        and saSint8     = getSizeAndAlign 2
        and saUint16    = getSizeAndAlign 3
        and saSint16    = getSizeAndAlign 4
        and saUint32    = getSizeAndAlign 5
        and saSint32    = getSizeAndAlign 6
        and saUint64    = getSizeAndAlign 7
        and saSint64    = getSizeAndAlign 8
        and saFloat     = getSizeAndAlign 9
        and saDouble    = getSizeAndAlign 10
        and saPointer   = getSizeAndAlign 11
        and saUChar     = getSizeAndAlign 12
        and saSChar     = getSizeAndAlign 13
        and saUShort    = getSizeAndAlign 14
        and saSShort    = getSizeAndAlign 15
        and saUint      = getSizeAndAlign 16
        and saSint      = getSizeAndAlign 17
        and saUlong     = getSizeAndAlign 18
        and saSlong     = getSizeAndAlign 19
    end
    
    val bigEndian : bool = LibrarySupport.bigEndian
    and wordSize : word = RunCall.bytesPerWord
    
    (* Minimum argument size. *)
    val ffiMinArgSize: Word.word = ffiGeneral (51, 15)
    end

end;
