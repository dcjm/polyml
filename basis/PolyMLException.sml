(*
    Title:      PolyML.Exception structure
    Author:     David C. J. Matthews
    Copyright (c) 2015

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

(* Add PolyML.Exception to the PolyML structure. *)
(* N.B. The effect of compiling this file is to extend the PolyML structure. *)

structure PolyML =
struct
    open PolyML

    local
        open RuntimeCalls
        (* This datatype is used in VALUE_OPS and FinalPolyML to define the format of a
           location in an exception packet.  It includes the possibility that the location
                   information may be missing. *)
        datatype RuntimeLocation =
            NoLocation
        |   SomeLocation of
                (* file: *) string * 
                (*startLine:*) FixedInt.int *  (*startPosition:*) FixedInt.int *
                (*endLine:*) FixedInt.int * (*endPosition:*) FixedInt.int
    in
        structure Exception =
        struct
            (* Backwards compatibility. *)
            fun traceException(f: unit->'a, _: string list * exn -> 'a): 'a = f()

            fun exceptionLocation(exn: exn): location option =
                case RunCall.loadWordFromImmutable(exn, 0w3) of
                    NoLocation => NONE
                |   SomeLocation(file, startLine, startPosition, endLine, endPosition) =>
                        SOME { file=file, startLine=startLine, startPosition=startPosition,
                               endLine=endLine, endPosition=endPosition }

            local
                (* If we use ML "raise" to raise an exception the location will be the
                   location of the raise.  If we have a handler that reraises an exception
                   it is often better to preserve the original location.  We need to add
                   a function that raises an exception without adding its own location. *)
                open PolyML.CodeTree
                val functionCode = mkFunction (mkRaise(mkLoadArgument 0), 1, "raiseFn", [], 0)
                (* N.B. genCode is redefined in FinalPolyML without the options argument. *)
                val compiledCode = genCode(functionCode, [], 0) ()
                val raiseFn = case evalue compiledCode of SOME c => c | NONE => raise Bind
            in
                (* Raise an exception using a given location rather than the value in the packet. *)
                fun raiseWithLocation(ex: exn, {file, startLine, startPosition, endLine, endPosition}: location) =
                let
                    open RunCall
                    fun getEntry n = RunCall.loadWordFromImmutable(ex, n)
                    val packet =
                        (getEntry 0w0, getEntry 0w1, getEntry 0w2,
                            SomeLocation(file, startLine, startPosition, endLine, endPosition))
                in
                    RunCall.unsafeCast raiseFn packet
                end
            end

            (* Re-raise an exception that has been handled preserving the location. *)
            fun reraise exn =
                case exceptionLocation exn of
                    NONE => raise exn
                |   SOME location => raiseWithLocation (exn, location)
        end
    end

    (* For backwards compatibility include these in the PolyML structure. *)
    val exceptionLocation = Exception.exceptionLocation
    and raiseWithLocation = Exception.raiseWithLocation
end;
