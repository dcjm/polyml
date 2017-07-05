(*
    Title:      Install a pretty printer for the exn type
    Author:     David Matthews
    Copyright   David Matthews 2009

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
local
    open PolyML
    open RunCall
    (* Print exception packet. Run-time system exceptions have
       to be processed specially because the IDs don't have printer functions. *)
    fun exnPrint depth _ exn =
    let
        val (exnId, exnName, exnArg, _) = unsafeCast exn

        (* This parenthesis code is used in various places and probably should be centralised. *)
        fun parenthesise(s as PrettyBlock(_, _, _, [ _ ])) = s
        |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("(")::_ ))) = s
        |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("{")::_ ))) = s
        |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("[")::_ ))) = s
        |   parenthesise(s as PrettyBlock _) =
                PrettyBlock(3, true, [], [ PrettyString "(", s, PrettyString ")" ])
        |   parenthesise s = s (* String or Break *)

        fun nullaryException s = PrettyString s
        and parameterException(s, param) =
            PrettyBlock(1, false, [],
                [
                    PrettyString s,
                    PrettyBreak(1, 1),
                    parenthesise param
                ])
        (* Use prettyRepresentation because this correctly quotes the string. *)
        fun stringException(s, arg: string) =
            parameterException(s, PolyML.prettyRepresentation(arg, depth-1))
    in
        if run_call1 RuntimeCalls.POLY_SYS_is_short exnId
        then
            case exn of
                Conversion s => stringException(exnName, s)
            |   Fail s => stringException(exnName, s)
            |   Foreign s => stringException(exnName, s)
            |   Thread s => stringException(exnName, s)
            |   XWindows s => stringException(exnName, s)
            |   OS.SysErr param =>
                    parameterException("SysErr",
                        if depth <= 1 then PrettyString "..." else PolyML.prettyRepresentation(param, depth-1))
            |   _ => (* Anything else is nullary. *)
                    nullaryException exnName
        else 
            (
                (* Exceptions generated within ML contain a printer function. *)
                case !exnId of
                    NONE => nullaryException exnName
                |   SOME printFn => parameterException(exnName, printFn(exnArg, depth-1))
            )
    end
in
    val () = addPrettyPrinter exnPrint
end;

(* Print a ref.  Because refs can form circular structures we include a check for a loop here. *)
local
    open PolyML
    (* If we have an expression as the argument we parenthesise it unless it is
       a simple string, a tuple, a record or a list. *)
    fun parenthesise(s as PrettyBlock(_, _, _, [ _ ])) = s
    |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("(")::_ ))) = s
    |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("{")::_ ))) = s
    |   parenthesise(s as PrettyBlock(_, _, _, (PrettyString("[")::_ ))) = s
    |   parenthesise(s as PrettyBlock _) =
            PrettyBlock(3, true, [], [ PrettyString "(", s, PrettyString ")" ])
    |   parenthesise s = s (* String or Break *)

    val printLimit: word ref list Universal.tag = Universal.tag()

    fun print_ref depth doArg (r as ref x) =
        if depth <= 0
        then PrettyString "..."
        else
        let
            (* We keep a list in thread-local storage of refs we're currently printing.
               This is thread-local to avoid interference between different threads. *)
            val currentRefs =
                case Thread.Thread.getLocal printLimit of
                    NONE => []
                |   SOME limit => limit
            val thisRef: word ref = RunCall.unsafeCast r
        in
            if List.exists(fn x => x = thisRef) currentRefs
            then PrettyString "..." (* We've already seen this ref. *)
            else
            (
                (* Add this to the list. *)
                Thread.Thread.setLocal (printLimit, thisRef :: currentRefs);
                (* Print it and reset the list*)
                (PrettyBlock(3, false, [],
                    [ PrettyString "ref", PrettyBreak(1, 0), parenthesise(doArg(x, depth-1)) ]))
                    before (Thread.Thread.setLocal (printLimit, currentRefs))
            ) handle exn =>
                (
                    (* Reset the list if there's been an exception. *)
                    Thread.Thread.setLocal (printLimit, currentRefs);
                    raise exn
                )
        end

in
    val () = addPrettyPrinter print_ref
end;

