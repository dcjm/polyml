(*
    Title:      Bootstrap code.
    Author:     David Matthews
    Copyright   David Matthews 2005-2008, 2017, 2020

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

val () = Bootstrap.print "******Bootstrap stage 7 of 7******\n"

(* Build the main basis library. *)
val () = Bootstrap.use "basis/build.sml";

(* We've now set up the new name space so everything has to be
   compiled into that rather than the old space. *)
    (* XWindows/Motif *)
val () =
let
    val xWindowsGeneralCall = RunCall.rtsCallFull1 "PolyXWindowsGeneral"
    val xcall: int*int->int*int = xWindowsGeneralCall
    (* See if the RTS supports the X GetTimeOfDay call. *)
    val isX = (xcall(30, 0); true) handle _ => false
in
   if isX
   then
   (
        PolyML.make "mlsource/extra/XWindows";
        PolyML.make "mlsource/extra/Motif"
   )
   else ()
end;

val () = PolyML.print_depth 10;

val () = print "******Writing object code******\n"

(* Write out the result as an export file. *)
local
    val args = CommandLine.arguments();
    (* If we have -o filename use that as the output name.
       N.B.  polyImport takes the first argument that is not recognised as
       an RTS argument and treats that as the file name so any -o must occur
       AFTER the import file. *)
    fun getFile [] = "polyexport" (* Default file name *)
      | getFile ("-o" :: outFile :: _) = outFile
      | getFile (_::tl) = getFile tl
    val fileName = getFile args
in
    val () = PolyML.shareCommonData PolyML.rootFunction;
    val () = PolyML.export(fileName, PolyML.rootFunction)
end;

(* And terminate. *)
OS.Process.exit OS.Process.success : unit;
