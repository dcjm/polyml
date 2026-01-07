(*
    Title:      Second stage bootstrap from interpreted code.
    Copyright   David Matthews 2020, 2026

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

(* Compile the compiler with enter-interpreter instructions. *)
print "******Bootstrap stage 2 of 7******\n";

PolyML.print_depth 0;
PolyML.make "mlsource/MLCompiler";

(* Create the initial basis *)
val globalTable : Make.gEnv = Make.makeGEnv ();
val () = Initialise.initGlobalEnv {globalTable=globalTable, intIsArbitraryPrecision=false, useEmulationForArbitrary=false};

(* PolyEndBootstrapMode sets the interpreter so that any function or return must
   have an enter-int instruction otherwise it is treated as machine code.
   The interpreted code we started from didn't have enter-ints so the
   function passed to PolyEndBootstrapMode must never return. *)
local
    fun thirdStage() =
    (
        MLCompiler.useIntoEnv globalTable [] "bootstrap/Stage3.sml"
            handle _ => ();
        OS.Process.exit OS.Process.failure
    )
in
    val () = RunCall.rtsCallFull1 "PolyEndBootstrapMode" thirdStage
end;
