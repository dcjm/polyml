(*
    Title:      Sixth stage bootstrap from interpreted code.
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

(* Compile the native code compiler under the native code compiler. *)
print "******Bootstrap stage 6 of 7******\n";

PolyML.print_depth 0;
PolyML.make "mlsource/MLCompiler";

(* Create the initial basis *)
val globalTable : Make.gEnv = Make.makeGEnv ();
val testIntIsArb = List.exists (fn "--intIsIntInf" => true | _ => false) (CommandLine.arguments())
val testArbUseEmulation = List.exists (fn "--useCompactIntInf" => true | _ => false) (CommandLine.arguments())
val () = Initialise.initGlobalEnv {globalTable=globalTable, intIsArbitraryPrecision=testIntIsArb, useEmulationForArbitrary=testArbUseEmulation};

(* Compile the basis in the new compiler. Export the result. *)
MLCompiler.useIntoEnv globalTable [] "bootstrap/Stage7.sml";
