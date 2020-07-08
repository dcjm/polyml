(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

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

(* Script to rebuild the compiler. *)
(* This is used only during testing.  The normal build process uses
   BuildExport to export a newly compiled compiler and exportPoly
   to compile the basis and produce an object file. *)

PolyML.print_depth 0;

PolyML.make "mlsource/MLCompiler";

(* Create the initial basis *)
val globalTable : Make.gEnv = Make.makeGEnv ();
val () = Initialise.initGlobalEnv {globalTable=globalTable, intIsArbitraryPrecision=false, useEmulationForArbitrary=false};

(* Compile the basis in the new compiler. *)
MLCompiler.useIntoEnv globalTable [] "mlsource/BuildBasis.sml";

(* Use useString to start the new shell because it allows this
   to be pasted into a window as a single item.  Otherwise the
   line to start the new shell will be in the buffer of the old
   TextIO.stdIn not the one we've just built. *)
MLCompiler.useStringIntoEnv globalTable "PolyML.rootFunction();";

