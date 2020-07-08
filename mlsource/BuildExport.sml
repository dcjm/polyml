(*
    Copyright (c) 2006-10, 2015-16, 2020  David C. J. Matthews

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

(* Compiler the compiler and export it as a portable file.  The rest of the
   bootstrap process is then done when the portable file is imported. *)

PolyML.print_depth 0;

PolyML.make "mlsource/MLCompiler";

fun compileBasisAndExport() =
let
    (* This function is the root of the "pre-built" compilers.  It compiles in
       the basis library and exports the compiler along with the basis library
       as an operating-system specific object file. *)
    val globalTable : Make.gEnv = Make.makeGEnv ()
    val testIntIsArb = List.exists (fn "--intIsIntInf" => true | _ => false) (CommandLine.arguments())
    val testArbUseEmulation = List.exists (fn "--useCompactIntInf" => true | _ => false) (CommandLine.arguments())
    val () = Initialise.initGlobalEnv{globalTable=globalTable,
                    intIsArbitraryPrecision=testIntIsArb, useEmulationForArbitrary=testArbUseEmulation}
in
    Make.shellProc globalTable()
end;

PolyML.shareCommonData compileBasisAndExport;

PolyML.exportPortable("polytemp",
    fn () => (compileBasisAndExport (); OS.Process.exit OS.Process.success) handle _ => OS.Process.exit OS.Process.failure);
