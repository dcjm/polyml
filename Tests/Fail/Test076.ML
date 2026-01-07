(* Record punning requires the variable to be defined. *)

PolyML.Compiler.languageExtensions := true;

let
    val x = 3
in
    {w = 2, x, y = 5, z} (* Variable `z` is missing. *)
end;
