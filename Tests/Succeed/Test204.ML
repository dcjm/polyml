(* Record punning can be enabled manually. *)

PolyML.Compiler.languageExtensions := true;

let
    val x = 3
    val z = 7
    val r = {w = 2, x, y = 5, z}
in
    #w r = 2 andalso #x r = x andalso #y r = 5 andalso #z r = z
end;
