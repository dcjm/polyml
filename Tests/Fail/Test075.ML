(* Record punning is disabled by default. *)

let
    val x = 3
    val z = 7
in
    {w = 2, x, y = 5, z}
end;
