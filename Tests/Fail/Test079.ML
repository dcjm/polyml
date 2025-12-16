(* Can't set the scope of an explicit type variable wider. *)

fun f x =
let
    fun 'a g (z: 'a) = x z
in
    0
end;
