(* This gave a Subscript exception in the simplifier. *)

datatype t = A of int * string | B of int * int * int;

fun g s =
let
    val q = [A (1, s)]
in
    case q of
        B(i, j, k) :: _ => k (* This wants a missing field. *)
    |   _ => raise Fail "bad"
end;
