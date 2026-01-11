(* This is incorrect but led to an infinite recursion. *)

type ('a, 'b) t = 'a;
type ('a, 'b, 'c) s = ('a, 'c) t;

fun f (x, y) =
let
    (* Equivalent to 'b -> 'a which is too general. *)
    val g :'b -> ('a, 'b, 'c) s = fn x => x

in
    if x then y else g y
end;


