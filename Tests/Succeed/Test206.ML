(* Occurs checks and other tests for pathological cases of type functions. *)

type ('a, 'b) t = 'a;
type ('a, 'b, 'c) s = ('a, 'c) t;


fun f (x, y) =
let
    val g :'a -> ('a, 'b, 'c) s = fn x => x

in
    if x then y else g y
end;


type ('a, 'b) t = 'a;
type ('a, 'b, 'c) s = ('a, 'c) t;

fun f (x, y) =
let
    val g :'b -> ('a, 'b, 'c) s = fn x => raise Match

in
    if x then y else g y
end;

