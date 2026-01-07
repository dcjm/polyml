(* Test for equality of local withtype. *)
fun f x =
let
    datatype t = A | B of s
    withtype s = t
    
    fun f (x: s, y) = x = y
in
    0
end;
