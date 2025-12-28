(* Blow up of type variables.  Pathological example.
   The previous implementation was quadratic in the number of type variables. *)

fun a _ = (raise Bind, raise Bind, raise Bind, raise Bind, raise Bind, raise Bind, raise Bind, raise Bind, raise Bind);
fun b _ = (a, a, a, a, a, a, a, a, a, a, a);
fun c _ = (b, b, b, b, b, b, b, b, b, b, b);
fun d _ = (c, c, c, c, c, c, c, c, c, c, c);
fun e _ = (d, d, d, d, d, d, d, d, d, d, d);
