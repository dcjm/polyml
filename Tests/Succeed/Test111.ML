(* Bug in X86 code-generator with "containers" that are not
   actually used.  The container was being popped from the stack
   before the function was called resulting in a segfault. *)

PolyML.Compiler.maxInlineSize := 1;
fun f (x: int) = ((x, x, x, x, x, x));
fun h s = s+s;
fun g y = let val a = ref 0 val _ = h y val _ = f y in ! a + y end;
g 0;
