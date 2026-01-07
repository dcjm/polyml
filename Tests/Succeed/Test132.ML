(* This caused a segfault in 5.5 and earlier due to an error in the optimiser.
   These examples are a special case because they could result in infinite
   inline expansion.  *)

datatype 'a ft = FT of 'a ft -> 'a;

fun Y f =
    (
        fn (FT x) =>
            (f
                (fn a => x (FT x) a)
            )
    )
    (FT
        (
            fn (FT x) =>
                (f
                    (fn a => x (FT x) a)
                )
        )
    );

fun Fact f n = if n = 0 then 1 else n * f(n-1);

val fact = Y Fact;
fact 4; (* This crashed. *)

(* A slightly different example.  This is infinitely recursive. *)
fun Z() =
let
    val f = fn(FT g) => g (FT g)
in
    f (FT f)
end;

(* A further example.  This time because the first function is a separate top-level
   declaration it is actually a constant but with inline code.  See issue #216. *)
datatype loop = L of loop -> loop;
val loop = fn (l as L f) => f l;
val go = fn () => loop (L loop);
