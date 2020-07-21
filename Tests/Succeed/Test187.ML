(* This could segfault if GMP was enabled due to a bug in neg_longc. *)

val x = ~ (LargeInt.fromInt(FixedInt.toInt(valOf FixedInt.maxInt))) - 1;
PolyML.IntInf.gcd(x, 0);
