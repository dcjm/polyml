(* Regression test for bug with caching X87 values. *)
fun f x = let val res = x+ 1.0  in if Real.isFinite res then res else raise Fail "bad" end;

f 3.0;
