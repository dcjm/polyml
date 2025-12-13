(* Explicit type variables at the current level are not allowed.
   c.f. Succeed/Test209. *)
fun f x = let val 'a x = ref ([]: 'a list) in 0 end;
