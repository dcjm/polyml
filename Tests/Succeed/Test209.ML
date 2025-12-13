(* Counterpart of Fail/Test078.  This is valid because the type variable is global. *)
fun 'a f x = let val x = ref ([]: 'a list) in 0 end;

(* And this is valid because the type variable isn't used. *)
type 'a t = int;
fun f x = let val 'a x = ref ([]: 'a t list) in 0 end;
