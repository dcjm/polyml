(* This failed an assertion on the PPC architecture. *)
fun f x = Word.andb(x, 0wx108000);
PolyML.fullGC ();
