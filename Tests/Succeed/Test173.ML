(* Problem with LargeWord.  Derived from Isabelle generated code. *)


PolyML.Compiler.maxInlineSize := 1;

fun set_bit x n b =
  let val mask = LargeWord.<< (0wx1, Word.fromLargeInt (IntInf.toLarge n))
  in if b then LargeWord.orb (x, mask)
     else LargeWord.andb (x, LargeWord.notb mask)
  end;


set_bit 0w4 3 true;
