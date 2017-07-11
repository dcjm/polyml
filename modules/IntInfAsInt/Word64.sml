(* Only built if LargeWord is 64-bits *)

val () = if LargeWord.wordSize = 64 then () else raise Fail "LargeWord.wordSize <> 64";

structure Word64 = LargeWord;
