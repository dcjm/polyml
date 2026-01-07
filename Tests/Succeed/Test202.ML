(* Conversion bug from Word types to Int where Word.toInt fails to raise
   overflow when the size of the word is double the maximum integer
   representable. Github issue #200. *)

case Int.precision of
    NONE => () (* bug not present in this case *)
  | SOME p =>
    let
        val maxInt = Option.getOpt(Int.maxInt, 0)
        val large = if maxInt = 0
                    then raise Fail "Unexpected maxInt"
                    else ((LargeInt.fromInt maxInt) + 1) * 2
        val _ = (LargeWord.toInt o LargeWord.fromLargeInt) large
    in
        raise Fail "Bad"
    end
    handle Overflow => () (* overflow expected *)
         | exn => raise exn
