(* Test for shifts and log2 for both short and long format numbers. *)
fun test n =
    if n > 300 then ()
    else
    let
        val a = IntInf.<<(1, Word.fromInt n)
        val _ = IntInf.log2 a = n orelse raise Fail "Incorrect"
        val _ = IntInf.log2(a-1) = n-1 orelse raise Fail "Incorrect"
    in
        test (n+1)
    end;

test 1;
