(* Checks for bug in NetHostDB. *)
fun checkIt s =
    if NetHostDB.toString (valOf(NetHostDB.fromString s)) = s
    then ()
    else raise Fail "checkIt";

checkIt "127.0.0.1";
checkIt "10.0.0.34";
checkIt "10.1.1.34";

