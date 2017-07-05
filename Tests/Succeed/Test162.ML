(* Negative values require special treatment in Foreign.sml *)

val c = Foreign.breakConversion Foreign.cInt16;

fun checkValue v =
let
    val m = Foreign.Memory.malloc (#size (#ctype c));
    val _ = #store c (m, v);
    val result = #load c m before Foreign.Memory.free m
in
    if result <> v then raise Fail "Incorrect" else ()
end;

checkValue ~1;
checkValue 1;
checkValue 0;
checkValue 32767;
checkValue ~32768;

