(* Some tests on C memory operations. *)

fun check f a b =
    if f a = b then () else raise Fail "Mismatch";
(* 64-bit operations are not implemented in 32-bit mode. *)
fun checkEx f a b =
    (if f a = b then () else raise Fail "Mismatch") handle Foreign.Foreign _ => ();

open Foreign.Memory;
val m = malloc 0w32;
val e = ++(m, 0w32);
val r = ref 0w0;
set32(m, 0w0, 0wx12343421);
set32(m, 0w1, 0w0);
set32(m, 0w2, 0w0);
set32(m, 0w3, 0w0);
set32(m, 0w4, 0w0);
set32(m, 0w5, 0w0);
set32(m, 0w6, 0wxabccccab);
set32(m, 0w7, 0wx12211221);

(* These are often handled differently depending on whether the offset is 
   a compile-time constant. *)
check get32 (m, 0w0) 0wx12343421;
check get32 (m, !r) 0wx12343421;

check get32 (e, ~ 0w8) 0wx12343421;
check get32 (e, !r - 0w8) 0wx12343421;

check get32 (m, 0w6) 0wxabccccab;
check get32 (e, ~ 0w2) 0wxabccccab;
r := 0w6;
check get32 (m, !r) 0wxabccccab;
check get32 (e, !r - 0w8) 0wxabccccab;

check get16 (m, 0w14) 0wx1221;
check get16 (e, ~ 0w2) 0wx1221;
r := 0w14;
check get16 (m, !r) 0wx1221;
check get16 (e, !r - 0w16) 0wx1221;

checkEx get64 (m, 0w1) 0w0;
checkEx get64 (e, ~ 0w3) 0w0;
r := 0w1;
checkEx get64 (m, !r) 0w0;
checkEx get64 (e, !r - 0w4) 0w0;

check get8 (m, 0w24) 0wxab;
check get8 (e, ~ 0w8) 0wxab;
r := 0w24;
check get8 (m, !r) 0wxab;
check get8 (e, !r - 0w32) 0wxab;

set8(m, !r, 0wx88);
check get8 (m, 0w24) 0wx88;
set8(e, !r - 0w32, 0wx77);
check get8 (e, ~ 0w8) 0wx77;

set64(m, 0w1, 0wx123456) handle Foreign.Foreign _ => ();
checkEx get64 (m, 0w1) 0wx123456;

set16(m, 0w4, 0wxffee);
check get16 (m, 0w4) 0wxffee;

setFloat(m, 0w2, 1.0);
if Real.==(getFloat(m, 0w2), 1.0) then () else raise Fail "Mismatch";
if Real.==(getFloat(e, ~ 0w6), 1.0) then () else raise Fail "Mismatch";
r := 0w2;
if Real.==(getFloat(m, !r), 1.0) then () else raise Fail "Mismatch";

setDouble(m, 0w2, 2.0);
if Real.==(getDouble(m, 0w2), 2.0) then () else raise Fail "Mismatch";
if Real.==(getDouble(e, ~ 0w2), 2.0) then () else raise Fail "Mismatch";
if Real.==(getDouble(m, !r), 2.0) then () else raise Fail "Mismatch";


free m;
