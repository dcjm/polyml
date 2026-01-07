(* More assorted tests from updating the basis library. *)

fun verify true = ()
|   verify false = raise Fail "wrong";

structure S = PrimIO(
    structure Vector = CharVector
    and Array = CharArray
    and VectorSlice = CharVectorSlice
    and ArraySlice = CharArraySlice
    val someElem = #" "
    type pos = int
    val compare = Int.compare);

val S.RD r = S.openVector "abcdefghijk";
verify(valOf(#readVec r) 2 = "ab");
verify(valOf(#readVec r) 2 = "cd");
#close r ();
(valOf(#readVec r) 2; raise Fail "wrong") handle IO.ClosedStream => ();
val S.RD r = S.openVector "a";
verify(valOf(#readVec r) 2 = "a");
verify(valOf(#readVec r) 2 = "");

Timer.checkCPUTimes(Timer.totalCPUTimer());

fun timeVerify(SOME(t, subs), tMatch, str) =
    if t = Time.fromMilliseconds tMatch andalso Substring.string subs = str
    then ()
    else raise Fail "wrong"
|   timeVerify _ = raise Fail "wrong";

timeVerify(Time.scan Substring.getc (Substring.full "  0.00X"), 0, "X");
timeVerify(Time.scan Substring.getc (Substring.full "  1.23X"), 1230, "X");
timeVerify(Time.scan Substring.getc (Substring.full "  0.001X"), 1, "X");
timeVerify(Time.scan Substring.getc (Substring.full "  10.X"), 10000, ".X"); (* Valid. The decimal point is not part of the number. *)
timeVerify(Time.scan Substring.getc (Substring.full "  0 .X"), 0, " .X");
timeVerify(Time.scan Substring.getc (Substring.full "  .12X"), 120, "X");
verify(let val SOME x = Time.fromString "  0.001X" in Time.toNanoseconds x end = 1000000);
verify(let val SOME x = Time.fromString "  0.000001X" in Time.toNanoseconds x end = 1000);
verify(abs(let val SOME x = Time.fromString "  0.000000001X" in Time.toNanoseconds x end) <= 1); (* May be below the resolution. *)


verify(List.collate Int.compare ([1,2,3], [1,2,3]) = EQUAL);
verify(List.collate Int.compare ([1,2], [1,2,3]) = LESS);
verify(List.collate Int.compare ([2,2], [1,2,3]) = GREATER);

verify(String.concatWith "a" ["X", "Y", "Z"] = "XaYaZ");
verify(String.concatWith "a" [] = "");

(* These examples are given in G&R 2004. *)
verify(String.fromString "\\q" = NONE);
verify(String.fromString "a\^D" = SOME "a");
verify(String.fromString "a\\ \\\\q" = SOME "a");
verify(String.fromString "\\ \\" = SOME "");
verify(String.fromString "" = SOME "");
verify(String.fromString "\\ \\\^D" = SOME "");
verify(String.fromString "\\ a" = NONE);

verify(Char.fromString "\\q" = NONE);
verify(Char.fromString "a\^D" = SOME #"a");
verify(Char.fromString "a\\ \\\\q" = SOME #"a");
verify(Char.fromString "\\ \\" = NONE);
verify(Char.fromString "" = NONE);
verify(Char.fromString "\\ \\\^D" = NONE);
verify(Char.fromString "\\ a" = NONE);

verify(String.fromString("a\\    \\bc") = SOME "abc");
verify(Char.fromString("\\    \\bc") = SOME #"b");

verify(Substring.isPrefix "abc" (Substring.substring("XXXabcdYYY", 3, 4)));
verify(not(Substring.isPrefix "abcd" (Substring.substring("XXXabcdYYY", 3, 3))));
verify(Substring.isPrefix "abc" (Substring.substring("XXXabcdYYY", 3, 3)));
verify(Substring.isPrefix "" (Substring.substring("XXXabcdYYY", 3, 3)));
verify(Substring.isPrefix "" (Substring.substring("XXXabcdYYY", 3, 0)));
verify(not(Substring.isPrefix "abc" (Substring.substring("XXXdabcYYY", 3, 4))));

verify(not(Substring.isSuffix "abc" (Substring.substring("XXXabcdYYY", 3, 4))));
verify(not(Substring.isSuffix "abcd" (Substring.substring("XXXabcdYYY", 3, 3))));
verify(Substring.isSuffix "abc" (Substring.substring("XXXabcdYYY", 3, 3)));
verify(Substring.isSuffix "" (Substring.substring("XXXabcdYYY", 3, 3)));
verify(Substring.isSuffix "" (Substring.substring("XXXabcdYYY", 3, 0)));
verify(Substring.isSuffix "abc" (Substring.substring("XXXdabcYYY", 3, 4)));

verify(Substring.isSubstring "abc" (Substring.substring("XXXabcdYYY", 3, 4)));
verify(not(Substring.isSubstring "abcd" (Substring.substring("XXXabcdYYY", 3, 3))));
verify(Substring.isSubstring "abc" (Substring.substring("XXXabcdYYY", 3, 3)));
verify(Substring.isSubstring "" (Substring.substring("XXXabcdYYY", 3, 3)));
verify(Substring.isSubstring "" (Substring.substring("XXXabcdYYY", 3, 0)));
verify(Substring.isSubstring "abc" (Substring.substring("XXXdabcYYY", 3, 4)));
verify(Substring.isSubstring "abc" (Substring.full "xyzabcdef"));
