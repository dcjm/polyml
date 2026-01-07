(* Equality for vectors has to be dealt with specially now that
   we generate special equality functions for datatypes. *)
fun check f = if f() then () else raise Fail "Wrong";

check (fn () => Vector.fromList [1,2,3] = Vector.fromList [1,2,3]);
check (fn () => Vector.fromList [1,3,3] <> Vector.fromList [1,2,3]);

check (fn () => Vector.fromList [true, false] = Vector.fromList [true, false]);
check (fn () => Vector.fromList [true, false] <> Vector.fromList [true, true]);

check (fn () => Vector.fromList [#"a", #"b"] = Vector.fromList [#"a", #"b"]);
check (fn () => Vector.fromList [#"a", #"b"] <> Vector.fromList [#"b", #"b"]);

check (fn () => Vector.fromList [0w1,0w2,0w3] = Vector.fromList [0w1,0w2,0w3]);
check (fn () => Vector.fromList [0w1,0w3,0w3] <> Vector.fromList [0w1,0w2,0w3]);

check (fn () => Vector.fromList [[1], [2,3], [4]] = Vector.fromList [[1], [2,3], [4]]);
check (fn () => Vector.fromList [[1], [3,3], [4]] <> Vector.fromList [[1], [2,3], [4]]);

check (fn () => IntVector.fromList [1,2,3] = IntVector.fromList [1,2,3]);
check (fn () => IntVector.fromList [1,3,3] <> IntVector.fromList [1,2,3]);

check (fn () => BoolVector.fromList [true, false] = BoolVector.fromList [true, false]);
check (fn () => BoolVector.fromList [true, false] <> BoolVector.fromList [true, true]);

check (fn () => CharVector.fromList [#"a", #"b"] = CharVector.fromList [#"a", #"b"]);
check (fn () => CharVector.fromList [#"a", #"b"] <> CharVector.fromList [#"b", #"b"]);

check (fn () => Word8Vector.fromList [0w1,0w2,0w3] = Word8Vector.fromList [0w1,0w2,0w3]);
check (fn () => Word8Vector.fromList [0w1,0w3,0w3] <> Word8Vector.fromList [0w1,0w2,0w3]);

(* Arrays *)
let val v = Array.fromList [1,2,3] in check(fn () => v = v) end;
check (fn () => Array.fromList [1,2,3] <> Array.fromList [1,2,3]);
check (fn () => Array.fromList [1,3,3] <> Array.fromList [1,2,3]);

let val v = Array.fromList [true, false] in check(fn () => v = v) end;
check (fn () => Array.fromList [true, false] <> Array.fromList [true, false]);
check (fn () => Array.fromList [true, false] <> Array.fromList [true, true]);

let val v = Array.fromList [#"a", #"b"] in check(fn () => v = v) end;
check (fn () => Array.fromList [#"a", #"b"] <> Array.fromList [#"a", #"b"]);
check (fn () => Array.fromList [#"a", #"b"] <> Array.fromList [#"b", #"b"]);

let val v = Array.fromList [0w1,0w2,0w3] in check(fn () => v = v) end;
check (fn () => Array.fromList [0w1,0w2,0w3] <> Array.fromList [0w1,0w2,0w3]);
check (fn () => Array.fromList [0w1,0w3,0w3] <> Array.fromList [0w1,0w2,0w3]);

let val v = IntArray.fromList [1,2,3] in check(fn () => v = v) end;
check (fn () => IntArray.fromList [1,2,3] <> IntArray.fromList [1,2,3]);
check (fn () => IntArray.fromList [1,3,3] <> IntArray.fromList [1,2,3]);

let val v = BoolArray.fromList [true, false] in check(fn () => v = v) end;
check (fn () => BoolArray.fromList [true, false] <> BoolArray.fromList [true, false]);
check (fn () => BoolArray.fromList [true, false] <> BoolArray.fromList [true, true]);

let val v = CharArray.fromList [#"a", #"b"] in check(fn () => v = v) end;
check (fn () => CharArray.fromList [#"a", #"b"] <> CharArray.fromList [#"a", #"b"]);
check (fn () => CharArray.fromList [#"a", #"b"] <> CharArray.fromList [#"b", #"b"]);

let val v = Word8Array.fromList [0w1,0w2,0w3] in check(fn () => v = v) end;
check (fn () => Word8Array.fromList [0w1,0w2,0w3] <> Word8Array.fromList [0w1,0w2,0w3]);
check (fn () => Word8Array.fromList [0w1,0w3,0w3] <> Word8Array.fromList [0w1,0w2,0w3]);

check (fn () => Vector.concat[Vector.fromList[1], Vector.fromList[2,3], Vector.fromList[4]] = Vector.fromList[1,2,3,4]);

