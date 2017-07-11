(* These were left over from the old single-character special case. *)

val arr = Word8Array.array(1, 0w0);
Word8ArraySlice.vector(Word8ArraySlice.slice(arr, 0, SOME 1));
val a = CharArray.array(1, #" ");
CharArraySlice.vector (CharArraySlice.full a);
