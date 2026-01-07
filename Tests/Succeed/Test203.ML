(* Check for inputAll on streams with multiple EOFs.  This may be useful for similar cases. *)

fun openStringList l =
let
    val isOpen = ref true
    val listP = ref l

    fun readVec n =
        if n < 0
        then raise Size
        else if not (! isOpen) then raise IO.ClosedStream
        else
        (
            case !listP of
                nil => ""
            |   hd :: tl =>
                    if String.size hd > n
                    then (* Split the string *)
                    (
                        listP := String.extract(hd, n, NONE) :: tl;
                        String.extract(hd, 0, SOME n)
                    )
                    else (* Return the head *)
                    (
                        listP := tl;
                        hd
                    )
        )

in
    TextPrimIO.RD {
    name = "openList",
    chunkSize = 10,
    readVec = SOME readVec,
    readArr = NONE,
    readVecNB = NONE,
    readArrNB = NONE,
    block = SOME(fn () => if !isOpen then () else raise IO.ClosedStream),
    canInput = SOME(fn () => if !isOpen then true else raise IO.ClosedStream),
    avail = fn () => if !isOpen then NONE else raise IO.ClosedStream,
    getPos = NONE,
    setPos = NONE,
    endPos = NONE,
    verifyPos = NONE,
    close = fn () => isOpen := false,
    ioDesc = NONE
    }
end;

val alist = ["abc", "d", "efg", "hijk", "lm", "", "no", "pqr", "", "stuv", "wxyz"];
val areader = openStringList alist;
val astream = TextIO.mkInstream(TextIO.StreamIO.mkInstream(areader, ""));
TextIO.inputAll astream = "abcdefghijklm" orelse raise Fail "wrong";
TextIO.inputAll astream = "nopqr" orelse raise Fail "wrong";
TextIO.inputAll astream = "stuvwxyz" orelse raise Fail "wrong";
TextIO.inputAll astream = "" orelse raise Fail "wrong";

