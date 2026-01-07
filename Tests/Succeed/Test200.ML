(* Checks for Real.scan, Real.fromString, IEEEReal.scan and IEEEReal.fromString. *)
(* There are a few more tests for overflow in the exponent in Test201.  These don't apply
   if int is IntInf.int. *)
fun verify true = () | verify false = raise Fail "incorrect";


fun ieeeVerify(SOME(iee, subs), ieeMatch, str) =
    if iee = ieeMatch andalso Substring.string subs = str
    then ()
    else raise Fail "wrong"
|   ieeeVerify _ = raise Fail "wrong";

ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  15E~1000"),
    {exp = ~998, sign = false, class = IEEEReal.NORMAL, digits = [1, 5]}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1.23E~1"),
    {exp = 0, sign = false, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1.23X"),
    {exp = 1, sign = false, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1.23E3X"),
    {exp = 4, sign = false, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1.23E 3X"),
    {exp = 1, sign = false, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "E 3X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  ~1.23E 3X"),
    {exp = 1, sign = true, class = IEEEReal.NORMAL, digits = [1, 2, 3]}, "E 3X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  .23E 3X"),
    {exp = 0, sign = false, class = IEEEReal.NORMAL, digits = [2, 3]}, "E 3X"); (* Valid, but E 3X isn't part of the number. *)
verify(not(isSome(IEEEReal.scan Substring.getc (Substring.full "  . 23E 3X")))); (* Not valid. *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  000.000X"),
    {exp = 0, sign = false, class = IEEEReal.ZERO, digits = []}, "X");
verify(not(isSome(IEEEReal.scan Substring.getc (Substring.full "  E2 3X")))); (* Not valid. *)
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  0X"),
    {exp = 0, sign = false, class = IEEEReal.ZERO, digits = []}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  1X"),
    {exp = 1, sign = false, class = IEEEReal.NORMAL, digits = [1]}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  12X"),
    {exp = 2, sign = false, class = IEEEReal.NORMAL, digits = [1, 2]}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  0.00X"),
    {exp = 0, sign = false, class = IEEEReal.ZERO, digits = []}, "X");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  10"),
    {exp = 2, sign = false, class = IEEEReal.NORMAL, digits = [1]}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "  10.X"),
    {exp = 2, sign = false, class = IEEEReal.NORMAL, digits = [1]}, ".X"); (* Valid. The decimal point is not part of the number. *)


fun expStr (n : Int.int, offset : IntInf.int) = IntInf.toString (IntInf.fromInt n + offset);

case Int.minInt of
  SOME minIntVal => (
    (* zero mantissa with literal exponent not in range of `Int.int`: result is zero
     *)
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~0E" ^ expStr (minIntVal, ~1))),
       {exp = 0, sign=true, class = IEEEReal.ZERO, digits = []}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 0E" ^ expStr (minIntVal, ~1))),
        {exp = 0, sign=false, class = IEEEReal.ZERO, digits = []}, "");

    (* non-zero mantissa with literal exponent below range of `Int.int` and
     * normalized exponent below range of `Int.int`: result is rounded to zero
     *)
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 1E" ^ expStr (minIntVal, ~2))),
       {exp = 0, sign=false, class = IEEEReal.ZERO, digits = []}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~1E" ^ expStr (minIntVal, ~2))),
        {exp = 0, sign=true, class = IEEEReal.ZERO, digits = []}, "");

    (* non-zero mantissa with literal exponent below range of `Int.int` and
     * normalized exponent in range of `Int.int`: result is not rounded
     *)
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 1E" ^ expStr (minIntVal, ~1))),
        {exp = minIntVal, sign=false, class = IEEEReal.NORMAL, digits = [1]}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~1E" ^ expStr (minIntVal, ~1))),
        {exp = minIntVal, sign=true, class = IEEEReal.NORMAL, digits = [1]}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 100000000000E" ^ expStr (minIntVal, ~12))),
        {exp = minIntVal, sign=false, class = IEEEReal.NORMAL, digits = [1]}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~100000000000E" ^ expStr (minIntVal, ~12))),
        {exp = minIntVal, sign=true, class = IEEEReal.NORMAL, digits = [1]}, "")
    )
| NONE =>
    let
      val hugeNegVal = ~ (List.foldl (op *) 1 (List.tabulate (100, fn i => (i + 1437) * 89)))
    in
      (* zero mantissa with huge negative literal exponent: result is zero
       *)
      ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~0E" ^ expStr (hugeNegVal, 0))),
         {exp = 0, sign=true, class = IEEEReal.ZERO, digits = []}, "");
      ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 0E" ^ expStr (hugeNegVal, 0))),
          {exp = 0, sign=false, class = IEEEReal.ZERO, digits = []}, "");

      (* non-zero mantissa with huge negative literal exponent: result is not rounded
       *)
      ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 1E" ^ expStr (hugeNegVal, ~1))),
         {exp = hugeNegVal, sign=false, class = IEEEReal.NORMAL, digits = [1]}, "");
      ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~1E" ^ expStr (hugeNegVal, ~1))),
          {exp = hugeNegVal, sign=true, class = IEEEReal.NORMAL, digits = [1]}, "")
    end;

case Int.maxInt of
  SOME maxIntVal => (

    (* zero mantissa with literal exponent not in range of `Int.int`: result is zero
     *)
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~0E" ^ expStr (maxIntVal, 1))),
       {exp = 0, sign=true, class = IEEEReal.ZERO, digits = []}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 0E" ^ expStr (maxIntVal, 1))),
        {exp = 0, sign=false, class = IEEEReal.ZERO, digits = []}, "");

    (* non-zero mantissa with literal exponent above range of `Int.int` and
     * normalized exponent above range of `Int.int`: result is rounded to infinity
     *)
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 0.1E" ^ expStr (maxIntVal, 1))),
        {exp = 0, sign=false, class = IEEEReal.INF, digits = []}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~0.1E" ^ expStr (maxIntVal, 1))),
        {exp = 0, sign=true, class = IEEEReal.INF, digits = []}, "");

    (* non-zero mantissa with literal exponent above range of `Int.int` and
     * normalized exponent in range of `Int.int`: result is not rounded
     *)
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 0.01E" ^ expStr (maxIntVal, 1))),
        {exp = maxIntVal, sign=false, class = IEEEReal.NORMAL, digits = [1]}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~0.01E" ^ expStr (maxIntVal, 1))),
        {exp = maxIntVal, sign=true, class = IEEEReal.NORMAL, digits = [1]}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 0.0000000000001E" ^ expStr (maxIntVal, 12))),
        {exp = maxIntVal, sign=false, class = IEEEReal.NORMAL, digits = [1]}, "");
    ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~0.0000000000001E" ^ expStr (maxIntVal, 12))),
        {exp = maxIntVal, sign=true, class = IEEEReal.NORMAL, digits = [1]}, "")
  )
| NONE =>
    let
      val hugePosVal = List.foldl (op *) 1 (List.tabulate (100, fn i => (i + 1437) * 89))
    in
      (* zero mantissa with huge positive literal exponent: result is zero
       *)
      ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~0E" ^ expStr (hugePosVal, 0))),
         {exp = 0, sign=true, class = IEEEReal.ZERO, digits = []}, "");
      ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 0E" ^ expStr (hugePosVal, 0))),
          {exp = 0, sign=false, class = IEEEReal.ZERO, digits = []}, "");

      (* non-zero mantissa with huge positive literal exponent: result is not rounded
       *)
      ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" 1E" ^ expStr (hugePosVal, ~1))),
         {exp = hugePosVal, sign=false, class = IEEEReal.NORMAL, digits = [1]}, "");
      ieeeVerify(IEEEReal.scan Substring.getc (Substring.full (" ~1E" ^ expStr (hugePosVal, ~1))),
          {exp = hugePosVal, sign=true, class = IEEEReal.NORMAL, digits = [1]}, "")
    end;


ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "infinity"),
    {exp = 0, sign = false, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "+infinity"),
    {exp = 0, sign = false, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "-infinity"),
    {exp = 0, sign = true, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "~infinity"),
    {exp = 0, sign = true, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "inf"),
    {exp = 0, sign = false, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "+inf"),
    {exp = 0, sign = false, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "-inf"),
    {exp = 0, sign = true, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "~inf"),
    {exp = 0, sign = true, class = IEEEReal.INF, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "nan"),
    {exp = 0, sign = false, class = IEEEReal.NAN, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "+nan"),
    {exp = 0, sign = false, class = IEEEReal.NAN, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "-nan"),
    {exp = 0, sign = true, class = IEEEReal.NAN, digits = []}, "");
ieeeVerify(IEEEReal.scan Substring.getc (Substring.full "~nan"),
    {exp = 0, sign = true, class = IEEEReal.NAN, digits = []}, "");


verify(not(Real.isFinite(valOf(Real.fromString "infinity"))));
verify(not(Real32.isFinite(valOf(Real32.fromString "infinity"))));

verify(Real.isNan(valOf(Real.fromString "nan")));
verify(Real32.isNan(valOf(Real32.fromString "nan")));
