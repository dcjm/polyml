(* Tests for IEEEReal.fromString which were wrong.  Taken from issue 182. *)

fun verify true = () | verify false = raise Fail "verify";

(* Leading and trailing zeros should be removed. *)
verify(IEEEReal.fromString "10.0" = SOME {class = IEEEReal.NORMAL, digits = [1], exp = 2, sign = false});
verify(IEEEReal.fromString "0.01" = SOME {class = IEEEReal.NORMAL, digits = [1], exp = ~1, sign = false});

(* Checks for overflow in exponent.  Not applicaable if int is arbitrary precision. *)
val minInt = case Int.minInt of NONE => raise NotApplicable | SOME i => Int.toLarge i;
val maxInt = case Int.maxInt of NONE => raise NotApplicable | SOME i => Int.toLarge i;

verify(IEEEReal.fromString ("0e" ^ LargeInt.toString maxInt) =
        SOME {class = IEEEReal.ZERO, digits = [], exp = 0, sign = false});
verify(IEEEReal.fromString ("0e" ^ LargeInt.toString (maxInt+1)) =
        SOME {class = IEEEReal.ZERO, digits = [], exp = 0, sign = false});
verify(IEEEReal.fromString ("1e" ^ LargeInt.toString minInt) =
        SOME {class = IEEEReal.NORMAL, digits = [1], exp = Int.fromLarge(minInt+1), sign = false});
verify(IEEEReal.fromString ("1e" ^ LargeInt.toString (minInt-1))  =
        SOME {class = IEEEReal.NORMAL, digits = [1], exp = Int.fromLarge minInt, sign = false});
verify(IEEEReal.fromString ("1e" ^ LargeInt.toString (minInt-2))  =
        SOME {class = IEEEReal.ZERO, digits = [], exp = 0, sign = false});
verify(IEEEReal.fromString("1e"  ^ LargeInt.toString (maxInt-1)) =
        SOME {class = IEEEReal.NORMAL, digits = [1], exp = Int.fromLarge maxInt, sign = false});
verify(IEEEReal.fromString("1e"  ^ LargeInt.toString maxInt) =
        SOME {class = IEEEReal.INF, digits = [], exp = 0, sign = false});
