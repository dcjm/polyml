(* Real.fromString is affected by the rounding mode at run-time but the conversion of
   literals should not be. *)


val () = IEEEReal.setRoundingMode IEEEReal.TO_POSINF handle Fail _ => ();
val lPos = 10e100;
val () = IEEEReal.setRoundingMode IEEEReal.TO_NEGINF handle Fail _ => ();
val lNeg = 10e100;
val () = IEEEReal.setRoundingMode IEEEReal.TO_ZERO handle Fail _ => ();
val lZero = 10e100;
val () = IEEEReal.setRoundingMode IEEEReal.TO_NEAREST handle Fail _ => ();
val lNearest = 10e100;

if Real.!= (lPos, lNeg) orelse Real.!= (lPos, lZero) orelse Real.!= (lPos, lNearest)
then raise Fail "Rounding affects literals"
else ();

