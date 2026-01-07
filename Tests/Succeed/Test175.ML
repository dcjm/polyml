(* Test122 rewritten for Real32.real. *)
fun check x = x orelse raise Fail "Test failed";

let
    val a = Real32.nextAfter(1.0, 2.0)
in
    check(a > 1.0 andalso a < 2.0)
    (* This was actually a code-generator bug. *)
end;


(* The old version of the basis library documentation said that this should
   print the digits.  That was removed before the book was published but not
   fixed in the source. *)
check(IEEEReal.toString {class = IEEEReal.NAN, sign = false, digits = [1,2,3], exp = 0} = "nan");

val pNan = valOf(Real32.fromDecimal { class = IEEEReal.NAN, sign = false, digits = [], exp = 0})
and mNan = valOf(Real32.fromDecimal { class = IEEEReal.NAN, sign = true, digits = [], exp = 0});

check(not(Real32.signBit pNan));
check(Real32.signBit mNan);

check(not(#sign(Real32.toDecimal pNan)));
check(#sign(Real32.toDecimal mNan));

check(not(Real32.signBit(abs(Real32.posInf * 0.0))));
check(not(Real32.signBit(abs(~ (Real32.posInf * 0.0)))));
check(not(Real32.signBit(Real32.abs(Real32.posInf * 0.0))));
check(not(Real32.signBit(Real32.abs(~ (Real32.posInf * 0.0)))));

check(Real32.fmt (StringCvt.SCI NONE) mNan = "nan");
check(Real32.fmt (StringCvt.FIX NONE) mNan = "nan");
check(Real32.fmt (StringCvt.GEN NONE) mNan = "nan");
check(Real32.fmt StringCvt.EXACT mNan = "nan");

check(not (Real32.isNan 0.0));
check(not (Real32.isNan ~1.0));
check(not (Real32.isNan Real32.posInf));
check(not (Real32.isNan Real32.negInf));

check (Real32.isNan pNan);
check (Real32.isNan mNan);

check(Real32.isFinite 0.0);
check(not (Real32.isFinite pNan));
check(not (Real32.isFinite mNan));
check(not (Real32.isFinite Real32.posInf));
check(not (Real32.isFinite Real32.negInf));

(* The exceptions with invalid specifications should be raised when
   Real32.fmt is applied to the spec. *)
check((Real32.fmt(StringCvt.SCI(SOME ~1)); false) handle Size => true);
check((Real32.fmt(StringCvt.FIX(SOME ~1)); false) handle Size => true);
check((Real32.fmt(StringCvt.GEN(SOME ~1)); false) handle Size => true);


check(not(Real32.unordered(1.0, 1.0)));
check(Real32.unordered(pNan, 1.0));
check(Real32.unordered(1.0, pNan));
check(Real32.unordered(pNan, pNan));
check(Real32.unordered(mNan, 1.0));
check(Real32.unordered(1.0, mNan));
check(Real32.unordered(mNan, mNan));

(* Additional checks from issue 183.  These were previously wrong because the code
   converted to Real. *)
check(Real32.fmt StringCvt.EXACT 0.1 = "0.1");
check(Real32.fmt StringCvt.EXACT 0.100000002 = "0.1");
check(Real32.fmt StringCvt.EXACT 0.100000007 = "0.10000001");

