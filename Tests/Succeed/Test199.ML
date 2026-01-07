(* Test for exact conversion. *)
(* We don't have a random number generator in Poly/ML so this will do. *)

functor Check(structure RealStr: REAL and Pack: PACK_REAL sharing type RealStr.real = Pack.real) =
struct
    local

        (* Random number generator.  From Isabelle. *)
        local
            fun rmod x y = x - y * Real.realFloor (x / y);
            val a = 16807.0;
            val m = 2147483647.0;
            val random_seed = ref 1.0;
        in

            fun randByte _ =
            let
                val r = rmod (a * ! random_seed) m
            in
                random_seed := r;
                Word8.fromLargeInt(Real.toLargeInt IEEEReal.TO_NEGINF r)
            end
        end

        fun repeat _ 0 = ()
        |   repeat f n = (f(); repeat f (n-1));

        open Pack

        fun checkAFloat r =
        let
            val exact = RealStr.fmt StringCvt.EXACT r
            val fromExact = valOf (RealStr.fromString exact)
            (* The values must be equal or both nans. *)
            val _ =
                RealStr.==(r, fromExact) orelse RealStr.isNan r andalso RealStr.isNan fromExact
                    orelse raise Fail ("Exact does not match:" ^ RealStr.toString r)
            val toDec = RealStr.toDecimal r
            val fromDec = valOf (RealStr.fromDecimal toDec)
            (* The conversion back from decimal should produce the same result. *)
            val _ =
                RealStr.==(r, fromDec) orelse RealStr.isNan r andalso RealStr.isNan fromDec
                    orelse raise Fail ("Decimal does not match:" ^ RealStr.toString r)
            (* The last digit should not be zero.  Trailing zeros should have been removed. *)
            val _ =
                case List.rev(#digits toDec) of
                    0::_ => raise Fail ("Decimal has trailing zero:" ^ RealStr.toString r)
                |   _ => ()
            (* The result should be minimal.  Dropping the last digit should change
               the value. *)
            val { class, digits, exp, sign } = toDec
            val numDigits = List.length digits
            val _ =
                if numDigits <= 1 then ()
                else
                let
                    val dec2 = {class=class, digits=List.take(digits, numDigits-1), exp=exp, sign=sign}
                    val fromDec2 = valOf (RealStr.fromDecimal dec2)
                in
                    RealStr.!=(fromDec2, r) orelse
                        raise Fail ("Decimal is not minimal:" ^ RealStr.toString r);
                    ()
                end
            (* Test a long string.  Double values have at most 16 digits of precision. *)
            val long = RealStr.fmt(StringCvt.SCI(SOME 22)) r
            val fromLong = valOf (RealStr.fromString long)
            val _ =
                RealStr.==(r, fromLong) orelse RealStr.isNan r andalso RealStr.isNan fromLong
                    orelse raise Fail ("Long string does not match:" ^ long)
        in
            ()
        end
        
        fun checkRandom () =
        let
            val v = Word8Vector.tabulate(bytesPerElem, randByte)
            val r = subVec(v, 0)
        in
            checkAFloat r
        end
        
        open RealStr
        val zero = fromInt 0
        val pInf = fromInt 1 / zero
        val nInf = fromInt ~1 / zero
        val aNan = zero / zero 
    in
        fun runCheck() =
        (
            repeat checkRandom 1000;
            checkAFloat zero;
            checkAFloat pInf;
            checkAFloat nInf;
            checkAFloat aNan
        )
    end

end;

structure CheckReal = Check(structure RealStr=Real and Pack=PackRealBig);
CheckReal.runCheck();

structure CheckReal32 = Check(structure RealStr=Real32 and Pack=PackReal32Big);
CheckReal32.runCheck();
