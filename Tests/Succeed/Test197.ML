fun f 0 = () | f i = (PolyML.objSize (); f (i-1));
f 2000; (* Bigger than the current save-vec size *)
