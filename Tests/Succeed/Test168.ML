(* This is a check that return addresses are correctly identified as
   references to code cells. *)

fun f () = (OS.Process.sleep(Time.fromSeconds 3); print "Done\n");
val _ = Thread.Thread.fork(f, []);
fun f () = print "A new function";

PolyML.fullGC();
fun f () = print "A new function";
PolyML.fullGC();
