(* This example gets the compiler into a loop repeatedly trying to
   re-optimise the function.  We need to add a time-out thread to
   interrupt it. *)

val m = Thread.Mutex.mutex();
local
    open Thread.Thread Thread.Mutex
    fun thrd () =
    (
        OS.Process.sleep(Time.fromSeconds 5);
        lock m;
        broadcastInterrupt();
        unlock m
    )
in
    val t = fork(thrd, [])
end;

(* This is the actual code. *)
datatype chain = Link of (int * (int -> chain));
fun ints(n) = Link(n+1, ints);


Thread.Mutex.lock m;
Thread.Thread.kill t handle Thread.Thread _ => ();
