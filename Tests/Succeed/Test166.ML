(* Test for condition variables. *)

open Thread;
open Thread Mutex ConditionVar;

val avar = conditionVar()
and amutex = mutex();

val bvar = conditionVar();

val cvar = conditionVar();

val dvar = conditionVar();

val stop = ref false

val avalue = ref 0

fun incValue threadNo () =
let
    val () = lock amutex
    fun loop() =
        (
            wait(avar, amutex);
            if ! stop
            then ()
            else
            (
                avalue := !avalue + 1;
                signal bvar;
                loop()
            )
        )
in
    signal dvar;
    loop();
    signal cvar;
    unlock amutex
end;

fun addCount 0 = ()
|   addCount n =
    (
        signal avar;
        wait(bvar, amutex);
        addCount (n-1)
    );

lock amutex;
fork(incValue 1, [EnableBroadcastInterrupt true, InterruptState InterruptSynch]);
fork(incValue 2, [EnableBroadcastInterrupt true, InterruptState InterruptSynch]);
fork(incValue 3, [EnableBroadcastInterrupt true, InterruptState InterruptSynch]);

wait(dvar, amutex);
addCount 10000;
OS.Process.sleep(Time.fromSeconds 2);

stop := true;
signal avar;
wait(cvar, amutex);
signal avar;
wait(cvar, amutex);
signal avar;
wait(cvar, amutex);

if !avalue = 10000 then () else raise Fail "Incorrect";
