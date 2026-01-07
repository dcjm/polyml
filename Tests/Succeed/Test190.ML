(* After a Posix fork there is only one thread in the child.
   There could be a GC before any exec so this is a check that it works. *)
(* This is really a problem.  Remove the GC and only test the basic
   fork functionality. *)
case #lookupStruct (PolyML.globalNameSpace) "Posix" of
    SOME _ => ()
|   NONE => raise NotApplicable;

let
    open Posix.Process
in
case fork() of
    NONE =>
        (
            (*PolyML.fullGC();*)
            OS.Process.terminate OS.Process.success (* terminate not exit *)
        )
    |   SOME pid => waitpid(W_CHILD pid, [])
end;

