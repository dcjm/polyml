(* Test Unix.execute. *)
case #lookupStruct (PolyML.globalNameSpace) "Unix" of
    SOME _ => ()
|   NONE => raise NotApplicable;

if OS.FileSys.access("/bin/ls", [OS.FileSys.A_EXEC]) then () else raise NotApplicable;

val dirExec: (TextIO.instream, TextIO.outstream) Unix.proc = Unix.execute("/bin/ls", ["."]);
TextIO.closeOut(Unix.textOutstreamOf dirExec);
val instr = Unix.textInstreamOf dirExec;
TextIO.inputAll instr;
TextIO.closeIn instr;
(* This is defined to be able to repeatedly return a result. *)
val res1 = Unix.reap dirExec;
val res2 = Unix.reap dirExec;

OS.Process.isSuccess res1;
OS.Process.isSuccess res2;

