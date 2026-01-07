(* Windows tests. *)

case #lookupStruct (PolyML.globalNameSpace) "Windows" of
    SOME _ => ()
|   NONE => raise NotApplicable;

val cmd = valOf(Windows.findExecutable "cmd");

val dirExec: (TextIO.instream, TextIO.outstream) Windows.proc = Windows.execute(cmd, "/c dir");
TextIO.closeOut(Windows.textOutstreamOf dirExec);
val instr = Windows.textInstreamOf dirExec;
TextIO.inputAll instr;
TextIO.closeIn instr;
(* This is defined to be able to repeatedly return a result. *)
val res1 = Windows.reap dirExec;
val res2 = Windows.reap dirExec;

OS.Process.isSuccess res1;
OS.Process.isSuccess res2;


val r = Windows.Reg.openKeyEx(Windows.Reg.currentUser, "Software", Windows.Key.read);
val a = Windows.Reg.enumKeyEx(r, 0);
val b = Windows.Reg.enumValueEx(r, 0);
Windows.Reg.closeKey r;
val r = Windows.Reg.openKeyEx(Windows.Reg.currentUser, "Environment", Windows.Key.read);
val a = Windows.Reg.enumKeyEx(r, 0);
val b = Windows.Reg.enumValueEx(r, 0);
case b of
    NONE => NONE
|   SOME t => Windows.Reg.queryValueEx(r, t);
Windows.Reg.closeKey r;

Windows.Config.getVersionEx();
Windows.Config.getWindowsDirectory();
Windows.Config.getUserName();
Windows.Config.getSystemDirectory();
Windows.Config.getComputerName();
