(* Unix socket test - Test078 converted to Unix socket *)
case #lookupStruct (PolyML.globalNameSpace) "UnixSock" of
    SOME _ => ()
|   NONE => raise NotApplicable;

val x = UnixSock.Strm.socket(): Socket.passive UnixSock.stream_sock
and y = UnixSock.Strm.socket(): Socket.active UnixSock.stream_sock;

val name = OS.FileSys.tmpName();
OS.FileSys.remove name handle OS.SysErr _ => ();

Socket.bind(x, UnixSock.toAddr name);
Socket.listen(x, 5);

Socket.connect(y, UnixSock.toAddr name);

val (cnct, _) = Socket.accept x;

(* Send the data. Use a separate thread so there's no possibility of blocking. *)
let
    fun sendData() =
        (Socket.sendVec(y, Word8VectorSlice.full(Byte.stringToBytes "A test")); ())
in
    Thread.Thread.fork(sendData, [])
end;

if Byte.bytesToString(Socket.recvVec(cnct, 6)) <> "A test"
then raise Fail "failed"
else ();

Socket.close cnct;
Socket.close x;
Socket.close y;
OS.FileSys.remove name handle OS.SysErr _ => ();
