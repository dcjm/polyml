(* Test simple TCP connection.  There was a problem with "connect" on Windows. *)
val x = INetSock.TCP.socket(): Socket.passive INetSock.stream_sock
and y = INetSock.TCP.socket(): Socket.active INetSock.stream_sock;
val SOME me = NetHostDB.getByName "localhost";
val localhost = NetHostDB.addr me;
Socket.bind(x, INetSock.toAddr(localhost, 0));
Socket.listen(x, 5);
let
val (_, port) = INetSock.fromAddr(Socket.Ctl.getSockName x)
in
Socket.connect(y, INetSock.toAddr(localhost, port))
end;
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

