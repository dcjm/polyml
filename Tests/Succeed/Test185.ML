(* UDP Test. *)
val x = INetSock.UDP.socket(): INetSock.dgram_sock;
val y = INetSock.UDP.socket(): INetSock.dgram_sock;

val SOME me = NetHostDB.getByName "localhost";
val localhost = NetHostDB.addr me;
(* Bind to an unused port. *)
Socket.bind(x, INetSock.toAddr(localhost, 0));
(* Get the port so we can connect to it. *)
val socketAddr: INetSock.sock_addr = Socket.Ctl.getSockName x;

(* Send the data. Use a separate thread so there's no possibility of blocking. *)
let
    fun sendData() =
        Socket.sendVecTo(y, socketAddr, Word8VectorSlice.full(Byte.stringToBytes "A test"))
in
    Thread.Thread.fork(sendData, [])
end;

val (data, addr: INetSock.sock_addr) = Socket.recvVecFrom(x, 100);

if Byte.bytesToString data <> "A test"
then raise Fail "failed"
else ();

Socket.close x;
Socket.close y;
