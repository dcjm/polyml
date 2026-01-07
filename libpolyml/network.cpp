/*
    Title:      Network functions.

    Copyright (c) 2000-7, 2016, 2018, 2019, 2022 David C. J. Matthews

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License version 2.1 as published by the Free Software Foundation.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

*/
#ifdef HAVE_CONFIG_H
#include "config.h"
#elif defined(_WIN32)
#include "winconfig.h"
#else
#error "No configuration file"
#endif

#ifdef HAVE_STDIO_H
#include <stdio.h>
#endif

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif

#ifdef HAVE_ASSERT_H
#include <assert.h>
#define ASSERT(x) assert(x)
#else
#define ASSERT(x) 0
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif

#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#ifdef HAVE_NETINET_TCP_H
#include <netinet/tcp.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif

#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#ifdef HAVE_SYS_FILIO_H
#include <sys/filio.h>
#endif

#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif

#ifdef HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif

#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#ifdef HAVE_LIMITS_H
#include <limits.h>
#endif

#ifndef HAVE_SOCKLEN_T
typedef int socklen_t;
#endif


#if (defined(_WIN32))
#include <winsock2.h>
#include <ws2tcpip.h> // For getaddrinfo
#else
typedef int SOCKET;
#endif

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

#include <new>

#include "globals.h"
#include "gc.h"
#include "arb.h"
#include "run_time.h"
#include "mpoly.h"
#include "processes.h"
#include "network.h"
#include "io_internal.h"
#include "sys.h"
#include "polystring.h"
#include "save_vec.h"
#include "rts_module.h"
#include "machine_dep.h"
#include "errors.h"
#include "rtsentry.h"
#include "timing.h"

extern "C" {
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetAddrList(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetSockTypeList(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkCreateSocket(POLYUNSIGNED threadId, POLYUNSIGNED af, POLYUNSIGNED st, POLYUNSIGNED prot);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkSetOption(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED sock, POLYUNSIGNED opt);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetOption(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkSetLinger(POLYUNSIGNED threadId, POLYUNSIGNED sock, POLYUNSIGNED linger);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetLinger(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetPeerName(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetSockName(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkBytesAvailable(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetAtMark(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkBind(POLYUNSIGNED threadId, POLYUNSIGNED sock, POLYUNSIGNED addr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkListen(POLYUNSIGNED threadId, POLYUNSIGNED sock, POLYUNSIGNED back);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkShutdown(POLYUNSIGNED threadId, POLYUNSIGNED skt, POLYUNSIGNED smode);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkCreateSocketPair(POLYUNSIGNED threadId, POLYUNSIGNED af, POLYUNSIGNED st, POLYUNSIGNED prot);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkUnixPathToSockAddr(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkUnixSockAddrToPath(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetServByName(POLYUNSIGNED threadId, POLYUNSIGNED servName);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetServByNameAndProtocol(POLYUNSIGNED threadId, POLYUNSIGNED servName, POLYUNSIGNED protName);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetServByPort(POLYUNSIGNED threadId, POLYUNSIGNED portNo);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetServByPortAndProtocol(POLYUNSIGNED threadId, POLYUNSIGNED portNo, POLYUNSIGNED protName);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetProtByName(POLYUNSIGNED threadId, POLYUNSIGNED protocolName);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetProtByNo(POLYUNSIGNED threadId, POLYUNSIGNED protoNo);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetHostName(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetAddrInfo(POLYUNSIGNED threadId, POLYUNSIGNED hostName, POLYUNSIGNED addrFamily);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetNameInfo(POLYUNSIGNED threadId, POLYUNSIGNED sockAddr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkCloseSocket(POLYUNSIGNED threadId, POLYUNSIGNED arg);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkSelect(POLYUNSIGNED threadId, POLYUNSIGNED fdVecTriple, POLYUNSIGNED maxMillisecs);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetSocketError(POLYUNSIGNED threadId, POLYUNSIGNED skt);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkConnect(POLYUNSIGNED threadId, POLYUNSIGNED skt, POLYUNSIGNED addr);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkAccept(POLYUNSIGNED threadId, POLYUNSIGNED skt);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkSend(POLYUNSIGNED threadId, POLYUNSIGNED args);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkSendTo(POLYUNSIGNED threadId, POLYUNSIGNED args);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkReceive(POLYUNSIGNED threadId, POLYUNSIGNED args);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkReceiveFrom(POLYUNSIGNED threadId, POLYUNSIGNED args);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetFamilyFromAddress(POLYUNSIGNED sockAddress);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetAddressAndPortFromIP4(POLYUNSIGNED threadId, POLYUNSIGNED sockAddress);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkCreateIP4Address(POLYUNSIGNED threadId, POLYUNSIGNED ip4Address, POLYUNSIGNED portNumber);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkReturnIP4AddressAny(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetAddressAndPortFromIP6(POLYUNSIGNED threadId, POLYUNSIGNED sockAddress);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkCreateIP6Address(POLYUNSIGNED threadId, POLYUNSIGNED ip6Address, POLYUNSIGNED portNumber);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkReturnIP6AddressAny(POLYUNSIGNED threadId);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkIP6AddressToString(POLYUNSIGNED threadId, POLYUNSIGNED ip6Address);
    POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkStringToIP6Address(POLYUNSIGNED threadId, POLYUNSIGNED stringRep);
}

#define SAVE(x) taskData->saveVec.push(x)
#define ALLOC(n) alloc_and_save(taskData, n)
#define SIZEOF(x) (sizeof(x)/sizeof(PolyWord))

#if (defined(_WIN32))
static int winsock_init = 0; /* Check that it has been initialised. */

#else
#define INVALID_SOCKET (-1)
#define SOCKET_ERROR    (-1)
#endif

#ifndef HAVE_SOCKLEN_T
typedef int socklen_t; // This must be int for Windows at least
#endif

#ifndef SHUT_RD
#define SHUT_RD     0
#endif

#ifndef SHUT_WR
#define SHUT_WR     1
#endif

#ifndef SHUT_RDWR
#define SHUT_RDWR   2
#endif

/* Address families.  Although this table is in ascending
   numerical order of address family nothing depends on that.
   The only requirement is that "INET" => AF_INET must always
   be present and "UNIX" => AF_UNIX must be present on Unix.
   Other entries are entirely optional and are for amusement
   only. */
struct af_tab_struct {
    const char *af_name;
    int af_num;
} af_table[] =
{
#ifdef AF_UNIX
    { "UNIX",       AF_UNIX }, /* This is nearly always there. */
#endif
#ifdef AF_LOCAL
    { "LOCAL",      AF_LOCAL },
#endif
    { "INET",       AF_INET }, /* This one should always be there. */
#ifdef AF_IMPLINK
    { "IMPLINK",    AF_IMPLINK },
#endif
#ifdef AF_PUP
    { "PUP",        AF_PUP },
#endif
#ifdef AF_CHAOS
    { "CHAOS",      AF_CHAOS },
#endif
#ifdef AF_IPX
    { "IPX",        AF_IPX },
#endif
#ifdef AF_NS
    { "NS",         AF_NS },
#endif
#ifdef AF_ISO
    { "ISO",        AF_ISO },
#endif
#ifdef AF_OSI
    { "OSI",        AF_OSI },
#endif
#ifdef AF_ECMA
    { "ECMA",       AF_ECMA },
#endif
#ifdef AF_DATAKIT
    { "DATAKIT",    AF_DATAKIT },
#endif
#ifdef AF_CCITT
    { "CCITT",      AF_CCITT },
#endif
#ifdef AF_SNA
    { "SNA",        AF_SNA },
#endif
#ifdef AF_DECnet
    { "DECnet",     AF_DECnet },
#endif
#ifdef AF_DLI
    { "DLI",        AF_DLI },
#endif
#ifdef AF_LAT
    { "LAT",        AF_LAT },
#endif
#ifdef AF_HYLINK
    { "HYLINK",     AF_HYLINK },
#endif
#ifdef AF_APPLETALK
    { "APPLETALK",  AF_APPLETALK },
#endif
#ifdef AF_NETBIOS
    { "NETBIOS",    AF_NETBIOS },
#endif
#ifdef AF_ROUTE
    { "ROUTE",      AF_ROUTE },
#endif
#ifdef AF_VOICEVIEW
    { "VOICEVIEW",  AF_VOICEVIEW },
#endif
#ifdef AF_FIREFOX
    { "FIREFOX",    AF_FIREFOX },
#endif
#ifdef AF_BAN
    { "BAN",        AF_BAN },
#endif
#ifdef AF_LINK
    { "LINK",       AF_LINK },
#endif
#ifdef AF_COIP
    { "COIP",       AF_COIP },
#endif
#ifdef AF_CNT
    { "CNT",        AF_CNT },
#endif
#ifdef AF_SIP
    { "SIP",        AF_SIP },
#endif
#ifdef AF_ISDN
    { "ISDN",       AF_ISDN },
#endif
#ifdef AF_E164
    { "E164",       AF_E164 },
#endif
#ifdef AF_INET6
    { "INET6",      AF_INET6 },  // This one should always be there.
#endif
#ifdef AF_NATM
    { "NATM",       AF_NATM },
#endif
#ifdef AF_ATM
    { "ATM",        AF_ATM },
#endif
#ifdef AF_NETGRAPH
    { "NETGRAPH",   AF_NETGRAPH },
#endif
#ifdef AF_CLUSTER
    { "CLUSTER",    AF_CLUSTER },
#endif
#ifdef AF_12844
    { "12844",      AF_12844 },
#endif
#ifdef AF_IRDA
    { "IRDA",       AF_IRDA },
#endif
#ifdef AF_NETDES
    { "NETDES",     AF_NETDES },
#endif
#ifdef AF_TCNPROCESS
    { "TCNPROCESS", AF_TCNPROCESS },
#endif
#ifdef AF_TCNMESSAGE
    { "TCNMESSAGE", AF_TCNMESSAGE },
#endif
#ifdef AF_ICLFXBM
    { "ICLFXBM",    AF_ICLFXBM },
#endif
#ifdef AF_BTH
    { "BTH",        AF_BTH },
#endif
#ifdef AF_HYPERV
    { "HYPERV",     AF_HYPERV },
#endif
#ifdef AF_FILE
    { "FILE",       AF_FILE },
#endif
#ifdef AF_AX25
    { "AX25",       AF_AX25 },
#endif
#ifdef AF_NETROM
    { "NETROM",     AF_NETROM },
#endif
#ifdef AF_BRIDGE
    { "BRIDGE",     AF_BRIDGE },
#endif
#ifdef AF_ATMPVC
    { "ATMPVC",     AF_ATMPVC },
#endif
#ifdef AF_X25
    { "X25",        AF_X25 },
#endif
#ifdef AF_ROSE
    { "ROSE",       AF_ROSE },
#endif
#ifdef AF_NETBEUI
    { "NETBEUI",    AF_NETBEUI },
#endif
#ifdef AF_SECURITY
    { "SECURITY",   AF_SECURITY },
#endif
#ifdef AF_KEY
    { "KEY",        AF_KEY },
#endif
#ifdef AF_NETLINK
    { "NETLINK",    AF_NETLINK },
#endif
#ifdef AF_PACKET
    { "PACKET",     AF_PACKET },
#endif
#ifdef AF_ASH
    { "ASH",        AF_ASH },
#endif
#ifdef AF_ECONET
    { "ECONET",     AF_ECONET },
#endif
#ifdef AF_ATMSVC
    { "ATMSVC",     AF_ATMSVC },
#endif
#ifdef AF_RDS
    { "RDS",        AF_RDS },
#endif
#ifdef AF_PPPOX
    { "PPPOX",      AF_PPPOX },
#endif
#ifdef AF_WANPIPE
    { "WANPIPE",    AF_WANPIPE },
#endif
#ifdef AF_LLC
    { "LLC",        AF_LLC },
#endif
#ifdef AF_IB
    { "IB",         AF_IB },
#endif
#ifdef AF_MPLS
    { "MPLS",       AF_MPLS },
#endif
#ifdef AF_CAN
    { "CAN",        AF_CAN },
#endif
#ifdef AF_TIPC
    { "TIPC",       AF_TIPC },
#endif
#ifdef AF_BLUETOOTH
    { "BLUETOOTH",  AF_BLUETOOTH },
#endif
#ifdef AF_IUCV
    { "IUCV",       AF_IUCV },
#endif
#ifdef AF_RXRPC
    { "RXRPC",      AF_RXRPC },
#endif
#ifdef AF_PHONET
    { "PHONET",     AF_PHONET },
#endif
#ifdef AF_IEEE802154
    { "IEEE802154", AF_IEEE802154 },
#endif
#ifdef AF_CAIF
    { "CAIF",       AF_CAIF },
#endif
#ifdef AF_ALG
    { "ALG",        AF_ALG },
#endif
#ifdef AF_NFC
    { "NFC",        AF_NFC },
#endif
#ifdef AF_VSOCK
    { "VSOCK",      AF_VSOCK },
#endif
#ifdef AF_KCM
    { "KCM",        AF_KCM },
#endif
};

/* Socket types.  Only STREAM and DGRAM are required.  */
struct sk_tab_struct {
    const char *sk_name;
    int sk_num;
} sk_table[] =
{
    { "STREAM",     SOCK_STREAM },
    { "DGRAM",      SOCK_DGRAM },
    { "RAW",        SOCK_RAW },
#ifdef SOCK_RDM
    { "RDM",        SOCK_RDM },
#endif
    { "SEQPACKET",  SOCK_SEQPACKET },
#ifdef SOCK_DCCP
    { "DCCP",       SOCK_DCCP },
#endif
};

static Handle makeProtoEntry(TaskData *taskData, struct protoent *proto);
static Handle mkAftab(TaskData *taskData, void*, char *p);
static Handle mkSktab(TaskData *taskData, void*, char *p);
static Handle setSocketOption(TaskData *taskData, Handle sockHandle, Handle optHandle, int level, int opt);
static Handle getSocketOption(TaskData *taskData, Handle args, int level, int opt);

#if (defined(_WIN32))
#define GETERROR     (WSAGetLastError())
#define TOOMANYFILES    WSAEMFILE
#define NOMEMORY        WSA_NOT_ENOUGH_MEMORY
#define STREAMCLOSED    WSA_INVALID_HANDLE
#define WOULDBLOCK      WSAEWOULDBLOCK
#define INPROGRESS      WSAEINPROGRESS
#define CALLINTERRUPTED WSAEINTR
#undef EBADF
#undef EMFILE
#undef EAGAIN
#undef EINTR
#undef EWOULDBLOCK
#undef ENOMEM
#else
#define GETERROR    (errno)
#define TOOMANYFILES EMFILE
#define NOMEMORY ENOMEM
#define STREAMCLOSED EBADF
#define ERRORNUMBER errno
#define FILEDOESNOTEXIST ENOENT
#define WOULDBLOCK EWOULDBLOCK
#define INPROGRESS EINPROGRESS
#define CALLINTERRUPTED EINTR
#endif

// On Haiku, errors returned by getsockopt(SO_ERROR) will still be
// negative, in spite of B_USE_POSITIVE_POSIX_ERRORS/posix_error_mapper
#ifdef B_TO_POSITIVE_ERROR
#define TRANSLATE_SO_ERROR(x) (B_TO_POSITIVE_ERROR(x))
#else
#define TRANSLATE_SO_ERROR(x) (x)
#endif

// Wait until "select" returns.  In Windows this is used only for networking.
class WaitSelect: public Waiter
{
public:
    WaitSelect(unsigned maxMillisecs=(unsigned)-1);
    virtual void Wait(unsigned maxMillisecs);
    void SetRead(SOCKET fd) {  FD_SET(fd, &readSet); }
    void SetWrite(SOCKET fd) {  FD_SET(fd, &writeSet); }
    void SetExcept(SOCKET fd)  {  FD_SET(fd, &exceptSet); }
    bool IsSetRead(SOCKET fd) { return FD_ISSET(fd, &readSet) != 0; }
    bool IsSetWrite(SOCKET fd) { return FD_ISSET(fd, &writeSet) != 0; }
    bool IsSetExcept(SOCKET fd) { return FD_ISSET(fd, &exceptSet) != 0; }
    // Save the result of the select call and any associated error
    int SelectResult(void) { return selectResult; }
    int SelectError(void) { return errorResult; }
private:
    fd_set readSet, writeSet, exceptSet;
    int selectResult;
    int errorResult;
    unsigned maxTime;
};

WaitSelect::WaitSelect(unsigned maxMillisecs)
{
    FD_ZERO(&readSet);
    FD_ZERO(&writeSet);
    FD_ZERO(&exceptSet);
    selectResult = 0;
    errorResult = 0;
    maxTime = maxMillisecs;
}

void WaitSelect::Wait(unsigned maxMillisecs)
{
    if (maxTime < maxMillisecs) maxMillisecs = maxTime;
    struct timeval toWait = { 0, 0 };
    toWait.tv_sec = maxMillisecs / 1000;
    toWait.tv_usec = (maxMillisecs % 1000) * 1000;
    selectResult = select(FD_SETSIZE, &readSet, &writeSet, &exceptSet, &toWait);
    if (selectResult < 0) errorResult = GETERROR;
}

#if (defined(_WIN32))
class WinSocket : public WinStreamBase
{
public:
    WinSocket(SOCKET skt) : socket(skt) {}

    virtual SOCKET getSocket() {
        return socket;
    }

    virtual int pollTest() {
        // We can poll for any of these.
        return POLL_BIT_IN | POLL_BIT_OUT | POLL_BIT_PRI;
    }

    virtual int poll(TaskData *taskData, int test);

public:
    SOCKET socket;
};

// Poll without blocking.
int WinSocket::poll(TaskData *taskData, int bits)
{
    int result = 0;
    if (bits & POLL_BIT_PRI)
    {
        u_long atMark = 0;
        if (ioctlsocket(socket, SIOCATMARK, &atMark) != 0)
            raise_syscall(taskData, "ioctlsocket failed", GETERROR);
        if (atMark) { result |= POLL_BIT_PRI; }
    }
    if (bits & (POLL_BIT_IN | POLL_BIT_OUT))
    {
        FD_SET readFds, writeFds;
        TIMEVAL poll = { 0, 0 };
        FD_ZERO(&readFds); FD_ZERO(&writeFds);
        if (bits & POLL_BIT_IN) FD_SET(socket, &readFds);
        if (bits & POLL_BIT_OUT) FD_SET(socket, &writeFds);
        int selRes = select(FD_SETSIZE, &readFds, &writeFds, NULL, &poll);
        if (selRes < 0)
            raise_syscall(taskData, "select failed", GETERROR);
        else if (selRes > 0)
        {
            // N.B. select only tells us about out-of-band data if SO_OOBINLINE is FALSE. */
            if (FD_ISSET(socket, &readFds)) result |= POLL_BIT_IN;
            if (FD_ISSET(socket, &writeFds)) result |= POLL_BIT_OUT;
        }
    }
    return result;
}

static SOCKET getStreamSocket(TaskData *taskData, PolyWord strm)
{
    WinSocket *winskt = *(WinSocket**)(strm.AsObjPtr());
    if (winskt == 0)
        raise_syscall(taskData, "Stream is closed", STREAMCLOSED);
    return winskt->getSocket();
}

static Handle wrapStreamSocket(TaskData *taskData, SOCKET skt)
{
    try {
        WinSocket *winskt = new WinSocket(skt);
        return MakeVolatileWord(taskData, winskt);
    }
    catch (std::bad_alloc&) {
        raise_syscall(taskData, "Insufficient memory", NOMEMORY);
    }
}

#else

static SOCKET getStreamSocket(TaskData *taskData, PolyWord strm)
{
    return getStreamFileDescriptor(taskData, strm);
}

static Handle wrapStreamSocket(TaskData *taskData, SOCKET skt)
{
    return wrapFileDescriptor(taskData, skt);
}
#endif

static Handle makeProtoEntry(TaskData *taskData, struct protoent *proto)
{
    int i;
    char **p;
    Handle aliases, name, protocol, result;

    /* Canonical name. */
    name = SAVE(C_string_to_Poly(taskData, proto->p_name));

    /* Aliases. */
    for (i=0, p = proto->p_aliases; *p != NULL; p++, i++);
    aliases = convert_string_list(taskData, i, proto->p_aliases);

    /* Protocol number. */
    protocol = Make_fixed_precision(taskData, proto->p_proto);

    /* Make the result structure. */
    result = ALLOC(3);
    DEREFHANDLE(result)->Set(0, name->Word());
    DEREFHANDLE(result)->Set(1, aliases->Word());
    DEREFHANDLE(result)->Set(2, protocol->Word());
    return result;
}

static Handle makeServEntry(TaskData *taskData, struct servent *serv)
{
    int i;
    char **p;
    Handle aliases, name, protocol, result, port;

    /* Canonical name. */
    name = SAVE(C_string_to_Poly(taskData, serv->s_name));

    /* Aliases. */
    for (i=0, p = serv->s_aliases; *p != NULL; p++, i++);
    aliases = convert_string_list(taskData, i, serv->s_aliases);

    /* Port number. */
    port = Make_fixed_precision(taskData, ntohs(serv->s_port));

    /* Protocol name. */
    protocol = SAVE(C_string_to_Poly(taskData, serv->s_proto));

    /* Make the result structure. */
    result = ALLOC(4);
    DEREFHANDLE(result)->Set(0, name->Word());
    DEREFHANDLE(result)->Set(1, aliases->Word());
    DEREFHANDLE(result)->Set(2, port->Word());
    DEREFHANDLE(result)->Set(3, protocol->Word());
    return result;
}

static Handle mkAftab(TaskData *taskData, void *arg, char *p)
{
    struct af_tab_struct *af = (struct af_tab_struct *)p;
    Handle result, name, num;
    /* Construct a pair of the string and the number. */
    name = SAVE(C_string_to_Poly(taskData, af->af_name));
    num = Make_fixed_precision(taskData, af->af_num);
    result = ALLOC(2);
    DEREFHANDLE(result)->Set(0, name->Word());
    DEREFHANDLE(result)->Set(1, num->Word());
    return result;
}

static Handle mkSktab(TaskData *taskData, void *arg, char *p)
{
    struct sk_tab_struct *sk = (struct sk_tab_struct *)p;
    Handle result, name, num;
    /* Construct a pair of the string and the number. */
    name = SAVE(C_string_to_Poly(taskData, sk->sk_name));
    num = Make_fixed_precision(taskData, sk->sk_num);
    result = ALLOC(2);
    DEREFHANDLE(result)->Set(0, name->Word());
    DEREFHANDLE(result)->Set(1, num->Word());
    return result;
}

/* This sets an option and can also be used to set an integer. */
static Handle setSocketOption(TaskData *taskData, Handle sockHandle, Handle optHandle, int level, int opt)
{
    SOCKET sock = getStreamSocket(taskData, sockHandle->Word());
    int onOff = get_C_int(taskData, optHandle->Word());
    if (setsockopt(sock, level, opt,
        (char*)&onOff, sizeof(int)) != 0)
        raise_syscall(taskData, "setsockopt failed", GETERROR);
    return Make_fixed_precision(taskData, 0);
}

// Get a socket option as an integer.
static Handle getSocketOption(TaskData *taskData, Handle args, int level, int opt)
{
    SOCKET sock = getStreamSocket(taskData, args->Word());
    int optVal = 0;
    socklen_t size = sizeof(int);
    if (getsockopt(sock, level, opt, (char*)&optVal, &size) != 0)
        raise_syscall(taskData, "getsockopt failed", GETERROR);
    return Make_fixed_precision(taskData, optVal);
}

// Get and clear the error state for the socket.  Returns a SysWord.word value.
POLYUNSIGNED PolyNetworkGetSocketError(POLYUNSIGNED threadId, POLYUNSIGNED skt)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        SOCKET sock = getStreamSocket(taskData, PolyWord::FromUnsigned(skt));
        int intVal = 0;
        socklen_t size = sizeof(int);
        if (getsockopt(sock, SOL_SOCKET, SO_ERROR, (char*)&intVal, &size) != 0)
            raise_syscall(taskData, "getsockopt failed", GETERROR);
        result = Make_sysword(taskData, TRANSLATE_SO_ERROR(intVal));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Helper function for selectCall.  Creates the result vector of active sockets.
static bool testBit(int offset, SOCKET fd, WaitSelect *pSelect)
{
    switch (offset)
    {
    case 0: return pSelect->IsSetRead(fd);
    case 1: return pSelect->IsSetWrite(fd);
    case 2: return pSelect->IsSetExcept(fd);
    default: return false;
    }
}

static Handle getSelectResult(TaskData *taskData, Handle args, int offset, WaitSelect *pSelect)
{
    /* Construct the result vectors. */
    PolyObject *inVec = DEREFHANDLE(args)->Get(offset).AsObjPtr();
    POLYUNSIGNED nVec = inVec->Length();
    int nRes = 0;
    POLYUNSIGNED i;
    for (i = 0; i < nVec; i++) {
        SOCKET sock = getStreamSocket(taskData, inVec->Get(i));
        if (testBit(offset, sock, pSelect)) nRes++;
    }
    if (nRes == 0)
        return ALLOC(0); /* None - return empty vector. */
    else {
        Handle result = ALLOC(nRes);
        inVec = DEREFHANDLE(args)->Get(offset).AsObjPtr(); /* It could have moved as a result of a gc. */
        nRes = 0;
        for (i = 0; i < nVec; i++) {
            SOCKET sock = getStreamSocket(taskData, inVec->Get(i));
            if (testBit(offset, sock, pSelect))
                DEREFWORDHANDLE(result)->Set(nRes++, inVec->Get(i));
        }
        return result;
    }
}

/* Wrapper for "select" call.  The arguments are arrays of socket ids.  These arrays are
   updated so that "active" sockets are left unchanged and inactive sockets are set to
   minus one.  */
POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkSelect(POLYUNSIGNED threadId, POLYUNSIGNED fdVecTriple, POLYUNSIGNED maxMillisecs)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;
    POLYUNSIGNED maxMilliseconds = PolyWord::FromUnsigned(maxMillisecs).UnTaggedUnsigned();
    Handle fdVecTripleHandle = taskData->saveVec.push(fdVecTriple);
    /* Set up the bitmaps for the select call from the arrays. */

    try {
        WaitSelect waitSelect((unsigned int)maxMilliseconds);
        PolyObject *readVec = fdVecTripleHandle->WordP()->Get(0).AsObjPtr();
        PolyObject *writeVec = fdVecTripleHandle->WordP()->Get(1).AsObjPtr();
        PolyObject *excVec = fdVecTripleHandle->WordP()->Get(2).AsObjPtr();
        for (POLYUNSIGNED i = 0; i < readVec->Length(); i++)
            waitSelect.SetRead(getStreamSocket(taskData, readVec->Get(i)));
        for (POLYUNSIGNED i = 0; i < writeVec->Length(); i++)
            waitSelect.SetWrite(getStreamSocket(taskData, writeVec->Get(i)));
        for (POLYUNSIGNED i = 0; i < excVec->Length(); i++)
            waitSelect.SetExcept(getStreamSocket(taskData, excVec->Get(i)));

        // Do the select.  This may return immediately if the maximum time-out is short.
        processes->ThreadPauseForIO(taskData, &waitSelect);
        if (waitSelect.SelectResult() < 0)
            raise_syscall(taskData, "select failed", waitSelect.SelectError());

        // Construct the result vectors.
        Handle rdResult = getSelectResult(taskData, fdVecTripleHandle, 0, &waitSelect);
        Handle wrResult = getSelectResult(taskData, fdVecTripleHandle, 1, &waitSelect);
        Handle exResult = getSelectResult(taskData, fdVecTripleHandle, 2, &waitSelect);
        result = ALLOC(3);
        DEREFHANDLE(result)->Set(0, rdResult->Word());
        DEREFHANDLE(result)->Set(1, wrResult->Word());
        DEREFHANDLE(result)->Set(2, exResult->Word());
    }
    catch (KillException&) {
        processes->ThreadExit(taskData); // TestAnyEvents may test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();

}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkConnect(POLYUNSIGNED threadId, POLYUNSIGNED skt, POLYUNSIGNED addr)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    try {
        SOCKET sock = getStreamSocket(taskData, PolyWord::FromUnsigned(skt));
        PolyStringObject * psAddr = (PolyStringObject *)(PolyWord::FromUnsigned(addr).AsObjPtr());
        struct sockaddr *psock = (struct sockaddr *)&psAddr->chars;
        // Begin the connection.  The socket is always non-blocking so this will return immediately.
        if (connect(sock, psock, (int)psAddr->length) != 0)
            raise_syscall(taskData, "connect failed", GETERROR);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned(); // Always returns unit
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkAccept(POLYUNSIGNED threadId, POLYUNSIGNED skt)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        SOCKET sock = getStreamSocket(taskData, PolyWord::FromUnsigned(skt));
        struct sockaddr_storage resultAddr;
        socklen_t addrLen = sizeof(resultAddr);
        SOCKET resultSkt = accept(sock, (struct sockaddr*)&resultAddr, &addrLen);
        if (resultSkt == INVALID_SOCKET)
            raise_syscall(taskData, "accept failed", GETERROR);
        if (addrLen > sizeof(resultAddr)) addrLen = sizeof(resultAddr);
        Handle addrHandle = taskData->saveVec.push(C_string_to_Poly(taskData, (char*)&resultAddr, addrLen));
        // Return a pair of the new socket and the address.
        Handle resSkt = wrapStreamSocket(taskData, resultSkt);
        result = alloc_and_save(taskData, 2);
        result->WordP()->Set(0, resSkt->Word());
        result->WordP()->Set(1, addrHandle->Word());
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkSend(POLYUNSIGNED threadId, POLYUNSIGNED argsAsWord)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle args = taskData->saveVec.push(argsAsWord);
#if(defined(_WIN32) && ! defined(_CYGWIN))
    int sent = 0;
#else
    ssize_t sent = 0;
#endif

    try {
        SOCKET sock = getStreamSocket(taskData, DEREFHANDLE(args)->Get(0));
        PolyWord pBase = DEREFHANDLE(args)->Get(1);
        POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFHANDLE(args)->Get(2));
#if(defined(_WIN32) && ! defined(_CYGWIN))
        int length = get_C_int(taskData, DEREFHANDLE(args)->Get(3));
#else
        ssize_t length = getPolyUnsigned(taskData, DEREFHANDLE(args)->Get(3));
#endif
        unsigned int dontRoute = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(4));
        unsigned int outOfBand = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(5));
        int flags = 0;
        if (dontRoute != 0) flags |= MSG_DONTROUTE;
        if (outOfBand != 0) flags |= MSG_OOB;
        char *base = (char*)pBase.AsObjPtr()->AsBytePtr();
        sent = send(sock, base + offset, length, flags);
        if (sent == SOCKET_ERROR)
            raise_syscall(taskData, "send failed", GETERROR);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(sent).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkSendTo(POLYUNSIGNED threadId, POLYUNSIGNED argsAsWord)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle args = taskData->saveVec.push(argsAsWord);
#if(defined(_WIN32) && ! defined(_CYGWIN))
    int sent = 0;
#else
    ssize_t sent = 0;
#endif

    try {
        SOCKET sock = getStreamSocket(taskData, DEREFHANDLE(args)->Get(0));
        PolyStringObject * psAddr = (PolyStringObject *)args->WordP()->Get(1).AsObjPtr();
        PolyWord pBase = DEREFHANDLE(args)->Get(2);

        POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFHANDLE(args)->Get(3));
#if(defined(_WIN32) && ! defined(_CYGWIN))
        int length = get_C_int(taskData, DEREFHANDLE(args)->Get(4));
#else
        size_t length = getPolyUnsigned(taskData, DEREFHANDLE(args)->Get(4));
#endif
        unsigned int dontRoute = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(5));
        unsigned int outOfBand = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(6));
        int flags = 0;
        if (dontRoute != 0) flags |= MSG_DONTROUTE;
        if (outOfBand != 0) flags |= MSG_OOB;
        char *base = (char*)pBase.AsObjPtr()->AsBytePtr();
        sent = sendto(sock, base + offset, length, flags,
                (struct sockaddr *)psAddr->chars, (int)psAddr->length);
        if (sent == SOCKET_ERROR)
            raise_syscall(taskData, "sendto failed", GETERROR);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(sent).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkReceive(POLYUNSIGNED threadId, POLYUNSIGNED argsAsWord)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle args = taskData->saveVec.push(argsAsWord);
#if(defined(_WIN32) && ! defined(_CYGWIN))
    int recvd = 0;
#else
    ssize_t recvd = 0;
#endif

    try {
        SOCKET sock = getStreamSocket(taskData, DEREFHANDLE(args)->Get(0));
        char *base = (char*)DEREFHANDLE(args)->Get(1).AsObjPtr()->AsBytePtr();
        POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFHANDLE(args)->Get(2));
#if(defined(_WIN32) && ! defined(_CYGWIN))
        int length = get_C_int(taskData, DEREFHANDLE(args)->Get(3));
#else
        size_t length = getPolyUnsigned(taskData, DEREFHANDLE(args)->Get(3));
#endif
        unsigned int peek = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(4));
        unsigned int outOfBand = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(5));
        int flags = 0;
        if (peek != 0) flags |= MSG_PEEK;
        if (outOfBand != 0) flags |= MSG_OOB;

        recvd = recv(sock, base + offset, length, flags);
        if (recvd == SOCKET_ERROR)
            raise_syscall(taskData, "recv failed", GETERROR);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(recvd).AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkReceiveFrom(POLYUNSIGNED threadId, POLYUNSIGNED argsAsWord)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle args = taskData->saveVec.push(argsAsWord);
    Handle result = 0;

    try {
        SOCKET sock = getStreamSocket(taskData, DEREFHANDLE(args)->Get(0));
        char *base = (char*)DEREFHANDLE(args)->Get(1).AsObjPtr()->AsBytePtr();
        POLYUNSIGNED offset = getPolyUnsigned(taskData, DEREFHANDLE(args)->Get(2));
#if(defined(_WIN32) && ! defined(_CYGWIN))
        int length = get_C_int(taskData, DEREFHANDLE(args)->Get(3));
#else
        size_t length = getPolyUnsigned(taskData, DEREFHANDLE(args)->Get(3));
#endif
        unsigned int peek = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(4));
        unsigned int outOfBand = get_C_unsigned(taskData, DEREFHANDLE(args)->Get(5));
        int flags = 0;
        struct sockaddr_storage resultAddr;
        socklen_t addrLen = sizeof(resultAddr);

        if (peek != 0) flags |= MSG_PEEK;
        if (outOfBand != 0) flags |= MSG_OOB;

#if(defined(_WIN32) && ! defined(_CYGWIN))
        int recvd;
#else
        ssize_t recvd;
#endif
        recvd = recvfrom(sock, base + offset, length, flags, (struct sockaddr*)&resultAddr, &addrLen);
        if (recvd == SOCKET_ERROR)
            raise_syscall(taskData, "recvfrom failed", GETERROR);

        if (recvd > (int)length) recvd = length;
        Handle lengthHandle = Make_fixed_precision(taskData, recvd);
        if (addrLen > sizeof(resultAddr)) addrLen = sizeof(resultAddr);
        Handle addrHandle = SAVE(C_string_to_Poly(taskData, (char*)&resultAddr, addrLen));
        result = ALLOC(2);
        DEREFHANDLE(result)->Set(0, lengthHandle->Word());
        DEREFHANDLE(result)->Set(1, addrHandle->Word());
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return a list of known address families. */
POLYUNSIGNED PolyNetworkGetAddrList(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = makeList(taskData, sizeof(af_table) / sizeof(af_table[0]),
            (char*)af_table, sizeof(af_table[0]), 0, mkAftab);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Return a list of known socket types. */
POLYUNSIGNED PolyNetworkGetSockTypeList(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = makeList(taskData, sizeof(sk_table) / sizeof(sk_table[0]),
            (char*)sk_table, sizeof(sk_table[0]),
            0, mkSktab);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Create a socket */
POLYUNSIGNED PolyNetworkCreateSocket(POLYUNSIGNED threadId, POLYUNSIGNED family, POLYUNSIGNED st, POLYUNSIGNED prot)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;
    int af = (int)PolyWord::FromUnsigned(family).UnTagged();
    int type = (int)PolyWord::FromUnsigned(st).UnTagged();
    int proto = (int)PolyWord::FromUnsigned(prot).UnTagged();

    try {
        SOCKET skt = 0;
        do {
            skt = socket(af, type, proto);
        } while (skt == INVALID_SOCKET && GETERROR == CALLINTERRUPTED);

        if (skt == INVALID_SOCKET)
            raise_syscall(taskData, "socket failed", GETERROR);

        /* Set the socket to non-blocking mode. */
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        unsigned long onOff = 1;
        if (ioctlsocket(skt, FIONBIO, &onOff) != 0)
#else
        int onOff = 1;
        if (ioctl(skt, FIONBIO, &onOff) < 0)
#endif
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            closesocket(skt);
#else
            close(skt);
#endif
            raise_syscall(taskData, "ioctl failed", GETERROR);
        }
        result = wrapStreamSocket(taskData, skt);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkSetOption(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED sock, POLYUNSIGNED opt)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedSock = taskData->saveVec.push(sock);
    Handle pushedOpt = taskData->saveVec.push(opt);

    try {
        switch (UNTAGGED(PolyWord::FromUnsigned(code)))
        {
        case 15: /* Set TCP No-delay option. */
            setSocketOption(taskData, pushedSock, pushedOpt, IPPROTO_TCP, TCP_NODELAY);
            break;

        case 17: /* Set Debug option. */
            setSocketOption(taskData, pushedSock, pushedOpt, SOL_SOCKET, SO_DEBUG);
            break;

        case 19: /* Set REUSEADDR option. */
            setSocketOption(taskData, pushedSock, pushedOpt, SOL_SOCKET, SO_REUSEADDR);
            break;

        case 21: /* Set KEEPALIVE option. */
            setSocketOption(taskData, pushedSock, pushedOpt, SOL_SOCKET, SO_KEEPALIVE);
            break;

        case 23: /* Set DONTROUTE option. */
            setSocketOption(taskData, pushedSock, pushedOpt, SOL_SOCKET, SO_DONTROUTE);
            break;

        case 25: /* Set BROADCAST option. */
            setSocketOption(taskData, pushedSock, pushedOpt, SOL_SOCKET, SO_BROADCAST);
            break;

        case 27: /* Set OOBINLINE option. */
            setSocketOption(taskData, pushedSock, pushedOpt, SOL_SOCKET, SO_OOBINLINE);
            break;

        case 29: /* Set SNDBUF size. */
            setSocketOption(taskData, pushedSock, pushedOpt, SOL_SOCKET, SO_SNDBUF);
            break;

        case 31: /* Set RCVBUF size. */
            setSocketOption(taskData, pushedSock, pushedOpt, SOL_SOCKET, SO_RCVBUF);
            break;
        }
    }
    catch (KillException&) {
        processes->ThreadExit(taskData); // May test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetOption(POLYUNSIGNED threadId, POLYUNSIGNED code, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle pushedArg = taskData->saveVec.push(arg);
    Handle result = 0;

    try {
        switch (UNTAGGED(PolyWord::FromUnsigned(code)))
        {
        case 16: /* Get TCP No-delay option. */
            result = getSocketOption(taskData, pushedArg, IPPROTO_TCP, TCP_NODELAY);
            break;

        case 18: /* Get Debug option. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_DEBUG);
            break;

        case 20: /* Get REUSEADDR option. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_REUSEADDR);
            break;

        case 22: /* Get KEEPALIVE option. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_KEEPALIVE);
            break;

        case 24: /* Get DONTROUTE option. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_DONTROUTE);
            break;

        case 26: /* Get BROADCAST option. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_BROADCAST);
            break;

        case 28: /* Get OOBINLINE option. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_OOBINLINE);
            break;

        case 30: /* Get SNDBUF size. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_SNDBUF);
            break;

        case 32: /* Get RCVBUF size. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_RCVBUF);
            break;

        case 33: /* Get socket type e.g. SOCK_STREAM. */
            result = getSocketOption(taskData, pushedArg, SOL_SOCKET, SO_TYPE);
            break;
        }
    }
    catch (KillException&) {
        processes->ThreadExit(taskData); // May test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Set Linger time. */
POLYUNSIGNED PolyNetworkSetLinger(POLYUNSIGNED threadId, POLYUNSIGNED sock, POLYUNSIGNED lingerTime)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        SOCKET skt = getStreamSocket(taskData, PolyWord::FromUnsigned(sock));
        int lTime = get_C_int(taskData, PolyWord::FromUnsigned(lingerTime));
        struct linger linger;
        /* We pass in a negative value to turn the option off,
           zero or positive to turn it on. */
        if (lTime < 0)
        {
            linger.l_onoff = 0;
            linger.l_linger = 0;
        }
        else
        {
            linger.l_onoff = 1;
            linger.l_linger = lTime;
        }
        if (setsockopt(skt, SOL_SOCKET, SO_LINGER,
            (char*)& linger, sizeof(linger)) != 0)
            raise_syscall(taskData, "setsockopt failed", GETERROR);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

/* Get Linger time. */
POLYUNSIGNED PolyNetworkGetLinger(POLYUNSIGNED threadId, POLYUNSIGNED sock)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        SOCKET skt = getStreamSocket(taskData, PolyWord::FromUnsigned(sock));
        socklen_t size = sizeof(linger);
        int lTime = 0;
        struct linger linger;
        if (getsockopt(skt, SOL_SOCKET, SO_LINGER, (char*)& linger, &size) != 0)
            raise_syscall(taskData, "getsockopt failed", GETERROR);
        /* If the option is off return a negative. */
        if (linger.l_onoff == 0) lTime = -1;
        else lTime = linger.l_linger;
        result = Make_arbitrary_precision(taskData, lTime); // Returns LargeInt.int
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Get peer name. */
POLYUNSIGNED PolyNetworkGetPeerName(POLYUNSIGNED threadId, POLYUNSIGNED sock)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        SOCKET skt = getStreamSocket(taskData, PolyWord::FromUnsigned(sock));
        struct sockaddr_storage sockA;
        socklen_t size = sizeof(sockA);
        if (getpeername(skt, (struct sockaddr*) & sockA, &size) != 0)
            raise_syscall(taskData, "getpeername failed", GETERROR);
        if (size > sizeof(sockA)) size = sizeof(sockA);
        /* Addresses are treated as strings. */
        result = (SAVE(C_string_to_Poly(taskData, (char*)& sockA, size)));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Get socket name. */
POLYUNSIGNED PolyNetworkGetSockName(POLYUNSIGNED threadId, POLYUNSIGNED sock)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        SOCKET skt = getStreamSocket(taskData, PolyWord::FromUnsigned(sock));
        struct sockaddr_storage sockA;
        socklen_t   size = sizeof(sockA);
        if (getsockname(skt, (struct sockaddr*) & sockA, &size) != 0)
            raise_syscall(taskData, "getsockname failed", GETERROR);
        if (size > sizeof(sockA)) size = sizeof(sockA);
        result = (SAVE(C_string_to_Poly(taskData, (char*)& sockA, size)));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Find number of bytes available. */
POLYUNSIGNED PolyNetworkBytesAvailable(POLYUNSIGNED threadId, POLYUNSIGNED sock)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        SOCKET skt = getStreamSocket(taskData, PolyWord::FromUnsigned(sock));
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        unsigned long readable;
        if (ioctlsocket(skt, FIONREAD, &readable) != 0)
            raise_syscall(taskData, "ioctlsocket failed", GETERROR);
#else
        int readable;
        if (ioctl(skt, FIONREAD, &readable) < 0)
            raise_syscall(taskData, "ioctl failed", GETERROR);
#endif
        result = Make_fixed_precision(taskData, readable);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Find out if we are at the mark. */
POLYUNSIGNED PolyNetworkGetAtMark(POLYUNSIGNED threadId, POLYUNSIGNED sock)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        SOCKET skt = getStreamSocket(taskData, PolyWord::FromUnsigned(sock));
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        unsigned long atMark;
        if (ioctlsocket(skt, SIOCATMARK, &atMark) != 0)
            raise_syscall(taskData, "ioctlsocket failed", GETERROR);
#else
        int atMark;
        if (ioctl(skt, SIOCATMARK, &atMark) < 0)
            raise_syscall(taskData, "ioctl failed", GETERROR);
#endif
        result = Make_fixed_precision(taskData, atMark == 0 ? 0 : 1);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Bind an address to a socket. */
POLYUNSIGNED PolyNetworkBind(POLYUNSIGNED threadId, POLYUNSIGNED sock, POLYUNSIGNED addr)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        SOCKET skt = getStreamSocket(taskData, PolyWord::FromUnsigned(sock));
        PolyStringObject* psAddr = (PolyStringObject*)PolyWord::FromUnsigned(addr).AsObjPtr();
        struct sockaddr* psock = (struct sockaddr*) & psAddr->chars;
        if (bind(skt, psock, (int)psAddr->length) != 0)
            raise_syscall(taskData, "bind failed", GETERROR);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

/* Put socket into listening mode. */
POLYUNSIGNED PolyNetworkListen(POLYUNSIGNED threadId, POLYUNSIGNED skt, POLYUNSIGNED back)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
         SOCKET sock = getStreamSocket(taskData, PolyWord::FromUnsigned(skt));
         int backlog = get_C_int(taskData, PolyWord::FromUnsigned(back));
         if (listen(sock, backlog) != 0)
             raise_syscall(taskData, "listen failed", GETERROR);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

/* Shutdown the socket. */
POLYUNSIGNED PolyNetworkShutdown(POLYUNSIGNED threadId, POLYUNSIGNED skt, POLYUNSIGNED smode)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    try {
        SOCKET sock = getStreamSocket(taskData, PolyWord::FromUnsigned(skt));
        int mode = 0;
        switch (get_C_ulong(taskData, PolyWord::FromUnsigned(smode)))
        {
        case 1: mode = SHUT_RD; break;
        case 2: mode = SHUT_WR; break;
        case 3: mode = SHUT_RDWR;
        }
        if (shutdown(sock, mode) != 0)
            raise_syscall(taskData, "shutdown failed", GETERROR);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    return TAGGED(0).AsUnsigned();
}

/* Create a socket pair. */
POLYUNSIGNED PolyNetworkCreateSocketPair(POLYUNSIGNED threadId, POLYUNSIGNED family, POLYUNSIGNED st, POLYUNSIGNED prot)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        /* Not implemented. */
       raise_syscall(taskData, "socketpair not implemented", WSAEAFNOSUPPORT);
#else
        int af = PolyWord::FromUnsigned(family).UnTagged();
        int type = PolyWord::FromUnsigned(st).UnTagged();
        int proto = PolyWord::FromUnsigned(prot).UnTagged();
        SOCKET skt[2];
        int skPRes = 0;

        do {
            skPRes = socketpair(af, type, proto, skt);
        } while (skPRes != 0 && GETERROR == CALLINTERRUPTED);

        int onOff = 1;
        /* Set the sockets to non-blocking mode. */
        if (ioctl(skt[0], FIONBIO, &onOff) < 0 ||
            ioctl(skt[1], FIONBIO, &onOff) < 0)
        {
            close(skt[0]);
            close(skt[1]);
            raise_syscall(taskData, "ioctl failed", GETERROR);
        }
        Handle str_token1 = wrapStreamSocket(taskData, skt[0]);
        Handle str_token2 = wrapStreamSocket(taskData, skt[1]);
        /* Return the two streams as a pair. */
        result = ALLOC(2);
        DEREFHANDLE(result)->Set(0, DEREFWORD(str_token1));
        DEREFHANDLE(result)->Set(1, DEREFWORD(str_token2));
#endif
    }
    catch (KillException&) {
        processes->ThreadExit(taskData); // May test for kill
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Create a Unix socket address from a string. */
POLYUNSIGNED PolyNetworkUnixPathToSockAddr(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        /* Not implemented. */
        raise_syscall(taskData, "Unix addresses not implemented", WSAEAFNOSUPPORT);
#else
        struct sockaddr_un addr;
        memset(&addr, 0, sizeof(addr));
        addr.sun_family = AF_UNIX;
#ifdef HAVE_STRUCT_SOCKADDR_UN_SUN_LEN
        addr.sun_len = sizeof(addr); // Used in FreeBSD only.
#endif
        POLYUNSIGNED length = Poly_string_to_C(PolyWord::FromUnsigned(arg), addr.sun_path, sizeof(addr.sun_path));
        if (length > (int)sizeof(addr.sun_path))
            raise_syscall(taskData, "Address too long", ENAMETOOLONG);
        result = SAVE(C_string_to_Poly(taskData, (char*)& addr, sizeof(addr)));
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

/* Get the file name from a Unix socket address. */
POLYUNSIGNED PolyNetworkUnixSockAddrToPath(POLYUNSIGNED threadId, POLYUNSIGNED arg)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
        /* Not implemented. */
        raise_syscall(taskData, "Unix addresses not implemented", WSAEAFNOSUPPORT);
#else
        PolyStringObject* psAddr = (PolyStringObject*)PolyWord::FromUnsigned(arg).AsObjPtr();
        struct sockaddr_un* psock = (struct sockaddr_un*) & psAddr->chars;
        result = SAVE(C_string_to_Poly(taskData, psock->sun_path));
#endif
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetServByName(POLYUNSIGNED threadId, POLYUNSIGNED serviceName)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    /* Get service given service name only. */
    TempCString servName(Poly_string_to_C_alloc(PolyWord::FromUnsigned(serviceName)));
    struct servent *serv = getservbyname (servName, NULL);
    // If this fails the ML function returns NONE
    Handle result = serv == NULL ? 0 : makeServEntry(taskData, serv);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetServByNameAndProtocol(POLYUNSIGNED threadId, POLYUNSIGNED serviceName, POLYUNSIGNED protName)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    /* Get service given service name and protocol name. */
    TempCString servName(Poly_string_to_C_alloc(PolyWord::FromUnsigned(serviceName)));
    TempCString protoName(Poly_string_to_C_alloc(PolyWord::FromUnsigned(protName)));
    struct servent *serv = getservbyname (servName, protoName);
    Handle result = serv == NULL ? 0 : makeServEntry(taskData, serv);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetServByPort(POLYUNSIGNED threadId, POLYUNSIGNED portNo)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    /* Get service given port number only. */
    long port = htons(get_C_ushort(taskData, PolyWord::FromUnsigned(portNo)));
    struct servent *serv = getservbyport(port, NULL);
    Handle result = serv == NULL ? 0 : makeServEntry(taskData, serv);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetServByPortAndProtocol(POLYUNSIGNED threadId, POLYUNSIGNED portNo, POLYUNSIGNED protName)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    /* Get service given port number and protocol name. */
    long port = htons(get_C_ushort(taskData, PolyWord::FromUnsigned(portNo)));
    TempCString protoName(Poly_string_to_C_alloc(PolyWord::FromUnsigned(protName)));
    struct servent *serv = getservbyport (port, protoName);
    Handle result = serv == NULL ? 0 : makeServEntry(taskData, serv);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetProtByName(POLYUNSIGNED threadId, POLYUNSIGNED protocolName)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    /* Look up protocol entry. */
    TempCString protoName(Poly_string_to_C_alloc(PolyWord::FromUnsigned(protocolName)));
    struct protoent *proto = getprotobyname(protoName);
    // If this fails the ML function returns NONE
    Handle result = proto == NULL ? 0 : makeProtoEntry(taskData, proto);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetProtByNo(POLYUNSIGNED threadId, POLYUNSIGNED protoNo)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();

    /* Look up protocol entry. */
    int pNum = get_C_int(taskData, PolyWord::FromUnsigned(protoNo));
    struct protoent *proto = getprotobynumber(pNum);
    Handle result = proto == NULL ? 0 : makeProtoEntry(taskData, proto);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetHostName(POLYUNSIGNED threadId)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try { /* Get the current host name. */
        // Since the maximum length of a FQDN is 256 bytes it should fit in the buffer.
#ifdef HOST_NAME_MAX
        char hostName[HOST_NAME_MAX+1];
#else
        char hostName[1024];
#endif
        int err = gethostname(hostName, sizeof(hostName));
        if (err != 0)
            raise_syscall(taskData, "gethostname failed", GETERROR);
        // Add a null at the end just in case.  See gethostname man page.
        hostName[sizeof(hostName) - 1] = 0;

        result = SAVE(C_string_to_Poly(taskData, hostName));
    }
    catch (...) { } // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkGetNameInfo(POLYUNSIGNED threadId, POLYUNSIGNED sockAddr)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        PolyStringObject* psAddr = (PolyStringObject*)PolyWord::FromUnsigned(sockAddr).AsObjPtr();
        struct sockaddr* psock = (struct sockaddr*) & psAddr->chars;
        // Since the maximum length of a FQDN is 256 bytes it should fit in the buffer.
        char hostName[1024];
        int gniRes = getnameinfo(psock, (socklen_t)psAddr->length, hostName, sizeof(hostName), NULL, 0, 0);
        if (gniRes != 0)
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            raise_syscall(taskData, "getnameinfo failed", GETERROR);
#else
            if (gniRes == EAI_SYSTEM)
                raise_syscall(taskData, "getnameinfo failed", GETERROR);
            else raise_syscall(taskData, gai_strerror(gniRes), 0);
#endif
        }
        result = SAVE(C_string_to_Poly(taskData, hostName));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Copy addrInfo data into ML memory.  We copy this although most of it
// is currently unused.
static Handle extractAddrInfo(TaskData *taskData, struct addrinfo *ainfo)
{
    if (ainfo == 0)
        return taskData->saveVec.push(ListNull);

    Handle reset = taskData->saveVec.mark();
    Handle tail = extractAddrInfo(taskData, ainfo->ai_next);
    Handle name = 0;
    // Only the first entry may have a canonical name.
    if (ainfo->ai_canonname == 0)
        name = taskData->saveVec.push(C_string_to_Poly(taskData, ""));
    else name = taskData->saveVec.push(C_string_to_Poly(taskData, ainfo->ai_canonname));

    Handle address = taskData->saveVec.push(C_string_to_Poly(taskData, (char*)ainfo->ai_addr, ainfo->ai_addrlen));

    Handle value = alloc_and_save(taskData, 6);
    value->WordP()->Set(0, TAGGED(ainfo->ai_flags));
    value->WordP()->Set(1, TAGGED(ainfo->ai_family));
    value->WordP()->Set(2, TAGGED(ainfo->ai_socktype));
    value->WordP()->Set(3, TAGGED(ainfo->ai_protocol));
    value->WordP()->Set(4, address->Word());
    value->WordP()->Set(5, name->Word());

    ML_Cons_Cell *next = (ML_Cons_Cell*)alloc(taskData, SIZEOF(ML_Cons_Cell));
    next->h = value->Word();
    next->t = tail->Word();

    taskData->saveVec.reset(reset);
    return taskData->saveVec.push(next);
}

POLYUNSIGNED PolyNetworkGetAddrInfo(POLYUNSIGNED threadId, POLYUNSIGNED hName, POLYUNSIGNED addrFamily)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;
    struct addrinfo *resAddr = 0;

    try {
        TempCString hostName(Poly_string_to_C_alloc(PolyWord::FromUnsigned(hName)));
        struct addrinfo hints;
        memset(&hints, 0, sizeof(hints));
        hints.ai_family = (int)UNTAGGED(PolyWord::FromUnsigned(addrFamily)); // AF_INET or AF_INET6 or, possibly, AF_UNSPEC.
        hints.ai_flags = AI_CANONNAME;

        int gaiRes = getaddrinfo(hostName, 0, &hints, &resAddr);
        if (gaiRes != 0)
        {
#if (defined(_WIN32) && ! defined(__CYGWIN__))
            raise_syscall(taskData, "getaddrinfo failed", GETERROR);
#else
            if (gaiRes == EAI_SYSTEM)
                raise_syscall(taskData, "getnameinfo failed", GETERROR);
            else raise_syscall(taskData, gai_strerror(gaiRes), 0);
#endif
        }

        result = extractAddrInfo(taskData, resAddr);
    }
    catch (...) { } // Could raise an exception if we run out of heap space

    if (resAddr) freeaddrinfo(resAddr);

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYUNSIGNED PolyNetworkCloseSocket(POLYUNSIGNED threadId, POLYUNSIGNED strm)
{
    TaskData *taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;
    Handle pushedStream = taskData->saveVec.push(strm);

    try {
        // This is defined to raise an exception if the socket has already been closed
#if (defined(_WIN32))
        WinSocket *winskt = *(WinSocket**)(pushedStream->WordP());
        if (winskt != 0)
        {
            if (closesocket(winskt->getSocket()) != 0)
                raise_syscall(taskData, "Error during close", GETERROR);
        }
        else raise_syscall(taskData, "Socket is closed", WSAEBADF);
        *(WinSocket **)(pushedStream->WordP()) = 0; // Mark as closed
#else
        int descr = getStreamFileDescriptorWithoutCheck(pushedStream->Word());
        if (descr >= 0)
        {
            if (close(descr) != 0)
                raise_syscall(taskData, "Error during close", GETERROR);
        }
        else raise_syscall(taskData, "Socket is closed", EBADF);
        *(int*)(pushedStream->WordP()) = 0; // Mark as closed
#endif
        result = Make_fixed_precision(taskData, 0);
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Return the family 
POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetFamilyFromAddress(POLYUNSIGNED sockAddress)
{
    PolyStringObject* psAddr = (PolyStringObject*)PolyWord::FromUnsigned(sockAddress).AsObjPtr();
    struct sockaddr* psock = (struct sockaddr*) & psAddr->chars;
    return TAGGED(psock->sa_family).AsUnsigned();
}

// Return internet address and port from an internet socket address.
// Assumes that we've already checked the address family.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetAddressAndPortFromIP4(POLYUNSIGNED threadId, POLYUNSIGNED sockAddress)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        PolyStringObject* psAddr = (PolyStringObject*)PolyWord::FromUnsigned(sockAddress).AsObjPtr();
        struct sockaddr_in* psock = (struct sockaddr_in*) & psAddr->chars;
        Handle ipAddr = Make_arbitrary_precision(taskData, ntohl(psock->sin_addr.s_addr)); // IPv4 addr is LargeInt.int
        result = alloc_and_save(taskData, 2);
        result->WordP()->Set(0, ipAddr->Word());
        result->WordP()->Set(1, TAGGED(ntohs(psock->sin_port)));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Create a socket address from a port number and internet address.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkCreateIP4Address(POLYUNSIGNED threadId, POLYUNSIGNED ip4Address, POLYUNSIGNED portNumber)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        struct sockaddr_in sockaddr;
        memset(&sockaddr, 0, sizeof(sockaddr));
        sockaddr.sin_family = AF_INET;
        sockaddr.sin_port = htons(get_C_ushort(taskData, PolyWord::FromUnsigned(portNumber)));
        sockaddr.sin_addr.s_addr = htonl(get_C_unsigned(taskData, PolyWord::FromUnsigned(ip4Address)));
        result = SAVE(C_string_to_Poly(taskData, (char*)&sockaddr, sizeof(sockaddr)));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Return the value of INADDR_ANY.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkReturnIP4AddressAny(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = Make_arbitrary_precision(taskData, INADDR_ANY); // IPv4 addr is LargeInt.int
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkGetAddressAndPortFromIP6(POLYUNSIGNED threadId, POLYUNSIGNED sockAddress)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        PolyStringObject* psAddr = (PolyStringObject*)PolyWord::FromUnsigned(sockAddress).AsObjPtr();
        if (psAddr->length != sizeof(struct sockaddr_in6))
            raise_fail(taskData, "Invalid length");
        struct sockaddr_in6* psock = (struct sockaddr_in6*) & psAddr->chars;
        Handle ipAddr = SAVE(C_string_to_Poly(taskData, (const char*)&psock->sin6_addr, sizeof(struct in6_addr)));
        result = alloc_and_save(taskData, 2);
        result->WordP()->Set(0, ipAddr->Word());
        result->WordP()->Set(1, TAGGED(ntohs(psock->sin6_port)));

    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkCreateIP6Address(POLYUNSIGNED threadId, POLYUNSIGNED ip6Address, POLYUNSIGNED portNumber)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        struct sockaddr_in6 addr;
        memset(&addr, 0, sizeof(addr));
        result = SAVE(C_string_to_Poly(taskData, (const char*)&addr, sizeof(struct in6_addr)));
        addr.sin6_family = AF_INET6;
        addr.sin6_port = htons(get_C_ushort(taskData, PolyWord::FromUnsigned(portNumber)));
        PolyStringObject* addrAsString = (PolyStringObject*)PolyWord::FromUnsigned(ip6Address).AsObjPtr();
        if (addrAsString->length != sizeof(addr.sin6_addr))
            raise_fail(taskData, "Invalid address length");
        memcpy(&addr.sin6_addr, addrAsString->chars, sizeof(addr.sin6_addr));
        result = SAVE(C_string_to_Poly(taskData, (char*)&addr, sizeof(addr)));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkReturnIP6AddressAny(POLYUNSIGNED threadId)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        result = SAVE(C_string_to_Poly(taskData, (const char*)&in6addr_any, sizeof(struct in6_addr)));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Convert an IPV6 address to string.  This could be done in ML but the rules
// for converting zeros to double-colon are complicated.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkIP6AddressToString(POLYUNSIGNED threadId, POLYUNSIGNED ip6Address)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        char buffer[80]; // 40 should actually be enough: 32 hex bytes, 7 colons and a null.
        PolyStringObject* addrAsString = (PolyStringObject*)PolyWord::FromUnsigned(ip6Address).AsObjPtr();
        if (addrAsString->length != sizeof(struct in6_addr))
            raise_fail(taskData, "Invalid address length");
        if (inet_ntop(AF_INET6, addrAsString->chars, buffer, sizeof(buffer)) == 0)
            raise_syscall(taskData, "inet_ntop", GETERROR);
        result = SAVE(C_string_to_Poly(taskData, buffer));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

// Convert a string to an IPv6 address.  The parsing has to be done in ML.
POLYEXTERNALSYMBOL POLYUNSIGNED PolyNetworkStringToIP6Address(POLYUNSIGNED threadId, POLYUNSIGNED stringRep)
{
    TaskData* taskData = TaskData::FindTaskForId(threadId);
    ASSERT(taskData != 0);
    taskData->PreRTSCall();
    Handle reset = taskData->saveVec.mark();
    Handle result = 0;

    try {
        struct in6_addr address;
        TempCString stringAddr(Poly_string_to_C_alloc(PolyWord::FromUnsigned(stringRep)));
        if (inet_pton(AF_INET6, stringAddr, &address) != 1)
            raise_fail(taskData, "Invalid IPv6 address");
        result = taskData->saveVec.push(C_string_to_Poly(taskData, (const char *)&address, sizeof(struct in6_addr)));
    }
    catch (...) {} // If an ML exception is raised

    taskData->saveVec.reset(reset);
    taskData->PostRTSCall();
    if (result == 0) return TAGGED(0).AsUnsigned();
    else return result->Word().AsUnsigned();
}

struct _entrypts networkingEPT[] =
{
    { "PolyNetworkGetAddrList",                 (polyRTSFunction)&PolyNetworkGetAddrList},
    { "PolyNetworkGetSockTypeList",             (polyRTSFunction)&PolyNetworkGetSockTypeList},
    { "PolyNetworkCreateSocket",                (polyRTSFunction)&PolyNetworkCreateSocket},
    { "PolyNetworkSetOption",                   (polyRTSFunction)&PolyNetworkSetOption},
    { "PolyNetworkGetOption",                   (polyRTSFunction)&PolyNetworkGetOption},
    { "PolyNetworkSetLinger",                   (polyRTSFunction)&PolyNetworkSetLinger},
    { "PolyNetworkGetLinger",                   (polyRTSFunction)&PolyNetworkGetLinger},
    { "PolyNetworkGetPeerName",                 (polyRTSFunction)&PolyNetworkGetPeerName},
    { "PolyNetworkGetSockName",                 (polyRTSFunction)&PolyNetworkGetSockName},
    { "PolyNetworkBytesAvailable",              (polyRTSFunction)&PolyNetworkBytesAvailable},
    { "PolyNetworkGetAtMark",                   (polyRTSFunction)&PolyNetworkGetAtMark},
    { "PolyNetworkBind",                        (polyRTSFunction)&PolyNetworkBind},
    { "PolyNetworkListen",                      (polyRTSFunction)&PolyNetworkListen},
    { "PolyNetworkShutdown",                    (polyRTSFunction)&PolyNetworkShutdown},
    { "PolyNetworkCreateSocketPair",            (polyRTSFunction)&PolyNetworkCreateSocketPair},
    { "PolyNetworkUnixPathToSockAddr",          (polyRTSFunction)&PolyNetworkUnixPathToSockAddr},
    { "PolyNetworkUnixSockAddrToPath",          (polyRTSFunction)&PolyNetworkUnixSockAddrToPath},
    { "PolyNetworkGetServByName",               (polyRTSFunction)&PolyNetworkGetServByName},
    { "PolyNetworkGetServByNameAndProtocol",    (polyRTSFunction)&PolyNetworkGetServByNameAndProtocol},
    { "PolyNetworkGetServByPort",               (polyRTSFunction)&PolyNetworkGetServByPort},
    { "PolyNetworkGetServByPortAndProtocol",    (polyRTSFunction)&PolyNetworkGetServByPortAndProtocol},
    { "PolyNetworkGetProtByName",               (polyRTSFunction)&PolyNetworkGetProtByName},
    { "PolyNetworkGetProtByNo",                 (polyRTSFunction)&PolyNetworkGetProtByNo},
    { "PolyNetworkGetHostName",                 (polyRTSFunction)&PolyNetworkGetHostName},
    { "PolyNetworkGetNameInfo",                 (polyRTSFunction)&PolyNetworkGetNameInfo},
    { "PolyNetworkCloseSocket",                 (polyRTSFunction)&PolyNetworkCloseSocket },
    { "PolyNetworkSelect",                      (polyRTSFunction)&PolyNetworkSelect },
    { "PolyNetworkGetSocketError",              (polyRTSFunction)&PolyNetworkGetSocketError },
    { "PolyNetworkConnect",                     (polyRTSFunction)&PolyNetworkConnect },
    { "PolyNetworkAccept",                      (polyRTSFunction)&PolyNetworkAccept },
    { "PolyNetworkSend",                        (polyRTSFunction)&PolyNetworkSend },
    { "PolyNetworkSendTo",                      (polyRTSFunction)&PolyNetworkSendTo },
    { "PolyNetworkReceive",                     (polyRTSFunction)&PolyNetworkReceive },
    { "PolyNetworkReceiveFrom",                 (polyRTSFunction)&PolyNetworkReceiveFrom },
    { "PolyNetworkGetAddrInfo",                 (polyRTSFunction)&PolyNetworkGetAddrInfo },
    { "PolyNetworkGetFamilyFromAddress",        (polyRTSFunction)&PolyNetworkGetFamilyFromAddress },
    { "PolyNetworkGetAddressAndPortFromIP4",    (polyRTSFunction)&PolyNetworkGetAddressAndPortFromIP4 },
    { "PolyNetworkCreateIP4Address",            (polyRTSFunction)&PolyNetworkCreateIP4Address },
    { "PolyNetworkReturnIP4AddressAny",         (polyRTSFunction)&PolyNetworkReturnIP4AddressAny },
    { "PolyNetworkGetAddressAndPortFromIP6",    (polyRTSFunction)&PolyNetworkGetAddressAndPortFromIP6 },
    { "PolyNetworkCreateIP6Address",            (polyRTSFunction)&PolyNetworkCreateIP6Address },
    { "PolyNetworkReturnIP6AddressAny",         (polyRTSFunction)&PolyNetworkReturnIP6AddressAny },
    { "PolyNetworkIP6AddressToString",          (polyRTSFunction)&PolyNetworkIP6AddressToString },
    { "PolyNetworkStringToIP6Address",          (polyRTSFunction)&PolyNetworkStringToIP6Address },

    { NULL, NULL} // End of list.
};

class Networking: public RtsModule
{
public:
    virtual void Init(void);
    virtual void Stop(void);
};

// Declare this.  It will be automatically added to the table.
static Networking networkingModule;

void Networking::Init(void)
{
#if (defined(_WIN32))
#define WINSOCK_MAJOR_VERSION   2
#define WINSOCK_MINOR_VERSION   2
    WSADATA wsaData;
    WORD wVersion = MAKEWORD(WINSOCK_MINOR_VERSION, WINSOCK_MAJOR_VERSION);
    /* Initialise the system and check that the version it supplied
       is the one we requested. */
    if(WSAStartup(wVersion, &wsaData) == 0)
    {
        if (wsaData.wVersion == wVersion)
            winsock_init = 1;
        else WSACleanup();
    }
#endif
}

void Networking::Stop(void)
{
#if (defined(_WIN32))
    if (winsock_init) WSACleanup();
    winsock_init = 0;
#endif
}
