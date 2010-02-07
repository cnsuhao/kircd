#ifndef KCODE_NETWORK_SOCKET_H
#define KCODE_NETWORK_SOCKET_H

struct KSOCK_svr_opt {
	const char *addr;
	unsigned short port;
	int backlog;
};

#define	KSOCK_INADDR_ANY		"KSOCK_INADDR_ANY"

// return fd if ok
// or -1 when fail
int KSOCK_svr_listen_tcp(struct KSOCK_svr_opt *svr_opt);

// return fd if ok
// or -1 when fail
int KSOCK_svr_accept(int fd);

void KSOCK_svr_close(int fd);

#endif
