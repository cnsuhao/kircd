#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>		// memset
#include <stdio.h>
#include "ksocket.h"

static int
KSOCK_set_addr_reuse(int fd)
{
	int reuseaddr_on = 1;
	return setsockopt(fd, SOL_SOCKET, SO_REUSEADDR,
			(const char *)&reuseaddr_on, sizeof(reuseaddr_on));
}

static int
KSOCK_set_nonblock(int fd)
{
	int flags;

	flags = fcntl(fd, F_GETFL);
	if (flags < 0)
		goto label_nonblock_error;
	flags |= O_NONBLOCK;
	if (fcntl(fd, F_SETFL, flags) < 0)
		goto label_nonblock_error;

	return 0;

label_nonblock_error:
	return -1;
}

int
KSOCK_svr_listen_tcp(struct KSOCK_svr_opt *svr_opt)
{
	struct sockaddr_in in_addr;
	int fd;

	fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if ( fd == -1 )
		return -1;

	if ( KSOCK_set_addr_reuse(fd) == -1 )
	{
		close(fd);
		return -1;
	}

	memset(&in_addr, 0, sizeof(in_addr));
	in_addr.sin_family      = AF_INET;
	in_addr.sin_addr.s_addr = (strcmp(svr_opt->addr, KSOCK_INADDR_ANY) == 0) ? htonl(INADDR_ANY) : inet_addr(svr_opt->addr);
	in_addr.sin_port        = htons(svr_opt->port);

	if ( bind(fd, (struct sockaddr *) &in_addr, sizeof(in_addr)) == -1 )
	{
		perror("abc");
		close(fd);
		return -1;
	}

	if ( listen(fd, svr_opt->backlog) == -1 )
	{
		close(fd);
		return -1;
	}

	if ( KSOCK_set_nonblock(fd) == -1 )
	{
		close(fd);
		return -1;
	}

	return fd;
}

int
KSOCK_svr_accept(int listenfd)
{
	return accept(listenfd, NULL, NULL);
}

void
KSOCK_svr_close(int fd)
{
	close(fd);
}

