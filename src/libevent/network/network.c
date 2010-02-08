#include <event.h>		// libevent
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "network.h"
#include "ksocket.h"

struct network_data {
	socket_readline_callback* readline_cb;
	socket_close_callback* close_cb;
	int listen_fd;
} g_data = { NULL, NULL, 0 };


// ------- internal callback --------

static void
network_error_callback(struct bufferevent *bufev, short what, void *arg)
{
	int fd;

	// if (what & (EVBUFFER_ERROR|EVBUFFER_EOF))
	fd = (int)arg;
	if ( g_data.close_cb )
		g_data.close_cb((int)bufev);
	printf("err or eof, fd = %d\n", fd);
	KSOCK_svr_close(fd);
}

static void
network_read_callback(struct bufferevent *bufev, void *arg)
{
	char *line;
	while (1)
	{
		line = evbuffer_readline(EVBUFFER_INPUT(bufev));
		if ( line == NULL )
			break;
		if ( g_data.readline_cb )
			g_data.readline_cb((int)bufev, line);
		free(line);
	}
}

static void 
network_listen_callback(int listen_fd, short events, void *arg)
{
	int fd = KSOCK_svr_accept(listen_fd);
	struct bufferevent *bufev;

	printf("fd %d conn...\n", fd);

	bufev = bufferevent_new(fd, network_read_callback,
		NULL, network_error_callback, (void *)fd);
	bufferevent_enable(bufev, EV_READ);
}


// ------- network init/shutdown --------

void network_init(unsigned short port)
{
	struct KSOCK_svr_opt opt;
    struct event *ev;
	int listen_fd;

	opt.addr    = KSOCK_INADDR_ANY;
	opt.port    = port;
	opt.backlog = 5;
	listen_fd = KSOCK_svr_listen_tcp(&opt);

	event_init();
	ev = malloc(sizeof(struct event));
	event_set(ev, listen_fd, EV_READ|EV_PERSIST,
				network_listen_callback, ev);
	event_add(ev, NULL);

	g_data.listen_fd = listen_fd;
	printf("listen port: %d\n", port);
}

void network_run()
{
	event_dispatch();
}


// ------- network callback --------

void network_set_readline_cb(socket_readline_callback *readline_cb)
{
	g_data.readline_cb = readline_cb;
}

void network_set_close_cb(socket_close_callback *close_cb)
{
	g_data.close_cb = close_cb;
}

void network_write_data(int handle, const char *data, unsigned int size)
{
	struct bufferevent *bufev = (struct bufferevent *) handle;
	bufferevent_write(bufev, data, size);
}

void network_write_line(int handle, const char *line)
{
	network_write_data(handle, line, strlen(line));
}

