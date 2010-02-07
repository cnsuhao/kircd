#ifndef KCODE_NETWORK_NETWORK_H
#define KCODE_NETWORK_NETWORK_H

typedef void (socket_readline_callback)(int handle, const char *line);
typedef void (socket_close_callback)(int handle);

void network_set_readline_cb(socket_readline_callback *readline_cb);
void network_set_close_cb(socket_close_callback *close_cb);
void network_write_line(int handle, const char *line);

void network_init(unsigned short port);
void network_run();
void network_shutdown();

#endif
