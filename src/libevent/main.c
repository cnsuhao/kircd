#include <stdio.h>
#include "network/network.h"

void readline_cb(int handle, const char *line)
{
	printf("%p line: %s\n", (void *)handle, line);
	network_write_line(handle, "back\r\n");
}

void close_cb(int handle)
{
	printf("%p close\n", (void *)handle);
}

int main()
{
	network_init(6666);
	network_set_readline_cb(readline_cb);
	network_set_close_cb(close_cb);

	network_run();
    return 0;
}

