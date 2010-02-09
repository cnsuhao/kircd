#include <stdio.h>
#include "common/kstring.h"
#include "network/network.h"
#include "irc/cmd.h"

void readline_cb(int handle, const char *line)
{
	printf("%p line: %s\n", (void *)handle, line);
	network_write_line(handle, "back\r\n");
}

void close_cb(int handle)
{
	printf("%p close\n", (void *)handle);
}


void foo_handler(struct kirc_cmd* kcmd)
{
	printf("nparam = %u\n", kirc_cmd_nparam(kcmd));
	printf("param0 = %s\n", kstring_cstr(kirc_cmd_get_param(kcmd, 0)));
}

int main()

{
/*
	network_init(6666);
	network_set_readline_cb(readline_cb);
	network_set_close_cb(close_cb);

	network_run();
*/

	struct kirc_cmd *kcmd;

	kirc_cmd_add_handler("PASS", foo_handler);

	kcmd = kirc_cmd_new("PASS a b  c");
	kirc_cmd_dispatch(kcmd);
	kirc_cmd_free(kcmd);
    return 0;
}

