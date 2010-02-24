#include <stdio.h>
#include "common/kstring.h"
#include "network/network.h"
#include "irc/cmd.h"

#define NELEMS(arr)             (sizeof(arr) / sizeof(arr[0]))
#define DECLARE_CMD(fn_name)    void fn_name(struct kirc_cmd* kcmd)

DECLARE_CMD(k_cmd_nick);

struct {
    const char *name;
    kirc_cmd_handler fn;
} all_cmd_handler[] = {
    { "NICK", k_cmd_nick, },
};

void readline_cb(int handle, const char *line)
{
	struct kirc_cmd *kcmd;
	printf("%p line: %s\n", (void *)handle, line);
	kcmd = kirc_cmd_new(line);
	kirc_cmd_dispatch(kcmd);
	kirc_cmd_free(kcmd);
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
    int i;

    for ( i = 0; i < NELEMS(all_cmd_handler); i++ )
    {
        kirc_cmd_add_handler(all_cmd_handler[i].name, all_cmd_handler[i].fn);
    }

	network_init(6667);
	network_set_readline_cb(readline_cb);
	network_set_close_cb(close_cb);

	network_run();
    return 0;
}

