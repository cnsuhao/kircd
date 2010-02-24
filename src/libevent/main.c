#include <stdio.h>
#include "common/kstring.h"
#include "network/network.h"
#include "irc/cmd.h"
#include "irc/user.h"
#include "irc/usermgr.h"

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


int main()
{
    struct kirc_user *user, *u2;

    user = kirc_user_new();
    printf("socket1 = %d\n", kirc_user_get_integer(user, "socket"));
    kirc_user_set_integer(user, "socket", 10);
    printf("socket2 = %d\n", kirc_user_get_integer(user, "socket"));

    kirc_user_set_string(user, "roja", "hello");
    printf("roja = %s\n", kstring_cstr(kirc_user_get_string(user, "roja")));

    kirc_usermgr_init();

    kirc_usermgr_set_user_by_socket(1, user);
    u2 = kirc_usermgr_get_user_by_socket(1);
    printf("socket3 = %d\n", kirc_user_get_integer(user, "socket"));

    kirc_usermgr_set_user_by_nickname("kasicass", user);
    u2 = kirc_usermgr_get_user_by_nickname("kasicass");
    printf("roja2 = %s\n", kstring_cstr(kirc_user_get_string(user, "roja")));
    

    kirc_usermgr_shutdown();

/*
    int i;

    for ( i = 0; i < NELEMS(all_cmd_handler); i++ )
    {
        kirc_cmd_add_handler(all_cmd_handler[i].name, all_cmd_handler[i].fn);
    }

	network_init(6667);
	network_set_readline_cb(readline_cb);
	network_set_close_cb(close_cb);

	network_run();
*/
    return 0;
}

