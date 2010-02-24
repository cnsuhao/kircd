#include <stdio.h>
#include "common/kstring.h"
#include "irc/cmd.h"

void k_cmd_nick(struct kirc_cmd* kcmd)
{
    printf("your nickname: %s\n", kstring_cstr(kirc_cmd_get_param(kcmd, 1)));
}

