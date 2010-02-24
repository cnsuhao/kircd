#include <assert.h>
#include <stdlib.h>

#include "common/khashmap.h"
#include "user.h"
#include "usermgr.h"

#define INVALID_USER        (struct kirc_user *)(-1)
#define MAX_FD2USER_CNT     (1024*10)
static struct kirc_user *fd2user_map[MAX_FD2USER_CNT];
static struct khashmap *nick2user_map;

void
kirc_usermgr_init()
{
    int i;
    for ( i = 0; i < MAX_FD2USER_CNT; i++ )
    {
        fd2user_map[i] = INVALID_USER;
    }

    nick2user_map = khashmap_new();
}

void
kirc_usermgr_shutdown()
{
    int i;
    for ( i = 0; i < MAX_FD2USER_CNT; i++ )
    {
        if ( fd2user_map[i] != INVALID_USER )
            kirc_user_free(fd2user_map[i]);
    }

    khashmap_free(nick2user_map);
}


// ---------------- get & set ------------------

struct kirc_user *
kirc_usermgr_get_user_by_socket(int fd)
{
    assert(fd2user_map[fd] != INVALID_USER);
    return fd2user_map[fd];
}

void
kirc_usermgr_set_user_by_socket(int fd, struct kirc_user *user)
{
    assert(fd2user_map[fd] == INVALID_USER);
    assert(user != NULL);
    fd2user_map[fd] = user;
}

void
kirc_usermgr_remove_user_by_socket(int fd)
{
    assert(fd2user_map[fd] != INVALID_USER);
    fd2user_map[fd] = INVALID_USER;
}


struct kirc_user * 
kirc_usermgr_get_user_by_nickname(const char *nickname)
{
    assert(khashmap_find(nick2user_map, nickname) != NULL);
    return (struct kirc_user *) khashmap_find(nick2user_map, nickname);
}

void
kirc_usermgr_set_user_by_nickname(const char *nickname, struct kirc_user *user)
{
    assert(khashmap_find(nick2user_map, nickname) == NULL);
    khashmap_insert(nick2user_map, nickname, user);
}

void
kirc_usermgr_remove_user_by_nickname(const char *nickname)
{
    assert(khashmap_find(nick2user_map, nickname) != NULL);
    khashmap_remove(nick2user_map, nickname);
}

