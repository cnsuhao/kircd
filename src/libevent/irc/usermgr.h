// socket ==> user
// nickname ==> user

#ifndef KCODE_IRC_USERMGR_H
#define KCODE_IRC_USERMGR_H

struct kirc_user;

void kirc_usermgr_init();
void kirc_usermgr_shutdown();

struct kirc_user* kirc_usermgr_get_user_by_socket(int fd);
void kirc_usermgr_set_user_by_socket(int fd, struct kirc_user *user);
void kirc_usermgr_remove_user_by_socket(int fd);

struct kirc_user* kirc_usermgr_get_user_by_nickname(const char *nickname);
void kirc_usermgr_set_user_by_nickname(const char *nickname, struct kirc_user *user);
void kirc_usermgr_remove_user_by_nickname(const char *nickname);

#endif

