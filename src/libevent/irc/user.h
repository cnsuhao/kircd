// A user in kIRCd is a struct hold some status and network connection of a client.

#ifndef KCODE_IRC_USER_H
#define	KCODE_IRC_USER_H

struct kstring;
struct kirc_user;

struct kirc_user* kirc_user_new();
void kirc_user_free(struct kirc_user* user);

void kirc_user_set_integer(struct kirc_user* user, const char* key, int val);
int  kirc_user_get_integer(struct kirc_user* user, const char* key);

void kirc_user_set_string(struct kirc_user* user, const char* key, const char* val);
struct kstring* kirc_user_get_string(struct kirc_user* user, const char* key);

#endif
