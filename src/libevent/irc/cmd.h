#ifndef KCODE_IRC_CMD_H
#define	KCODE_IRC_CMD_H

struct kstring;

struct kirc_cmd;
typedef void (*kirc_cmd_handler)(struct kirc_cmd*);

struct kirc_cmd* kirc_cmd_new(const char *line);
void kirc_cmd_free(struct kirc_cmd* kcmd);

struct kstring *kirc_cmd_get_param(struct kirc_cmd *kcmd, unsigned int i);
unsigned int kirc_cmd_nparam(struct kirc_cmd *kcmd);

void kirc_cmd_add_handler(const char *cmd, kirc_cmd_handler handler);
void kirc_cmd_dispatch(struct kirc_cmd* kcmd);

#endif
