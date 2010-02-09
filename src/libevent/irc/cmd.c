#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include "common/kstring.h"
#include "irc/cmd.h"

struct kirc_cmd
{
	struct kstring **param;	// param list
	unsigned int nparam;

	char *line;
	char **idx_list;
};

#define	MAX_CMD		1024
struct kirc_cmd_dispatch {
	struct kstring *cmd_list[MAX_CMD];
	kirc_cmd_handler handler_list[MAX_CMD];
	unsigned int ncmd;
};

static struct kirc_cmd_dispatch dinfo;

// ---------------- new & free ------------------

struct kirc_cmd*
kirc_cmd_new(const char *line)
{
	char *token, *string, *tofree;
	struct kirc_cmd *kcmd;
	unsigned int nparam, i;

	assert(line != NULL && *line != '\0');

	kcmd = malloc(sizeof(struct kirc_cmd));
	kcmd->line = strdup(line);

	nparam = 0;
	tofree = string = strdup(line);
	while ((token = strsep(&string, " ")) != NULL)
	{
		if ( strlen(token) == 0 ) continue;
		nparam++;
	}
	free(tofree);

	kcmd->idx_list = malloc(sizeof(char *) * nparam);
	kcmd->param    = malloc(sizeof(struct kstring *) * nparam);
	kcmd->nparam   = nparam;

	i = 0;
	tofree = string = strdup(line);
	while ((token = strsep(&string, " ")) != NULL)
	{
		if ( strlen(token) == 0 ) continue;
		kcmd->param[i]    = kstring_new(token);
		kcmd->idx_list[i] = kcmd->line + (token - tofree);
		i++;
	}
	free(tofree);

	return kcmd;
}

void
kirc_cmd_free(struct kirc_cmd* kcmd)
{
	unsigned int i;

	assert(kcmd != NULL);

	for ( i = 0; i < kcmd->nparam; i++ )
	{
		kstring_free(kcmd->param[i]);
	}

	free(kcmd->idx_list);
	free(kcmd->line);
	free(kcmd);
}

struct kstring *
kirc_cmd_get_param(struct kirc_cmd *kcmd, unsigned int i)
{
	assert(kcmd != NULL);
	assert(i < kcmd->nparam);
	return kcmd->param[i];
}

unsigned int
kirc_cmd_nparam(struct kirc_cmd *kcmd)
{
	assert(kcmd != NULL);
	return kcmd->nparam;
}

// ----------- handler management ------------

void
kirc_cmd_add_handler(const char *cmd, kirc_cmd_handler handler)
{
	assert(cmd != NULL && *cmd != '\0');
	assert(handler != NULL);
	assert(dinfo.ncmd < MAX_CMD);

	dinfo.cmd_list[dinfo.ncmd]     = kstring_new(cmd);
	dinfo.handler_list[dinfo.ncmd] = handler;
	dinfo.ncmd++;
}

void
kirc_cmd_dispatch(struct kirc_cmd* kcmd)
{
	struct kstring *cmd;
	unsigned int i;

	assert(kcmd != NULL);
	assert(kirc_cmd_nparam(kcmd) > 0);

	cmd = kirc_cmd_get_param(kcmd, 0);
	for ( i = 0; i < dinfo.ncmd; i++ )
	{
		if ( kstring_equal(dinfo.cmd_list[i], cmd) )
		{
			dinfo.handler_list[i](kcmd);
			break;
		}
	}
}

