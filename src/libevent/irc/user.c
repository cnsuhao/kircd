#include <stdlib.h>
#include <assert.h>

#include "common/kstring.h"
#include "common/khashmap.h"
#include "user.h"

struct kirc_user {
    struct khashmap *intmap;
    struct khashmap *strmap;
};


// ---------------- new & free ------------------

struct kirc_user*
kirc_user_new()
{
    struct kirc_user *user = malloc(sizeof(struct kirc_user));
    user->intmap = khashmap_new();
    user->strmap = khashmap_new();
    return user;
}

void
kirc_user_free(struct kirc_user* user)
{
    assert(user != NULL);

    // free all kstring in strmap
    khashmap_visit(user->strmap, (khashmap_visit_func)kstring_free);

    // free map
    khashmap_free(user->intmap);
    khashmap_free(user->strmap);
    free(user);
}


// ---------------- operator ------------------

void
kirc_user_set_integer(struct kirc_user* user, const char* key, int val)
{
    assert(user != NULL);
    assert(key != NULL);

    khashmap_insert_or_update(user->intmap, key, (void *)val);
}

int
kirc_user_get_integer(struct kirc_user* user, const char* key)
{
    return (int) khashmap_find(user->intmap, key);
}

void
kirc_user_set_string(struct kirc_user* user, const char* key, const char* val)
{
    khashmap_insert_or_update(user->strmap, key, (void *) kstring_new(val));
}

struct kstring*
kirc_user_get_string(struct kirc_user* user, const char* key)
{
    return (struct kstring *) khashmap_find(user->strmap, key);
}

