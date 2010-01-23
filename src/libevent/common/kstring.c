#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include "kstring.h"
#include "kutil.h"

#define SMALLEST_ALLOC_SIZE       32

struct kstring {
    char *s;            // '\0'-terminated
    ssize_t len;
    ssize_t alloc_len;
};

// ---------------- new & free ------------------

static void
_kstring_check_len(struct kstring *str, ssize_t need)
{
    ssize_t alloc_len;

    need = need + 1;    // '\0' terminated
    if ( str->alloc_len >= need )
        return;

    alloc_len = (str->alloc_len > 0) ? str->alloc_len : SMALLEST_ALLOC_SIZE;
    while (alloc_len < need)
        alloc_len <<= 1;
    str->s = realloc(str->s, alloc_len);
    str->alloc_len = alloc_len;
}

struct kstring *
kstring_new_len(const char *init, ssize_t len)
{
    struct kstring *str;

    str = (struct kstring *)malloc(sizeof(struct kstring));
    assert(str != NULL);

    str->s   = NULL;
    str->len = 0;
    str->alloc_len = 0;

    _kstring_check_len(str, len);

    memcpy(str->s, init, len);
    str->s[len] = '\0';
    str->len    = len;

    return str;
}

struct kstring *
kstring_new(const char *init)
{
    if (init == NULL) init = "";
    return kstring_new_len(init, strlen(init));
}

void
kstring_free(struct kstring *str)
{
    assert(str != NULL);
    free(str->s);
    free(str);
}


// ---------------- property ------------------

ssize_t
kstring_length(struct kstring *str)
{
    return str->len;
}

const char *
kstring_cstr(struct kstring *str)
{
    return str->s;
}


// ---------------- append ------------------

struct kstring *
kstring_append_len(struct kstring *str, const char *val, ssize_t len)
{
    ssize_t need_len = str->len + len;
    _kstring_check_len(str, need_len);
    memcpy(str->s + str->len, val, len);
    str->s[need_len] = '\0';
    str->len = need_len;
    return str;
}

struct kstring *
kstring_append(struct kstring *str, const char *val)
{
    return kstring_append_len(str, val, strlen(val));
}


// ---------------- insert ------------------

struct kstring *
kstring_insert_len(struct kstring *str, ssize_t pos, const char *val, ssize_t len)
{
    ssize_t need_len;

    if ( len == 0 )
        return str;

    if ( pos == (str->len - 1) )
        return kstring_append_len(str, val, len);

    need_len = str->len + len;
    _kstring_check_len(str, need_len);

    memmove(str->s + pos + len, str->s + pos, str->len - pos);
    memcpy(str->s + pos, val, len);

    str->len = need_len;
    str->s[need_len] = '\0';
    return str;
}

struct kstring *
kstring_insert(struct kstring *str, ssize_t pos, const char *val)
{
    return kstring_insert_len(str, pos, val, strlen(val));
}


// ---------------- vprintf ------------------
void kstring_vprintf(struct kstring *str, const char *format, ...)
{
    va_list args;
    char *buf;
    int len;

    va_start(args, format);

    len = vasprintf(&buf, format, args);
    if ( len >= 0 )
    {
        _kstring_check_len(str, len);
        memcpy(str->s, buf, len);

        str->s[len] = '\0';
        str->len    = len;

        free(buf);
    }

    va_end(args);
}


// ---------------- misc ------------------

unsigned int
kstring_hash(const struct kstring *str)
{
    return kutil_hash(str->s);
}

int
kstring_equal(const struct kstring *v1, const struct kstring *v2)
{
    if ( v1 == v2 )
        return 1;

    return (strcmp(v1->s, v2->s) == 0);
}

