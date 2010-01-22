#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include "kstring.h"

#define SMALLEST_ALLOC_SIZE       32

struct KString_s {
    char *s;            // '\0'-terminated
    ssize_t len;
    ssize_t alloc_len;
};

// ---------------- new & free ------------------

static void
_kstring_check_len(KString *str, ssize_t need)
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

KString *
kstring_new_len(const char *init, ssize_t len)
{
    KString *str;

    str = (KString *)malloc(sizeof(KString));
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

KString *
kstring_new(const char *init)
{
    if (init == NULL) init = "";
    return kstring_new_len(init, strlen(init));
}

void
kstring_free(KString *str)
{
    assert(str != NULL);
    free(str->s);
    free(str);
}


// ---------------- property ------------------

ssize_t
kstring_length(KString *str)
{
    return str->len;
}

const char *
kstring_cstr(KString *str)
{
    return str->s;
}


// ---------------- append ------------------

KString *
kstring_append_len(KString *str, const char *val, ssize_t len)
{
    ssize_t need_len = str->len + len;
    _kstring_check_len(str, need_len);
    memcpy(str->s + str->len, val, len);
    str->s[need_len] = '\0';
    str->len = need_len;
    return str;
}

KString *
kstring_append(KString *str, const char *val)
{
    return kstring_append_len(str, val, strlen(val));
}


// ---------------- insert ------------------

KString *
kstring_insert_len(KString *str, ssize_t pos, const char *val, ssize_t len)
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

KString *
kstring_insert(KString *str, ssize_t pos, const char *val)
{
    return kstring_insert_len(str, pos, val, strlen(val));
}


// ---------------- vprintf ------------------
void kstring_vprintf(KString *str, const char *format, ...)
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
kstring_hash(const KString *str)
{
    const char *p = str->s;
    ssize_t n = str->len;
    unsigned int h = 0;

    while (n--)
    {
        h = (h << 5) - h + *p;
        p++;
    }

    return h;
}

int
kstring_equal(const KString *v1, const KString *v2)
{
    if ( v1 == v2 )
        return 1;

    return (strcmp(v1->s, v2->s) == 0);
}

