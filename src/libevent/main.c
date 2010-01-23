#include <stdio.h>
#include "common/kstring.h"

int main()
{
    struct kstring *s, *s2;

    s = kstring_new("hello");
    printf("s = %s, l = %d\n", kstring_cstr(s), kstring_length(s));

    kstring_append(s, " is good!");
    printf("s = %s, h = %u\n", kstring_cstr(s), kstring_hash(s));

    s2 = kstring_new("hello is good!");
    printf("e = %d\n", kstring_equal(s, s2));
    kstring_free(s2);

    s2 = kstring_new(NULL);
    printf("null str = %s\n", kstring_cstr(s2));
    kstring_free(s2);

    s2 = kstring_new("hello");
    kstring_insert(s2, 2, "abc");
    printf("insert = %s\n", kstring_cstr(s2));

    kstring_vprintf(s2, "hello = %s, %d", "haha", 10);
    printf("vprintf = %s\n", kstring_cstr(s2));
    kstring_free(s2);

    return 0;
}

