#include <stdio.h>
#include "common/khashmap.h"

int main()
{  
    struct khashmap *m;
    m = khashmap_new();

    khashmap_insert(m, "foo", (void *)10);
    khashmap_insert(m, "bar", (void *)20);
    printf("foo = %d, bar = %d\n", (int)khashmap_find(m, "foo"), (int)khashmap_find(m, "bar"));

    khashmap_update(m, "foo", (void *)30);
    printf("foo = %d\n", (int)khashmap_find(m, "foo"));

    khashmap_remove(m, "bar");
    printf("bar = %d\n", (int)khashmap_find(m, "bar"));

    khashmap_free(m);
    return 0;
}

