#include <cgreen.h>
#include "common/khashmap.h"

static void
hashmap_all()
{
	struct khashmap *m;
	m = khashmap_new();

	assert_equal(khashmap_insert(m, "foo", (void *)10), 0);
	assert_equal(khashmap_find(m, "foo"), 10);

	assert_equal(khashmap_insert(m, "foo", (void *)20), -1);
	assert_equal(khashmap_update(m, "foo", (void *)20), 0);
	assert_equal(khashmap_find(m, "foo"), 20);

	assert_equal(khashmap_remove(m, "foo"), 0);
	assert_equal(khashmap_remove(m, "foo"), -1);
	assert_equal(khashmap_update(m, "foo", (void *)30), -1);

    khashmap_free(m);
}

TestSuite *
common_khashmap_test()
{
    TestSuite *suite = create_test_suite();
    add_test(suite, hashmap_all);
    return suite;
}

