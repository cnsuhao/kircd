#include <cgreen.h>
#include "common/kstring.h"

static void
string_len_version()
{
    struct kstring *s;

    s = kstring_new_len("hello dummy", 5);
    assert_string_equal(kstring_cstr(s), "hello");

    kstring_append_len(s, "foo dummy", 3);
    assert_string_equal(kstring_cstr(s), "hellofoo");

    kstring_insert_len(s, 2, "bar dummy", 3);
    assert_string_equal(kstring_cstr(s), "hebarllofoo");

    kstring_free(s);
}

static void
string_no_len_version()
{
    struct kstring *s;

    s = kstring_new("hello");
    assert_string_equal(kstring_cstr(s), "hello");

    kstring_append(s, "foo");
    assert_string_equal(kstring_cstr(s), "hellofoo");

    kstring_insert(s, 2, "bar");
    assert_string_equal(kstring_cstr(s), "hebarllofoo");

    kstring_free(s);
}

static void
string_misc()
{
    struct kstring *s1, *s2;

    s1 = kstring_new("hello = 10");
    assert_equal(kstring_length(s1), 10);

    s2 = kstring_new(NULL);
    kstring_vprintf(s2, "%s = %d", "hello", 10);
    assert_string_equal(kstring_cstr(s2), "hello = 10");

    assert_true(kstring_equal(s1, s2) == 1);
    assert_equal(kstring_hash(s1), kstring_hash(s2));

    kstring_free(s1);
    kstring_free(s2);
}

TestSuite *
common_kstring_test()
{
    TestSuite *suite = create_test_suite();
    add_test(suite, string_len_version);
    add_test(suite, string_no_len_version);
    add_test(suite, string_misc);
    return suite;
}

