#ifndef KCODE_KSTRING_H
#define KCODE_KSTRING_H

#include <sys/types.h>

struct KString_s;
typedef struct KString_s KString;

KString *kstring_new(const char *init);
KString *kstring_new_len(const char *init, ssize_t len);
void kstring_free(KString *str);

ssize_t kstring_length(KString *str);
const char *kstring_cstr(KString *str);

KString *kstring_append(KString *str, const char *val);
KString *kstring_append_len(KString *str, const char *val, ssize_t len);

KString *kstring_insert(KString *str, ssize_t pos, const char *val);
KString *kstring_insert_len(KString *str, ssize_t pos, const char *val, ssize_t len);

void kstring_vprintf(KString *str, const char *format, ...);

unsigned int kstring_hash(const KString *str);
int kstring_equal(const KString *v1, const KString *v2);

#endif
