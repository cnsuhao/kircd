#ifndef KCODE_KSTRING_H
#define KCODE_KSTRING_H

#include <sys/types.h>      // size_t

struct KString;

struct KString *kstring_new(const char *init);
struct KString *kstring_new_len(const char *init, ssize_t len);
void kstring_free(struct KString *str);

ssize_t kstring_length(struct KString *str);
const char *kstring_cstr(struct KString *str);

struct KString *kstring_append(struct KString *str, const char *val);
struct KString *kstring_append_len(struct KString *str, const char *val, ssize_t len);

struct KString *kstring_insert(struct KString *str, ssize_t pos, const char *val);
struct KString *kstring_insert_len(struct KString *str, ssize_t pos, const char *val, ssize_t len);

void kstring_vprintf(struct KString *str, const char *format, ...);

unsigned int kstring_hash(const struct KString *str);
int kstring_equal(const struct KString *v1, const struct KString *v2);

#endif
