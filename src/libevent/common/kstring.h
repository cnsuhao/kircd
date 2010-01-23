#ifndef KCODE_COMMON_STRING_H
#define KCODE_COMMON_STRING_H

#include <sys/types.h>      // size_t

struct kstring;

struct kstring *kstring_new(const char *init);
struct kstring *kstring_new_len(const char *init, size_t len);
void kstring_free(struct kstring *str);

size_t kstring_length(struct kstring *str);
const char *kstring_cstr(struct kstring *str);

struct kstring *kstring_append(struct kstring *str, const char *val);
struct kstring *kstring_append_len(struct kstring *str, const char *val, size_t len);

struct kstring *kstring_insert(struct kstring *str, size_t pos, const char *val);
struct kstring *kstring_insert_len(struct kstring *str, size_t pos, const char *val, size_t len);

void kstring_vprintf(struct kstring *str, const char *format, ...);

unsigned int kstring_hash(const struct kstring *str);
int kstring_equal(const struct kstring *v1, const struct kstring *v2);

#endif
