#ifndef KCODE_HASHMAP_H
#define KCODE_HASHMAP_H

struct khashmap;

struct khashmap *khashmap_new();
void khashmap_free(struct khashmap *map);

// pointer - found
// NULL    - not found
void* khashmap_find(struct khashmap *map, const char *key);

// update node value if key existed
// or insert it
void khashmap_insert(struct khashmap *map, const char *key, void *value);

// remove it if key existed
// or do nothing
void khashmap_remove(struct khashmap *map, const char *key);

// walk around all elems in hashmap
typedef void (*khashmap_visit_func) (void *value);
void khashmap_visit(struct khashmap *map, khashmap_visit_func func);

#endif
