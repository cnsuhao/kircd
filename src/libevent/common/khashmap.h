#ifndef KCODE_HASHMAP_H
#define KCODE_HASHMAP_H

struct khashmap;

struct khashmap *khashmap_new();
void khashmap_free(struct khashmap *map);

// pointer - found
// NULL    - not found
void* khashmap_find(struct khashmap *map, const char *key);

// insert it if key not existed
//  0 - insert ok
// -1 - key exist
int khashmap_insert(struct khashmap *map, const char *key, void *value);

// update node value if key existed
//  0 - update ok
// -1 - key not exist
int khashmap_update(struct khashmap *map, const char *key, void *value);

// remove it if key existed
//  0 - remove ok
// -1 - key not exist
int khashmap_remove(struct khashmap *map, const char *key);

// walk around all elems in hashmap
typedef void (*khashmap_visit_func) (void *value);
void khashmap_visit(struct khashmap *map, khashmap_visit_func func);

#endif
