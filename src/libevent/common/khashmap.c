// hashmap
//   slot[0] --> double-linked list
//   slot[1] --> double-linked list

#include <stdlib.h>
#include "khashmap.h"
#include "klist.h"
#include "kutil.h"

#define MAX_SLOT        128             // must power of 2

struct khashmap_node {
    unsigned long keyhash;
    void *value;
    K_LIST_ENTRY(khashmap_node) list;
};

K_LIST_HEAD(khashmap_node_list, khashmap_node);

struct khashmap {
    unsigned int slot_num;
    unsigned int slot_mask;
    struct khashmap_node_list slot[1];
};


// ---------------- new & free ------------------

struct khashmap *
khashmap_new()
{
    int i;
    struct khashmap *map;

    map = (struct khashmap *) malloc(sizeof(struct khashmap) +
                    sizeof(struct khashmap_node_list)*(MAX_SLOT-1));

    map->slot_num  = MAX_SLOT;
    map->slot_mask = map->slot_num - 1;

    for ( i = 0; i < map->slot_num; i++ )
    {
        K_LIST_INIT(&map->slot[i]);
    }

    return map;
}

void
khashmap_free(struct khashmap *map)
{
    int i;
    struct khashmap_node *node, *tmp;

    for ( i = 0; i < map->slot_num; i++ )
    {
        K_LIST_FOREACH_SAFE(node, &map->slot[i], list, tmp)
        {
            free(node);
        }
    }

    free(map);
}


// ---------------- find/insert/remove ------------------
static inline struct khashmap_node *
_khashmap_find_node(struct khashmap *map, const char *key)
{
    unsigned long keyhash, idx;
    struct khashmap_node *node;

    keyhash = kutil_hash(key);
    idx = keyhash & map->slot_mask;
    K_LIST_FOREACH(node, &map->slot[idx], list)
    {
        if ( node->keyhash == keyhash ) return node;
    }
    return NULL;
}

static inline void
_khashmap_insert_node(struct khashmap *map, const char *key, void *value)
{
    unsigned long idx;
    struct khashmap_node *node;

    node = (struct khashmap_node*) malloc(sizeof(struct khashmap_node));
    node->keyhash = kutil_hash(key);
    node->value   = value;
  
    idx = node->keyhash & map->slot_mask;
    K_LIST_INSERT_HEAD(&map->slot[idx], node, list);
}

void* khashmap_find(struct khashmap *map, const char *key)
{
    struct khashmap_node *node = _khashmap_find_node(map, key);
    return node ? node->value : NULL;
}

void khashmap_insert(struct khashmap *map, const char *key, void *value)
{
    struct khashmap_node *node = _khashmap_find_node(map, key);
    if ( node )
    {
        node->value = value;
    }
    else
    {
        _khashmap_insert_node(map, key, value);
    }
}

void khashmap_remove(struct khashmap *map, const char *key)
{
    struct khashmap_node *node = _khashmap_find_node(map, key);
    if ( node ) K_LIST_REMOVE(node, list);
}

