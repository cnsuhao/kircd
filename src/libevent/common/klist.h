#ifndef KCODE_COMMON_LIST_H
#define KCODE_COMMON_LIST_H

// copy from FreeBSD's /sys/queue.h


// ------------- double-linked list ---------------

#define	K_LIST_EMPTY(head)	((head)->lh_first == NULL)

#define	K_LIST_FIRST(head)	((head)->lh_first)

#define	K_LIST_FOREACH(var, head, field)            \
    for ((var) = K_LIST_FIRST((head));              \
        (var);                                      \
        (var) = K_LIST_NEXT((var), field))

#define	K_LIST_INIT(head) do {                      \
    K_LIST_FIRST((head)) = NULL;                    \
} while (0)

#define	K_LIST_INSERT_AFTER(listelm, elm, field) do {                         \
    if ((K_LIST_NEXT((elm), field) = K_LIST_NEXT((listelm), field)) != NULL)  \
        K_LIST_NEXT((listelm), field)->field.le_prev =                        \
            &K_LIST_NEXT((elm), field);                                       \
    K_LIST_NEXT((listelm), field) = (elm);                                    \
    (elm)->field.le_prev = &K_LIST_NEXT((listelm), field);                    \
} while (0)

#define	K_LIST_INSERT_BEFORE(listelm, elm, field) do {                        \
    (elm)->field.le_prev = (listelm)->field.le_prev;                          \
    K_LIST_NEXT((elm), field) = (listelm);                                    \
    *(listelm)->field.le_prev = (elm);                                        \
    (listelm)->field.le_prev = &K_LIST_NEXT((elm), field);                    \
} while (0)

#define	K_LIST_INSERT_HEAD(head, elm, field) do {                             \
    if ((K_LIST_NEXT((elm), field) = K_LIST_FIRST((head))) != NULL)           \
        K_LIST_FIRST((head))->field.le_prev = &K_LIST_NEXT((elm), field);     \
        K_LIST_FIRST((head)) = (elm);                                         \
    (elm)->field.le_prev = &K_LIST_FIRST((head));                             \
} while (0)

#define	K_LIST_NEXT(elm, field)	((elm)->field.le_next)

#define	K_LIST_REMOVE(elm, field) do {                      \
    if (K_LIST_NEXT((elm), field) != NULL)                  \
        K_LIST_NEXT((elm), field)->field.le_prev =          \
            (elm)->field.le_prev;                           \
    *(elm)->field.le_prev = K_LIST_NEXT((elm), field);      \
} while (0)

#endif
