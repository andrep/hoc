#include <stdlib.h>
#include <stdint.h>

struct hoc_ivar {
    char *ivar_name;
    char *ivar_types;
    size_t ivar_size;
    uint8_t ivar_alignment;
};

#define IVAR_PTR_ALIGN              ((uint8_t) sizeof(void *))

struct hoc_ivar_list {
    int ivar_count;
    
    /* variable length structure */
    struct hoc_ivar ivar_list[1];
};

struct hoc_ivar_list * makeIvarList(int n);

void setIvarInList(
        struct hoc_ivar_list *list,
        int i,
        char *name,     /* never deallocate this */
        char *types,    /* never deallocate this */
        size_t size,
        uint8_t alignment
    );

#ifndef __OBJC2__

struct objc_ivar_list * buildIndexedIvarList(
        struct hoc_ivar_list *list,
        int start_offset,
        int *instance_size      /* out */
    );

#endif