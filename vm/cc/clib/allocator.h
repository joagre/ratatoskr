#ifndef __ALLOCATOR_H__
#define __ALLOCATOR_H__

#include <stdlib.h>

typedef struct _allocator_t
{
    void* (*alloc)(size_t size);
    void* (*realloc)(void* ptr, size_t size);
    void (*free)(void* ptr);
} allocator_t;

#define ALLOCATOR_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define ALLOCATOR_API
#else
#define ALLOCATOR_API __attribute__ ((unused))
#endif

ALLOCATOR_LOCAL allocator_t std_allocator =
{
    .alloc = malloc,
    .realloc = realloc,
    .free = free
};

ALLOCATOR_LOCAL void* allocator_alloc(allocator_t*, size_t) ALLOCATOR_API;
ALLOCATOR_LOCAL void* allocator_realloc(allocator_t*, void*, size_t) ALLOCATOR_API;
ALLOCATOR_LOCAL void  allocator_free(allocator_t*, void*) ALLOCATOR_API;
ALLOCATOR_LOCAL allocator_t* allocator_std(void) ALLOCATOR_API;

ALLOCATOR_LOCAL allocator_t* allocator_std(void)
{
    return &std_allocator;
}

ALLOCATOR_LOCAL void* allocator_alloc(allocator_t* ap, size_t size)
{
    return (*ap->alloc)(size);
}

ALLOCATOR_LOCAL void* allocator_realloc(allocator_t* ap, void* ptr, size_t size)
{
    return (*ap->realloc)(ptr, size);
}

ALLOCATOR_LOCAL void allocator_free(allocator_t* ap, void* ptr)
{
    return (*ap->free)(ptr);
}

#endif
