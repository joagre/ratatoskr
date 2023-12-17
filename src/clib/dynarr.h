#ifndef __DYNARR_H__

#include <stdint.h>
#include <memory.h>

#include "allocator.h"

// Grow factor
#define DYN_GROW0 2.0000f
#define DYN_GROW1 1.6180f
#define DYN_GROW2 1.2500f

// switch limit
#define DYN_GROW0_LIMIT 1024
#define DYN_GROW1_LIMIT (1024*1024)

#define DYN_ADDR(dp,i) ((void*)((intptr_t)((dp)->base) + ((i)*(dp)->width)))

typedef struct _dynarray_t
{
    size_t capacity;   // max number of elements
    size_t size;       // assigned size <= capacity
    size_t width;      // element size
    allocator_t* alloc;
    void*  base;       // array base address
} dynarray_t;

typedef struct  {
    dynarray_t* arr;  // current array
    int cur;          // current pos
} dynarray_iter_t;

#define DYNARR_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define DYNARR_API
#else
#define DYNARR_API __attribute__ ((unused))
#endif

DYNARR_LOCAL size_t dynarray_next_size(size_t cur, size_t capacity) DYNARR_API;
DYNARR_LOCAL size_t dynarray_first_size(size_t capacity) DYNARR_API;
DYNARR_LOCAL int dynarray_init(dynarray_t* dp, allocator_t* alloc, size_t initial_capacity,size_t width) DYNARR_API;
DYNARR_LOCAL void dynarray_clear(dynarray_t* dp) DYNARR_API;
DYNARR_LOCAL size_t dynarray_capacity(dynarray_t* dp) DYNARR_API;
DYNARR_LOCAL size_t dynarray_size(dynarray_t* dp) DYNARR_API;
DYNARR_LOCAL int dynarray_set_capacity(dynarray_t* dp, size_t capacity) DYNARR_API;
DYNARR_LOCAL int dynarray_resize(dynarray_t* dp, size_t size) DYNARR_API;
DYNARR_LOCAL void* dynarray_element(dynarray_t* dp, int i) DYNARR_API;
DYNARR_LOCAL int dynarray_setelement(dynarray_t* dp,int i,void* data) DYNARR_API;
DYNARR_LOCAL void* dynarray_add(dynarray_t* dp) DYNARR_API;
DYNARR_LOCAL void* dynarray_delete(dynarray_t* dp, int i) DYNARR_API;
DYNARR_LOCAL void* dynarray_unordered_delete(dynarray_t* dp, int i) DYNARR_API;

DYNARR_LOCAL void* dynarray_prepend(dynarray_t* dp, void* ptr) DYNARR_API;
DYNARR_LOCAL void* dynarray_append(dynarray_t* dp, void* ptr) DYNARR_API;
DYNARR_LOCAL void* dynarray_drop(dynarray_t* dp) DYNARR_API;
DYNARR_LOCAL void* dynarray_pop(dynarray_t* dp) DYNARR_API;


DYNARR_LOCAL void dynarray_iter_init(dynarray_iter_t* iter, dynarray_t* arr) DYNARR_API;
DYNARR_LOCAL void* dynarray_iter_current(dynarray_iter_t* iter) DYNARR_API;
DYNARR_LOCAL int dynarray_iter_end(dynarray_iter_t* iter) DYNARR_API;
DYNARR_LOCAL void* dynarray_iter_next(dynarray_iter_t* iter) DYNARR_API;
DYNARR_LOCAL void dynarray_iter_remove(dynarray_iter_t* iter) DYNARR_API;


DYNARR_LOCAL size_t dynarray_next_size(size_t cur, size_t capacity)
{
    while((cur < DYN_GROW0_LIMIT) && (cur < capacity))
	cur *= DYN_GROW0;
    while((cur < DYN_GROW1_LIMIT) && (cur < capacity))
	cur *= DYN_GROW1;
    while((cur < capacity))
	cur *= DYN_GROW2;
    return cur;
}

DYNARR_LOCAL size_t dynarray_first_size(size_t capacity)
{
    return dynarray_next_size(1, capacity);
}

DYNARR_LOCAL int dynarray_init(dynarray_t* dp, allocator_t* alloc,
			       size_t initial_capacity,size_t width)
{
    void* base;
    size_t capacity;

    if (alloc == NULL) alloc = allocator_std();

    if (initial_capacity == 0) {
	capacity = 0;
	base = NULL;
    }
    else {
	capacity = dynarray_first_size(initial_capacity);
	if ((base = allocator_alloc(alloc, capacity*width)) == NULL)
	    return -1;
    }
    dp->capacity = capacity;
    dp->size     = 0;
    dp->width    = width;
    dp->alloc    = alloc;
    dp->base     = base;
    return 0;
}

DYNARR_LOCAL void dynarray_clear(dynarray_t* dp)
{
    allocator_free(dp->alloc, dp->base);
    dp->base = NULL;
    dp->size = 0;
    dp->capacity = 0;
}


DYNARR_LOCAL size_t dynarray_capacity(dynarray_t* dp)
{
    return (dp == NULL) ? 0 : dp->capacity;
}

DYNARR_LOCAL size_t dynarray_size(dynarray_t* dp)
{
    return (dp == NULL) ? 0 : dp->size;
}

DYNARR_LOCAL int dynarray_set_capacity(dynarray_t* dp, size_t capacity)
{
    void* base;

    if ((base = allocator_realloc(dp->alloc,dp->base,capacity*dp->width))==NULL)
	return -1;
    dp->base = base;
    dp->capacity = capacity;
    dp->size = dp->size < capacity ? dp->size : capacity;
    return 0;
}

DYNARR_LOCAL int dynarray_resize(dynarray_t* dp, size_t size)
{
    size_t size0;
    if (size > dp->capacity) {
	size_t size0 = (dp->capacity < 1) ? 1 : dp->capacity;
	size_t capacity = dynarray_next_size(size0, size);
	if (dynarray_set_capacity(dp, capacity) < 0)
	    return -1;
    }
    if ((size0 = dp->size) < size) {
	void* ptr = (uint8_t*)dp->base + dp->size*dp->width;
	memset(ptr, 0, (size-size0)*dp->width);
    }
    dp->size = size;
    return 0;
}

DYNARR_LOCAL void* dynarray_element(dynarray_t* dp, int i)
{
    if (dp == NULL) return NULL;
    if (i == 0) return dp->base;
    if ((i < 0) || (i >= (int)dp->size)) return NULL;
    return DYN_ADDR(dp, i);
}

// copy data into dynarray resize if needed
DYNARR_LOCAL int dynarray_setelement(dynarray_t* dp,int i,void* data)
{
    if (dp == NULL || (i < 0)) return -1;
    if (i >= (int)dp->size) {
	if (dynarray_resize(dp, i+1) < 0)
	    return -1;
    }
    memcpy(DYN_ADDR(dp, i), data, dp->width);
    return 0;
}

DYNARR_LOCAL void* dynarray_add(dynarray_t* dp)
{
    size_t n;
    if (dp == NULL) return NULL;
    n = dp->size;
    if (dynarray_resize(dp, n+1) < 0)
	return NULL;
    return DYN_ADDR(dp, n);
}

DYNARR_LOCAL void* dynarray_unordered_delete(dynarray_t* dp, int i)
{
    uint8_t* src;
    uint8_t* dst;

    if ((i<0) || (dp == NULL) || (i >= (int)dp->size))
	return NULL;
    dst = DYN_ADDR(dp, i);
    src = DYN_ADDR(dp, dp->size-1);
    memmove(dst, src, dp->width);
    dp->size--;
    return dst;
}

// FIXME: add a swap version or a flag!
DYNARR_LOCAL void* dynarray_delete(dynarray_t* dp, int i)
{
    uint8_t* src;
    uint8_t* dst;
    size_t len;

    if ((i<0) || (dp == NULL) || (i >= (int)dp->size))
	return NULL;
    dst = DYN_ADDR(dp, i);
    src = dst + dp->width;
    if ((len = (dp->size - i -1)) > 0)
	memmove(dst, src, len*dp->width);
    dp->size--;
    return dst;
}

DYNARR_LOCAL void* dynarray_prepend(dynarray_t* dp, void* ptr)
{
    size_t size = dp->size;
    uint8_t* src;
    uint8_t* dst;
    if (dynarray_resize(dp, size+1) < 0)
	return NULL;
    src = DYN_ADDR(dp, 0);
    dst = DYN_ADDR(dp, 1);
    memmove(dst, src, size*dp->width);
    if (dynarray_setelement(dp, 0, ptr) < 0)
	return NULL;
    return src;
}

DYNARR_LOCAL void* dynarray_append(dynarray_t* dp, void* ptr)
{
    int i = dp->size;
    if (dynarray_resize(dp, i+1) < 0)
	return NULL;
    if (dynarray_setelement(dp, i, ptr) < 0)
	return NULL;
    return DYN_ADDR(dp, i);
}

DYNARR_LOCAL void* dynarray_drop(dynarray_t* dp)
{
    size_t size = dp->size;
    if (size > 1) {
	uint8_t* src;
	uint8_t* dst;
	// first make sure we have space for an extra slot
	if (dynarray_resize(dp, size+1) < 0)
	    return NULL;
	// now move first element after the last (preserve for return!)
	src = DYN_ADDR(dp, 0);
	dst = DYN_ADDR(dp, size);
	memmove(dst, src, dp->width);
	// now move all elements to the left
	memmove(src, src+dp->width, (size-1)*dp->width);
	dynarray_resize(dp, size-1);
	return dst;
    }
    else if (size == 1) {
	uint8_t* src = DYN_ADDR(dp, 0);
	dynarray_resize(dp, 0);
	return src;
    }
    return NULL;
}

// return the last element and remove it
// element returned is accessible until overwritten!
DYNARR_LOCAL void* dynarray_pop(dynarray_t* dp)
{
    if (dp->size > 0) {
	int i = dp->size-1;
	void* ptr = DYN_ADDR(dp, i);
	if (dynarray_resize(dp, i) < 0)
	    return NULL;
	return ptr;
    }
    return NULL;
}

DYNARR_LOCAL void dynarray_iter_init(dynarray_iter_t* iter, dynarray_t* arr)
{
    iter->arr = arr;
    iter->cur = 0;
}

DYNARR_LOCAL void* dynarray_iter_current(dynarray_iter_t* iter)
{
    return DYN_ADDR(iter->arr, iter->cur);
}

DYNARR_LOCAL int dynarray_iter_end(dynarray_iter_t* iter)
{
    return (iter->cur >= (int) iter->arr->size);
}

DYNARR_LOCAL void* dynarray_iter_next(dynarray_iter_t* iter)
{
    int i = iter->cur+1;
    if (i < (int) iter->arr->size) {
	iter->cur = i;
	return DYN_ADDR(iter->arr, i);
    }
    return NULL;
}

DYNARR_LOCAL void dynarray_iter_remove(dynarray_iter_t* iter)
{
    int i = iter->cur;
    if (i < (int) iter->arr->size)
	dynarray_delete(iter->arr, i);
}

#endif
