#ifndef __SEGARR_H__
#define __SEGARR_H__

#include <stdint.h>
#include "allocator.h"

// Segmented array
typedef struct _segarray_t
{
    size_t capacity;   // max number of elements
    size_t size;       // assigned size <= capacity
    size_t chunk_size; // number of elements per chunk (power of 2)
    size_t width;      // element size
    size_t shift;      // 2^shift = segment size
    size_t mask;       // mask = 2^shift -1
    size_t nsegs;      // number of segments allocated
    allocator_t* alloc;
    void** segment;
} segarray_t;

#define MIN_CHUNK_MEM_SIZE 4096

#define SEG_ADDR(sp,i) ((void*)(((intptr_t)(sp)->segment[(i) >> sp->shift]) + (((i) & (sp)->mask)*sp->width)))

typedef struct  {
    segarray_t* arr;  // current array
    int cur;          // current pos
} segarray_iter_t;

#define SEGARR_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define SEGARR_API
#else
#define SEGARR_API __attribute__ ((unused))
#endif

// Grow factor
#define SEG_GROW0 2.0000f
#define SEG_GROW1 1.6180f
#define SEG_GROW2 1.2500f

// switch limit
#define SEG_GROW0_LIMIT 1024
#define SEG_GROW1_LIMIT (1024*1024)


SEGARR_LOCAL size_t segarray_next_size(size_t cur, size_t capacity) SEGARR_API;
SEGARR_LOCAL size_t segarray_first_size(size_t capacity) SEGARR_API;
SEGARR_LOCAL int segarray_init(segarray_t* dp, allocator_t* alloc,
			       size_t initial_capacity,size_t width) SEGARR_API;
SEGARR_LOCAL void segarray_clear(segarray_t* dp) SEGARR_API;
SEGARR_LOCAL size_t segarray_capacity(segarray_t* dp) SEGARR_API;
SEGARR_LOCAL size_t segarray_size(segarray_t* dp) SEGARR_API;
SEGARR_LOCAL int segarray_set_capacity(segarray_t* dp, size_t capacity) SEGARR_API;
SEGARR_LOCAL int segarray_resize(segarray_t* dp, size_t size) SEGARR_API;
SEGARR_LOCAL void* segarray_element(segarray_t* dp, int i) SEGARR_API;
SEGARR_LOCAL int segarray_setelement(segarray_t* dp,int i,void* data) SEGARR_API
;
SEGARR_LOCAL void* segarray_add(segarray_t* dp) SEGARR_API;
SEGARR_LOCAL void* segarray_delete(segarray_t* dp, int i) SEGARR_API;
SEGARR_LOCAL void* segarray_unordered_delete(segarray_t* dp, int i) SEGARR_API;

SEGARR_LOCAL void* segarray_prepend(segarray_t* dp, void* ptr) SEGARR_API;
SEGARR_LOCAL void* segarray_append(segarray_t* dp, void* ptr) SEGARR_API;
SEGARR_LOCAL void* segarray_drop(segarray_t* dp) SEGARR_API;
// SEGARR_LOCAL void* segarray_pop(segarray_t* dp) SEGARR_API;


SEGARR_LOCAL void segarray_iter_init(segarray_iter_t* iter, segarray_t* arr) SEGARR_API;
SEGARR_LOCAL void* segarray_iter_current(segarray_iter_t* iter) SEGARR_API;
SEGARR_LOCAL int segarray_iter_end(segarray_iter_t* iter) SEGARR_API;
SEGARR_LOCAL void* segarray_iter_next(segarray_iter_t* iter) SEGARR_API;
SEGARR_LOCAL void segarray_iter_remove(segarray_iter_t* iter) SEGARR_API;


SEGARR_LOCAL size_t segarray_next_size(size_t cur, size_t capacity)
{
    while((cur < SEG_GROW0_LIMIT) && (cur < capacity))
	cur *= SEG_GROW0;
    while((cur < SEG_GROW1_LIMIT) && (cur < capacity))
	cur *= SEG_GROW1;
    while((cur < capacity))
	cur *= SEG_GROW2;
    return cur;
}

SEGARR_LOCAL size_t segarray_first_size(size_t capacity)
{
    return segarray_next_size(1, capacity);
}


SEGARR_LOCAL int segarray_set_capacity(segarray_t* arr, size_t capacity)
{
    void** segment;
    size_t chunk_size = arr->chunk_size;
    size_t chunk_bytes = chunk_size*arr->width;
    size_t nsegs = (capacity+chunk_size-1) >> arr->shift;
    int i;
    
    if ((segment = allocator_realloc(arr->alloc,arr->segment,
				     nsegs*sizeof(void*)))==NULL)
	return -1;
    for (i = arr->nsegs; i < (int)nsegs; i++) {
	void* chunk;
	if ((chunk = allocator_alloc(arr->alloc, chunk_bytes)) == NULL)
	    return -1;
	memset(chunk, 0, chunk_bytes);
	segment[i] = chunk;
    }
    capacity = nsegs*chunk_size;
    arr->nsegs = nsegs;
    arr->segment = segment;
    arr->capacity = capacity;
    arr->size = arr->size < capacity ? arr->size : capacity;
    return 0;
}

SEGARR_LOCAL int segarray_init(segarray_t* arr, allocator_t* alloc,
			       size_t initial_capacity,size_t width)
{
    size_t shift = 0;
    size_t chunk;
    
    if (alloc == NULL) alloc = allocator_std();
    chunk = MIN_CHUNK_MEM_SIZE / width;  // approx chunk size
    while (chunk) {  // scale to power of 2
	shift++;
	chunk >>= 1;
    }
    arr->shift = shift;
    arr->mask = (1 << shift) - 1;

    arr->capacity   = 0;
    arr->size       = 0;
    arr->chunk_size = (1 << shift);
    arr->width      = width;
    arr->alloc      = alloc;
    arr->nsegs      = 0;
    arr->segment    = NULL;

    if (initial_capacity) {
	if (segarray_set_capacity(arr, initial_capacity) < 0)
	    return -1;
    }

    return 0;
}

SEGARR_LOCAL void segarray_clear(segarray_t* arr)
{
    int i;
    for (i = 0; i < (int) arr->nsegs; i++)
	allocator_free(arr->alloc, arr->segment[i]);
    allocator_free(arr->alloc, arr->segment);
    arr->segment = NULL;
    arr->size = 0;
    arr->capacity = 0;
}


SEGARR_LOCAL size_t segarray_capacity(segarray_t* arr)
{
    return (arr == NULL) ? 0 : arr->capacity;
}

SEGARR_LOCAL size_t segarray_size(segarray_t* arr)
{
    return (arr == NULL) ? 0 : arr->size;
}

SEGARR_LOCAL int segarray_resize(segarray_t* arr, size_t size)
{
    if (size > arr->capacity) {
	size_t size0 = (arr->capacity < 1) ? 1 : arr->capacity;
	size_t capacity = segarray_next_size(size0, size);
	if (segarray_set_capacity(arr, capacity) < 0)
	    return -1;
    }
    if (arr->size < size) {
	int i;
	// fixme memset over chunks if possible!
	for (i = arr->size; i < (int) size; i++) {
	    void* ptr = SEG_ADDR(arr, i);
	    memset(ptr, 0, arr->width);
	}
    }
    arr->size = size;
    return 0;
}

SEGARR_LOCAL void* segarray_element(segarray_t* arr, int i)
{
    if (arr == NULL) return NULL;
    if ((i < 0) || (i >= (int)arr->size)) return NULL;
    return SEG_ADDR(arr,i);
}

// copy data into segarray resize if needed
SEGARR_LOCAL int segarray_setelement(segarray_t* arr,int i,void* data)
{
    if (arr == NULL || (i < 0)) return -1;
    if (i >= (int)arr->size) {
	if (segarray_resize(arr, i+1) < 0)
	    return -1;
    }
    memcpy(SEG_ADDR(arr, i), data, arr->width);
    return 0;
}

SEGARR_LOCAL void* segarray_add(segarray_t* arr)
{
    size_t n;
    if (arr == NULL) return NULL;
    n = arr->size;
    if (segarray_resize(arr, n+1) < 0)
	return NULL;
    return SEG_ADDR(arr, n);
}

SEGARR_LOCAL void* segarray_unordered_delete(segarray_t* arr, int i)
{
    uint8_t* src;
    uint8_t* dst;

    if ((i<0) || (arr == NULL) || (i >= (int)arr->size))
	return NULL;
    dst = SEG_ADDR(arr, i);
    src = SEG_ADDR(arr, arr->size-1);
    memmove(dst, src, arr->width);
    arr->size--;
    return dst;
}


// FIXME: add a swap version or a flag!
SEGARR_LOCAL void* segarray_delete(segarray_t* arr, int i)
{
    uint8_t* src;
    uint8_t* dst;
    size_t len;

    if ((i<0) || (arr == NULL) || (i >= (int)arr->size))
	return NULL;
    dst = SEG_ADDR(arr, i);
    src = dst + arr->width;
    if ((len = (arr->size - i -1)) > 0)
	memmove(dst, src, len*arr->width);
    arr->size--;
    return dst;
}

SEGARR_LOCAL void* segarray_prepend(segarray_t* arr, void* ptr)
{
    size_t size = arr->size;
    uint8_t* src;
    uint8_t* dst;
    int i;
    if (segarray_resize(arr, size+1) < 0)
	return NULL;
    for (i = size-1; i >= 0; i--) {
	dst = SEG_ADDR(arr, i+1);
	src = SEG_ADDR(arr, i);
	memcpy(dst, src, arr->width);
    }
    if (segarray_setelement(arr, 0, ptr) < 0)
	return NULL;
    return SEG_ADDR(arr, 0);
}


SEGARR_LOCAL void* segarray_append(segarray_t* arr, void* ptr)
{
    int i = arr->size;
    if (segarray_resize(arr, i+1) < 0)
	return NULL;
    if (segarray_setelement(arr, i, ptr) < 0)
	return NULL;
    return SEG_ADDR(arr, i);
}

SEGARR_LOCAL void* segarray_drop(segarray_t* arr)
{
    size_t size = arr->size;
    if (size > 1) {
	int i;
	void* src;
	void* dst;
	void* dst0;
	// first make sure we have space for an extra slot
	if (segarray_resize(arr, size+1) < 0)
	    return NULL;
	// now move first element after the last  (preserve for return!)
	src = SEG_ADDR(arr, 0);
	dst0 = SEG_ADDR(arr, size);
	memmove(dst0, src, arr->width);

	// move all elements to the left
	size -= 1;
	for (i = 0; i < (int) size; i++) {
	    src = SEG_ADDR(arr, i+1);	    
	    dst = SEG_ADDR(arr, i);
	    memmove(dst, src, arr->width);
	}
	segarray_resize(arr, size);
	return dst0;
    }
    else if (size == 1) {
	uint8_t* src = SEG_ADDR(arr, 0);
	segarray_resize(arr, 0);
	return src;
    }
    return NULL;
}


SEGARR_LOCAL void segarray_iter_init(segarray_iter_t* iter, segarray_t* arr)
{
    iter->arr = arr;
    iter->cur = 0;
}

SEGARR_LOCAL void* segarray_iter_current(segarray_iter_t* iter)
{
    return SEG_ADDR(iter->arr, iter->cur);
}

SEGARR_LOCAL int segarray_iter_end(segarray_iter_t* iter)
{
    return (iter->cur >= (int)iter->arr->size);
}

SEGARR_LOCAL void* segarray_iter_next(segarray_iter_t* iter)
{
    int i = iter->cur+1;
    if (i < (int) iter->arr->size) {
	iter->cur = i;
	return SEG_ADDR(iter->arr, i);
    }
    return NULL;
}

SEGARR_LOCAL void segarray_iter_remove(segarray_iter_t* iter)
{
    int i = iter->cur;
    if (i < (int) iter->arr->size)
	segarray_delete(iter->arr, i);
}

#endif
