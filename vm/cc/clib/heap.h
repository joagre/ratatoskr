#ifndef __SHEAP_H__
#define __SHEAP_H__

#include <stdint.h>
#include "slist.h"

typedef struct _heap_block_t // :slink_t
{
    slink_t  link;     // next heap block link
    uint8_t* current;  // must be aligned
    uint8_t* end;      // end of current block
    uint8_t base[0];   // start of block memory
} heap_block_t;

typedef struct _heap_t  // :slist
{
    slist_t blocks;      // blocks of memory
    allocator_t* alloc;  // block alloactor
} heap_t;

#define HEAP_BLOCK_SIZE      4096
#define MAX_HEAP_ALLOC_SIZE  (HEAP_BLOCK_SIZE - sizeof(heap_block_t))
#define HEAP_ALIGN           sizeof(void*)

#define AMASK(ptr,align) (((intptr_t)(ptr)) & ((align)-1))
#define ALIGN(ptr,align) ((((intptr_t) (ptr))+(align)-1) & ~((align)-1))
#define PAD(ptr,align)   (((align)-AMASK(ptr,align)) & ((align)-1))

static void heap_init(heap_t* hp, allocator_t* alloc)
{
    hp->alloc = alloc ? alloc : allocator_std();
    slist_init(&hp->blocks);
}

static heap_block_t* heap_block_new(heap_t* hp)
{
    heap_block_t* bp;

    if ((bp = allocator_alloc(hp->halloc,HEAP_BLOCK_SIZE+HEAP_ALIGN-1))==NULL)
	return NULL;
    bp->current = hp->base + PAD(hp->base,HEAP_ALIGN);
    bp->end = hp->current + MAX_HEAP_ALLOC_SIZE;
    return hp;
}

static void* heap_alloc(heap_t* hp, size_t size)
{
    heap_block_t* bp;
    void* ptr;

    if (size > MAX_HEAP_ALLOC_SIZE)
	return NULL;
    if (slist_is_empty(&hp->blocks))
	bp = slist_insert_first(&hp->blocks, heap_block_new(hp));
    else {
	bp = slist_first(&hp->blocks);
	if (bp->current + size >= hp->end)
	    bp = slist_insert_first(&hp->blocks, heap_block_new(hp));
    }
    ptr = bp->current;
    bp->current += size;
    return ptr;
}

static void heap_cleanup(heap_t* hp)
{
    heap_t* bp = slist_first(&hp->blocks);
    slist_iter_t iter;

    slist_iter_init(&hp->blocks, &iter);
    while(!slist_iter_eol(&iter)) {
	heap_block_t* bp = slist_iter_current(&iter);
	slist_iter_remove(&iter);
	allocator_free(hp->alloc, bp);
    }
}

#endif
