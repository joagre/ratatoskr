#ifndef __LHASH_H__
#define __LHASH_H__

#include <stdint.h>

#include "allocator.h"
#include "slist.h"

typedef unsigned long lhash_value_t;

typedef struct _hlink_t // :slink_t in slist_t
{
    slink_t link;
    lhash_value_t hvalue;
} hlink_t;

typedef lhash_value_t (*lhash_hash_t)(void* key, void* arg);
typedef int (*lhash_cmp_t)(void* key, void* obj, void* arg);

typedef struct _lhash_t
{
    slist_t*   tab;
    size_t     size;  // number of elements
    size_t     asize; // allocated size of tab
    allocator_t* alloc;    
    lhash_hash_t hash;
    lhash_cmp_t  cmp;
} lhash_t;

typedef struct _lhash_iter_t
{
    lhash_t* lh;      // current table
    int slot;         // current slot
    slist_iter_t si;  // slot iterator
} lhash_iter_t;

#define LHASH_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define LHASH_API
#else
#define LHASH_API __attribute__ ((unused))
#endif

#define lhash_islot(lh, hvalue) ((hvalue) & ((lh)->asize - 1))

LHASH_LOCAL void lhash_init(lhash_t*, allocator_t*,
			    lhash_hash_t, lhash_cmp_t) LHASH_API;
LHASH_LOCAL void lhash_clear(lhash_t*) LHASH_API;
LHASH_LOCAL void lhash_reset(lhash_t*) LHASH_API;
LHASH_LOCAL int lhash_find(lhash_t*, void* key, void** ptr) LHASH_API;
LHASH_LOCAL int lhash_insert(lhash_t*, void* key, void* data) LHASH_API;
LHASH_LOCAL int lhash_remove(lhash_t* lhash, void* key, void** ptr) LHASH_API;
LHASH_LOCAL size_t lhash_size(lhash_t*)  LHASH_API;
LHASH_LOCAL int lhash_is_empty(lhash_t*)  LHASH_API;

// iterator
LHASH_LOCAL void lhash_iter_init(lhash_iter_t*, lhash_t*) LHASH_API;
LHASH_LOCAL int lhash_iter_current(lhash_iter_t*, void** data) LHASH_API;
LHASH_LOCAL int lhash_iter_end(lhash_iter_t*) LHASH_API;
LHASH_LOCAL int lhash_iter_next(lhash_iter_t*) LHASH_API;
LHASH_LOCAL int lhash_iter_remove(lhash_iter_t*) LHASH_API;


LHASH_LOCAL size_t lhash_size(lhash_t* lh)
{
    return lh->size;
}

LHASH_LOCAL int lhash_is_empty(lhash_t* lh)
{
    return (lh->size == 0);
}

LHASH_LOCAL void lhash_init(lhash_t* lh, allocator_t* alloc,
			    lhash_hash_t hash, lhash_cmp_t cmp)
{
    if (alloc == NULL) alloc = allocator_std();    
    lh->tab = NULL;
    lh->size = 0;
    lh->asize = 0;
    lh->alloc = alloc;
    lh->hash = hash;
    lh->cmp  = cmp;
}

// remove all elements
LHASH_LOCAL void lhash_clear(lhash_t* lh)
{
    if (lh->size > 0) {
	int slot = 0;
	for (slot = 0; slot < (int)lh->asize; slot++)
	    slist_init(&lh->tab[slot]);
	lh->size = 0;
    }
}

LHASH_LOCAL void lhash_reset(lhash_t* lh)
{
    lhash_clear(lh);
    allocator_free(lh->alloc, lh->tab);
    lh->tab = NULL;
    lh->asize = 0;
}

// INTERNAL
// return 2^r when 2^r > size
static size_t lhash_next_pow2(size_t size)
{
    int is_pow2 = (size>0) && ((size & (size-1)) == 0);
    size_t next_size;

    next_size = is_pow2 ? size : 1;
    while(next_size <= size)
	next_size *= 2;
    return next_size;
}

// INTERNAL
// grow hash table by double the size
LHASH_LOCAL int lhash_grow(lhash_t* lh)
{
    size_t size0 = lh->asize;
    size_t size  = lhash_next_pow2(size0);
    slist_t* tab;
    void* ptr;
    int slot;

    // printf("grow to size %lu\n", size);

    if ((tab=allocator_realloc(lh->alloc,lh->tab,size*sizeof(slist_t)))==NULL)
	return -1;
    ptr = (uint8_t*)tab + lh->asize*sizeof(slist_t);
    memset(ptr, 0, (size-size0)*sizeof(slist_t));
    lh->tab = tab;
    lh->asize = size;
    
    // move elements that rehash to the upper part
    for (slot = 0; slot < (int)size0; slot++) {
	slist_iter_t iter;
	slist_iter_init(&iter, &lh->tab[slot]);

	while(!slist_iter_end(&iter)) {
	    hlink_t* hp = slist_iter_current(&iter);
	    int new_slot = hp->hvalue & (size-1);  // hash with new size
	    if (slot == new_slot) // element stay
		slist_iter_next(&iter);
	    else { // element move to new location j=(i+2^r) (top half)
		slist_iter_remove(&iter);
		slist_insert_last(&lh->tab[new_slot], hp);
	    }
	}
    }
    return 0;
}

// internal - advance iterator to element 
LHASH_LOCAL int lhash_iter_find(slist_iter_t* iter,
				lhash_cmp_t cmp,
				lhash_value_t hvalue,
				void* key,
				void* arg)
{
    while(!slist_iter_end(iter)) {
	hlink_t* hp = slist_iter_current(iter);
	if (hvalue == hp->hvalue) {
	    if ((*cmp)(key, hp, arg) == 0)
		return 1;  // found
	}
	slist_iter_next(iter);	
    }
    return 0;
}


LHASH_LOCAL int lhash_insert(lhash_t* lh, void* key, void* data)
{
    lhash_value_t hvalue = (*lh->hash)(key, lh);
    hlink_t* hp = (hlink_t*) data;
    int slot;

    if (lh->size+1 >= lh->asize) {  // fixme
	if (lhash_grow(lh) < 0)
	    return -1;
    }
    slot = hvalue & (lh->asize-1);
    hp->hvalue = hvalue;
    // printf("insert %ld hvalue=%lu in slot %d\n", (long)key, hvalue, slot);
    slist_insert_last(&lh->tab[slot], hp);  // first?
    lh->size++;
    return 1;
}

LHASH_LOCAL int lhash_remove(lhash_t* lh, void* key, void** ptr)
{
    lhash_value_t hvalue = (*lh->hash)(key, lh);
    slist_iter_t iter;
    hlink_t* hp;
    int slot;

    slot = lhash_islot(lh, hvalue);
    slist_iter_init(&iter, &lh->tab[slot]);
    if (!lhash_iter_find(&iter, lh->cmp, hvalue, key, lh))
	return 0;  // not found
    hp = slist_iter_current(&iter);
    // printf("remove %ld from slot %d\n", (long)key, slot);
    slist_iter_remove(&iter);
    if (ptr) *ptr = hp;
    lh->size--;
    return 1;
}

LHASH_LOCAL int lhash_find(lhash_t* lh, void* key, void** ptr)
{
    if (lh->asize > 0) {
	lhash_value_t hvalue = (*lh->hash)(key, lh);
	int slot = lhash_islot(lh, hvalue);
	hlink_t* hp;
	slist_iter_t iter;
	
	slist_iter_init(&iter, &lh->tab[slot]);
	if (!lhash_iter_find(&iter, lh->cmp, hvalue, key, lh))
	    return 0;  // not found
	hp = slist_iter_current(&iter);
	if (ptr) *ptr = hp;
	return 1;
    }
    return 0;
}

// iterator

// step forward
LHASH_LOCAL void lhash_iter_step(lhash_iter_t* iter, lhash_t* lh, int slot)
{
    while ((slot < lh->asize) &&
	   (slist_length(&lh->tab[slot]) == 0))
	slot++;
    iter->slot = slot;
    if (slot < lh->asize)
	slist_iter_init(&iter->si, &lh->tab[slot]);
}

LHASH_LOCAL void lhash_iter_init(lhash_iter_t* iter, lhash_t* lh)
{
    iter->lh = lh;
    lhash_iter_step(iter, lh, 0);
}

LHASH_LOCAL int lhash_iter_current(lhash_iter_t* iter, void** data)
{
    hlink_t* hp;
    if (iter->slot >= iter->lh->asize)
	return 0;
    hp = slist_iter_current(&iter->si);
    if (data) *data = hp;
    return 1;
}

LHASH_LOCAL int lhash_iter_end(lhash_iter_t* iter)
{
    return (iter->slot >= iter->lh->asize);
}

LHASH_LOCAL int lhash_iter_next(lhash_iter_t* iter)
{
    int slot;
    lhash_t* lh = iter->lh;
    
    if ((slot = iter->slot) >= lh->asize)
	return 0;
    if (!slist_iter_next(&iter->si)) {
	lhash_iter_step(iter, lh, slot+1);
	return (iter->slot < lh->asize);
    }
    return 1;	
}

LHASH_LOCAL int lhash_iter_remove(lhash_iter_t* iter)
{
    if (iter->slot < iter->lh->asize) {
	slist_iter_remove(&iter->si);
	if (slist_iter_end(&iter->si))
	    lhash_iter_step(iter, iter->lh, iter->slot+1);
	return 1;
    }
    return 0;
}

#endif
