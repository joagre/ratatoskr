#ifndef __LHASH_KV_H__
#define __LHASH_KV_H__

#include <stdint.h>

#include "lhash.h"

#ifndef LHASH_KV_KEY_TYPE
#define LHASH_KV_KEY_TYPE void*
#endif
#ifndef LHASH_KV_VALUE_TYPE
#define LHASH_KV_VALUE_TYPE void*
#endif

typedef LHASH_KV_KEY_TYPE lhash_kv_key_t;
typedef LHASH_KV_VALUE_TYPE lhash_kv_value_t;

typedef struct _hlink_kv_t // :hlink_t in lhash_t
{
    hlink_t h;
    lhash_kv_key_t key;
    lhash_kv_value_t data;
} hlink_kv_t;

typedef lhash_t lhash_kv_t;
typedef lhash_iter_t lhash_kv_iter_t;

#define LHASH_KV_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define LHASH_KV_API
#else
#define LHASH_KV_API __attribute__ ((unused))
#endif

LHASH_KV_LOCAL void lhash_kv_init(lhash_kv_t*, allocator_t* alloc,
				  lhash_hash_t, lhash_cmp_t) LHASH_KV_API;
LHASH_KV_LOCAL void lhash_kv_clear(lhash_kv_t*) LHASH_KV_API;
LHASH_KV_LOCAL void lhash_kv_reset(lhash_kv_t* lhk) LHASH_KV_API;
LHASH_KV_LOCAL int lhash_kv_find(lhash_kv_t*, lhash_kv_key_t key, lhash_kv_value_t* data) LHASH_KV_API;
LHASH_KV_LOCAL int lhash_kv_insert(lhash_kv_t*, lhash_kv_key_t key, lhash_kv_value_t data) LHASH_KV_API;
LHASH_KV_LOCAL int lhash_kv_remove(lhash_kv_t* lhash, lhash_kv_key_t key, lhash_kv_value_t* data) LHASH_KV_API;
LHASH_KV_LOCAL size_t lhash_kv_size(lhash_kv_t*)  LHASH_KV_API;
LHASH_KV_LOCAL int lhash_kv_is_empty(lhash_kv_t*)  LHASH_KV_API;

// iterator
LHASH_KV_LOCAL void lhash_kv_iter_init(lhash_kv_iter_t*, lhash_kv_t*) LHASH_KV_API;
LHASH_KV_LOCAL int lhash_kv_iter_current(lhash_kv_iter_t*, lhash_kv_key_t* key, lhash_kv_value_t* data) LHASH_KV_API;
LHASH_KV_LOCAL int lhash_kv_iter_end(lhash_kv_iter_t*) LHASH_KV_API;
LHASH_KV_LOCAL int lhash_kv_iter_next(lhash_kv_iter_t*) LHASH_KV_API;
LHASH_KV_LOCAL int lhash_kv_iter_remove(lhash_kv_iter_t*) LHASH_KV_API;


LHASH_KV_LOCAL size_t lhash_kv_size(lhash_kv_t* lhk)
{
    return lhash_size((lhash_t*)lhk);
}
    
LHASH_KV_LOCAL int lhash_kv_is_empty(lhash_kv_t* lhk)
{
    return lhash_is_empty((lhash_t*)lhk);
}

// lhash_kv hash and compare work on keys not objects!!!
// they must be wrapped
LHASH_KV_LOCAL void lhash_kv_init(lhash_kv_t* lhk, allocator_t* alloc,
				  lhash_hash_t hash, lhash_cmp_t cmp)
{
    lhash_init((lhash_t*)lhk, alloc, hash, cmp);
}

// remove all elements and deallocate internal kv links
LHASH_KV_LOCAL void lhash_kv_clear(lhash_kv_t* lhk)
{
    int slot = 0;
    lhash_t* lh = (lhash_t*) lhk;

    while(lh->size > 0) {
	while((slot < (int)lh->asize) &&
	      (slist_length(&lh->tab[slot]) == 0))
	    slot++;
	if (slot < (int)lh->asize) {
	    slist_iter_t iter;
	    slist_iter_init(&iter, &lh->tab[slot]);
	    while(!slist_iter_end(&iter)) {
		hlink_kv_t* hpk = slist_iter_current(&iter);
		slist_iter_remove(&iter);
		allocator_dtor(lh->alloc,hpk);		
		allocator_free(lh->alloc,hpk);
		lh->size--;
	    }
	}
    }
}

// clear table and shrink table to zero
LHASH_KV_LOCAL void lhash_kv_reset(lhash_kv_t* lhk)
{
    lhash_kv_clear(lhk);
    lhash_reset((lhash_t*)lhk);
}
    
LHASH_LOCAL int lhash_kv_find(lhash_kv_t* lhk, lhash_kv_key_t key, lhash_kv_value_t* data)
{
    hlink_kv_t* p;
    if (lhash_find((lhash_t*)lhk, (void*)((uintptr_t)key), (void**)&p)) {
	if (data) *data = p->data;
	return 1;
    }
    return 0;
}

LHASH_LOCAL int lhash_kv_insert(lhash_kv_t* lhk, lhash_kv_key_t key, lhash_kv_value_t data)
{
    hlink_kv_t* p;
    if ((p = allocator_alloc(lhk->alloc, sizeof(hlink_kv_t))) == NULL)
	return -1;
    p->key = key;
    p->data = data;
    if (!lhash_insert((lhash_t*) lhk, (void*)(uintptr_t)key, p)) {
	allocator_free(lhk->alloc,p);
	return 0;
    }
    return 1;
}

LHASH_LOCAL int lhash_kv_remove(lhash_t* lhk, lhash_kv_key_t key, lhash_kv_value_t* data)
{
    hlink_kv_t* p;
    if (!lhash_remove((lhash_t*) lhk, (void*)(uintptr_t)key, (void**)&p))
	return 0; // not found
    if (data) *data = p->data;
    allocator_free(lhk->alloc,p);
    return 1;
}

// iterator
LHASH_KV_LOCAL void lhash_kv_iter_init(lhash_kv_iter_t* iter, lhash_kv_t* lhk)
{
    lhash_iter_init(iter, (lhash_t*) lhk);
}

LHASH_KV_LOCAL int lhash_kv_iter_current(lhash_kv_iter_t* iter, lhash_kv_key_t* key, lhash_kv_value_t* data)
{
    hlink_kv_t* p;
    if (lhash_iter_current(iter, (void**) &p)) {
	if (key) *key = p->key;
	if (data) *data = p->data;
	return 1;
    }
    return 0;
}

LHASH_KV_LOCAL int lhash_kv_iter_end(lhash_kv_iter_t* iter)
{
    return lhash_iter_end(iter);
}

LHASH_KV_LOCAL int lhash_kv_iter_next(lhash_kv_iter_t* iter)
{
    return lhash_iter_next(iter);
}

LHASH_KV_LOCAL int lhash_kv_iter_remove(lhash_kv_iter_t* iter)
{
    hlink_kv_t* p;
    if (lhash_iter_current(iter, (void**) &p)) {
	lhash_iter_remove(iter);
	allocator_free(iter->lh->alloc,p);
	return 1;
    }
    return 0;
}

#endif
