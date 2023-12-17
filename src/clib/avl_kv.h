#ifndef __AVL_KV_H__
#define __AVL_KV_H__

#include "allocator.h"
#include "avl.h"

typedef struct _avl_kv_node_t // :avk_node_t
{
    avl_node_t node;
    void* key;
    void* data;
} avl_kv_node_t;

typedef struct _avl_kv_t
{
    avl_t avl;
    allocator_t* alloc;
} avl_kv_t;

typedef avl_iter_t avl_kv_iter_t;

#define AVL_KV_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define AVL_KV_API
#else
#define AVL_KV_API __attribute__ ((unused))
#endif

AVL_KV_LOCAL void avl_kv_init(avl_kv_t*, allocator_t*, avl_cmp_t) AVL_KV_API;
AVL_KV_LOCAL void avl_kv_clear(avl_kv_t*) AVL_KV_API;
AVL_KV_LOCAL int avl_kv_find(avl_kv_t*, void* key, void** data) AVL_KV_API;
AVL_KV_LOCAL int avl_kv_insert(avl_kv_t*, void* key, void* data) AVL_KV_API;
AVL_KV_LOCAL int avl_kv_remove(avl_kv_t* avl, void* key, void** data) AVL_KV_API;
AVL_KV_LOCAL size_t avl_kv_size(avl_kv_t*)  AVL_KV_API;
AVL_KV_LOCAL int avl_kv_is_empty(avl_kv_t*)  AVL_KV_API;
AVL_KV_LOCAL int avl_kv_height(avl_kv_t*) AVL_KV_API;

// iterator
AVL_KV_LOCAL int avl_kv_iter_init(avl_kv_iter_t*, avl_kv_t*) AVL_KV_API;
AVL_KV_LOCAL int avl_kv_iter_clear(avl_kv_iter_t*) AVL_KV_API;
AVL_KV_LOCAL int avl_kv_iter_current(avl_kv_iter_t*, void** key, void** data) AVL_KV_API;
AVL_KV_LOCAL int avl_kv_iter_end(avl_kv_iter_t*) AVL_KV_API;
AVL_KV_LOCAL int avl_kv_iter_next(avl_kv_iter_t*) AVL_KV_API;
AVL_KV_LOCAL int avl_kv_iter_remove(avl_kv_iter_t* iter) AVL_KV_API;

AVL_KV_LOCAL int avl_kv_BF(avl_kv_node_t *)  AVL_API;
AVL_KV_LOCAL void avl_kv_node_clear(avl_kv_t*, avl_node_t *) AVL_API;

AVL_KV_LOCAL void* avl_kv_key(void* obj)
{
    return ((avl_kv_node_t*)obj)->key;
}

AVL_KV_LOCAL void avl_kv_init(avl_kv_t* avlk,allocator_t* alloc,avl_cmp_t cmp)
{
    if (alloc == NULL) alloc = allocator_std();
    avl_init((avl_t*)avlk, cmp, avl_kv_key);
    avlk->alloc = alloc;
}

// must free all nodes since they are allocated by avl_kv
AVL_KV_LOCAL void avl_kv_clear(avl_kv_t* avlk)
{
    if (avlk->avl.root)
	avl_kv_node_clear(avlk, avlk->avl.root);
    avl_clear((avl_t*)avlk);
}


AVL_KV_LOCAL int avl_kv_find(avl_kv_t* avlk, void* key, void** data)
{
    avl_kv_node_t* n;
    if (avl_find((avl_t*)avlk, key, (void**) &n)) {
	if (data) *data = n->data;
	return 1;
    }
    return 0;
}

AVL_KV_LOCAL int avl_kv_insert(avl_kv_t* avlk, void* key, void* data)
{
    avl_kv_node_t* n;

    if ((n = allocator_alloc(avlk->alloc, sizeof(avl_kv_node_t))) == NULL)
	return -1;
    n->key = key;
    n->data = data;
    if (avl_insert((avl_t*)avlk, key, n))
	return 1;
    allocator_free(avlk->alloc, n);
    return 0;    
}

AVL_KV_LOCAL int avl_kv_remove(avl_kv_t* avlk, void* key, void** data)
{
    avl_kv_node_t* n;
    if (avl_remove((avl_t*)avlk, key, (void**)&n)) {
	if (data) *data = n->data;
	allocator_free(avlk->alloc, n);
	return 1;
    }
    return 0;
}

AVL_KV_LOCAL size_t avl_kv_size(avl_kv_t* avlk)
{
    return avl_size((avl_t*)avlk);
}

AVL_KV_LOCAL int avl_kv_is_empty(avl_kv_t* avlk)
{
    return avl_is_empty((avl_t*)avlk);
}

AVL_KV_LOCAL int avl_kv_height(avl_kv_t* avlk)
{
    return avl_height((avl_t*)avlk);
}


AVL_KV_LOCAL int avl_kv_BF(avl_kv_node_t *n)
{
    return avl_BF((avl_node_t*) n);
}

AVL_KV_LOCAL void avl_kv_node_clear(avl_kv_t* avlk, avl_node_t* n)
{
    if (n->left != NULL) avl_kv_node_clear(avlk, n->left);
    if (n->right != NULL) avl_kv_node_clear(avlk, n->right);
    allocator_free(avlk->alloc, n);
}

// iterator
AVL_KV_LOCAL int avl_kv_iter_init(avl_kv_iter_t* iter, avl_kv_t* avlk)
{
    return avl_iter_init(iter, (avl_t*)avlk);
}

AVL_KV_LOCAL int avl_kv_iter_clear(avl_kv_iter_t* iter)
{
    return avl_iter_clear(iter);
}


AVL_KV_LOCAL int avl_kv_iter_current(avl_kv_iter_t* iter, void** key, void** data)
{
    avl_kv_node_t* n;
    if (avl_iter_current(iter, (void**) &n)) {
	if (key) *key = n->key;
	if (data) *data = n->data;
	return 1;
    }
    return 0;
}

AVL_KV_LOCAL int avl_kv_iter_end(avl_kv_iter_t* iter)
{
    return avl_iter_end(iter);
}

AVL_KV_LOCAL int avl_kv_iter_next(avl_kv_iter_t* iter)
{
    return avl_iter_next(iter);
}

AVL_KV_LOCAL int avl_kv_iter_remove(avl_kv_iter_t* iter)
{
    avl_kv_node_t* n;
    if (avl_iter_current(iter, (void**) &n)) {
	if (avl_iter_remove(iter)) {
	    avl_kv_t* avlk = (avl_kv_t*) iter->avl;
	    allocator_free(avlk->alloc, n);
	    return 1;
	}
    }
    return 0;
}

#endif
