#ifndef __AVL_H__
#define __AVL_H__

#include <stdlib.h>
#include <stdint.h>

#include "dynarr.h"

typedef struct _avl_node_t
{
    struct _avl_node_t *left;
    struct _avl_node_t *right;
    int ht;
} avl_node_t;

typedef int (*avl_cmp_t)(void* key, avl_node_t* obj);
typedef void* (*avl_key_t)(avl_node_t* obj);

typedef struct _avl_t
{
    avl_node_t* root;    // root of the tree
    size_t size;         // number of elements in tree
    avl_cmp_t cmp;       // compare function
    avl_key_t key;       // access key function    
}  avl_t;

typedef struct _avl_iter_t
{
    avl_t* avl;
    int sp;
    dynarray_t stack;
} avl_iter_t;

#define AVL_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define AVL_API
#else
#define AVL_API __attribute__ ((unused))
#endif

AVL_LOCAL void avl_init(avl_t*, avl_cmp_t, avl_key_t) AVL_API;
AVL_LOCAL void avl_clear(avl_t*) AVL_API;
AVL_LOCAL int avl_find(avl_t*, void* key, void** obj) AVL_API;
AVL_LOCAL int avl_insert(avl_t*, void* key, void* obj) AVL_API;
AVL_LOCAL int avl_remove(avl_t* avl, void* key, void** obj) AVL_API;
AVL_LOCAL size_t avl_size(avl_t*)  AVL_API;
AVL_LOCAL int avl_is_empty(avl_t*)  AVL_API;
AVL_LOCAL int avl_height(avl_t*) AVL_API;

// iterator
AVL_LOCAL int avl_iter_init(avl_iter_t*, avl_t*) AVL_API;
AVL_LOCAL int avl_iter_clear(avl_iter_t*) AVL_API;
AVL_LOCAL int avl_iter_set(avl_iter_t*, avl_t*, void* key) AVL_API;
AVL_LOCAL int avl_iter_current(avl_iter_t*, void** data) AVL_API;
AVL_LOCAL int avl_iter_end(avl_iter_t*) AVL_API;
AVL_LOCAL int avl_iter_next(avl_iter_t*) AVL_API;
AVL_LOCAL int avl_iter_remove(avl_iter_t*) AVL_API;
AVL_LOCAL int avl_iter_postfix_next(avl_iter_t* iter) AVL_API;

AVL_LOCAL avl_node_t* avl_node_find(avl_t*, avl_node_t*,void* key) AVL_API;
AVL_LOCAL avl_node_t* avl_node_next(avl_t*, avl_node_t*) AVL_API;
AVL_LOCAL int avl_node_insert(avl_t*, avl_node_t**, avl_node_t*,
			      void* key, avl_node_t* obj) AVL_API;
//AVL_LOCAL int avl_node_delete(avl_t*,avl_node_t**,avl_node_t*,
//			      void* key, void** ptr) AVL_API;
AVL_LOCAL int avl_node_unlink(avl_t*,avl_node_t**,avl_node_t*,
			      void* key, avl_node_t** ptr) AVL_API;
AVL_LOCAL int avl_node_height(avl_node_t*) AVL_API;

AVL_LOCAL avl_node_t* avl_node_rotate_right(avl_node_t *) AVL_API;
AVL_LOCAL avl_node_t* avl_node_rotate_left(avl_node_t *) AVL_API;
AVL_LOCAL avl_node_t *avl_RR(avl_node_t *) AVL_API;
AVL_LOCAL avl_node_t *avl_LL(avl_node_t *) AVL_API;
AVL_LOCAL avl_node_t *avl_LR(avl_node_t *) AVL_API;
AVL_LOCAL avl_node_t *avl_RL(avl_node_t *) AVL_API;
AVL_LOCAL int avl_BF(avl_node_t *)  AVL_API;

AVL_LOCAL size_t avl_size(avl_t* avl)
{
    return avl->size;
}

AVL_LOCAL int avl_is_empty(avl_t* avl)
{
    return avl->size == 0;
}

AVL_LOCAL int avl_height(avl_t* avl)
{
    avl_node_t* n = avl->root;
    return (n == NULL) ? 0 : n->ht;
}

AVL_LOCAL void avl_init(avl_t* avl, avl_cmp_t cmp, avl_key_t key)
{
    avl->root = NULL;
    avl->size = 0;
    avl->cmp = cmp;
    avl->key = key;
}

// delete all nodes in the tree
AVL_LOCAL void avl_clear(avl_t* avl)
{
    avl->root = NULL;
    avl->size = 0;
}

AVL_LOCAL int avl_insert(avl_t* avl, void* key, void* obj)
{
    return avl_node_insert(avl, &avl->root, avl->root, key, (avl_node_t*)obj);
}

// find and delete the node by the 'key' return 1 if found
// and this case also store the 'data' associcated with 'key'
// in the location pointed to by 'ptr', unless 'ptr' is NULL.
// If 'key' is not found then 'ptr' is not touched and 0 is
// returned
AVL_LOCAL int avl_remove(avl_t* avl, void* key, void** obj)
{
    avl_node_t* n;
    if (avl_node_unlink(avl, &avl->root, avl->root, key, &n)) {
	if (obj) *obj = n;
	return 1;
    }
    return 0;
}

// find and node by the 'key' return 1 if found
// and this case also store the 'data' associcated with 'key'
// in the location pointed to by 'ptr', unless 'ptr' is NULL.
// If 'key' is not found then 'ptr' is not touched and 0 is
// returned
AVL_LOCAL int avl_find(avl_t* avl, void* key, void** obj)
{
    avl_node_t* n;
    if ((n = avl_node_find(avl, avl->root, key)) != NULL) {
	if (obj) *obj = (void*) n;
	return 1;
    }
    return 0;
}

AVL_LOCAL avl_node_t* avl_node_find(avl_t* avl, avl_node_t* n, void* key)
{
    while(n != NULL) {
	int c = (*avl->cmp)(key, n);
	if (c > 0)
	    n = n->right;
	else if (c < 0)
	    n = n->left;
	else
	    return n;
    }
    return NULL;
}

// return successor node of n
AVL_LOCAL avl_node_t* avl_node_next(avl_t* avl, avl_node_t* n)
{
    (void) avl;
    if ((n = n->right) != NULL) {
	while(n->left != NULL)
	    n = n->left;
    }
    return n;
}

AVL_LOCAL int avl_node_insert(avl_t* avl, avl_node_t** rn, avl_node_t* n,
			      void* key, avl_node_t* obj)
{
    int r;
    if (n == NULL) {
	avl->size++;
	obj->left = NULL;
	obj->right = NULL;
	obj->ht = 0;
	*rn = obj;
	return 1;
    }
    else {
	int c = (*avl->cmp)(key, n);
	if (c > 0) {
	    r = avl_node_insert(avl,&n->right,n->right,key,obj);
	    if (r) {
		if (avl_BF(n) == -2) {
		    c = (*avl->cmp)(key, n->right);
		    if (c > 0)
			n = avl_RR(n);
		    else
			n = avl_RL(n);
		    *rn = n;
		}
		n->ht = avl_node_height(n);
	    }
	    return r;
	}
	else if (c < 0) {
	    r = avl_node_insert(avl,&n->left,n->left,key,obj);
	    if (r) {
		if (avl_BF(n) == 2) {
		    c = (*avl->cmp)(key, n->left);
		    if (c < 0)
			n = avl_LL(n);
		    else
			n = avl_LR(n);
		    *rn = n;
		}
		n->ht = avl_node_height(n);
	    }
	    return r;	    
	}
	return 0;
    }
}

// find successor to n and unlink it and store it in ptr
// update rn with rebalanced node
AVL_LOCAL void avl_node_unlink_successor(avl_node_t** rn,
					 avl_node_t* n,
					 avl_node_t** ptr)
{
    if (n->left != NULL) {
	avl_node_unlink_successor(&n->left,n->left, ptr);
	if (avl_BF(n) == -2) {	       // rebalance during windup
	    if (avl_BF(n->right) <= 0)
		n = avl_RR(n);
	    else
		n = avl_RL(n);
	    *rn = n;
	}
	n->ht = avl_node_height(n);
    }
    else { // found successor n, (n->left = NULL)
	*ptr = n;
	if ((n = n->right) != NULL) {
	    if (avl_BF(n) == 2) { //rebalance during windup
		if (avl_BF(n->left) >=0)
		    n = avl_LL(n);
		else
		    n = avl_LR(n);
	    }
	    n->ht = avl_node_height(n);
	}
	*rn = n;
    }
}


AVL_LOCAL int avl_node_unlink(avl_t* avl,
			      avl_node_t** rn, avl_node_t* n,
			      void* key, avl_node_t** ptr)
{
    int c;
    int r;
    
    if (n == NULL)
	return 0;
    c = (*avl->cmp)(key, n);
    if (c > 0) {  // unlink in right subtree
	r = avl_node_unlink(avl,&n->right,n->right,key,ptr);
	if (r) {
	    if (avl_BF(n) == 2) {
		if (avl_BF(n->left) >= 0)
		    n = avl_LL(n);
		else
		    n = avl_LR(n);
		*rn = n;
	    }
	    n->ht = avl_node_height(n);
	}
	return r;
    }
    else if (c < 0) {
	r = avl_node_unlink(avl,&n->left,n->left,key,ptr);
	if (r) {
	    if (avl_BF(n) == -2) {	       // rebalance during windup
		if (avl_BF(n->right) <= 0)
		    n = avl_RR(n);
		else
		    n = avl_RL(n);
		*rn = n;
	    }
	    n->ht = avl_node_height(n);
	}
	return r;
    }
    else {
	if (n->right != NULL) { // unlink its inorder succesor
	    avl_node_t* p;
	    avl_node_unlink_successor(&n->right, n->right, &p);

	    // link p as n in place
	    p->left = n->left;
	    p->right = n->right;
	    
	    *rn = p;
	    *ptr = n;
	    n = p;
	    if (avl_BF(n)==2) { //rebalance during windup
		if (avl_BF(n->left)>=0)
		    n = avl_LL(n);
		else
		    n = avl_LR(n);
		*rn = n;
	    }
	    n->ht = avl_node_height(n);
	}
	else {
	    *ptr = n;
	    *rn = n->left;
	}
	avl->size--;
	return 1;
    }
}

AVL_LOCAL int avl_node_height(avl_node_t *n)
{
    int lh,rh;
    
    if (n == NULL)
	return(0);
    lh = n->left  ? 1+n->left->ht : 0;
    rh = n->right ? 1+n->right->ht : 0;
    return (lh>rh) ? lh : rh;
}
 
AVL_LOCAL avl_node_t* avl_node_rotate_right(avl_node_t *n)
{
    avl_node_t* y = n->left;
    n->left = y->right;
    y->right = n;
    n->ht = avl_node_height(n);
    y->ht = avl_node_height(y);
    return y;
}
 
AVL_LOCAL avl_node_t* avl_node_rotate_left(avl_node_t *n)
{
    avl_node_t* y = n->right;
    n->right = y->left;
    y->left = n;
    n->ht = avl_node_height(n);
    y->ht = avl_node_height(y);
    return y;
}
 
AVL_LOCAL avl_node_t* avl_RR(avl_node_t *n)
{
    n = avl_node_rotate_left(n);
    return n;
}
 
AVL_LOCAL avl_node_t* avl_LL(avl_node_t *n)
{
    n = avl_node_rotate_right(n);
    return n;
}
 
AVL_LOCAL avl_node_t *avl_LR(avl_node_t *n)
{
    n->left = avl_node_rotate_left(n->left);
    n = avl_node_rotate_right(n);
    return n;
}
 
AVL_LOCAL avl_node_t* avl_RL(avl_node_t *n)
{
    n->right = avl_node_rotate_right(n->right);
    n = avl_node_rotate_left(n);
    return n;
}
 
AVL_LOCAL int avl_BF(avl_node_t *n)
{
    int lh,rh;
    if (n == NULL)
	return 0;
    lh = n->left  ? 1+n->left->ht : 0;
    rh = n->right ? 1+n->right->ht : 0;
    return lh-rh;
}

// iterator
AVL_LOCAL avl_node_t* avl_iter_element(avl_iter_t* iter, int i, int* markp)
{
    uintptr_t* ptr = dynarray_element(&iter->stack, i);
    if (markp) *markp = (*ptr & 1);
    return (avl_node_t*) (*ptr & ~1);
}

AVL_LOCAL void avl_iter_setelement(avl_iter_t* iter, int i,
				   avl_node_t* n, int mark)
{
    uintptr_t u = mark | (uintptr_t) n;
    dynarray_setelement(&iter->stack, i, &u);
}


AVL_LOCAL int avl_iter_init(avl_iter_t* iter, avl_t* avl)
{
    avl_node_t* n;
    size_t depth = avl_height(avl);
    int sp;
    
    dynarray_init(&iter->stack, NULL, depth, sizeof(uintptr_t));
    dynarray_resize(&iter->stack, depth);
    sp = -1;
    n = avl->root;
    while(n != NULL) {
	avl_iter_setelement(iter, ++sp, n, 0);
	n = n->left;
    }
    iter->sp = sp;
    return 0;
}

AVL_LOCAL int avl_iter_clear(avl_iter_t* iter)
{
    iter->sp = -1;
    dynarray_clear(&iter->stack);
    return 0;
}


// setup iterator stack to point to key node
AVL_LOCAL int avl_iter_set(avl_iter_t* iter, avl_t* avl, void* key)
{
    avl_node_t* n = avl->root;
    int sp = -1;

    while(n != NULL) {
	int c = (*avl->cmp)(key, n);
	if (c > 0) {
	    avl_iter_setelement(iter, ++sp, n, 1);
	    n = n->right;
	}
	else if (c < 0) {
	    avl_iter_setelement(iter, ++sp, n, 0);
	    n = n->left;
	}
	else {
	    avl_iter_setelement(iter, ++sp, n, 0);
	    iter->sp = sp;
	    return 1;
	}
    }
    return 0;
}


AVL_LOCAL int avl_iter_current(avl_iter_t* iter, void** data)
{
    if (iter->sp < 0)
	return 0;
    else {
	if (data)
	    *data = avl_iter_element(iter, iter->sp, NULL);
	return 1;
    }
}

AVL_LOCAL int avl_iter_end(avl_iter_t* iter)
{
    return (iter->sp < 0);
}

// inorder next
AVL_LOCAL int avl_iter_next(avl_iter_t* iter)
{
    if (iter->sp < 0)
	return -1;
    else {
	int is_marked;
	avl_node_t* n = avl_iter_element(iter, iter->sp, &is_marked);

	if (!is_marked) {
	    // mark node and push right child subtree
	    avl_iter_setelement(iter, iter->sp, n, 1);
	    is_marked = 1;

	    if ((n = n->right) != NULL) {
		avl_iter_setelement(iter, ++iter->sp, n, 0);
		is_marked = 0;
		n = n->left;  // go down the right child left path
		while(n != NULL) {
		    avl_iter_setelement(iter, ++iter->sp, n, 0);
		    n = n->left;
		}
	    }
	}
	while (is_marked && (iter->sp > 0))
	    (void) avl_iter_element(iter, --iter->sp, &is_marked);

	if (is_marked && (iter->sp == 0)) {
	    avl_iter_clear(iter);
	    return 0;
	}
	return 1;
    }
}

AVL_LOCAL int avl_iter_postfix_next(avl_iter_t* iter)
{
    if (iter->sp < 0)
	return -1;
    else if (iter->sp == 0) {
	avl_iter_clear(iter);
	return 0;
    }
    else {
	int is_marked;
	avl_node_t* n = avl_iter_element(iter, iter->sp, &is_marked);	
	if (is_marked)
	    iter->sp--;  // pop stack
	else { // left child
	    avl_node_t* p;
	    p = avl_iter_element(iter, iter->sp-1, NULL); // get parent
	    if ((n = p->right) == NULL)
		iter->sp--;  // pop if now right child
	    else {
		avl_iter_setelement(iter, iter->sp, n, 1);
		n = n->left;  // go down the right child left path
		while(n != NULL) {
		    avl_iter_setelement(iter, ++iter->sp, n, 0);
		    n = n->left;
		}
	    }
	}
    }
    return 0;
}

// remove can only be used once now since traverse a tree that is
// rebalancing is a bit tricky (could work)
AVL_LOCAL int avl_iter_remove(avl_iter_t* iter)
{
    if (iter->sp >= 0) {
	avl_node_t* p = avl_iter_element(iter, iter->sp, NULL);
	avl_node_t* n;
	if (avl_remove(iter->avl, (*iter->avl->key)(p), (void**)&n)) {
	    if ((n = avl_node_next(iter->avl, n)) == NULL)
		avl_iter_clear(iter);
	    else
		avl_iter_set(iter, iter->avl, (*iter->avl->key)(n));
	    return 1;
	}
    }
    return 0;
}

#endif
