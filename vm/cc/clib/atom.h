#ifndef __ATOM_H__
#define __ATOM_H__

#include <stdint.h>

#include "lhash.h"

// Atoms are max 255 length. The length is stored in the first
// byte and the last byte is a terminating zero, neither are counted for!
// atom("foo") = [3][f][o][o][0]
// use atom_name(atm) to get a regular c string (ascii|utf8) pointer
typedef struct _atom_t // :hlink_t in lhash_t
{
    hlink_t h;
    char atm[0];
} atom_t;

#define MAX_ATOM_LEN 255

typedef struct _atom_table_t
{
    lhash_t h;
    char* atm_nil;
    char* atm_true;
    char* atm_false;
    char* atm_undefined;
} atom_table_t;

typedef lhash_iter_t atom_iter_t;

#ifndef ATOM_ALLOC
#define ATOM_ALLOC(n) malloc((n))
#define ATOM_REALLOC(ptr,n) realloc((ptr),(n))
#define ATOM_FREE(ptr) free((ptr))
#endif

#define ATOM_LOCAL static
#if defined(__WIN32__) || defined(_WIN32)
#define ATOM_API
#else
#define ATOM_API __attribute__ ((unused))
#endif

ATOM_LOCAL void atom_init(atom_table_t*) ATOM_API;
ATOM_LOCAL void atom_clear(atom_table_t*) ATOM_API;
ATOM_LOCAL int atom_find(atom_table_t*, char* key, char** data) ATOM_API;
ATOM_LOCAL int atom_insert(atom_table_t*, char* key, char** data) ATOM_API;
ATOM_LOCAL int atom_insert_len(atom_table_t*, char* key, size_t len, char** data) ATOM_API;
ATOM_LOCAL int atom_add(atom_table_t*, char* key, char** data) ATOM_API;
ATOM_LOCAL int atom_remove(atom_table_t* lhash, char* key) ATOM_API;
ATOM_LOCAL size_t atom_size(atom_table_t*)  ATOM_API;
ATOM_LOCAL int atom_is_empty(atom_table_t*)  ATOM_API;
ATOM_LOCAL char* atom_nil(atom_table_t*)  ATOM_API;
ATOM_LOCAL char* atom_true(atom_table_t*)  ATOM_API;
ATOM_LOCAL char* atom_false(atom_table_t*)  ATOM_API;
ATOM_LOCAL char* atom_undefined(atom_table_t*)  ATOM_API;


// iterator
ATOM_LOCAL void atom_iter_init(atom_iter_t*, atom_table_t*) ATOM_API;
ATOM_LOCAL int atom_iter_current(atom_iter_t*, char** key) ATOM_API;
ATOM_LOCAL int atom_iter_end(atom_iter_t*) ATOM_API;
ATOM_LOCAL int atom_iter_next(atom_iter_t*) ATOM_API;
ATOM_LOCAL int atom_iter_remove(atom_iter_t*) ATOM_API;


ATOM_LOCAL char* atom_nil(atom_table_t* tab)
{
    if (tab->atm_nil == NULL)
	atom_insert(tab, "nil", &tab->atm_nil);
    return tab->atm_nil;
}

ATOM_LOCAL char* atom_true(atom_table_t* tab)
{
    if (tab->atm_true == NULL)
	atom_insert(tab, "true", &tab->atm_true);
    return tab->atm_true;
}

ATOM_LOCAL char* atom_false(atom_table_t* tab)
{
    if (tab->atm_false == NULL)
	atom_insert(tab, "false", &tab->atm_false);
    return tab->atm_false;
}

ATOM_LOCAL char* atom_undefined(atom_table_t* tab)
{
    if (tab->atm_undefined == NULL)
	atom_insert(tab, "undefined", &tab->atm_undefined);
    return tab->atm_undefined;
}
    
// length part of an atom (char pointer) found in atom_table
static ssize_t atom_len(char* atm)
{
    return (ssize_t) ((unsigned char*)atm)[0];
}

// name part of an atom (char pointer) found in atom_table
static char* atom_name(char* atm)
{
    return (char*)(atm+1);
}

ATOM_LOCAL size_t atom_size(atom_table_t* lhk)
{
    return lhash_size((lhash_t*)lhk);
}
    
ATOM_LOCAL int atom_is_empty(atom_table_t* lhk)
{
    return lhash_is_empty((lhash_t*)lhk);
}

ATOM_LOCAL lhash_value_t atom_hash_(char* key, void* arg)
{
    lhash_value_t h = 5381;
    uint8_t* ptr = (uint8_t*) key;
    size_t len = strlen(key);
    while(len--)
	h = ((h << 5) + h) + (*ptr++);
    return h;
}

ATOM_LOCAL int atom_cmp_(char* key, atom_t* obj, void* arg)
{
    return strcmp(key, obj->atm+1);
}

// atom hash and compare work on keys not objects!!!
// they must be wrapped
ATOM_LOCAL void atom_init(atom_table_t* tab)
{
    memset(tab, 0, sizeof(atom_table_t));
    lhash_init((lhash_t*)tab, NULL,
	       (lhash_hash_t)atom_hash_, (lhash_cmp_t)atom_cmp_);
}

// remove all elements and deallocate internal kv links
ATOM_LOCAL void atom_clear(atom_table_t* tab)
{
    int slot = 0;
    lhash_t* lh = (lhash_t*) tab;

    while(lh->size > 0) {
	while((slot < (int)lh->asize) &&
	      (slist_length(&lh->tab[slot]) == 0))
	    slot++;
	if (slot < (int)lh->asize) {
	    slist_iter_t iter;
	    slist_iter_init(&iter, &lh->tab[slot]);
	    while(!slist_iter_end(&iter)) {
		atom_t* ap = slist_iter_current(&iter);
		slist_iter_remove(&iter);
		ATOM_FREE(ap);
		lh->size--;
	    }
	}
    }
}

// clear table and shrink table to zero
ATOM_LOCAL void atom_reset(atom_table_t* tab)
{
    atom_clear(tab);
    lhash_reset((lhash_t*)tab);
}

LHASH_LOCAL int atom_find(atom_table_t* tab, char* key, char** data)
{
    atom_t* p;
    if (lhash_find((lhash_t*)tab, (void*)key, (void**)&p)) {
	if (data) *data = p->atm;
	return 1;
    }
    return 0;
}

LHASH_LOCAL int atom_insert_len(atom_table_t* tab, char* key, size_t len,
				char** data)
{
    atom_t* p;
    if (len > MAX_ATOM_LEN) return -1;  // too long
    if ((p = ATOM_ALLOC(sizeof(atom_t)+len+2)) == NULL)
	return -1;
    p->atm[0] = len;
    memcpy(p->atm+1, key, len);
    p->atm[len+1] = '\0';
    if (!lhash_insert((lhash_t*) tab, key, p)) {
	ATOM_FREE(p);
	return 0;
    }
    if (data) *data = p->atm;
    return 1;
}

LHASH_LOCAL int atom_insert(atom_table_t* tab, char* key, char** data)
{
    return atom_insert_len(tab, key, strlen(key), data);
}

LHASH_LOCAL int atom_add(atom_table_t* tab, char* key, char** data)
{
    if (atom_find(tab, key, data))
	return 1;
    return atom_insert(tab, key, data);
}

LHASH_LOCAL int atom_remove(atom_table_t* tab, char* key)
{
    atom_t* p;
    if (!lhash_remove((lhash_t*) tab, key, (void**)&p))
	return 0; // not found
    ATOM_FREE(p);
    return 1;
}

// iterator
ATOM_LOCAL void atom_iter_init(atom_iter_t* iter, atom_table_t* tab)
{
    lhash_iter_init(iter, (lhash_t*) tab);
}

ATOM_LOCAL int atom_iter_current(atom_iter_t* iter, char** key)
{
    atom_t* p;
    if (lhash_iter_current(iter, (void**) &p)) {
	if (key) *key = p->atm;
	return 1;
    }
    return 0;
}

ATOM_LOCAL int atom_iter_end(atom_iter_t* iter)
{
    return lhash_iter_end(iter);
}

ATOM_LOCAL int atom_iter_next(atom_iter_t* iter)
{
    return lhash_iter_next(iter);
}

ATOM_LOCAL int atom_iter_remove(atom_iter_t* iter)
{
    atom_t* p;
    if (lhash_iter_current(iter, (void**) &p)) {
	lhash_iter_remove(iter);
	ATOM_FREE(p);
	return 1;
    }
    return 0;
}

#endif
