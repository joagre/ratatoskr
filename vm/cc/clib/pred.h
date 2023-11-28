#ifndef __PRED_H__
#define __PRED_H__

#include <stdint.h>
#include <stdlib.h>
#include <memory.h>

typedef uintptr_t parg_t;

typedef enum
{
    PARG_TYPE_INT =  0,   // integer constant
    PARG_TYPE_ATM =  1,   // nil | bool | atom variable | function name
    PARG_TYPE_TPL =  2,   // tuple | function application
    PARG_TYPE_LST =  3,   // list cell (pair)
} parg_type_t;

typedef enum
{
    PRED_TYPE_BOOL  = 0,
    PRED_TYPE_INT   = 1,
    PRED_TYPE_UINT  = 2,
    PRED_TYPE_BITS  = 3
} pred_type_t;

// atom names are stored as
// name[0] = length (8-bit)
// ...
// name[len+1] = 0

typedef uint8_t pred_atom_t;

typedef struct _pred_t
{
    pred_atom_t* name; // point to atom string table
    pred_type_t type;  // 
    size_t arity;      // arity
} pred_t;

static ssize_t pred_atom_len(pred_atom_t* atm)
{
    return (ssize_t) atm[0];
}

static char* pred_atom_name(pred_atom_t* atm)
{
    return (char*)(atm+1);
}

static intptr_t parg_int(parg_t arg)
{
    return ((intptr_t)arg)>>2;
}

static pred_atom_t* parg_atom(parg_t arg)
{
    return (pred_atom_t*) (arg & ~3);
}

static parg_t* parg_tpl(parg_t arg)
{
    return (parg_t*) (arg & ~3);
}

static parg_t* parg_lst(parg_t arg)
{
    return (parg_t*) (arg & ~3);
}

static parg_type_t parg_type(parg_t arg)
{
    return (parg_type_t) (arg & 3);
}

parg_t parg_make_int(intptr_t x)
{
    return ((x << 2)|PARG_TYPE_INT);
}

parg_t parg_make_ari(uintptr_t x)
{
    return ((x << 2)|PARG_TYPE_INT);
}

parg_t parg_make_atm(pred_atom_t* atm)
{
    return (((parg_t)atm)|PARG_TYPE_ATM);
}

parg_t parg_make_tpl(parg_t* app)
{
    return (((parg_t)app)|PARG_TYPE_TPL);
}

parg_t parg_make_app(parg_t* app)
{
    return (((parg_t)app)|PARG_TYPE_TPL);
}

parg_t parg_make_lst(parg_t* app)
{
    return (((parg_t)app)|PARG_TYPE_LST);
}

static int parg_cmp(parg_t a, parg_t b)
{
    parg_type_t at = (int) parg_type(a);
    parg_type_t bt = (int) parg_type(b);
    int r;

    if ((r = (int)at - (int)bt) == 0) {
	switch(at) {
	case PARG_TYPE_INT:
	    r = parg_int(a) - parg_int(b);
	    break;
	case PARG_TYPE_ATM:
	    r = strcmp(pred_atom_name(parg_atom(a)),
		       pred_atom_name(parg_atom(b)));
	    break;
	case PARG_TYPE_TPL: {
	    parg_t* as = parg_tpl(a);
	    parg_t* bs = parg_tpl(b);
	    int a_arity = parg_int(as[0]);
	    int b_arity = parg_int(bs[0]);
	    if ((r = (a_arity - b_arity)) == 0) {
		int arity = (a_arity < 0) ? (-a_arity-1)+1 : a_arity;
		int i;
		for (i = 1; i <= (int)arity; i++) {
		    if ((r = parg_cmp(as[i], bs[i])) != 0)
			return r;
		}
	    }
	    break;
	}
	case PARG_TYPE_LST: {
	    parg_t* as = parg_lst(a);
	    parg_t* bs = parg_lst(b);
	    if ((r = parg_cmp(as[0], bs[0])) == 0)
		r = parg_cmp(as[1], bs[1]);
	    break;
	}
	}
    }
    return r;
}

int pred_cmp(pred_t* a, parg_t* as, pred_t* b, parg_t* bs)
{
    int r;

    if ((r = strcmp(pred_atom_name(a->name),
		    pred_atom_name(b->name))) == 0) {
	if ((r = a->arity-b->arity) == 0) {
	    int i;
	    for (i = 0; i < (int)a->arity; i++) {
		if ((r = parg_cmp(as[i], bs[i])) != 0)
		    return r;
	    }
	    return 0;
	}
    }
    return r;
}


#endif
