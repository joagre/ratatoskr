#ifndef __PRED_FORMAT_H__
#define __PRED_FORMAT_H__

#include "pred.h"


static ssize_t parg_format_int_size(parg_t arg)
{
    intptr_t n = parg_int(arg);
    ssize_t size;

    if (n == 0) return 1;
    size = 0;
    if (n < 0) {
	n = -n;
	size++;  // count leading '-' sign
    }
    while(n) {
	size++;
	n /= 10;
    }
    return size;
}

// fixme: pass on the atom table!
static int parg_is_nil(parg_t arg)
{
    if (parg_type(arg) == PARG_TYPE_ATM) {
	pred_atom_t* name = parg_atom(arg);
	if ((name[0]==3) && (name[1]=='n') &&
	    (name[2]=='i') && (name[3]=='l') &&
	    (name[4]=='\0'))
	    return 1;
    }
    return 0;
}


static ssize_t parg_format_atm_size(parg_t arg)
{
    return pred_atom_len((pred_atom_t*) (arg & ~3));
}

static ssize_t parg_format_size(parg_t arg);

static ssize_t parg_format_tpl_size(parg_t arg)
{
    parg_t* tpl = parg_tpl(arg);
    ssize_t size;
    intptr_t arity;
    int i;

    switch(parg_type(tpl[0])) {
    case PARG_TYPE_INT:
	if ((arity = parg_int(tpl[0])) < 0) return -1;
	size = 2;  // '{' '}'
	if (arity > 0) { // 'a1','a2','...','an'
	    size += (arity-1);  // commas
	    for (i = 0; i < (int)arity; i++) {
		ssize_t r;
		if ((r = parg_format_size(tpl[i+1])) < 0)
		    return -1;
		size += r;
	    }
	}
	break;
    case PARG_TYPE_ATM:
	if ((arity = parg_int(tpl[1])) < 0) return -1;
	size = pred_atom_len(parg_atom(tpl[0]));
	size += 2;  // '(' and ')'
	if (arity > 0) {
	    size += (arity-1);  // ','
	    for (i = 0; i < (int)arity; i++) {
		ssize_t r;
		if ((r = parg_format_size(tpl[i+2])) < 0)
		    return -1;
		size += r;
	    }
	}
	break;
    default:
	return -1;
    }
    return size;
}

// format as: '[' hd '|' tl ']'
static ssize_t parg_format_lst_size(parg_t arg)
{
    parg_t* lst = parg_lst(arg);
    ssize_t size = 3;
    ssize_t r;
    if ((r = parg_format_size(lst[0])) < 0)
	return -1;
    size += r;
    if ((r = parg_format_size(lst[1])) < 0)
	return -1;
    size += r;
    return size;
}

// <int> | <str>
// '{' a1','...',' an '}' | '<func>'(' a1 ','...',' an ')'
// '[' h '|'' t ']'
static ssize_t parg_format_size(parg_t arg)
{
    switch(parg_type(arg)) {
    case PARG_TYPE_INT:
	return parg_format_int_size(arg);
    case PARG_TYPE_ATM:
	return parg_format_atm_size(arg);
    case PARG_TYPE_TPL:
	return parg_format_tpl_size(arg);
    case PARG_TYPE_LST:
	return parg_format_lst_size(arg);	
    default: return -1;
    }
}

static ssize_t pred_format_size(pred_t* pred, parg_t* tpl)
{
    ssize_t size = pred_atom_len(pred->name);
    if (pred->arity > 0) {
	int i;
	size += (2 + pred->arity-1);  // '(' and ')' and ',' * (arity-1)
	for (i = 0; i < pred->arity; i++) {
	    ssize_t r;
	    if ((r = parg_format_size(tpl[i])) < 0)
		return -1;
	    size += r;
	}
    }
    return size+1;  // include terminatring '\0'
}

static int parg_format_(parg_t arg, char* ptr);


static int parg_format_list_(parg_t* vec, size_t len, char* ptr)
{
    int i, r;
    int n = 0;

    r = parg_format_(vec[0], ptr);
    ptr += r;
    n += r;
    for (i = 1; i < len; i++) {
	*ptr++ = ',';
	n++;
	r = parg_format_(vec[i], ptr);
	ptr += r;
	n += r;
    }
    return n;
}

static int parg_format_tail_(parg_t tl, char* ptr)
{
    int r;
    int n = 0;

tail:
    if (parg_is_nil(tl)) {
	*ptr++ = ']';
	n++;
    }
    else if (parg_type(tl) == PARG_TYPE_LST) {
	parg_t* lst = parg_lst(tl);
	*ptr++ = ',';
	n++;
	r = parg_format_(lst[0], ptr);
	ptr += r;
	n += r;
	tl = lst[1];
	goto tail;
    }
    else {
	*ptr++ = '|';
	n++;
	r = parg_format_(tl, ptr);
	ptr += r;
	n += r;
	*ptr++ = ']';
	n++;
    }
    return n;
}

// unchecked format arg (check is done before this call)
static int parg_format_(parg_t arg, char* ptr)
{
    switch(parg_type(arg)) {
    case PARG_TYPE_INT:
	return sprintf(ptr, "%ld", parg_int(arg));
	
    case PARG_TYPE_ATM: {  // like variable name
	pred_atom_t* atm = parg_atom(arg);
	int len = (int) pred_atom_len(atm);
	// fixme: add escape and single quotes when needed
	memcpy(ptr, pred_atom_name(atm), len);
	return len;
    }
	
    case PARG_TYPE_TPL: {  // tuple | function application
	parg_t* tpl = parg_tpl(arg);
	int n = 0;
	int arity;
	switch(parg_type(tpl[0])) {
	case PARG_TYPE_INT: {  // tuple
	    if ((arity = parg_int(tpl[0])) < 0) return -1;
	    *ptr++ = '{';
	    n++;
	    if (arity > 0) {
		int r = parg_format_list_(tpl+1, arity, ptr);
		ptr += r;
		n += r;
	    }
	    *ptr++ = '}';
	    n++;
	    break;
	}
	case PARG_TYPE_ATM: { // function app
	    pred_atom_t* name = parg_atom(tpl[0]);
	    int len = (int) pred_atom_len(name);
	    if ((arity = parg_int(tpl[1])) < 0) return -1;
	    // fixme: add escape and single quotes when needed
	    memcpy(ptr, pred_atom_name(name), len);
	    ptr += len;
	    n += len;
	    *ptr++ = '(';
	    n++;
	    if (arity > 0) {
		int r = parg_format_list_(tpl+2, arity, ptr);
		ptr += r;
		n += r;
	    }
	    *ptr++ = ')';
	    n++;
	    break;
	}
	default:
	    return -1;
	}
	return n;
    }

    case PARG_TYPE_LST: {  // list cell (pair)
	int n = 0;
	int r;
	parg_t* lst = parg_lst(arg);

	*ptr++ = '[';
	n++;
	r = parg_format_(lst[0], ptr);
	ptr += r;
	n += r;
	r = parg_format_tail_(lst[1], ptr);
	ptr += r;
	n += r;
	return n;
    }
    }
}

static ssize_t parg_format(parg_t arg,char* buf,size_t size)
{
    char* ptr = buf;
    ssize_t n;    
    int r;
    
    if (((n = parg_format_size(arg)) < 0) || (n > size))
	return -1;
    r = parg_format_(arg, ptr);
    ptr += r;
    *ptr = '\0';
    return (ptr - buf);    
}


static ssize_t pred_format(pred_t* pred,parg_t* as,char* buf,size_t size)
{
    char* ptr = buf;
    ssize_t n;

    if (((n = pred_format_size(pred, as)) < 0) || (n > size))
	return -1;
    n = pred_atom_len(pred->name);
    // fixme: add escape and single quotes when needed    
    memcpy(ptr, pred_atom_name(pred->name), n);
    ptr += n;

    if (pred->arity > 0) {
	int i, r;
	*ptr++ = '(';
	r = parg_format_list_(as, pred->arity, ptr);
	ptr += r;
	*ptr++ = ')';
    }
    *ptr = '\0';
    return (ptr - buf);
}

#endif
