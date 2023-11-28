#ifndef __PRED_PARSE_H__
#define __PRED_PARSE_H__

#include "pred.h"

// parse terms
// <term> :=
//       ('0'..'9')+
//   '-' ('0'..'9')+
//   ('a'..'z')+
//   ('a'..'z')+ '(' (<term> [',' <term> ]*)? ')'
//  '{' (<term> [',' <term> ]*)? '}'
//  '[' <term> '|' <term> ']'
//
static char* skip_ws(char* ptr)
{
    while((*ptr == ' ') || (*ptr == '\t') || (*ptr == '\r') || (*ptr == '\n'))
	ptr++;
    return ptr;
}

static int is_digit(char* ptr)
{
    return (*ptr >= '0') && (*ptr <= '9');
}

static int is_lower(char* ptr)
{
    return (*ptr >= 'a') && (*ptr <= 'z');
}

static char* parse_int(char* ptr, intptr_t* rp)
{
    int sign = 0;
    if (*ptr == '-') {
	sign = 1;
	ptr++;
    }
    if (is_digit(ptr)) {
	intptr_t x = *ptr++ - '0';
	while(is_digit(ptr))
	    x = 10*x + (*ptr++ - '0');
	if (sign) x = -x;
	*rp = parg_make_int(x);
	return ptr;
    }
    return NULL;
}

// need atom table
static char* parse_atm(char* ptr, atom_table_t* tab, parg_t* rp)
{
    char buf[MAX_ATOM_LEN+1];
    char* dst = &buf[0];
    char* atm;
    size_t n = 0;
    while((n <= MAX_ATOM_LEN) && is_lower(ptr)) {
	*dst++ = *ptr++;
	n++;
    }
    if (n > MAX_ATOM_LEN) return NULL; // atom too big
    *dst = '\0';
    atom_add(tab, buf, &atm);
    *rp = parg_make_atm(atm);
    return ptr;
}

static char* parg_parse(char* ptr, atom_table_t* tab, parg_t* rp);

// estimate the number of element in a tuple
// { }   =>  0
// { 12 } => 1
// { [], {} } => 2
// { [a,b], 12, {1,{2},3} } => 3
//
#define RB 0  // round bracket ()
#define CB 1  // curly bracket {}
#define SB 2  // square bracket []

// FIXME: could be improved if we could calculate once
// when parsing a deep structure or if we could parse
// bottom up?
static int parg_parse_num_elements(char* ptr,char bracket)
{
    int cnt[3];
    int empty = 1;
    int i, j, k, n=0;

    switch(bracket) { // start bracket type
    case '(': i=RB; j=CB; k=SB; break;
    case '{': i=CB; j=SB; k=RB; break;
    case '[': i=SB; j=CB; k=RB; break;
    default: return -1;
    }
    cnt[i] = 1;
    cnt[j] = 0;
    cnt[k] = 0;

    while(*ptr && cnt[i]) {
	switch(*ptr) {
	case '{': cnt[CB]++; empty = 0; break;
	case '}': cnt[CB]--; break;
	case '[': cnt[SB]++; empty = 0; break;
	case ']': cnt[SB]--; break;
	case '(': cnt[RB]++; empty = 0; break;
	case ')': cnt[RB]--; break;	    
	case ',':
	    if ((cnt[k]==0) && (cnt[j]==0) && (cnt[i]==1)) n++;
	    break;
	case ' ':
	case '\t':
	case '\r':
	case '\n':
	    break;
	default:
	    empty = 0;
	    break;
	}
	ptr++;
    }
    return n + !empty;
}

static char* parg_parse_tpl(char* ptr, atom_table_t* tab, parg_t* rp)
{
    size_t n;
    parg_t* dst;

    n = parg_parse_num_elements(ptr, '{');
    dst = (parg_t*) malloc(sizeof(parg_t)*(n+1));
    n = 0;
    while((*ptr != '}')) {
	if ((ptr = parg_parse(ptr, tab, &dst[n+1])) == NULL)
	    return NULL;
	n++;
	ptr = skip_ws(ptr);
	if (*ptr == ',') {
	    ptr++;
	    ptr = skip_ws(ptr);
	}
	else if (*ptr != '}')
	    return NULL;
    }
    if (*ptr == '}')
	ptr++;
    else {
	free(dst);
	return NULL;
    }
    dst[0] = parg_make_int(n);
    *rp = parg_make_tpl(dst);
    return ptr;
}

static char* parg_parse_fun(char* ptr,atom_table_t* tab,parg_t name,parg_t* rp)
{
    intptr_t n;
    parg_t* dst;

    n = parg_parse_num_elements(ptr, '(');
    dst = (parg_t*) malloc(sizeof(parg_t)*(n+2));
    n = 0;
    while((*ptr != ')')) {
	if ((ptr = parg_parse(ptr, tab, &dst[n+2])) == NULL)
	    return NULL;
	n++;
	ptr = skip_ws(ptr);
	if (*ptr == ',')
	    ptr = skip_ws(ptr+1);  // first skip ','
	else if (*ptr != ')')
	    return NULL;
    }
    if (*ptr == ')')
	ptr++;
    else {
	free(dst);
	return NULL;
    }	
    dst[0] = name;
    dst[1] = parg_make_ari(n);
    *rp = parg_make_tpl(dst);
    return ptr;
}

static char* parg_parse_tail(char* ptr, atom_table_t* tab, parg_t* rp)
{
    if (*ptr == ']') {
	*rp = parg_make_atm(atom_nil(tab));
	return ptr+1;
    }
    else if (*ptr == ',') {
	parg_t hd, tl;
	parg_t* dst;
	ptr = skip_ws(ptr+1);  // first skip ','
	if ((ptr = parg_parse(ptr, tab, &hd)) == NULL)
	    return NULL;
	ptr = skip_ws(ptr);
	if ((ptr = parg_parse_tail(ptr, tab, &tl)) == NULL)
	    return NULL;
	dst = (parg_t*) malloc(sizeof(parg_t)*2);
	dst[0] = hd;
	dst[1] = tl;
	*rp = parg_make_lst(dst);
	return ptr;
    }
    else if (*ptr == '|') {
	parg_t tl;
	ptr = skip_ws(ptr+1); // first skip '|'
	if ((ptr = parg_parse(ptr, tab, &tl)) == NULL)
	    return NULL;
	ptr = skip_ws(ptr);
	if (*ptr != ']') return NULL;
	*rp = tl;
	return ptr+1;
    }
    return NULL;
}

static char* parg_parse_lst(char* ptr, atom_table_t* tab, parg_t* rp)
{
    parg_t hd, tl;
    parg_t* dst;
    
    if (*ptr == ']') {
	*rp = parg_make_atm(atom_nil(tab));
	return ptr+1;
    }
    if ((ptr = parg_parse(ptr, tab, &hd)) == NULL)
	return NULL;
    ptr = skip_ws(ptr);
    if ((ptr = parg_parse_tail(ptr, tab, &tl)) == NULL)
	return NULL;
    dst = (parg_t*) malloc(sizeof(parg_t)*2);
    dst[0] = hd;
    dst[1] = tl;
    *rp = parg_make_lst(dst);
    return ptr;
}


static char* parg_parse(char* ptr, atom_table_t* tab, parg_t* rp)
{
    ptr = skip_ws(ptr);
    if (is_digit(ptr) || ((*ptr == '-') && is_digit(ptr+1))) {
	intptr_t x;
	if ((ptr = parse_int(ptr, rp)) == NULL)
	    return NULL;
	return ptr;
    }
    else if (is_lower(ptr)) {
	parg_t name;
	if ((ptr = parse_atm(ptr, tab, &name)) == NULL)
	    return NULL;
	ptr = skip_ws(ptr);
	if (*ptr != '(') {
	    *rp = name;
	    return ptr;
	}
	return parg_parse_fun(ptr+1, tab, name, rp);
    }
    else if (*ptr == '{') {
	ptr = skip_ws(ptr+1);	
	return parg_parse_tpl(ptr, tab, rp);
    }
    else if (*ptr == '[') {
	ptr = skip_ws(ptr+1);
	return parg_parse_lst(ptr, tab, rp);
    }
    return NULL;
}


#endif
