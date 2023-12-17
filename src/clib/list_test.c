//
//  Test various list implementations
//

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define CAT_HELPER3(p,x,y) p ## x ## y
#define CAT3(p,x,y) CAT_HELPER3(p,x,y)

#define CAT_HELPER2(x,y) x ## y
#define CAT2(x,y) CAT_HELPER2(x,y)

#if defined(DLIST)
#include "dlist.h"
#define LINK dlink
#define LIST dlist
#elif defined(CDLIST)
#include "cdlist.h"
#define LINK cdlink
#define LIST cdlist
#elif defined(SLIST)
#include "slist.h"
#define LINK slink
#define LIST slist
#else
#error "DLIST,SLIST or CDLIST must be defined"
#endif

#define LINK_T CAT2(LINK, _t)
#define LIST_T CAT2(LIST, _t)

#define list_init(l) CAT2(LIST, _init)((l))
#define list_insert_first(l,e) CAT2(LIST, _insert_first)((l),(e))
#define list_insert_last(l,e) CAT2(LIST, _insert_last)((l),(e))
#define list_take_first(l) CAT2(LIST, _take_first)((l))

#define ITER CAT2(LIST, _iter)
#define ITER_T CAT2(ITER, _t)

#define iter_init(i,l) CAT2(ITER, _init)((i),(l))
#define iter_end(i) CAT2(ITER, _end)((i))
#define iter_current(i) CAT2(ITER, _current)((i))
#define iter_next(i) CAT2(ITER, _next)((i))
#define iter_remove(i) CAT2(ITER, _remove)((i))

typedef struct {
    LINK_T link;
    char c;
} link_t;

void* new_link(char c)
{
    link_t* link = malloc(sizeof(link_t));
    link->c = c;
    return link;
}

void interpret(LIST_T* list, char* ptr)
{
    int c;
    int cur = 0;
    // int rep = 1;
    
    while((c = *ptr++) != '\0') {
	if (isalpha(c))
	    cur = c;
//	else if (isdigit(c))
//	    rep = (c - '0');
	else {
	    switch(c) {
	    case '+':
		list_insert_first(list, new_link(cur));
		break;
	    case '-':
		list_insert_last(list, new_link(cur));
		break;
	    case '*':
		list_take_first(list);
		break;
	    case '/': {
		ITER_T iter;
		iter_init(&iter, list);
		while(!iter_end(&iter)) {
		    if (((link_t*)iter_current(&iter))->c == cur)
			iter_remove(&iter);
		    else
			iter_next(&iter);
		}
	    }
	       break;
	    }
	}
    }
}

void list_to_string(LIST_T* list, char* ptr)
{
    ITER_T iter;
    iter_init(&iter, list);
    while(!iter_end(&iter)) {
	*ptr++ = ((link_t*)iter_current(&iter))->c;
	iter_next(&iter);
    }
    *ptr++ = '\0';
}

void list_free(LIST_T* list)
{
    ITER_T iter;
    iter_init(&iter, list);
    while(!iter_end(&iter)) {
	link_t* p = iter_current(&iter);
	iter_remove(&iter);
	free(p);
    }
}

int run_case(char* test, char* expect)
{
    LIST_T list;
    char result[64];
    
    list_init(&list);

    interpret(&list, test);
    list_to_string(&list, result);
    if (strcmp(result, expect) != 0) {
	printf("fail case: %s\n", test);
	printf("  expect: %s\n", expect);
	printf("     got: %s\n", result);
	return 0;
    }
    list_free(&list);
    
    return 1;
}


int main(int argc, char** argv)
{
    (void) argc;
    (void) argv;
    
    run_case("A+", "A");
    run_case("A-", "A");

    run_case("A-B+", "BA");
    run_case("A+B-", "AB");

    run_case("A+B+C+D+", "DCBA");

    run_case("A+B+C+A/", "CB");
    run_case("A+B+C+B/", "CA");
    run_case("A+B+C+C/", "BA");    

    run_case("A-B-C-D-E-", "ABCDE");
    run_case("A-B-C-D-E-A/B/C/D/E/", "");    
    run_case("A-B-C-D-E-A/B/C/D/E/A+", "A");
    
    run_case("A+B+C+D+E+***", "BA");
    run_case("A+B+C+D+E+****", "A");
    run_case("A+B+C+D+E+*****", "");
    run_case("A+B+C+D+E+*****A-B-C-", "ABC");

    exit(0);
}
