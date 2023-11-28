//
//  Test various array implementations
//

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define CAT_HELPER3(p,x,y) p ## x ## y
#define CAT3(p,x,y) CAT_HELPER3(p,x,y)

#define CAT_HELPER2(x,y) x ## y
#define CAT2(x,y) CAT_HELPER2(x,y)

#if defined(DYNARR)
#include "dynarr.h"
#define ARRAY_T dynarray_t
#define ARR     dynarray
#elif defined(SEGARR)
#include "segarr.h"
#define ARRAY_T segarray_t
#define ARR     segarray
#else
#error "DYNARR or SEGARR must be defined"
#endif

#define array_init(arr,alc,cap,wid) CAT2(ARR, _init)((arr),(alc),(cap),(wid))
#define array_clear(arr) CAT2(ARR, _clear)((arr))
#define array_prepend(arr, ptr) CAT2(ARR, _prepend)((arr),(ptr))
#define array_append(arr, ptr) CAT2(ARR, _append)((arr),(ptr))
#define array_drop(arr) CAT2(ARR, _drop)((arr))
#define array_delete(arr,ind) CAT2(ARR, _delete)((arr),(ind))
#define array_element(arr,ind) CAT2(ARR, _element)((arr),(ind))
#define array_size(arr) CAT2(ARR, _size)((arr))

typedef struct {
    char c;
} elem_t;

void interpret(ARRAY_T* arr, char* ptr)
{
    int c;
    int cur = 0;
    int rep = 1;
    elem_t elem;
    
    while((c = *ptr++) != '\0') {
	if (isalpha(c))
	    elem.c = c;
	else if (isdigit(c))
	    rep = (c - '0');
	else {
	    switch(c) {
	    case '+':
		array_prepend(arr, &elem);
		break;
	    case '-':
		array_append(arr, &elem);
		break;
	    case '*':
		array_drop(arr);
		break;
	    case '/': {
		int i = 0;
		while(i < array_size(arr)) {
		    if (((elem_t*)array_element(arr, i))->c == elem.c)
			array_delete(arr, i);
		    else
			i++;
		}
		break;
	    }
	    default:
		break;
	    }
	}
    }
}

void array_to_string(ARRAY_T* arr, char* ptr)
{
    int i;
    for (i = 0; i < array_size(arr); i++) {
	elem_t* ep = array_element(arr, i);
	*ptr++ = ep->c;
    }
    *ptr++ = '\0';
}

void array_free(ARRAY_T* arr)
{
    array_clear(arr);
}

int run_case(char* test, char* expect)
{
    ARRAY_T array;
    char result[64];
    size_t len;

    printf("run case %s\n", test);
    
    array_init(&array, NULL, 0, sizeof(elem_t));

    interpret(&array, test);
    array_to_string(&array, result);
    len = array_size(&array);
    if (strcmp(result, expect) != 0) {
	printf("fail case: %s\n", test);
	printf("  expect: %s\n", expect);
	printf("     got: %s\n", result);
	return 0;
    }
    if (len != strlen(expect)) {
	printf("fail case: %s\n", test);
	printf("  expect len: %d\n", (int)strlen(expect));
	printf("     got len: %d\n", (int)len);
	return 0;
    }
    array_free(&array);
    
    return 1;
}


int main(int argc, char** argv)
{
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
