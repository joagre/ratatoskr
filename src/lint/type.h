#ifndef __TYPE_H__
#define __TYPE_H__

#include "dynarr.h"

typedef dynarray_t types_t;

typedef enum {
    TYPE_TAG_BASIC_TYPE,
    TYPE_TAG_VARIABLE,
    TYPE_TAG_FUNCTION
} type_tag_t;

typedef enum {
    TYPE_BASIC_TYPE_INT,
    TYPE_BASIC_TYPE_BOOL
} type_basic_type_t;

typedef uint32_t type_variable_t;

typedef struct type {
    type_tag_t tag;
    union {
	type_basic_type_t basic_type;
	type_variable_t variable;
	struct {
	    types_t* arg_types;
	    struct type* return_type;
	} function_type;
    };
} type_t;

type_t* type_new_basic_type(type_basic_type_t basic_type);
type_t* type_new_variable(void);
type_t* type_new_function(types_t* arg_types, type_t* return_type);
char* type_basic_type_to_string(type_basic_type_t basic_type);
void type_print_type(type_t* type);

#endif
