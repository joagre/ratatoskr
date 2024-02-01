#include "type.h"
#include "types.h"
#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

// Forward declarations of local functions (alphabetical order)
static type_variable_t next_type_variable(void);

type_t* type_new_basic_type(type_basic_type_t basic_type) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_BASIC_TYPE;
    type->basic_type = basic_type;
    return type;
}

type_t* type_new_variable(void) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_VARIABLE;
    type->variable = next_type_variable();
    return type;
}

type_t* type_new_function(types_t* arg_types, type_t* return_type) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_FUNCTION;
    type->function_type.arg_types = arg_types;
    type->function_type.return_type = return_type;
    return type;
}

char* type_basic_type_to_string(type_basic_type_t basic_type) {
    switch (basic_type) {
	case TYPE_BASIC_TYPE_INT:
	    return "Int";
	case TYPE_BASIC_TYPE_BOOL:
	    return "Bool";
	default:
	    assert(false);
    }
}

void type_print_type(type_t* type) {
    switch (type->tag) {
	case TYPE_TAG_BASIC_TYPE:
	    switch (type->basic_type) {
		case TYPE_BASIC_TYPE_INT:
		    printf("int");
		    break;
		case TYPE_BASIC_TYPE_BOOL:
		    printf("bool");
		    break;
	    }
	    break;
	case TYPE_TAG_VARIABLE:
	    printf("%d", type->variable);
	    break;
	case TYPE_TAG_FUNCTION:
	    printf("{");
	    printf("[");
	    for (size_t i = 0; i < type->function_type.arg_types->size; i++) {
		type_t* arg_type =
		    types_lookup(type->function_type.arg_types, i);
		type_print_type(arg_type);
		if (i < type->function_type.arg_types->size - 1) {
		    printf(", ");
		}
	    }
	    printf("], ");
	    type_print_type(type->function_type.return_type);
	    printf("}");
	    break;
    }
}

//
// Local functions (alphabetical order)
//

static type_variable_t next_type_variable(void) {
    static type_variable_t next_type_variable = 0;
    return next_type_variable++;
}
