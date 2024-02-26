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

type_t* type_new_constructor_type(char* name, types_t* types) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_CONSTRUCTOR_TYPE;
    type->constructor_type.name = name;
    type->constructor_type.types = types;
    return type;
}

type_t* type_new_function_type(types_t* arg_types, type_t* return_type) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_FUNCTION_TYPE;
    type->function_type.arg_types = arg_types;
    type->function_type.return_type = return_type;
    return type;
}

type_t* type_new_list_type(type_t* list_type) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_LIST_TYPE;
    type->list_type = list_type;
    return type;
}

type_t* type_new_empty_list_type(void) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_EMPTY_LIST_TYPE;
    return type;
}

type_t* type_new_map_type(type_t* key_type, type_t* value_type) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_MAP_TYPE;
    type->map_type.key_type = key_type;
    type->map_type.value_type = value_type;
    return type;
}

type_t* type_new_empty_map_type(void) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_EMPTY_MAP_TYPE;
    return type;
}

type_t* type_new_tuple_type(types_t* tuple_types) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_TUPLE_TYPE;
    type->tuple_types = tuple_types;
    return type;
}

type_t* type_new_empty_tuple_type(void) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_EMPTY_TUPLE_TYPE;
    return type;
}

type_t* type_new_type_variable(void) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_TYPE_VARIABLE;
    type->type_variable = next_type_variable();
    return type;
}

char* type_basic_type_to_string(type_basic_type_t basic_type) {
    switch (basic_type) {
	case TYPE_BASIC_TYPE_BOOL:
	    return "Bool";
	case TYPE_BASIC_TYPE_INT:
	    return "Int";
	case TYPE_BASIC_TYPE_FLOAT:
	    return "Float";
	case TYPE_BASIC_TYPE_STRING:
	    return "String";
	case TYPE_BASIC_TYPE_TASK:
	    return "Task";
	default:
	    assert(false);
    }
}

type_basic_type_t type_string_to_basic_type(const char* string) {
    if (strcmp(string, "Bool") == 0) {
	return TYPE_BASIC_TYPE_BOOL;
    } else if (strcmp(string, "Int") == 0) {
	return TYPE_BASIC_TYPE_INT;
    } else if (strcmp(string, "Float") == 0) {
	return TYPE_BASIC_TYPE_FLOAT;
    } else if (strcmp(string, "String") == 0) {
	return TYPE_BASIC_TYPE_STRING;
    } else if (strcmp(string, "Task") == 0) {
	return TYPE_BASIC_TYPE_TASK;
    } else {
	assert(false);
    }
}

void type_print_type(type_t* type) {
    switch (type->tag) {
	case TYPE_TAG_BASIC_TYPE:
	    switch (type->basic_type) {
		case TYPE_BASIC_TYPE_BOOL:
		    printf("bool");
		    break;
		case TYPE_BASIC_TYPE_INT:
		    printf("int");
		    break;
		case TYPE_BASIC_TYPE_FLOAT:
		    printf("float");
		    break;
		case TYPE_BASIC_TYPE_STRING:
		    printf("string");
		    break;
		case TYPE_BASIC_TYPE_TASK:
		    printf("task");
		    break;
		default:
		    assert(false);
	    }
	    break;
	case TYPE_TAG_LIST_TYPE:
	    printf("{list, ");
	    type_print_type(type->list_type);
	    printf("}");
	    break;
	case TYPE_TAG_EMPTY_LIST_TYPE:
	    printf("empty_list");
	    break;
	case TYPE_TAG_FUNCTION_TYPE:
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
	case TYPE_TAG_TUPLE_TYPE:
	    printf("{tuple, [");
	    for (size_t i = 0; i < type->tuple_types->size; i++) {
		type_t* tuple_type =
		    types_lookup(type->tuple_types, i);
		type_print_type(tuple_type);
		if (i < type->tuple_types->size - 1) {
		    printf(", ");
		}
	    }
	    printf("]}");
	    break;
	case TYPE_TAG_MAP_TYPE:
	    printf("{map, ");
	    type_print_type(type->map_type.key_type);
	    printf(", ");
	    type_print_type(type->map_type.value_type);
	    printf("}");
	    break;
	case TYPE_TAG_EMPTY_MAP_TYPE:
	    printf("empty_map");
	    break;
	case TYPE_TAG_CONSTRUCTOR_TYPE:
	    printf("{constructor, %s, [", type->constructor_type.name);
	    for (size_t i = 0; i < type->constructor_type.types->size; i++) {
		type_t* constructor_type =
		    types_lookup(type->constructor_type.types, i);
		type_print_type(constructor_type);
		if (i < type->constructor_type.types->size - 1) {
		    printf(", ");
		}
	    }
	    printf("]}");
	    break;
	case TYPE_TAG_TYPE_VARIABLE:
	    printf("%d", type->type_variable);
	    break;
	default:
	    printf("default\n");
	    assert(false);
    }
}

//
// Local functions (alphabetical order)
//

static type_variable_t next_type_variable(void) {
    static type_variable_t next_type_variable = 0;
    return next_type_variable++;
}
