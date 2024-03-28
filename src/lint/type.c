#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "types.h"

// Forward declarations of local functions
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

type_t* type_new_function_type(types_t* generic_types,
			       types_t* arg_types, type_t* return_type) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_FUNCTION_TYPE;
    type->function_type.generic_types = generic_types;
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
/*
type_t* type_new_record_def_member_method_type(
    char* name, record_def_member_method_modifier_t modifier, type_t* type) {
    type_t* record_def_member_method_type = malloc(sizeof(type_t));
    record_def_member_method_type->tag = TYPE_TAG_RECORD_DEF_MEMBER_METHOD_TYPE;
    record_def_member_method_type->record_def_member_method_type.name = name;
    record_def_member_method_type->record_def_member_method_type.modifier =
	modifier;
    record_def_member_method_type->record_def_member_method_type.type = type;
    return record_def_member_method_type;
}

type_t* type_new_record_def_member_property_type(
    char* name, record_def_member_property_modifier_t modifier, type_t* type) {
    type_t* record_def_member_property_type = malloc(sizeof(type_t));
    record_def_member_property_type->tag =
	TYPE_TAG_RECORD_DEF_MEMBER_PROPERTY_TYPE;
    record_def_member_property_type->record_def_member_property_type.name =
	name;
    record_def_member_property_type->record_def_member_property_type.modifier =
	modifier;
    record_def_member_property_type->record_def_member_property_type.type =
	type;
    return record_def_member_property_type;
}

type_t* type_new_record_type(char* name) {
    type_t* record_type = malloc(sizeof(type_t));
    record_type->tag = TYPE_TAG_RECORD_TYPE;
    record_type->record_name = name;
    return record_type;
}
*/

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

void type_free(type_t* type) {
    free(type);
}

char* type_basic_type_to_string(type_basic_type_t basic_type) {
    switch (basic_type) {
	case TYPE_BASIC_TYPE_BOOL:
	    return "Bool";
	case TYPE_BASIC_TYPE_INT:
	    return "Int";
	case TYPE_BASIC_TYPE_FLOAT:
	    return "Float";
	case TYPE_BASIC_TYPE_CHAR:
	    return "Char";
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
    } else if (strcmp(string, "Char") == 0) {
	return TYPE_BASIC_TYPE_CHAR;
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
		case TYPE_BASIC_TYPE_CHAR:
		    printf("char");
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
	case TYPE_TAG_CONSTRUCTOR_TYPE:
	    printf("{constructor, %s, [", type->constructor_type.name);
	    for (uint16_t i = 0; i < type->constructor_type.types->size; i++) {
		type_t* constructor_type =
		    types_get(type->constructor_type.types, i);
		type_print_type(constructor_type);
		if (i < type->constructor_type.types->size - 1) {
		    printf(", ");
		}
	    }
	    printf("]}");
	    break;
	case TYPE_TAG_FUNCTION_TYPE:
	    printf("{function, ");
	    // Generic types
	    printf("[");
	    for (uint16_t i = 0;
		 i < type->function_type.generic_types->size; i++) {
		type_t* generic_type =
		    types_get(type->function_type.generic_types, i);
		type_print_type(generic_type);
		if (i < type->function_type.generic_types->size - 1) {
		    printf(", ");
		}
	    }
	    printf("], ");
	    // Argument types
	    printf("[");
	    for (uint16_t i = 0;
		 i < type->function_type.arg_types->size; i++) {
		type_t* arg_type =
		    types_get(type->function_type.arg_types, i);
		type_print_type(arg_type);
		if (i < type->function_type.arg_types->size - 1) {
		    printf(", ");
		}
	    }
	    printf("], ");
	    type_print_type(type->function_type.return_type);
	    printf("}");
	    break;
	case TYPE_TAG_LIST_TYPE:
	    printf("{list, ");
	    type_print_type(type->list_type);
	    printf("}");
	    break;
	case TYPE_TAG_EMPTY_LIST_TYPE:
	    printf("empty_list");
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
	    /*
	case TYPE_TAG_RECORD_DEF_MEMBER_METHOD_TYPE:
	    printf("{method, %s, ", type->record_member_method_type.name);
	    switch (type->record_member_method_type.modifier) {
		case RECORD_DEF_MEMBER_METHOD_MODIFIER_PUBLIC:
		    printf("public, ");
		    break;
		case RECORD_DEF_MEMBER_METHOD_MODIFIER_PRIVATE:
		    printf("private, ");
		    break;
		default:
		    assert(false);
	    }
	    type_print_type(type->record_member_method_type.type);
	    printf("}");
	    break;
	case TYPE_TAG_RECORD_DEF_MEMBER_PROPERTY_TYPE:
	    printf("{property, %s, ", type->record_member_property_type.name);
	    switch (type->record_member_property_type.modifier) {
		case RECORD_DEF_MEMBER_PROPERTY_MODIFIER_PUBLIC:
		    printf("public, ");
		    break;
		case RECORD_DEF_MEMBER_PROPERTY_MODIFIER_PUBLIC_CONSTANT:
		    printf("public_constant, ");
		    break;
		case RECORD_DEF_MEMBER_PROPERTY_MODIFIER_PRIVATE:
		    printf("private, ");
		    break;
		case RECORD_DEF_MEMBER_PROPERTY_MODIFIER_PRIVATE_CONSTANT:
		    printf("private_constant, ");
		    break;
		case RECORD_DEF_MEMBER_PROPERTY_MODIFIER_READONLY:
		    printf("readonly, ");
		    break;
		default:
		    assert(false);
	    }
	    type_print_type(type->record_member_property_type.type);
	    printf("}");
	    break;
	case TYPE_TAG_RECORD_DEF_TYPE:
	    printf("{record, [");
	    for (uint16_t i = 0; i < type->record_type.types->size; i++) {
		type_t* record_type =
		    types_get(type->record_type.types, i);
		type_print_type(record_type);
		if (i < type->record_type.types->size - 1) {
		    printf(", ");
		}
	    }
	    printf("]}");
	    break;
	    */
	case TYPE_TAG_TUPLE_TYPE:
	    printf("{tuple, [");
	    for (uint16_t i = 0; i < type->tuple_types->size; i++) {
		type_t* tuple_type =
		    types_get(type->tuple_types, i);
		type_print_type(tuple_type);
		if (i < type->tuple_types->size - 1) {
		    printf(", ");
		}
	    }
	    printf("]}");
	    break;
	case TYPE_TAG_EMPTY_TUPLE_TYPE:
	    printf("empty_tuple");
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
// Local functions
//

static type_variable_t next_type_variable(void) {
    static type_variable_t next_type_variable = 0;
    return next_type_variable++;
}
