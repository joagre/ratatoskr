#ifndef LINT_TYPE_H
#define LINT_TYPE_H

// types_t is defined here instead of being included from
// types.h. Including types.h here would lead to a cyclic dependency.
#include "dynarr.h"
typedef dynarray_t types_t;

typedef enum {
    TYPE_TAG_BASIC_TYPE,
    TYPE_TAG_CONSTRUCTOR_TYPE, // FIXME
    TYPE_TAG_FUNCTION_TYPE,
    TYPE_TAG_LIST_TYPE,
    TYPE_TAG_EMPTY_LIST_TYPE,
    TYPE_TAG_MAP_TYPE,
    TYPE_TAG_EMPTY_MAP_TYPE,
//    TYPE_TAG_RECORD_DEF_MEMBER_METHOD_TYPE,
//    TYPE_TAG_RECORD_DEF_MEMBER_PROPERTY_TYPE,
//    TYPE_TAG_RECORD_TYPE,
    TYPE_TAG_TUPLE_TYPE,
    TYPE_TAG_EMPTY_TUPLE_TYPE,
    TYPE_TAG_TYPE_VARIABLE
} type_tag_t;

typedef enum {
    TYPE_BASIC_TYPE_BOOL,
    TYPE_BASIC_TYPE_INT,
    TYPE_BASIC_TYPE_FLOAT,
    TYPE_BASIC_TYPE_CHAR,
    TYPE_BASIC_TYPE_STRING,
    TYPE_BASIC_TYPE_TASK
} type_basic_type_t;

/*
typedef enum {
    RECORD_DEF_MEMBER_METHOD_MODIFIER_PUBLIC,
    RECORD_DEF_MEMBER_METHOD_MODIFIER_PRIVATE
} record_def_member_method_modifier_t;

typedef enum {
    RECORD_DEF_MEMBER_PROPERTY_MODIFIER_PUBLIC,
    RECORD_DEF_MEMBER_PROPERTY_MODIFIER_PUBLIC_CONSTANT,
    RECORD_DEF_MEMBER_PROPERTY_MODIFIER_PRIVATE,
    RECORD_DEF_MEMBER_PROPERTY_MODIFIER_PRIVATE_CONSTANT,
    RECORD_DEF_MEMBER_PROPERTY_MODIFIER_READONLY
} record_def_member_property_modifier_t;
*/

typedef uint32_t type_variable_t;

typedef struct type {
    type_tag_t tag;
    union {
	// Basic type
	type_basic_type_t basic_type;
	// Constructor type
	struct {
	    char* name;
	    types_t* types;
	} constructor_type;
	// Function type
	struct {
	    types_t* generic_types;
	    types_t* arg_types;
	    struct type* return_type;
	} function_type;
	// List type
	struct type* list_type;
	// Map type
	struct {
	    struct type* key_type;
	    struct type* value_type;
	} map_type;
	// Record definition member method type
//	struct {
//	    char* name;
//	    record_def_member_method_modifier_t modifier;
//	    struct type* type;
//	} record_def_member_method_type;
	// Record definition member property type
//	struct {
//	    char* name;
//	    record_def_member_property_modifier_t modifier;
//	    struct type* type;
//	} record_def_member_property_type;
	// Record type
//	char* record_name;
	// Tuple type
	types_t* tuple_types;
	// Type variable
	type_variable_t type_variable;
    };
} type_t;

type_t* type_new_basic_type(type_basic_type_t basic_type);

type_t* type_new_constructor_type(char* name, types_t* types);

type_t* type_new_function_type(types_t* generic_types,
			       types_t* arg_types, type_t* return_type);

type_t* type_new_list_type(type_t* list_type);
type_t* type_new_empty_list_type(void);

type_t* type_new_map_type(type_t* key_type, type_t* value_type);
type_t* type_new_empty_map_type(void);

/*
type_t* type_new_record_def_member_method_type(
    char* name, record_def_member_method_modifier_t modifier, type_t* type);
type_t* type_new_record_def_member_property_type(
    char* name, record_def_member_property_modifier_t modifier, type_t* type);
type_t* type_new_record_type(char *name);
*/

type_t* type_new_tuple_type(types_t* tuple_types);
type_t* type_new_empty_tuple_type(void);

type_t* type_new_type_variable(void);

void type_free(type_t* type);

char* type_basic_type_to_string(type_basic_type_t basic_type);
type_basic_type_t type_string_to_basic_type(const char* string);
void type_print_type(type_t* type);

#endif
