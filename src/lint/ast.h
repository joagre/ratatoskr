#ifndef LINT_AST_H
#define LINT_AST_H

#include <stdio.h>
#include <dynarr.h>
#include <assert.h>
#include <stdbool.h>
#include "type.h"

typedef dynarray_t node_array_t;

#define GENERATE_ENUM(ENUM) ENUM,
#define GENERATE_STRING(STRING) #STRING,
#define FOREACH_NODE_NAME(NODE_NAME) \
	NODE_NAME(ADD_INT) \
        NODE_NAME(ADD_FLOAT) \
        NODE_NAME(ALIAS_DEF)   \
	NODE_NAME(AND) \
        NODE_NAME(AS) \
        NODE_NAME(FUNCTION_TYPE) \
	NODE_NAME(ARGS) \
	NODE_NAME(ARG_TYPES) \
        NODE_NAME(BIND) \
        NODE_NAME(BITWISE_AND) \
        NODE_NAME(BITWISE_COMPLEMENT) \
        NODE_NAME(BITWISE_OR) \
        NODE_NAME(BITWISE_XOR) \
        NODE_NAME(BLOCK) \
        NODE_NAME(BOOL_TYPE)  \
        NODE_NAME(BOUND_NAME)  \
        NODE_NAME(BSL) \
        NODE_NAME(BSR) \
        NODE_NAME(CASE) \
        NODE_NAME(CHAR) \
	NODE_NAME(CHAR_TYPE) \
        NODE_NAME(CONSTRUCTOR) \
        NODE_NAME(CONSTRUCTOR_LITERAL) \
        NODE_NAME(CONSTRUCTORS)	\
        NODE_NAME(CONCAT_LIST)\
	NODE_NAME(CONCAT_MAP) \
	NODE_NAME(CONCAT_STRING) \
	NODE_NAME(CONS) \
	NODE_NAME(CONST) \
        NODE_NAME(CONSTRUCTOR_TYPE ) \
        NODE_NAME(DEFAULT) \
        NODE_NAME(DIV_INT) \
        NODE_NAME(DIV_FLOAT) \
        NODE_NAME(ELIF) \
        NODE_NAME(ELSE) \
	NODE_NAME(EMPTY_MAP_TYPE) \
	NODE_NAME(EMPTY_LIST_TYPE) \
	NODE_NAME(EMPTY_TUPLE_TYPE) \
        NODE_NAME(ENUM) \
        NODE_NAME(ENUM_DEF) \
        NODE_NAME(ENUM_LITERAL) \
	NODE_NAME(ENUM_VALUE) \
        NODE_NAME(ENUMS) \
        NODE_NAME(EQ) \
        NODE_NAME(EQ_TYPE) \
        NODE_NAME(ESCAPE_CHAR) \
        NODE_NAME(EXP) \
        NODE_NAME(EXPORT) \
        NODE_NAME(EXPR) \
        NODE_NAME(EXPRS) \
        NODE_NAME(FALSE) \
        NODE_NAME(FLOAT) \
        NODE_NAME(FLOAT_TYPE) \
        NODE_NAME(FUNCTION_CALL) \
        NODE_NAME(FUNCTION_DEF) \
        NODE_NAME(FUNCTION_LITERAL) \
        NODE_NAME(FUNCTION_NAME) \
        NODE_NAME(GT_INT) \
        NODE_NAME(GT_FLOAT) \
        NODE_NAME(GTE_INT) \
        NODE_NAME(GTE_FLOAT) \
        NODE_NAME(IDENTIFIER) \
        NODE_NAME(IF) \
        NODE_NAME(IMPORT) \
        NODE_NAME(IMPORTED_DEFS) \
        NODE_NAME(IMPORTS) \
        NODE_NAME(INDEX_VALUE) \
        NODE_NAME(INT) \
	NODE_NAME(INT_TYPE) \
        NODE_NAME(INTERFACE) \
        NODE_NAME(INTERFACE_DEF) \
        NODE_NAME(INTERFACE_MEMBER_METHOD) \
        NODE_NAME(INTERFACE_MEMBER_PROPERTY) \
        NODE_NAME(INTERFACE_MEMBERS) \
        NODE_NAME(INTERFACE_METHOD) \
        NODE_NAME(INTERFACES) \
        NODE_NAME(IS) \
	NODE_NAME(LAUNCH_TASK) \
        NODE_NAME(LIST_LITERAL) \
        NODE_NAME(LIST_LOOKUP) \
        NODE_NAME(LIST_SLICE)   \
        NODE_NAME(LIST_TYPE) \
        NODE_NAME(LIST_UPDATE) \
        NODE_NAME(LT_INT) \
        NODE_NAME(LTE_INT) \
        NODE_NAME(LT_FLOAT) \
        NODE_NAME(LTE_FLOAT) \
        NODE_NAME(MAP_KEY_VALUE) \
        NODE_NAME(MAP_KEY_VALUES) \
        NODE_NAME(MAP_LITERAL) \
	NODE_NAME(MAP_LOOKUP) \
        NODE_NAME(MAP_TYPE) \
        NODE_NAME(MAP_UPDATE) \
	NODE_NAME(MEMBER_METHOD) \
        NODE_NAME(MEMBER_PROPERTY) \
	NODE_NAME(NEW_RECORD) \
	NODE_NAME(SUB_INT) \
        NODE_NAME(SUB_FLOAT) \
        NODE_NAME(MODULE) \
        NODE_NAME(MODULE_ALIAS) \
        NODE_NAME(MOD) \
        NODE_NAME(MUL_INT) \
        NODE_NAME(MUL_FLOAT) \
        NODE_NAME(NAME) \
        NODE_NAME(NAMED_ARG) \
	NODE_NAME(NAMED_ARG_NAME) \
        NODE_NAME(NAMED_ARGS) \
        NODE_NAME(NE) \
        NODE_NAME(NEG_INT) \
        NODE_NAME(NEG_FLOAT) \
        NODE_NAME(NON_QUOTE_CHAR) \
        NODE_NAME(NOT) \
        NODE_NAME(NOT_SET) \
        NODE_NAME(OR) \
        NODE_NAME(PARAM_NAME) \
        NODE_NAME(PARAMS) \
	NODE_NAME(POS_INT) \
        NODE_NAME(POS_FLOAT) \
        NODE_NAME(POSTFIX) \
        NODE_NAME(PRIVATE) \
        NODE_NAME(PROGRAM) \
        NODE_NAME(PUBLIC) \
        NODE_NAME(RAW_STRING) \
        NODE_NAME(READONLY) \
        NODE_NAME(RECORD_DEF) \
        NODE_NAME(RECORD_DOT) \
        NODE_NAME(RECORD_MEMBERS) \
        NODE_NAME(REGULAR_STRING) \
        NODE_NAME(SLICE_LENGTH) \
	NODE_NAME(STRING_TYPE) \
	NODE_NAME(SWITCH) \
        NODE_NAME(TASK) \
        NODE_NAME(TASK_CALL) \
	NODE_NAME(TASK_CAST) \
        NODE_NAME(TASK_DEF) \
        NODE_NAME(TASK_TYPE) \
        NODE_NAME(THIS) \
        NODE_NAME(TIMEOUT) \
        NODE_NAME(TOP_LEVEL_DEFS) \
        NODE_NAME(TRUE) \
        NODE_NAME(TUPLE_LITERAL) \
        NODE_NAME(TUPLE_TYPE) \
        NODE_NAME(TYPE) \
        NODE_NAME(TYPE_DEF) \
        NODE_NAME(TYPES) \
        NODE_NAME(TYPE_VARIABLE) \
        NODE_NAME(TYPE_VARIABLES) \
        NODE_NAME(UNBOUND_NAME) \
	NODE_NAME(UNBOUND_NO_NAME) \
	NODE_NAME(WHEN)

typedef enum {
    FOREACH_NODE_NAME(GENERATE_ENUM)
} node_name_t;

typedef uint32_t type_variable_t;

typedef struct ast_node {
    node_name_t name;
    type_t* type;
    char *value;
    uint32_t row;
    uint32_t column;
    node_array_t* children;
} ast_node_t;

const char* ast_node_name_to_string(node_name_t name);
ast_node_t* ast_get_child(ast_node_t* node, uint32_t i);
uint16_t ast_number_of_children(ast_node_t* node);
ast_node_t* ast_last_child(ast_node_t* node);
void ast_print(ast_node_t* node, uint16_t level);

#endif
