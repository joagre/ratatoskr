#ifndef __AST_H__
#define __AST_H__

#include <stdio.h>
#include <dynarr.h>
#include <assert.h>
#include <stdbool.h>
#include "hm.h"

typedef dynarray_t node_array_t;

#define GENERATE_ENUM(ENUM) ENUM,
#define GENERATE_STRING(STRING) #STRING,

#define FOREACH_TYPE(TYPE) \
        TYPE(ALIAS) \
        TYPE(AND) \
        TYPE(ARGS) \
        TYPE(BIND) \
        TYPE(BITWISE_AND) \
        TYPE(BITWISE_COMPLEMENT) \
        TYPE(BITWISE_OR) \
        TYPE(BITWISE_XOR) \
        TYPE(BLOCK_EXPR) \
        TYPE(BLOCK_LEVEL_EXPR) \
        TYPE(BOUND_NAME) \
        TYPE(BSL) \
        TYPE(BSR) \
        TYPE(CASE) \
        TYPE(CAST) \
        TYPE(CHANNEL) \
        TYPE(CHANNEL_NAME) \
        TYPE(CHANNELS) \
        TYPE(CHARACTER_LITERAL) \
        TYPE(CLASS_DEF) \
        TYPE(CLASS_MEMBERS) \
        TYPE(CLASS_NAME) \
        TYPE(CONCAT) \
        TYPE(CONST) \
        TYPE(CONSTRUCTOR) \
        TYPE(DEFAULT) \
        TYPE(DEFAULT_PARAM) \
        TYPE(DEFAULT_PARAM_NAME) \
        TYPE(DEFAULT_PARAMS) \
        TYPE(DESTRUCTOR) \
        TYPE(DIVIDE) \
        TYPE(DOT_NAME) \
        TYPE(DOTTED_NAME) \
        TYPE(ELIF) \
        TYPE(ELSE) \
        TYPE(ENUM) \
        TYPE(ENUM_DEF) \
        TYPE(ENUM_DEF_NAME) \
        TYPE(ENUM_LITERAL) \
        TYPE(ENUM_NAME) \
        TYPE(ENUMS) \
        TYPE(ENUM_VALUE) \
        TYPE(EQ) \
        TYPE(ESCAPE_CHARACTER) \
        TYPE(EXPONENTIATE) \
        TYPE(EXPORT) \
        TYPE(EXPR) \
        TYPE(EXPRS) \
        TYPE(FALSE) \
        TYPE(FLOATING_POINT) \
        TYPE(FLOAT_TYPE) \
        TYPE(FUNCTION_CALL) \
        TYPE(FUNCTION_DEF) \
        TYPE(FUNCTION_LITERAL) \
        TYPE(FUNCTION_NAME) \
        TYPE(GT) \
        TYPE(GTE) \
        TYPE(IDENTIFIER) \
        TYPE(IF) \
        TYPE(IF_EXPR) \
        TYPE(IMPORT) \
        TYPE(IMPORTED_NAME) \
        TYPE(IMPORTED_NAMES) \
        TYPE(IMPORTS) \
        TYPE(IN) \
        TYPE(INDEX_VALUE) \
        TYPE(INDEX_VALUES)        \
        TYPE(INTEGRAL) \
        TYPE(INTERFACE) \
        TYPE(INTERFACE_DEF) \
        TYPE(INTERFACE_MEMBER_METHOD) \
        TYPE(INTERFACE_MEMBER_PROPERTY) \
        TYPE(INTERFACE_MEMBERS) \
        TYPE(INTERFACE_METHOD) \
        TYPE(INTERFACE_NAME) \
        TYPE(INTERFACES) \
        TYPE(INT_TYPE) \
        TYPE(LIST_LITERAL) \
        TYPE(LIST_LOOKUP) \
        TYPE(LIST_SLICE)   \
        TYPE(LIST_UPDATE) \
        TYPE(LT) \
        TYPE(LTE) \
        TYPE(MAP_KEY_VALUE) \
        TYPE(MAP_KEY_VALUES) \
        TYPE(MAP_LITERAL) \
        TYPE(MAP_UPDATE) \
        TYPE(MATCH_EXPR) \
        TYPE(MATCH_EXPRS) \
        TYPE(MATCH_IS) \
        TYPE(MEMBER_METHOD) \
        TYPE(MEMBER_PROPERTY) \
        TYPE(MINUS) \
        TYPE(MODULE) \
        TYPE(MODULE_COMPONENT) \
        TYPE(MODULUS) \
        TYPE(MULTIPLY) \
        TYPE(NAME) \
        TYPE(NAMED_ARG) \
        TYPE(NAMED_ARGS) \
        TYPE(NE) \
        TYPE(NEW_EXPR) \
        TYPE(NIL) \
        TYPE(NON_DEFAULT_PARAM) \
        TYPE(NON_DEFAULT_PARAMS) \
        TYPE(NON_QUOTE_CHARACTER) \
        TYPE(NOT) \
        TYPE(NOT_SET) \
        TYPE(OR) \
        TYPE(PARAMS) \
        TYPE(PLUS) \
        TYPE(POSITIONAL_ARGS) \
        TYPE(POSTFIX_EXPR) \
        TYPE(PRIVATE) \
        TYPE(PROGRAM) \
        TYPE(PUBLIC) \
        TYPE(RAW_STRING) \
        TYPE(READONLY) \
        TYPE(RECEIVE) \
        TYPE(RECEIVE_EXPR) \
        TYPE(REGULAR_STRING) \
        TYPE(SELF) \
        TYPE(SLICE_LENGTH) \
        TYPE(SWITCH) \
        TYPE(SWITCH_EXPR) \
        TYPE(THIS) \
        TYPE(TIMEOUT) \
        TYPE(TOP_LEVEL_DEFS) \
        TYPE(TRUE) \
        TYPE(TUPLE_LITERAL) \
        TYPE(UNARY_MINUS) \
        TYPE(UNARY_PLUS) \
        TYPE(UNBOUND_NAME) \
        TYPE(WHEN)

typedef enum {
    FOREACH_TYPE(GENERATE_ENUM)
} node_type_t;

typedef uint32_t type_variable_t;

typedef struct ast_node {
    node_type_t type;
    hm_type_t* hm_type;
    char *value;
    uint32_t row;
    uint32_t column;
    node_array_t* children;
} ast_node_t;

const char* ast_type_to_string(node_type_t type);
ast_node_t* ast_get_child(ast_node_t* node, uint32_t i);
void ast_print(ast_node_t* node, uint16_t level);

#endif
