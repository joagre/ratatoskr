#ifndef __SATIE_AUXIL_H__
#define __SATIE_AUXIL_H__

#include <stdarg.h>
#include <stdio.h>
#include <dynarr.h>

typedef dynarray_t node_array_t;

#define FOREACH_TYPE(TYPE) \
        TYPE(PROGRAM) \
        TYPE(TOP_LEVEL_DEFS) \
        TYPE(EXPR) \
        TYPE(BIND) \
        TYPE(OR) \
        TYPE(AND) \
        TYPE(BITWISE_AND) \
        TYPE(BITWISE_XOR) \
        TYPE(BITWISE_OR) \
        TYPE(GTE) \
        TYPE(GT) \
        TYPE(LTE) \
        TYPE(LT) \
        TYPE(NE) \
        TYPE(EQ) \
        TYPE(IN) \
        TYPE(BSR) \
        TYPE(BSL) \
        TYPE(CONCAT) \
        TYPE(MINUS) \
        TYPE(PLUS) \
        TYPE(MODULUS) \
        TYPE(DIVIDE) \
        TYPE(MULTIPLY) \
        TYPE(EXPONENTIATE) \
        TYPE(CAST) \
        TYPE(BITWISE_COMPLEMENT) \
        TYPE(NOT) \
        TYPE(UNARY_PLUS) \
        TYPE(UNARY_MINUS) \
        TYPE(POSTFIX_EXPR) \
        TYPE(INT_TYPE) \
        TYPE(FLOAT_TYPE) \
        TYPE(NIL) \
        TYPE(THIS) \
        TYPE(SELF) \
        TYPE(SLICE_LENGTH) \
        TYPE(NAME) \
        TYPE(UNBOUND_NAME) \
        TYPE(BOUND_NAME) \
        TYPE(IDENTIFIER) \
        TYPE(TRUE) \
        TYPE(FALSE) \
        TYPE(FLOATING_POINT) \
        TYPE(INTEGRAL) \
        TYPE(CHARACTER_LITERAL) \
        TYPE(ESCAPE_CHARACTER) \
        TYPE(NON_QUOTE_CHARACTER) \
        TYPE(REGULAR_STRING) \
        TYPE(RAW_STRING) \
        TYPE(LIST_LITERAL) \
        TYPE(LIST_SLICE)   \
        TYPE(LIST_UPDATE) \
        TYPE(MAP_UPDATE) \
        TYPE(DOT_NAME) \
        TYPE(LIST_LOOKUP) \
        TYPE(FUNCTION_CALL) \
        TYPE(DOTTED_NAME) \
        TYPE(INDEX_VALUES)        \
        TYPE(INDEX_VALUE) \
        TYPE(MAP_LITERAL) \
        TYPE(MAP_KEY_VALUES) \
        TYPE(MAP_KEY_VALUE) \
        TYPE(FUNCTION_LITERAL) \
        TYPE(ENUM_LITERAL) \
        TYPE(TUPLE_LITERAL) \
        TYPE(EXPRS) \
        TYPE(IMPORTS) \
        TYPE(IMPORT) \
        TYPE(ALIAS) \
        TYPE(MODULE) \
        TYPE(MODULE_COMPONENT) \
        TYPE(IMPORTED_NAMES) \
        TYPE(IMPORTED_NAME) \
        TYPE(ENUM_DEF) \
        TYPE(ENUM_DEF_NAME) \
        TYPE(ENUMS) \
        TYPE(ENUM) \
        TYPE(ENUM_NAME) \
        TYPE(ENUM_VALUE) \
        TYPE(FUNCTION_DEF) \
        TYPE(FUNCTION_NAME) \
        TYPE(EXPORT) \
        TYPE(PARAMS) \
        TYPE(NON_DEFAULT_PARAMS) \
        TYPE(NON_DEFAULT_PARAM) \
        TYPE(DEFAULT_PARAMS) \
        TYPE(DEFAULT_PARAM) \
        TYPE(DEFAULT_PARAM_NAME) \
        TYPE(BLOCK_EXPR) \
        TYPE(BLOCK_LEVEL_EXPR) \
        TYPE(MATCH_EXPRS) \
        TYPE(IF_EXPR) \
        TYPE(IF) \
        TYPE(ELIF) \
        TYPE(ELSE) \
        TYPE(MATCH_IS) \
        TYPE(SWITCH_EXPR) \
        TYPE(SWITCH) \
        TYPE(CASE) \
        TYPE(WHEN) \
        TYPE(DEFAULT) \
        TYPE(MATCH_EXPR) \
        TYPE(RECEIVE_EXPR) \
        TYPE(RECEIVE) \
        TYPE(TIMEOUT) \
        TYPE(NEW_EXPR) \
        TYPE(CHANNEL) \
        TYPE(CHANNEL_NAME) \
        TYPE(CHANNELS) \
        TYPE(CLASS_DEF) \
        TYPE(CLASS_NAME) \
        TYPE(INTERFACES) \
        TYPE(INTERFACE) \
        TYPE(CLASS_MEMBERS) \
        TYPE(CONSTRUCTOR) \
        TYPE(DESTRUCTOR) \
        TYPE(MEMBER_METHOD) \
        TYPE(PUBLIC) \
        TYPE(PRIVATE) \
        TYPE(MEMBER_PROPERTY) \
        TYPE(CONST) \
        TYPE(READONLY) \
        TYPE(ARGS) \
        TYPE(POSITIONAL_ARGS) \
        TYPE(NAMED_ARGS) \
        TYPE(NAMED_ARG) \
        TYPE(INTERFACE_DEF) \
        TYPE(INTERFACE_NAME) \
        TYPE(INTERFACE_MEMBERS) \
        TYPE(INTERFACE_MEMBER_METHOD) \
        TYPE(INTERFACE_METHOD) \
        TYPE(INTERFACE_MEMBER_PROPERTY) \
        TYPE(NOT_SET)

#define GENERATE_ENUM(ENUM) ENUM,

typedef enum {
    FOREACH_TYPE(GENERATE_ENUM)
} node_type_t;

#define GENERATE_STRING(STRING) #STRING,

typedef uint32_t type_variable_t;

typedef struct {
    node_type_t type;
    type_variable_t type_variable;
    char *value;
    uint32_t row;
    uint32_t column;
    node_array_t* children;
} ast_node_t;

typedef struct {
    uint32_t row;
} satie_auxil_t;

satie_auxil_t* satie_auxil_new();
const char* type_to_string(node_type_t type);
ast_node_t* new_node(satie_auxil_t* auxil, node_type_t type);
ast_node_t* retype_node(ast_node_t* node, node_type_t type);
ast_node_t* create_terminal(satie_auxil_t* auxil, node_type_t type,
			    const char* value);
ast_node_t* create_node(satie_auxil_t* auxil, node_type_t type,
			uint16_t n, ...);
void add_child(satie_auxil_t* auxil, ast_node_t* parent_node, ast_node_t* node);
void print_ast(ast_node_t* node, uint16_t level);

#define RN(node, type) retype_node(node, type)
#define CT(type, value) create_terminal(auxil, type, value)
#define CN(type, n, ...) create_node(auxil, type, n, __VA_ARGS__)
#define AC(parent_node, node) add_child(auxil, parent_node, node)

#endif
