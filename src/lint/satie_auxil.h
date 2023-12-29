#ifndef SATIE_AUXIL_H
#define SATIE_AUXIL_H

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <dynarr.h>

static int ROW = 1;

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

static const char* type_strings[] = {
    FOREACH_TYPE(GENERATE_STRING)
};

static const char* type_to_string(node_type_t type) {
    return type_strings[type];
}

typedef struct {
    node_type_t type;
    char *value;
    uint32_t row;
    uint32_t column;
    node_array_t* children;
} ast_node_t;

typedef struct {
    uint32_t row;
} satie_auxil_t;

static satie_auxil_t* satie_auxil_new() {
    fprintf(stderr, "** satie_auxil_new\n");
    satie_auxil_t* auxil = malloc(sizeof(satie_auxil_t));
    auxil->row = 1;
    return auxil;
}

static ast_node_t* new_node(satie_auxil_t* auxil, node_type_t type) {
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = type;
    node->value = NULL;
    node->row = auxil->row;
    node->column = 1;
    node->children = NULL;
    return node;
}

static ast_node_t* retype_node(ast_node_t* node, node_type_t type) {
    fprintf(stderr, "** retype_node\n");
    if (node == NULL) {
        fprintf(stderr, "WARNING: An undefined node cannot be retyped to %s\n",
                type_to_string(type));
    } else {
        node->type = type;
    }
    return node;
}

#define RN(node, type) retype_node(node, type)

static ast_node_t* create_terminal(satie_auxil_t* auxil, node_type_t type,
                                   const char* value) {
    fprintf(stderr, "** create_terminal: %s (%s)\n", value,
            type_to_string(type));
    ast_node_t* node = new_node(auxil, type);
    if (value != NULL) {
        node->value = strdup(value);
    } else {
        node->value = NULL;
    }
    return node;
}

#define CT(type, value) create_terminal(auxil, type, value)

static ast_node_t* create_node(satie_auxil_t* auxil, node_type_t type,
                               uint16_t n, ...) {
    fprintf(stderr, "** create_children_node: %s\n", type_to_string(type));
    ast_node_t* node = new_node(auxil, type);
    node->children = malloc(sizeof(node_array_t));
    dynarray_init(node->children, NULL, 0, sizeof(ast_node_t));
    va_list args;
    va_start(args, n);
    for (uint16_t i = 0; i < n; i++) {
        ast_node_t* child_node = va_arg(args, ast_node_t*);
        if (child_node != NULL) {
            if (child_node->type == POSTFIX_EXPR &&
                dynarray_size(child_node->children) == 1) {
                ast_node_t* grand_child_node =
                    dynarray_element(child_node->children, 0);
                free(child_node);
                child_node = grand_child_node;
            }
            dynarray_append(node->children, child_node);
        } else {
            fprintf(stderr,
                    "WARNING: An undefined child node %d is ignored by %s\n",
                    i, type_to_string(type));
        }
    }
    /*
    if (dynarray_size(node->children) == 0) {
        free(node->children);
        return NULL;
    }
    */
    return node;
}

#define CN(type, n, ...) create_node(auxil, type, n, __VA_ARGS__)

static void add_child(satie_auxil_t* auxil, ast_node_t* parent_node,
                      ast_node_t* node) {
    fprintf(stderr, "** add_child\n");
    if (node == NULL) {
        fprintf(stderr, "WARNING: An undefined node cannot be appended to %s\n",
                type_to_string(parent_node->type));
    } else {
        fprintf(stderr, "** add_child1\n");
        if (node->type == POSTFIX_EXPR &&
            dynarray_size(node->children) == 1) {
            fprintf(stderr, "** add_child2\n");
            ast_node_t* child_node = dynarray_element(node->children, 0);
            fprintf(stderr, "** add_child3\n");
            free(node);
            fprintf(stderr, "** add_child4\n");
            node = child_node;
        }
        dynarray_append(parent_node->children, node);
    }
}

#define AC(parent_node, node) add_child(auxil, parent_node, node)

static void print_ast(ast_node_t* node, uint16_t level) {
    if (node == NULL) {
        printf("Tree: NULL\n");
        return;
    }
    for (uint16_t i = 0; i < level; i++) {
        printf("  ");
    }
    printf("%s", type_to_string(node->type));
    if (node->value != NULL) {
        printf(": %s", node->value);
    }
    printf("\n");
    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
            ast_node_t* child_node = dynarray_element(node->children, i);
            print_ast(child_node, level + 1);
        }
    }
}

static int satie_getchar(satie_auxil_t* _auxil) {
    int c = getchar();
    if (c == '\n') {
        ROW++;
    }
    return c;
}

static void panic(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(stderr, "\033[31mError:\033[0m ");
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
    va_end(args);
    exit(1);
}

static void satie_error(satie_auxil_t* auxil) {
    panic("Bailing out near line %d", ROW);
    exit(1);
}

#endif
