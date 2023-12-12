#ifndef SATIE_AUXIL_H
#define SATIE_AUXIL_H

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "../vm/c/clib/dynarr.h"

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
        TYPE(INDEX_VALUES) \
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
        TYPE(PARAMS) \
        TYPE(NON_DEFAULT_PARAMS) \
        TYPE(NON_DEFAULT_PARAM) \
        TYPE(DEFAULT_PARAMS) \
        TYPE(DEFAULT_PARAM) \
        TYPE(DEFAULT_PARAM_NAME) \
        TYPE(NOT_SET)

#define GENERATE_ENUM(ENUM) ENUM,

typedef enum {
    FOREACH_TYPE(GENERATE_ENUM)
} satie_node_type_t;

#define GENERATE_STRING(STRING) #STRING,

static const char* type_strings[] = {
    FOREACH_TYPE(GENERATE_STRING)
};

static const char* type_to_string(satie_node_type_t type) {
    return type_strings[type];
}

typedef struct {
    satie_node_type_t type;
    node_array_t* array;
} satie_siblings_t;

typedef struct {
    uint32_t row;
    uint8_t stack_index;
    satie_siblings_t* siblings[256];
} satie_auxil_t;

typedef struct {
    satie_node_type_t type;
    char *value;
    uint32_t row;
    uint32_t column;
    node_array_t* children;
} ast_node_t;

static satie_auxil_t* satie_auxil_new() {
    satie_auxil_t* auxil = malloc(sizeof(satie_auxil_t));
    auxil->row = 1;
    auxil->stack_index = 0;
    auxil->siblings[auxil->stack_index] = NULL;
    return auxil;
}

static ast_node_t* create_node(satie_auxil_t* auxil, satie_node_type_t type) {
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = type;
    node->value = NULL;
    node->row = auxil->row;
    node->column = 1;
    node->children = NULL;
    return node;
}

static ast_node_t* create_terminal(satie_auxil_t* auxil,
                                   satie_node_type_t type, const char* value) {
    fprintf(stderr, "** create_terminal: %s\n", value);
    ast_node_t* node = create_node(auxil, type);
    if (value != NULL) {
        node->value = strdup(value);
    } else {
        node->value = NULL;
    }
    return node;
}

#define CT(type, value) create_terminal(auxil, type, value)

static void add_nesting(satie_auxil_t* auxil, satie_node_type_t type) {
    if (!(auxil->stack_index == 0 &&
          auxil->siblings[auxil->stack_index] == NULL)) {
        auxil->stack_index++;
    }
    auxil->siblings[auxil->stack_index] = malloc(sizeof(satie_siblings_t));
    auxil->siblings[auxil->stack_index]->type = type;
    auxil->siblings[auxil->stack_index]->array = malloc(sizeof(node_array_t));
    dynarray_init(auxil->siblings[auxil->stack_index]->array, NULL, 0,
                  sizeof(ast_node_t));
}

static void append_terminal(satie_auxil_t* auxil, satie_node_type_t type,
                            const char* value) {
    fprintf(stderr, "** add_terminal: %s\n", value);
    if (auxil->siblings[auxil->stack_index] == NULL ||
        auxil->siblings[auxil->stack_index]->type != type) {
        add_nesting(auxil, type);
    }
    ast_node_t* node = create_node(auxil, type);
    node->value = strdup(value);
    dynarray_append(auxil->siblings[auxil->stack_index]->array, node);
}

#define AT(type, value) append_terminal(auxil, type, value)

static void append_node(satie_auxil_t* auxil, satie_node_type_t type,
                        ast_node_t* node) {
    fprintf(stderr, "** append_node: %s\n", type_to_string(type));
    if (node == NULL) {
        fprintf(stderr, "WARNING: Node of type %s is NULL\n",
                type_to_string(type));
        return;
    }
    if (auxil->siblings[auxil->stack_index] == NULL ||
        auxil->siblings[auxil->stack_index]->type != type) {
        add_nesting(auxil, type);
    }
    dynarray_append(auxil->siblings[auxil->stack_index]->array, node);
}

#define AN(type, node) append_node(auxil, type, node)

static ast_node_t* create_siblings_node(satie_auxil_t* auxil,
                                        satie_node_type_t type) {
    if (auxil->siblings[auxil->stack_index] == NULL ||
        dynarray_size(auxil->siblings[auxil->stack_index]->array) == 0) {
        fprintf(stderr, "WARNING: No siblings to create node from: %s\n",
                type_to_string(type));
        return NULL;
    }
    fprintf(stderr, "** create_siblings_node: %s\n", type_to_string(type));
    ast_node_t* node = create_node(auxil, type);
    //fprintf(stderr, "***1 create_siblings_node\n");
    node->children = auxil->siblings[auxil->stack_index]->array;
    //fprintf(stderr, "***2 create_siblings_node\n");
    free(auxil->siblings[auxil->stack_index]);
    //fprintf(stderr, "***3 create_siblings_node\n");
    auxil->stack_index--;
    //fprintf(stderr, "***4 create_siblings_node\n");
    return node;
}

#define CSN(type) create_siblings_node(auxil, type)

static ast_node_t* create_children_node(satie_auxil_t* auxil,
                                        satie_node_type_t type,
                                        uint16_t n, ...) {
    fprintf(stderr, "** create_children_node: %s\n", type_to_string(type));
    ast_node_t* node = create_node(auxil, type);
    node->children = malloc(sizeof(node_array_t));
    dynarray_init(node->children, NULL, 0, sizeof(ast_node_t));
    va_list args;
    va_start(args, n);
    for (uint16_t i = 0; i < n; i++) {
        ast_node_t* child_node = va_arg(args, ast_node_t*);
        if (child_node != NULL) {
            dynarray_append(node->children, child_node);
        }
    }
    return node;
}

#define CCN(type, n, ...) create_children_node(auxil, type, n, __VA_ARGS__)

static void print_ast(ast_node_t* node, uint16_t level) {
    //fprintf(stderr, "BAJJA\n");
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
