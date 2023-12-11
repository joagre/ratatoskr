#ifndef SATIE_AUXIL_H
#define SATIE_AUXIL_H

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include "../vm/c/clib/dynarr.h"

static int ROW = 1;

typedef dynarray_t node_array_t;

typedef enum {
    PROGRAM = 0,
    IMPORTS,
    IMPORT,
    ALIAS,
    MODULE,
    MODULE_COMPONENT,
    IMPORTED_NAMES,
    IMPORTED_NAME,
    NOT_SET
} satie_node_type_t;

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

static char* type_to_string(satie_node_type_t type) {
    switch (type) {
    case PROGRAM:
        return "PROGRAM";
    case IMPORTS:
        return "IMPORTS";
    case IMPORT:
        return "IMPORT";
    case ALIAS:
        return "ALIAS";
    case MODULE:
        return "MODULE";
    case MODULE_COMPONENT:
        return "MODULE_COMPONENT";
    case IMPORTED_NAMES:
        return "IMPORTED_NAMES";
    case IMPORTED_NAME:
        return "IMPORTED_NAME";
    default:
        assert(false);
    }
}

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
    ast_node_t* node = create_node(auxil, type);
    node->value = strdup(value);
    return node;
}

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
    if (auxil->siblings[auxil->stack_index] == NULL ||
        auxil->siblings[auxil->stack_index]->type != type) {
        add_nesting(auxil, type);
    }
    ast_node_t* node = create_node(auxil, type);
    node->value = strdup(value);
    dynarray_append(auxil->siblings[auxil->stack_index]->array, node);
}

static void append_node(satie_auxil_t* auxil, satie_node_type_t type,
                        ast_node_t* node) {
    if (auxil->siblings[auxil->stack_index] == NULL ||
        auxil->siblings[auxil->stack_index]->type != type) {
        add_nesting(auxil, type);
    }
    dynarray_append(auxil->siblings[auxil->stack_index]->array, node);
}

static ast_node_t* create_siblings_node(satie_auxil_t* auxil,
                                        satie_node_type_t type) {
    ast_node_t* node = create_node(auxil, type);
    node->children = auxil->siblings[auxil->stack_index]->array;
    free(auxil->siblings[auxil->stack_index]);
    auxil->stack_index--;
    return node;
}

static ast_node_t* create_children_node(satie_auxil_t* auxil,
                                        satie_node_type_t type,
                                        uint16_t n, ...) {
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

static void print_ast(ast_node_t* node, uint16_t level) {
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
