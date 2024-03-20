#ifndef LINT_EQUATION_H
#define LINT_EQUATION_H

#include <stdbool.h>
#include "type.h"
#include "ast.h"

typedef struct {
    type_t* left;
    type_t* right;
    ast_node_t* origin_node;
    ast_node_t* node;
    bool user_defined;
} equation_t;

equation_t equation_new(type_t* left, type_t* right, ast_node_t* origin_node,
			ast_node_t* node, bool user_defined);

#endif
