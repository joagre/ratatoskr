#ifndef FUNCTION_H
#define FUNCTION_H

#include <satie_error.h>
#include <stdbool.h>

#include "ast.h"
#include "equations.h"
#include "type_variable.h"
#include "types.h"

typedef struct {
    types_t* type_variables_types;
    types_t* param_types;
    type_t* return_type;
    type_t* block_expr_type;
} function_types_t;

bool function_deconstruct(ast_node_t* node, equations_t* equations,
			  uint16_t index, function_types_t* function_types,
			  satie_error_t* error);

#endif
