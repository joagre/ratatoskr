#ifndef TYPE_VARIABLE_H
#define TYPE_VARIABLE_H

#include <stdint.h>
#include <satie_error.h>

#include "equations.h"
#include "hm.h"
#include "types.h"
#include "symbol_tables.h"

bool type_variable_create(ast_node_t* node, symbol_tables_t* tables,
			  uint32_t block_expr_id,
			  satie_error_t* error);
bool type_variable_associate(equations_t* equations,
			     types_t* type_variables_types, type_t* type,
			     satie_error_t* error);

#endif
