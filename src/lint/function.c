#include <log.h>

#include "function.h"

bool function_deconstruct(ast_node_t* node, equations_t* equations,
			  uint16_t index, function_types_t* function_types,
			  satie_error_t* error) {
    ast_node_t* child_node = ast_get_child(node, index);
    // Extract type variables (if any)
    ast_node_t* type_variables_node = NULL;
    if (child_node->name == TYPE_VARIABLES) {
	type_variables_node = child_node;
	child_node = ast_get_child(node, ++index);
    }
    // Extract parameters (if any)
    ast_node_t* params_node = NULL;
    if (child_node->name == PARAMS) {
	params_node = child_node;
	child_node = ast_get_child(node, ++index);
    }
    // Extract return type (if any)
    ast_node_t* type_node = NULL;
    if (child_node->name == TYPE) {
	type_node = child_node;
	child_node = ast_get_child(node, ++index);
    }
    ast_node_t* block_expr_node = child_node;
    // Extract type variables types (if any)
    types_t* type_variables_types = types_new();
    if (type_variables_node != NULL) {
	uint16_t n = ast_number_of_children(type_variables_node);
	for (uint16_t i = 0; i < n; i++) {
	    ast_node_t* type_variable_node =
		ast_get_child(type_variables_node, i);
	    LOG_ASSERT(type_variable_node->name == TYPE_VARIABLE,
		       "Expected a TYPE_VARIABLE node");
	    type_t* type_variable_type =
		type_new_type_variable(type_variable_node->value);
	    types_add(type_variables_types, type_variable_type);
	}
    }
    // Extract parameter types (if any)
    types_t* param_types = types_new();
    if (params_node != NULL) {
	uint16_t n = ast_number_of_children(params_node);
	for (uint16_t i = 0; i < n; i++) {
	    ast_node_t* param_node = ast_get_child(params_node, i);
	    ast_node_t* param_type_node = ast_get_child(param_node, 0);
	    // Has a type been specified?
	    if (param_type_node != NULL) {
		LOG_ASSERT(param_type_node->name == TYPE,
			   "Expected a TYPE node");
		if (!type_variable_associate(equations, type_variables_types,
					     param_type_node->type, error)) {
		    return false;
		}
		// Equation: Parameter type
		equation_t param_type_equation =
		    equation_new(param_node->type, param_type_node->type,
				 node, param_node, true);
		equations_append(equations, &param_type_equation);
	    }
	    types_add(param_types, param_node->type);
	}
    }
    // Has a return type been specified?
    if (type_node != NULL) {
	if (!type_variable_associate(equations, type_variables_types,
				     type_node->type, error)) {
	    return false;
	}
	// Equation: Return type
	equation_t return_type_equation =
	    equation_new(block_expr_node->type, type_node->type, node,
			 type_node, true);
	equations_append(equations, &return_type_equation);
    }
    CLEAR_ERROR(error);
    // Instantiate function types
    function_types->type_variables_types = type_variables_types;
    function_types->param_types = param_types;
    function_types->return_type = block_expr_node->type;
    return true;
}
