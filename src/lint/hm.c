//#define MUTE_LOG_DEBUG 1

#include <assert.h>
#include <log.h>
#include "hm.h"
#include "types.h"
#include "equation.h"
#include "equations.h"
#include "symbol_table.h"
#include "symbol_tables.h"

// Forward declarations of local functions
static bool is_valid(ast_node_t* parent_node, ast_node_t* node,
		     satie_error_t* error);
static void add_forward_type_variables(ast_node_t* node,
				       symbol_tables_t* tables);
static bool add_type_variables(ast_node_t* node, symbol_tables_t* tables,
			       uint32_t block_expr_id, satie_error_t* error);
static void add_type_equations(ast_node_t* node, symbol_tables_t* tables,
			       equations_t* equations);
static type_t* extract_type(ast_node_t* type_node);
static uint32_t unique_id(void);

bool hm_infer_types(ast_node_t* node, satie_error_t* error) {
    // Check if tree is semantically valid
    if (!is_valid(NULL, node, error)) {
	return false;
    }
    // Create the top level symbol table
    symbol_tables_t tables;
    symbol_tables_init(&tables);
    symbol_table_t* table = symbol_table_new();
    symbol_tables_insert_table(&tables, table, unique_id());
    // Add forward type variables for function definitions
    add_forward_type_variables(node, &tables);
    // Add all other type variables
    if (!add_type_variables(node, &tables, unique_id(), error)) {
	return false;
    }
    // Print parse tree
    ast_print(node, 0);
    printf("\n");
    // Add equations
    equations_t equations;
    equations_init(&equations);
    add_type_equations(node, &tables, &equations);
    // Print equations
    equations_print(&equations);
    return true;
}

//
// Local functions
//

static bool is_valid(ast_node_t* parent_node, ast_node_t* node,
		     satie_error_t* error) {
    if (node->name == BIND &&
	parent_node != NULL && parent_node->name != BLOCK_EXPR) {
	SET_ERROR_MESSAGE(
	    error, COMPONENT_COMPILER,
	    "%d: The := operator is only allowed as a top-level expression in {...} expressions",
	    node->row);
	return false;
    }
    size_t n = ast_number_of_children(node);
    if (n > 0) {
        for (uint16_t i = 0; i < n; i++) {
	    if (!is_valid(node, ast_get_child(node, i), error)) {
		return false;
	    }
	}
    }
    CLEAR_ERROR(error);
    return true;
}

static void add_forward_type_variables(ast_node_t* node,
				       symbol_tables_t* tables) {
    LOG_ASSERT(node->name == PROGRAM, "Expected a PROGRAM node");
    ast_node_t* top_level_defs_node = ast_get_child(node, 0);
    size_t n = ast_number_of_children(top_level_defs_node);
    for (uint16_t i = 0; i < n; i++) {
	ast_node_t* child_node = ast_get_child(top_level_defs_node, i);
	if (child_node->name == FUNCTION_DEF) {
	    ast_node_t* function_name_node = ast_get_child(child_node, 0);
	    LOG_ASSERT(function_name_node->name == FUNCTION_NAME,
		       "Expected a FUNCTION_NAME node");
	    type_t* type = type_new_type_variable();
	    function_name_node->type = type;
	    symbol_tables_insert(tables, function_name_node->value, type);
	}
    }
}

static bool add_type_variables(ast_node_t* node, symbol_tables_t* tables,
			       uint32_t block_expr_id, satie_error_t* error) {
    bool traverse_children = true;
    if (node->name == ELSE ||
	node->name == EQ_TYPE ||
	node->name == IF ||
	node->name == INT_TYPE ||
	node->name == NON_DEFAULT_PARAMS ||
	node->name == POSITIONAL_ARGS ||
	node->name == PROGRAM ||
	node->name == TOP_LEVEL_DEFS) {
	// These nodes have no type variable
    } else if (node->name == BIND ||
	       node->name == EQ ||
	       node->name == FALSE ||
	       node->name == FUNCTION_CALL ||
	       node->name == IF_EXPR ||
	       node->name == INT ||
	       node->name == POSTFIX_EXPR ||
	       node->name == TUPLE_LITERAL ||
	       node->name == TRUE) {
	// Assign a type variable
	type_t* type = type_new_type_variable();
	node->type = type;
    } else if (node->name == NAME) {
	// Names should be in a symbol table or else it is an error
	node->type = symbol_tables_lookup(tables, node->value);
	if (node->type == NULL) {
	    SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
			      "%d: '%s' has not been bound to a value",
			      node->row, node->value);
	    return false;
	}
    } else if (node->name == UNBOUND_NAME) {
	// Assign a type variable and add it to the symbol table
	type_t* type = type_new_type_variable();
	node->type = type;
	symbol_tables_insert(tables, node->value, type);
    } else if (node->name == FUNCTION_DEF) {
	/*
	  A function definition must add its function name to the
	  top symbol table but its paramaters (if any) must be added
	  to the same symbol tables as any names bound in the function
	  block expression. The normal handling of block expressions
	  is therefore overridden below.
	*/
        // Handle all possible combinations of function definition nodes
       	ast_node_t* unknown_node = ast_get_child(node, 1);
	ast_node_t* non_default_params_node = NULL;
	ast_node_t* return_type_node = NULL;
	ast_node_t* block_expr_node;
	if (unknown_node->name == BLOCK_EXPR) {
	    block_expr_node = unknown_node;
	} else if (unknown_node->name == NON_DEFAULT_PARAMS) {
	    non_default_params_node = unknown_node;
	    unknown_node = ast_get_child(node, 2);
	    if (unknown_node->name == RETURN_TYPE) {
		return_type_node = unknown_node;
		block_expr_node = ast_get_child(node, 3);
	    } else {
		block_expr_node = unknown_node;
	    }
	} else if (unknown_node->name == RETURN_TYPE) {
	    return_type_node = unknown_node;
	    block_expr_node = ast_get_child(node, 2);
	} else {
	    block_expr_node = unknown_node;
	}
	// Create a new symbol table for the function parameters and
	// the upcoming function defintion block expression
	uint32_t block_expr_id = unique_id();
	symbol_table_t* table = symbol_table_new();
	symbol_tables_insert_table(tables, table, block_expr_id);
	// Add type variables for the function parameters (if any)
	if (non_default_params_node != NULL) {
	    size_t n = ast_number_of_children(non_default_params_node);
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* param_node =
		    ast_get_child(non_default_params_node, i);
		LOG_ASSERT(param_node->name == PARAM_NAME,
			   "Expected a PARAM_NAME node");
		type_t* param_type = type_new_type_variable();
		param_node->type = param_type;
		symbol_tables_insert(tables, param_node->value, param_type);
	    }
	}
	// Assign a type variable to the return type (if any)
	if (return_type_node != NULL) {
	    // Add a type variable to the return type
	    type_t* return_type = type_new_type_variable();
	    return_type_node->type = return_type;
	}
	// Assign type variables to the function definition block expression
	if (!add_type_variables(block_expr_node, tables, block_expr_id,
				error)) {
	    return false;
	}
	// Remove the symbol table create by the function defintion above
	symbol_tables_delete_by_id(tables, block_expr_id);
	traverse_children = false;
    } else if(node->name == BLOCK_EXPR) {
	// Assign a type variable
	type_t* type = type_new_type_variable();
	node->type = type;
	// Create a new symbol table
	symbol_table_t* table = symbol_table_new();
	block_expr_id = unique_id();
	symbol_tables_insert_table(tables, table, block_expr_id);
	// Add type variables to the expression in the block expression
	size_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!add_type_variables(ast_get_child(node, i), tables,
				    block_expr_id, error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	// Remove the symbol table adhering to the block expression
	symbol_tables_delete_by_id(tables, block_expr_id);
	traverse_children = false;
    } else {
	LOG_ABORT("Not handled node: %s\n",
		  ast_node_name_to_string(node->name));
    }

    // Traverse children (if any)
    if (traverse_children) {
	size_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!add_type_variables(
		    ast_get_child(node, i), tables, block_expr_id, error)) {
		return false;
	    }
	}
    }

    CLEAR_ERROR(error);
    return true;
}

static void add_type_equations(ast_node_t *node, symbol_tables_t* tables,
			       equations_t* equations) {
    if (node->name == FUNCTION_TYPE ||
	node->name == ARG_TYPES ||
	node->name == BOOL_TYPE ||
	node->name == ELSE ||
	node->name == EQ_TYPE ||
	node->name == FUNCTION_CALL ||
	node->name == FUNCTION_NAME ||
	node->name == IF ||
	node->name == INT_TYPE ||
	node->name == NON_DEFAULT_PARAMS ||
	node->name == NAME ||
	node->name == PARAM_NAME ||
	node->name == POSITIONAL_ARGS ||
	node->name == PROGRAM ||
	node->name == RETURN_TYPE ||
	node->name == TOP_LEVEL_DEFS ||
	node->name == UNBOUND_NAME) {
	// These nodes do not produce any equations
    } else if (node->name == INT) {
	// Equation: Int constant
	equation_t equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_INT),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == FALSE || node->name == TRUE) {
	// Equation: Bool constant
	equation_t equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == TUPLE_LITERAL) {
	// Equation: tuple literal
	size_t n = ast_number_of_children(node);
	types_t* tuple_types = types_new();
	for (uint16_t i = 0; i < n; i++) {
	    ast_node_t* tuple_element_node = ast_get_child(node, i);
	    types_add(tuple_types, tuple_element_node->type);
	}
	equation_t tuple_literal_equation =
	    equation_new(node->type, type_new_tuple_type(tuple_types), node,
			 node, false);
	equations_add(equations, &tuple_literal_equation);
    } else if (node->name == EQ) {
	// Equation: eq
	equation_t eq_equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
			 node, node, false);
	equations_add(equations, &eq_equation);
        // Extract the eq type
	ast_node_t* eq_type_node = ast_get_child(node, 1);
	LOG_ASSERT(eq_type_node->name == EQ_TYPE, "Expected an EQ_TYPE node");
	type_basic_type_t eq_type;
	if (strcmp(eq_type_node->value, "Int") == 0) {
	    eq_type = TYPE_BASIC_TYPE_INT;
	} else if (strcmp(eq_type_node->value, "Bool") == 0) {
	    eq_type = TYPE_BASIC_TYPE_BOOL;
	} else {
	    LOG_ABORT("Unknown eq type: %s", eq_type_node->value);
	}
	// Equation: left operand
	ast_node_t* left_node = ast_get_child(node, 0);
	equation_t left_equation =
	    equation_new(left_node->type, type_new_basic_type(eq_type),
			 node, left_node, false);
	equations_add(equations, &left_equation);
	// Equation: right operand
	ast_node_t* right_node = ast_get_child(node, 2);
	equation_t right_equation =
	    equation_new(right_node->type, type_new_basic_type(eq_type),
			 node, right_node, false);
	equations_add(equations, &right_equation);
    } else if (node->name == IF_EXPR) {
	// Extract all nodes constituting the if expression
	ast_node_t* if_node = ast_get_child(node, 0);
	LOG_ASSERT(if_node->name == IF, "Expected an IF node");
	ast_node_t* if_conditional_node = ast_get_child(if_node, 0);
	LOG_ASSERT(if_conditional_node->name == POSTFIX_EXPR,
		   "Expected a POSTFIX_EXPR node");
	ast_node_t* if_block_expr_node = ast_get_child(if_node, 1);
	LOG_ASSERT(if_block_expr_node->name == BLOCK_EXPR,
		   "Expected a BLOCK_EXPR node");
	ast_node_t* else_node = ast_get_child(node, 1);
	LOG_ASSERT(else_node->name == ELSE, "Expected an ELSE node");
	ast_node_t* else_block_expr_node = ast_get_child(else_node, 0);
	LOG_ASSERT(else_block_expr_node->name == BLOCK_EXPR,
		   "Expected a BLOCK_EXPR node");
	// Equation: if conditional
	equation_t if_conditional_equation =
	    equation_new(if_conditional_node->type,
			 type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
			 node, if_conditional_node, false);
	equations_add(equations, &if_conditional_equation);
        // Equation: if block expression
	LOG_ASSERT(node->type != NULL, "Expected a type");
	equation_t if_block_expr_equation =
	    equation_new(if_block_expr_node->type, node->type, node,
			 if_block_expr_node, false);
	equations_add(equations, &if_block_expr_equation);
	// Equation: else block expression
	LOG_ASSERT(node->type != NULL, "Expected a type");
	equation_t else_block_expr_equation =
	    equation_new(else_block_expr_node->type, node->type, node,
			 else_block_expr_node, false);
	equations_add(equations, &else_block_expr_equation);
    } else if (node->name == FUNCTION_DEF) {
	// Extract all nodes constituting the function definition
	ast_node_t* function_name_node = ast_get_child(node, 0);
	LOG_ASSERT(function_name_node->name == FUNCTION_NAME,
		   "Expected a FUNCTION_NAME node");
       	ast_node_t* unknown_node = ast_get_child(node, 1);
	ast_node_t* non_default_params_node = NULL;
	ast_node_t* return_type_node = NULL;
	ast_node_t* block_expr_node;
	if (unknown_node->name == BLOCK_EXPR) {
	    block_expr_node = unknown_node;
	} else if (unknown_node->name == NON_DEFAULT_PARAMS) {
	    non_default_params_node = unknown_node;
	    unknown_node = ast_get_child(node, 2);
	    if (unknown_node->name == RETURN_TYPE) {
		return_type_node = unknown_node;
		block_expr_node = ast_get_child(node, 3);
	    } else {
		block_expr_node = unknown_node;
	    }
	} else if (unknown_node->name == RETURN_TYPE) {
	    return_type_node = unknown_node;
	    block_expr_node = ast_get_child(node, 2);
	} else {
	    block_expr_node = unknown_node;
	}
	// Extract all parameter types
	types_t* arg_types = types_new();
	if (non_default_params_node != NULL) {
	    size_t n = ast_number_of_children(non_default_params_node);
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* param_node =
		    ast_get_child(non_default_params_node, i);
		LOG_ASSERT(param_node->name == PARAM_NAME,
			   "Expected a PARAM_NAME node");
		if (ast_number_of_children(param_node) > 0) {
		    ast_node_t* arg_type_node = ast_get_child(param_node, 0);
		    type_t* arg_type = extract_type(arg_type_node);
		    // Equation: arg type (if any)
		    LOG_ASSERT(arg_type != NULL, "Expected a type");
		    equation_t arg_type_equation =
			equation_new(param_node->type, arg_type, node,
				     param_node, true);
		    equations_add(equations, &arg_type_equation);
		}
		types_add(arg_types, param_node->type);
	    }
	}
	// Equation: function
	equation_t function_equation =
	    equation_new(function_name_node->type,
			 type_new_function_type(arg_types,
						block_expr_node->type),
			 node, node, false);
	equations_add(equations, &function_equation);
	// Has a return type been specified?
	if (return_type_node != NULL) {
	    // Equation: return type
	    ast_node_t* type_node = ast_get_child(return_type_node, 0);
	    type_t* return_type = extract_type(type_node);
	    LOG_ASSERT(return_type != NULL, "Expected a type");
	    equation_t return_type_equation =
		equation_new(block_expr_node->type, return_type, node,
			     return_type_node, true);
	    equations_add(equations, &return_type_equation);
	}
    } else if (node->name == BIND) {
	// Extract all nodes constituting the bind expression
	ast_node_t* left_node = ast_get_child(node, 0);
	ast_node_t* right_node = ast_get_child(node, 1);
	// Equation: left := right
	LOG_ASSERT(right_node->type != NULL, "Expected a type");
	equation_t bind_left_right_equation =
	    equation_new(left_node->type, right_node->type, node, left_node,
			 false);
	equations_add(equations, &bind_left_right_equation);
	// Equation: bind
	LOG_ASSERT(right_node->type != NULL, "Expected a type");
	equation_t bind_equation =
	    equation_new(node->type, right_node->type, node, node, false);
	equations_add(equations, &bind_equation);
    } else if (node->name == POSTFIX_EXPR) {
	/*
	 * NOTE: This is hardwired only handle function calls
	 */
	// Extract all nodes constituting the function call
	ast_node_t* name_node = ast_get_child(node, 0);
	LOG_ASSERT(name_node->name == NAME, "Expected a NAME node");
	ast_node_t* function_call_node = ast_get_child(node, 1);
	LOG_ASSERT(function_call_node->name == FUNCTION_CALL,
		   "Expected a FUNCTION_CALL node");
	ast_node_t* positional_args_node = ast_get_child(function_call_node, 0);
	// Extract all argument types
	types_t* arg_types = types_new();
	if (positional_args_node != NULL) {
	    LOG_ASSERT(positional_args_node->name == POSITIONAL_ARGS,
		       "Expected a POSITIONAL_ARGS node");
	    size_t n = ast_number_of_children(positional_args_node);
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* arg_node = ast_get_child(positional_args_node, i);
		types_add(arg_types, arg_node->type);
	    }
	}
	// Add a return type equation if the function name is known
	type_t* function_name_type =
	    symbol_tables_lookup(tables, name_node->value);
	if (function_name_type != NULL) {
	    LOG_ASSERT(function_name_type->tag == TYPE_TAG_TYPE_VARIABLE,
		       "Expected a type variable");
	    equation_t* signature_equation =
		equations_lookup(equations, function_name_type->type_variable);
	    LOG_ASSERT(signature_equation->right->tag == TYPE_TAG_FUNCTION_TYPE,
		       "Expected an function type");
	    type_t* return_type =
		signature_equation->right->function_type.return_type;
	    equation_t return_type_equation =
		equation_new(function_call_node->type, return_type,
			     node, node, false);
	    equations_add(equations, &return_type_equation);
	}
        // Equation: function call type
	equation_t function_equation =
	    equation_new(name_node->type,
			 type_new_function_type(arg_types,
						function_call_node->type),
			 node, node, false);
	equations_add(equations, &function_equation);
        // Equation: postfix expression
	equation_t postfix_expr_equation =
	    equation_new(node->type, function_call_node->type, node, node,
			 false);
	equations_add(equations, &postfix_expr_equation);
    } else if (node->name == BLOCK_EXPR) {
	// Equation: block expression
	ast_node_t* last_block_expr_node = ast_last_child(node);
	LOG_ASSERT(last_block_expr_node->type != NULL, "Expected a type");
	equation_t equation =
	    equation_new(node->type, last_block_expr_node->type, node, node,
			 false);
	equations_add(equations, &equation);
    } else {
	LOG_ABORT("Not handled node: %s\n",
		  ast_node_name_to_string(node->name));
    }

    // Traverse children (if any)
    size_t n = ast_number_of_children(node);
    for (uint16_t i = 0; i < n; i++) {
	add_type_equations(ast_get_child(node, i), tables, equations);
    }
}

static type_t* extract_type(ast_node_t* type_node) {
    switch (type_node->name) {
	case BOOL_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_BOOL);
	case INT_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_INT);
	case FLOAT_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_FLOAT);
	case STRING_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_STRING);
	case JOB_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_JOB);
	case CHANNEL_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_CHANNEL);
	case LIST_TYPE:
	    return type_new_list_type(
		extract_type(ast_get_child(type_node, 0)));
	case FUNCTION_TYPE: {
	    ast_node_t* arg_types_node = ast_get_child(type_node, 0);
	    LOG_ASSERT(arg_types_node->name == ARG_TYPES,
		       "Expected an ARG_TYPES node");
	    types_t* arg_types = types_new();
	    for (uint16_t i = 0; i < ast_number_of_children(arg_types_node);
		 i++) {
		ast_node_t* arg_type_node = ast_get_child(arg_types_node, i);
		type_t* arg_type = extract_type(arg_type_node);
		types_add(arg_types, arg_type);
	    }
	    ast_node_t* return_type_node = ast_get_child(type_node, 1);
	    type_t* return_type = extract_type(return_type_node);
	    return type_new_function_type(arg_types, return_type);
	}
	case TUPLE_TYPE: {
	    types_t* tuple_types = types_new();
	    for (uint16_t i = 0; i < ast_number_of_children(type_node); i++) {
		ast_node_t* tuple_type_node = ast_get_child(type_node, i);
		type_t* tuple_type = extract_type(tuple_type_node);
		types_add(tuple_types, tuple_type);
	    }
	    return type_new_tuple_type(tuple_types);
	}
	case MAP_TYPE: {
	    ast_node_t* key_type_node = ast_get_child(type_node, 0);
	    type_t* key_type = extract_type(key_type_node);
	    ast_node_t* value_type_node = ast_get_child(type_node, 1);
	    type_t* value_type = extract_type(value_type_node);
	    return type_new_map_type(key_type, value_type);
	}
	case CONSTRUCTOR_TYPE: {
	    ast_node_t* name_node = ast_get_child(type_node, 0);
	    LOG_ASSERT(name_node->name == NAME, "Expected a NAME node");
	    types_t* types = types_new();
	    for (uint16_t i = 1; i < ast_number_of_children(type_node); i++) {
		ast_node_t* type_node = ast_get_child(type_node, i);
		type_t* type = extract_type(type_node);
		types_add(types, type);
	    }
	    return type_new_constructor_type(name_node->value, types);
	}
	case TYPE_VARIABLE:
	    return type_new_type_variable();
	default:
	    LOG_ABORT("Unknown type node: %s",
		      ast_node_name_to_string(type_node->name));
    }
    assert(false);
}

static uint32_t unique_id(void) {
    static uint32_t id = 0;
    return id++;
}
