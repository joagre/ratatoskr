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
static bool add_type_variables(ast_node_t* node, symbol_tables_t* tables,
			       uint32_t block_expr_id, satie_error_t* error);
static void add_type_equations(ast_node_t* node, equations_t* equations);
static type_t* extract_type(ast_node_t* type_node);
static uint32_t unique_id(void);

bool hm_infer_types(ast_node_t* node, satie_error_t* error) {
    // Check if tree is semantically valid
    if (!is_valid(NULL, node, error)) {
	return false;
    }
    // Add type variables
    symbol_tables_t tables;
    symbol_tables_init(&tables);
    if (!add_type_variables(node, &tables, unique_id(), error)) {
	return false;
    }
    // Print parse tree
    ast_print(node, 0);
    printf("\n");
    // Add equations
    equations_t equations;
    equations_init(&equations);
    add_type_equations(node, &equations);
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
	       node->name == TRUE) {
	// Assign a type variable
	type_t* type = type_new_type_variable();
	node->type = type;
    } else if (node->name == NAME) {
	// Should be in a symbol table or else it is erroneously unbound
	node->type = symbol_tables_lookup(tables, node->value);
	if (node->type == NULL) {
	    SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
			      "%d: '%s' has not been bound to a value",
			      node->row, node->value);
	    return false;
	}
    } else if (node->name == UNBOUND_NAME) {
	// Assign a type variable and add to the symbol table
	type_t* type = type_new_type_variable();
	node->type = type;
	symbol_tables_insert(tables, node->value, type);
    } else if (node->name == FUNCTION_DEF) {
	// Assign a type variable
	type_t* type = type_new_type_variable();
	node->type = type;
        // A FUNCTION_DEF node must add its parameters to the symbol
	// table before its block expression has created a new symbol
	// table. This is because the parameters are in scope in the
	// upcoming block expression. The normal block expression is
	// therefore overridden here.
	ast_node_t* params_node = ast_get_child(node, 1);
	LOG_ASSERT(params_node->name == NON_DEFAULT_PARAMS,
		   "Expected a NON_DEFAULT_PARAMS node");
	size_t n = ast_number_of_children(params_node);
	// Create a new symbol table and add the parameters to it
	uint32_t block_expr_id = unique_id();
	symbol_table_t* table = symbol_table_new();
	symbol_tables_insert_table(tables, table, block_expr_id);
	for (uint16_t i = 0; i < n; i++) {
	    ast_node_t* param_node = ast_get_child(params_node, i);
	    LOG_ASSERT(param_node->name == PARAM_NAME,
		       "Expected a PARAM_NAME node");
	    type_t* param_type = type_new_type_variable();
	    param_node->type = param_type;
	    symbol_tables_insert(tables, param_node->value, param_type);
	}
	// Handle the return type accordingly (it may or may not exist)
	ast_node_t* return_type_node = ast_get_child(node, 2);
	ast_node_t* body_node;
	if (return_type_node->name == RETURN_TYPE) {
	    // Add a type variable to the return type
	    type_t* return_type = type_new_type_variable();
	    return_type_node->type = return_type;
	    body_node = ast_get_child(node, 3);
	} else {
	    // There is no return type
	    body_node = return_type_node;
	    return_type_node = NULL;
	}
	// Add type variables to the body
	if (!add_type_variables(body_node, tables, block_expr_id, error)) {
	    return false;
	}
	// Remove the symbol table adhering to the block expression
	// (created above)
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

static void add_type_equations(ast_node_t *node, equations_t* equations) {
    if (node->name == INT) {
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
    } else if (node->name == EQ) {
	// Equation: eq
	equation_t eq_equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
			 node, node, false);
	equations_add(equations, &eq_equation);
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
	ast_node_t* left_node = ast_get_child(node, 0);
        // Equation: left operand
	equation_t left_equation =
	    equation_new(left_node->type, type_new_basic_type(eq_type),
			 node, left_node, false);
	equations_add(equations, &left_equation);
	ast_node_t* right_node = ast_get_child(node, 2);
	// Equation: right operand
	equation_t right_equation =
	    equation_new(right_node->type, type_new_basic_type(eq_type),
			 node, right_node, false);
	equations_add(equations, &right_equation);
    } else if (node->name == IF_EXPR) {
	ast_node_t* if_node = ast_get_child(node, 0);
	LOG_ASSERT(if_node->name == IF, "Expected an IF node");
	ast_node_t* if_conditional_node = ast_get_child(if_node, 0);
	LOG_ASSERT(if_conditional_node->name == POSTFIX_EXPR,
		   "Expected a POSTFIX_EXPR node");
	ast_node_t* if_body_node = ast_get_child(if_node, 1);
	LOG_ASSERT(if_body_node->name == BLOCK_EXPR,
		   "Expected a BLOCK_EXPR node");
	ast_node_t* else_node = ast_get_child(node, 1);
	LOG_ASSERT(else_node->name == ELSE, "Expected an ELSE node");
	ast_node_t* else_body_node = ast_get_child(else_node, 0);
	LOG_ASSERT(else_body_node->name == BLOCK_EXPR,
		   "Expected a BLOCK_EXPR node");
	// Equation: if conditional
	equation_t if_conditional_equation =
	    equation_new(if_conditional_node->type,
			 type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
			 node, if_conditional_node, false);
	equations_add(equations, &if_conditional_equation);
        // Equation: if body
	equation_t if_body_equation =
	    equation_new(if_body_node->type, node->type, node, if_body_node,
			 false);
	equations_add(equations, &if_body_equation);
	// Equation: else body
	equation_t else_body_equation =
	    equation_new(else_body_node->type, node->type, node,
			 else_body_node, false);
	equations_add(equations, &else_body_equation);
    } else if (node->name == FUNCTION_DEF) {
	ast_node_t* params_node = ast_get_child(node, 1);
	LOG_ASSERT(params_node->name == NON_DEFAULT_PARAMS,
		   "Expected a NON_DEFAULT_PARAMS node");
	ast_node_t* return_type_node = ast_get_child(node, 2);
	ast_node_t* body_node;
	if (return_type_node->name == RETURN_TYPE) {
	    body_node = ast_get_child(node, 3);
	} else {
	    body_node = return_type_node;
	    return_type_node = NULL;
	}
	LOG_ASSERT(body_node->name == BLOCK_EXPR,
		   "Expected a BLOCK_EXPR node");
	types_t* arg_types = types_new();
	size_t n = ast_number_of_children(params_node);
	for (uint16_t i = 0; i < n; i++) {
	    ast_node_t* param_node = ast_get_child(params_node, i);
	    LOG_ASSERT(param_node->name == PARAM_NAME,
		       "Expected a PARAM_NAME node");
	    if (ast_number_of_children(param_node) > 0) {
		ast_node_t* arg_type_node = ast_get_child(param_node, 0);
		type_t* arg_type = extract_type(arg_type_node);
                // Equation: arg type (if any)
		equation_t arg_type_equation =
		    equation_new(param_node->type, arg_type, node, param_node,
				 true);
		equations_add(equations, &arg_type_equation);
	    }
	    types_add(arg_types, param_node->type);
	}
	// Equation: app type
	equation_t function_equation =
	    equation_new(node->type,
			 type_new_app_type(arg_types, body_node->type),
			 node, node, false);
	equations_add(equations, &function_equation);
	if (return_type_node != NULL) {
	    ast_node_t* type_node = ast_get_child(return_type_node, 0);
	    type_t* return_type = extract_type(type_node);
	    // Equation: return type (if any)
	    equation_t return_type_equation =
		equation_new(body_node->type, return_type, node,
			     return_type_node, true);
	    equations_add(equations, &return_type_equation);
	}
    } else if (node->name == BIND) {
	ast_node_t* left_node = ast_get_child(node, 0);
	ast_node_t* right_node = ast_get_child(node, 1);
	// Equation: eq
	equation_t bind_left_right_equation =
	    equation_new(left_node->type, right_node->type, node, left_node,
			 false);
	equations_add(equations, &bind_left_right_equation);
	equation_t bind_equation =
	    equation_new(node->type, right_node->type, node, node, false);
	equations_add(equations, &bind_equation);
    } else if (node->name == POSTFIX_EXPR) {
	ast_node_t* name_node = ast_get_child(node, 0);
	LOG_ASSERT(name_node->name == NAME, "Expected a NAME node");
	ast_node_t* function_call_node = ast_get_child(node, 1);
	LOG_ASSERT(function_call_node->name == FUNCTION_CALL,
		   "Expected a FUNCTION_CALL node");
	ast_node_t* positional_args_node = ast_get_child(function_call_node, 0);
	LOG_ASSERT(positional_args_node->name == POSITIONAL_ARGS,
		   "Expected a POSITIONAL_ARGS node");
	types_t* arg_types = types_new();
	size_t n = ast_number_of_children(positional_args_node);
	for (uint16_t i = 0; i < n; i++) {
	    ast_node_t* arg_node = ast_get_child(positional_args_node, i);
	    types_add(arg_types, arg_node->type);
	}
	// Equation: function call type
	equation_t app_equation =
	    equation_new(name_node->type,
			 type_new_app_type(arg_types, function_call_node->type),
			 node, node, false);
	equations_add(equations, &app_equation);
	// Equation: postfix expression
	equation_t postfix_expr_equation =
	    equation_new(node->type, function_call_node->type, node, node,
			 false);
	equations_add(equations, &postfix_expr_equation);
    } else if (node->name == BLOCK_EXPR) {
	ast_node_t* last_block_expr_node = ast_last_child(node);
	// Equation: block expression
	equation_t equation =
	    equation_new(node->type, last_block_expr_node->type, node, node,
			 false);
	equations_add(equations, &equation);
    }

    // Traverse children (if any)
    size_t n = ast_number_of_children(node);
    for (uint16_t i = 0; i < n; i++) {
	add_type_equations(ast_get_child(node, i), equations);
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
	case APP_TYPE: {
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
	    return type_new_app_type(arg_types, return_type);
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
