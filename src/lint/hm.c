//#define MUTE_LOG_DEBUG 1

#include <assert.h>
#include <log.h>
#include "hm.h"
#include "types.h"
#include "equations.h"
#include "symbol_table.h"

// Forward declarations of local functions
static void add_type_variables(ast_node_t* node, symbol_table_t* table);
static void add_type_equations(ast_node_t* node, equations_t* equations);

void hm_infer_types(ast_node_t* node) {
    symbol_table_t table;
    symbol_table_init(&table);
    add_type_variables(node, &table);
    printf("Type Variables\n--------------\n");
    ast_print(node, 0);
    equations_t equations;
    equations_init(&equations);
    add_type_equations(node, &equations);
    printf("\nType Equations\n--------------\n");
    print_equations(&equations);
}

//
// Local functions
//

static void add_type_variables(ast_node_t* node, symbol_table_t* table) {
    if (node->name == PROGRAM ||
	node->name == TOP_LEVEL_DEFS ||
	node->name == FUNCTION_NAME ||
	node->name == NON_DEFAULT_PARAMS ||
	node->name == IF ||
	node->name == ELSE ||
	node->name == POSITIONAL_ARGS) {
	// Ignore
    } else if (node->name == NAME) {
	node->type = symbol_table_lookup(table, node->value);
	//fprintf(stderr, "name: %s\n", node->value);
	LOG_ASSERT(node->type != NULL, "Name '%s' is not in symbol table",
		   node->value);
    } else if (node->name == PARAM_NAME) {
	type_t* type = type_new_variable();
	symbol_table_insert(table, node->value, type);
	node->type = type;
    } else if (node->name == EQ ||
	       node->name == IF_EXPR ||
	       node->name == FUNCTION_DEF ||
	       node->name == BLOCK_EXPR ||
	       node->name == POSTFIX_EXPR ||
	       node->name == FUNCTION_CALL) {
	type_t* type = type_new_variable();
	node->type = type;
    } else if (node->name == NAME) {
	type_t* type = type_new_variable();
	symbol_table_insert(table, node->value, type);
	node->type = type;
    } else if (node->name == INT) {
	type_t* type = type_new_basic_type(TYPE_BASIC_TYPE_INT);
	symbol_table_insert(table, node->value, type);
	node->type = type;
    } else if (node->name == TRUE || node->name == FALSE) {
	type_t* type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL);
	symbol_table_insert(table, node->value, type);
	node->type = type;
    }

    if (node->children != NULL) {
        for (uint16_t i = 0; i < ast_number_of_children(node); i++) {
	    add_type_variables(ast_get_child(node, i), table);
        }
    }
}

void add_type_equations(ast_node_t *node, equations_t* equations) {
    if (node->name == INT) {
	// Equation: Int constant
	equation_t equation = {
	    .arg_type = node->type,
	    .return_type = type_new_basic_type(TYPE_BASIC_TYPE_INT),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &equation);
    } else if (node->name == TRUE || node->name == FALSE) {
	// Equation: Bool constant
	equation_t equation = {
	    .arg_type = node->type,
	    .return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &equation);
    } else if (node->name == EQ) {
	// Equation: eq
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
	equation_t eq_equation = {
	    .arg_type = node->type,
	    .return_type = type_new_basic_type(eq_type),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &eq_equation);
        // Equation: left operand
	ast_node_t* left_node = ast_get_child(node, 0);
	equation_t left_equation = {
	    .arg_type = left_node->type,
	    .return_type = type_new_basic_type(eq_type),
	    .origin_node = node,
	    .node = left_node
	};
	equations_add(equations, &left_equation);
	// Equation: right operand
	ast_node_t* right_node = ast_get_child(node, 2);
	equation_t right_equation = {
	    .arg_type = right_node->type,
	    .return_type = type_new_basic_type(eq_type),
	    .origin_node = node,
	    .node = right_node
	};
	equations_add(equations, &right_equation);
    } else if (node->name == BLOCK_EXPR) {
	// Equation: block expression
	ast_node_t* last_block_expr_node = ast_last_child(node);
	equation_t equation = {
	    .arg_type = node->type,
	    .return_type = last_block_expr_node->type,
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &equation);
    } else if (node->name == POSTFIX_EXPR) {
	ast_node_t* name_node = ast_get_child(node, 0);
	LOG_ASSERT(name_node->name == NAME, "Expected a NAME node");
	ast_node_t* function_call_node = ast_get_child(node, 1);
	LOG_ASSERT(function_call_node->name == FUNCTION_CALL,
		   "Expected a FUNCTION_CALL node");
	ast_node_t* positional_args_node = ast_get_child(function_call_node, 0);
	LOG_ASSERT(positional_args_node->name == POSITIONAL_ARGS,
		   "Expected a POSITIONAL_ARGS node");
	// Equation: function type
	types_t* arg_types = types_new();
	for (uint16_t i = 0;
	     i < ast_number_of_children(positional_args_node); i++) {
	    ast_node_t* arg_node = ast_get_child(positional_args_node, i);
	    types_add(arg_types, arg_node->type);
	}
	equation_t function_equation = {
	    .arg_type = name_node->type,
	    .return_type =
	        type_new_function(arg_types, function_call_node->type),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &function_equation);
	// Equation: postfix expression
	equation_t postfix_expr_equation = {
	    .arg_type = node->type,
	    .return_type = function_call_node->type,
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &postfix_expr_equation);
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
	equation_t if_conditional_equation = {
	    .arg_type = if_conditional_node->type,
	    .return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
	    .origin_node = node,
	    .node = if_conditional_node
	};
	equations_add(equations, &if_conditional_equation);
	// Equation: if body
	equation_t if_body_equation = {
	    .arg_type = if_body_node->type,
	    .return_type = node->type,
	    .origin_node = node,
	    .node = if_body_node
	};
	equations_add(equations, &if_body_equation);
	// Equation: else body
	equation_t else_body_equation = {
	    .arg_type = else_body_node->type,
	    .return_type = node->type,
	    .origin_node = node,
	    .node = else_body_node
	};
	equations_add(equations, &else_body_equation);
    } else if (node->name == FUNCTION_DEF) {
	ast_node_t* params_node = ast_get_child(node, 1);
	LOG_ASSERT(params_node->name == NON_DEFAULT_PARAMS,
		   "Expected a NON_DEFAULT_PARAMS node");
	ast_node_t* body_node = ast_get_child(node, 2);
	LOG_ASSERT(body_node->name == BLOCK_EXPR,
		   "Expected a BLOCK_EXPR node");
	// Equation: function type
	types_t* arg_types = types_new();
	for (uint16_t i = 0; i < ast_number_of_children(params_node); i++) {
	    ast_node_t* param_node = ast_get_child(params_node, i);
	    types_add(arg_types, param_node->type);
	}
	equation_t function_equation = {
	    .arg_type = node->type,
	    .return_type = type_new_function(arg_types, body_node->type),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &function_equation);
    }

    if (node->children != NULL) {
        for (uint16_t i = 0; i < ast_number_of_children(node); i++) {
	    add_type_equations(ast_get_child(node, i), equations);
	}
    }
}
