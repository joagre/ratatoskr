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
    if (node->name == BOUND_NAME) {
	node->type = symbol_table_lookup(table, node->value);
	LOG_ASSERT(node->type != NULL, "Unbound name: '%s'", node->value);
    } else if (node->name == EQ ||
	       node->name == IF_EXPR ||
	       node->name == FUNCTION_DEF ||
	       node->name == BLOCK_EXPR ||
	       node->name == POSTFIX_EXPR ||
	       node->name == FUNCTION_CALL) {
	type_t* type = type_new_variable();
	node->type = type;
    } else if (node->name == NON_DEFAULT_PARAM) {
	type_t* type = type_new_variable();
	symbol_table_insert(table, node->value, type);
	node->type = type;
    } else if (node->name == INTEGRAL) {
	type_t* type = type_new_basic_type(TYPE_BASIC_TYPE_INTEGRAL);
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
    if (node->name == INTEGRAL) {
	// Equation: integral constant
	equation_t equation = {
	    .arg_type = node->type,
	    .return_type = type_new_basic_type(TYPE_BASIC_TYPE_INTEGRAL),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &equation);
    } else if (node->name == TRUE || node->name == FALSE) {
	// Equation: bool constant
	equation_t equation = {
	    .arg_type = node->type,
	    .return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &equation);
    } else if (node->name == EQ) {
	// Equation: eq
	equation_t eq_equation = {
	    .arg_type = node->type,
	    .return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &eq_equation);
	// Equation: left operand
	ast_node_t* left_node = ast_get_child(node, 0);
	equation_t left_equation = {
	    .arg_type = left_node->type,
	    .return_type = type_new_basic_type(TYPE_BASIC_TYPE_INTEGRAL),
	    .origin_node = node,
	    .node = left_node
	};
	// Equation: right operand
	equations_add(equations, &left_equation);
	ast_node_t* right_node = ast_get_child(node, 1);
	equation_t right_equation = {
	    .arg_type = right_node->type,
	    .return_type = type_new_basic_type(TYPE_BASIC_TYPE_INTEGRAL),
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
	ast_node_t* bound_name_node = ast_get_child(node, 0);
	assert(bound_name_node->name == BOUND_NAME);
	ast_node_t* function_call_node = ast_get_child(node, 1);
	assert(function_call_node->name == FUNCTION_CALL);
	ast_node_t* positional_args_node = ast_get_child(function_call_node, 0);
	assert(positional_args_node->name == POSITIONAL_ARGS);
	// Equation: function type
	types_t* arg_types = types_new();
	for (uint16_t i = 0;
	     i < ast_number_of_children(positional_args_node); i++) {
	    ast_node_t* arg_node = ast_get_child(positional_args_node, i);
	    types_add(arg_types, arg_node->type);
	}
	equation_t function_equation = {
	    .arg_type = bound_name_node->type,
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
	assert(if_node->name == IF);
	ast_node_t* if_conditional_node = ast_get_child(if_node, 0);
	assert(if_conditional_node->name == POSTFIX_EXPR);
	ast_node_t* if_body_node = ast_get_child(if_node, 1);
	assert(if_body_node->name == BLOCK_EXPR);
	ast_node_t* else_node = ast_get_child(node, 1);
	assert(else_node->name == ELSE);
	ast_node_t* else_body_node = ast_get_child(else_node, 0);
	assert(else_body_node->name == BLOCK_EXPR);
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
	ast_node_t* function_def_node = ast_get_child(node, 0);
	assert(function_def_node->name == FUNCTION_NAME);
	ast_node_t* params_node = ast_get_child(node, 1);
	assert(params_node->name == NON_DEFAULT_PARAMS);
	ast_node_t* body_node = ast_get_child(node, 2);
	assert(body_node->name == BLOCK_EXPR);
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

/*
    } else if (node->type == FUNCTION_DEF) {
	constraint_t constraint = {
	    .type = HM_FUNCTION_DEF,
	    .type_variable = node->type_variable,
	    .function_def.param_types = ok;
	    .function_def.return_type = ok;
	    .node = node
	};
	constraints_add(constraints, constraint);
    */
