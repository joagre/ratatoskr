//#define MUTE_LOG_DEBUG 1

#include <assert.h>
#include <log.h>
#include "hm.h"
#include "symbol_table.h"
#include "ast.h"

// Forward declarations of local functions (alphabetical order)
static void add_type_equations(ast_node_t *node, hm_equations_t *equations);
static void add_type_variables(ast_node_t* node, symbol_table_t* table);
static void equations_init(hm_equations_t* equations);
static void equations_add(hm_equations_t* equations, hm_equation_t* equation);
static hm_type_t* new_basic_type(hm_basic_type_t basic_type);
static hm_type_t* new_type_variable(void);
static type_variable_t next_type_variable(void);
static void print_equations(hm_equations_t* equations);

void hm_infer_types(ast_node_t* node) {
    symbol_table_t table;
    symbol_table_init(&table);
    add_type_variables(node, &table);
    fprintf(stderr, "Type Variables\n--------------\n");
    ast_print(node, 0);
    hm_equations_t equations;
    equations_init(&equations);
    add_type_equations(node, &equations);
    fprintf(stderr, "\nType Equations\n--------------\n");
    print_equations(&equations);
}

//
// Local functions (alphabetical order)
//

void add_type_equations(ast_node_t *node, hm_equations_t *equations) {
    if (node->name == INTEGRAL) {
	hm_equation_t equation = {
	    .argument_type = node->type,
	    .return_type = new_basic_type(HM_BASIC_TYPE_INTEGRAL),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &equation);
    } else if (node->name == TRUE || node->name == FALSE) {
	hm_equation_t equation = {
	    .argument_type = node->type,
	    .return_type = new_basic_type(HM_BASIC_TYPE_BOOL),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &equation);
    } else if (node->name == EQ) {
	hm_equation_t op_equation = {
	    .argument_type = node->type,
	    .return_type = new_basic_type(HM_BASIC_TYPE_BOOL),
	    .origin_node = node,
	    .node = node
	};
	equations_add(equations, &op_equation);
	ast_node_t* left_node = ast_get_child(node, 0);
	hm_equation_t left_equation = {
	    .argument_type = left_node->type,
	    .return_type = new_basic_type(HM_BASIC_TYPE_INTEGRAL),
	    .origin_node = node,
	    .node = left_node
	};
	equations_add(equations, &left_equation);
	ast_node_t* right_node = ast_get_child(node, 1);
	hm_equation_t right_equation = {
	    .argument_type = right_node->type,
	    .return_type = new_basic_type(HM_BASIC_TYPE_INTEGRAL),
	    .origin_node = node,
	    .node = right_node
	};
	equations_add(equations, &right_equation);
    }

    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
	    add_type_equations(dynarray_element(node->children, i), equations);
        }
    }
}

/*
    } else if (node->type == BOOL) {
	constraint_t constraint = {
	    .node = node,
	    .type_variable = node->type_variable,
	    .type = HM_TYPE_BOOL,

	};
	constraints_add(constraints, constraint);
    } else if (node->type == EQ) {
	ast_node_t* left_node = satie_auxil_get_child(node, 0);
	constraint_t constraint = {
	    .node = left_node,
	    .type_variable = left_node->type_variable,
	    .type = HM_TYPE_INTEGRAL
	};
	ast_node_t* right_node = satie_auxil_get_child(node, 0);
	constraint_t constraint = {
	    .node = left_node,
	    .type_variable = left_node->type_variable,
	    .type = HM_TYPE_INTEGRAL
	};
	constraint_t constraint = {
	    .node = node,
	    .type_variable = node->type_variable,
	    .type = HM_TYPE_BOOL
	};
	constraints_add(constraints, constraint);
    } else if (node->type == FUNCTION_DEF) {
	constraint_t constraint = {
	    .type = HM_FUNCTION_DEF,
	    .type_variable = node->type_variable,
	    .function_def.param_types = ok;
	    .function_def.return_type = ok;
	    .node = node
	};
	constraints_add(constraints, constraint);
    } else if (node->type == IF_EXPR) {
	// if conditional constraint
	ast_node_t* if_node = satie_auxil_get_child(node, 0);
	ast_node_t* if_conditional_node = satie_auxil_get_child(if_node, 0);
	constraint_t constraint = {
	    .argument_type = if_conditional_node->type,
	    .return_type = {
		.tag = HM_TYPE_TAG_BASIC_TYPE,
		.basic_type = HM_TYPE_BOOL
	    },
	    .node = node
	};
	// if body constraint
	ast_node_t* if_body_node = satie_auxil_get_child(if_node, 1);
	ast_node_t* last_if_body_node = satie_auxil_last_child(if_body_node);
	constraint_t constraint = {
	    .argument_type = node->type,
	    .return_type = ast_if_body_node->type,
	    .node = node
	};
	// else body constraint
	ast_node_t* else_node = satie_auxil_get_child(node, 1);
	ast_node_t* else_body_node = satie_auxil_get_child(else_node, 0);
	ast_node_t* last_else_body_node = satie_auxil_last_child(else_body_node);
	constraint_t constraint = {
	    .argument_type = node->type,
	    .return_type = ast_else_body_node->type,
	    .node = node
	};
    }

	for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
	    add_constraints(dynarray_element(node->children, i), list);
	}

    */
//}


static void add_type_variables(ast_node_t* node, symbol_table_t* table) {
    if (node->name == BOUND_NAME) {
	node->type = symbol_table_lookup(table, node->value);
	LOG_ASSERT(node->type != NULL, "Unbound name: '%s'", node->value);
    } else if (node->name == EQ ||
	       node->name == IF_EXPR ||
	       node->name == FUNCTION_DEF ||
	       node->name == POSTFIX_EXPR ||
	       node->name == FUNCTION_CALL) {
	hm_type_t* type = new_type_variable();
	node->type = type;
    } else if (node->name == NON_DEFAULT_PARAM) {
	hm_type_t* type = new_type_variable();
	symbol_table_insert(table, node->value, type);
	node->type = type;
    } else if (node->name == INTEGRAL) {
	hm_type_t* type = new_basic_type(HM_BASIC_TYPE_INTEGRAL);
	symbol_table_insert(table, node->value, type);
	node->type = type;
    } else if (node->name == TRUE || node->name == FALSE) {
	hm_type_t* type = new_basic_type(HM_BASIC_TYPE_BOOL);
	symbol_table_insert(table, node->value, type);
	node->type = type;
    }

    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
	    add_type_variables(dynarray_element(node->children, i), table);
        }
    }
}

static void equations_init(hm_equations_t* equations) {
    dynarray_init(equations, NULL, 0, sizeof(hm_equation_t));
}

static void equations_add(hm_equations_t* equations, hm_equation_t* equation) {
    dynarray_append(equations, equation);
}

static hm_type_t* new_basic_type(hm_basic_type_t basic_type) {
    hm_type_t* type = malloc(sizeof(hm_type_t));
    type->tag = HM_TYPE_TAG_BASIC_TYPE;
    type->basic_type = basic_type;
    return type;
}

static hm_type_t* new_type_variable(void) {
    hm_type_t* type = malloc(sizeof(hm_type_t));
    type->tag = HM_TYPE_TAG_VARIABLE;
    type->variable = next_type_variable();
    return type;
}

static type_variable_t next_type_variable(void) {
    static type_variable_t next_type_variable = 0;
    return next_type_variable++;
}

static void print_equations(hm_equations_t* equations) {
    for (uint16_t i = 0; i < dynarray_size(equations); i++) {
	hm_equation_t* equation = dynarray_element(equations, i);
	fprintf(stderr, "%s",
		ast_node_name_to_string(equation->origin_node->name));
	if (equation->node->value != NULL) {
	    fprintf(stderr, " (%s): ", equation->node->value);
	} else {
	    fprintf(stderr, ": ");
	}
	switch (equation->argument_type->tag) {
	    case HM_TYPE_TAG_BASIC_TYPE:
		switch (equation->argument_type->basic_type) {
		    case HM_BASIC_TYPE_INTEGRAL:
			fprintf(stderr, "integral ->");
			break;
		    case HM_BASIC_TYPE_BOOL:
			fprintf(stderr, "bool ->");
			break;
		}
		break;
	    case HM_TYPE_TAG_VARIABLE:
		fprintf(stderr, "t%d ->", equation->argument_type->variable);
		break;
	}
	switch (equation->return_type->tag) {
	    case HM_TYPE_TAG_BASIC_TYPE:
		switch (equation->return_type->basic_type) {
		    case HM_BASIC_TYPE_INTEGRAL:
			fprintf(stderr, " integral\n");
			break;
		    case HM_BASIC_TYPE_BOOL:
			fprintf(stderr, " bool\n");
			break;
		}
		break;
	    case HM_TYPE_TAG_VARIABLE:
		fprintf(stderr, " t%d\n", equation->return_type->variable);
		break;
	}
    }
}
