//#define MUTE_LOG_DEBUG 1

#include <assert.h>
#include <log.h>
#include "hm.h"
#include "symbol_table.h"
#include "satie_auxil.h"

// Forward declarations of local functions (alphabetical order)
static type_variable_t next_type_variable(void);
static hm_type_t* new_basic_type(hm_basic_type_t basic_type);
static hm_type_t* new_type_variable(void);

void hm_add_type_variables(ast_node_t *node, symbol_table_t *table) {
    if (node->type == BOUND_NAME) {
	node->hm_type = symbol_table_lookup(table, node->value);
	LOG_ASSERT(node->hm_type != NULL, "Unbound name: '%s'", node->value);
    } else if (node->type == INTEGRAL) {
	hm_type_t* hm_type = new_basic_type(HM_TYPE_INTEGRAL);
	symbol_table_insert(table, node->value, hm_type);
	node->hm_type = hm_type;
    } else if (node->type == TRUE || node->type == FALSE) {
	hm_type_t* hm_type = new_basic_type(HM_TYPE_BOOL);
	symbol_table_insert(table, node->value, hm_type);
	node->hm_type = hm_type;
    } else if (node->type == NON_DEFAULT_PARAM) {
	hm_type_t* hm_type = new_type_variable();
	symbol_table_insert(table, node->value, hm_type);
	node->hm_type = hm_type;
    } else if (node->type == EQ ||
	       node->type == IF_EXPR ||
	       node->type == FUNCTION_DEF ||
	       node->type == POSTFIX_EXPR ||
	       node->type == FUNCTION_CALL) {
	hm_type_t* hm_type = new_type_variable();
	node->hm_type = hm_type;
    }

    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
	    hm_add_type_variables(dynarray_element(node->children, i), table);
        }
    }
}

//void add_constraints(ast_node_t *node, constraints_t *constraints) {
/*
  if (node->type == INT) {
	constraint_t constraint = {
	    .node = node,
	    .type_variable = node->type_variable,
	    .type = HM_TYPE_INTEGRAL,
	};
	constraints_add(constraints, constraint);
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
		.tag = HM_BASIC_TYPE,
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

//
// Local functions (alphabetical order)
//

static type_variable_t next_type_variable(void) {
    static type_variable_t next_type_variable = 0;
    return next_type_variable++;
}

static hm_type_t* new_basic_type(hm_basic_type_t basic_type) {
    hm_type_t* hm_type = malloc(sizeof(hm_type_t));
    hm_type->tag = HM_TYPE_TAG_BASIC_TYPE;
    hm_type->basic_type = basic_type;
    return hm_type;
}

static hm_type_t* new_type_variable(void) {
    hm_type_t* hm_type = malloc(sizeof(hm_type_t));
    hm_type->tag = HM_TYPE_TAG_VARIABLE;
    hm_type->variable = next_type_variable();
    return hm_type;
}
