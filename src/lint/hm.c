//#define MUTE_LOG_DEBUG 1

#include <assert.h>
#include <log.h>
#include "hm.h"
#include "symbol_table.h"
#include "satie_auxil.h"

// Forward declarations of local functions (alphabetical order)
static void add_type_variables(ast_node_t* node, symbol_table_t* table);
static hm_type_t* new_basic_type(hm_basic_type_t basic_type);
static hm_type_t* new_type_variable(void);
static type_variable_t next_type_variable(void);

void hm_infer_types(ast_node_t* node) {
//    symbol_table_t table;
//    symbol_table_init(&table);
//    add_type_variables(node, &table);
//    rules_t rules;
//    rules_init(&rules);
//    add_constraints(program, &rules);
}


/*
void hm_add_type_rules(ast_node_t *node, rules_t *rules) {
    if (node->type == INTEGRAL) {
	hm_type_t* return_type = hm_new_basic_type(HM_TYPE_INTEGRAL);
	hm_rule_t rule = rule_new(node->hm_type, return_type, node);
	rules_add(rules, rule);
    }
}
*/

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

//
// Local functions (alphabetical order)
//

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
