//#define MUTE_LOG_DEBUG 1

#include <assert.h>
#include <log.h>
#include "hm.h"

int type_variable_counter = 0;

void add_type_variables(ast_node_t *node, symbol_table_t *table) {
    if (node->type == FUNCTION_DEF ||
	node->type == NON_DEFAULT_PARAM ||
	node->type == IF_EXPR ||
	node->type == EQ ||
	node->type == BOUND_NAME ||
	node->type == INTEGRAL ||
	node->type == FUNCTION_CALL) {
	if (node->value != NULL) {
	    type_variable_t type_variable = symbol_table_lookup(table, node->value);
	    if (type_variable == 0) {
		node->type_variable = ++type_variable_counter;
		symbol_table_insert(table, node->value, node->type_variable);
	    } else {
		node->type_variable = type_variable;
	    }
	} else {
	    node->type_variable = ++type_variable_counter;
	}
    }
    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
	    add_type_variables(dynarray_element(node->children, i), table);
        }
    }
}
