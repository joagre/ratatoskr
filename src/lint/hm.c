#include <assert.h>
#include "hm.h"

int type_variable_counter = 0;

void add_type_variables(ast_node_t *node, symbol_table_t *symbol_table) {
    if (node->type == FUNCTION_DEF ||
	node->type == NON_DEFAULT_PARAMS ||
	node->type == IF_EXPR ||
	node->type == EQ ||
	node->type == BOUND_NAME ||
	node->type == INTEGRAL ||
	node->type == FUNCTION_CALL) {
	node->type_variable = ++type_variable_counter;
	//symbol_table_insert(symbol_table, node->value, node->type_variable);
    }
    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
	    add_type_variables(dynarray_element(node->children, i),
			       symbol_table);
        }
    }
}
