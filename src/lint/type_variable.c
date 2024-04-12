#include <log.h>

#include "type_variable.h"

// Forward declarations of local functions (alphabetical order)
static bool associate(equations_t* equations, types_t* type_variables_types,
		      uint16_t number_of_type_variables, type_t* type,
		      satie_error_t* error);

bool type_variable_create(ast_node_t* node, symbol_tables_t* tables,
			  uint32_t block_expr_id,
			  satie_error_t* error) {
    bool traverse_children = true;
    if (node->name == CONS ||
	node->name == CONST ||
	node->name == CONCAT_STRING ||
	node->name == DIV_INT ||
	node->name == DIV_FLOAT ||
	node->name == ESCAPE_CHAR ||
	node->name == EXP ||
	node->name == ADD_INT ||
	node->name == ADD_FLOAT ||
	node->name == AND ||
	node->name == BITWISE_AND ||
	node->name == BITWISE_OR ||
	node->name == BSL ||
	node->name == BSR ||
	node->name == AS ||
	node->name == ARGS ||
	node->name == ARG_TYPES ||
	node->name == BOOL_TYPE ||
	node->name == CASE ||
	node->name == CHAR ||
	node->name == CHAR_TYPE ||
	node->name == CONCAT_LIST ||
	node->name == CONCAT_MAP ||
	node->name == DEFAULT ||
	node->name == ELIF ||
	node->name == ELSE ||
	node->name == EMPTY_LIST_TYPE ||
	node->name == EMPTY_MAP_TYPE ||
	node->name == EMPTY_TUPLE_TYPE ||
	node->name == EXPORT ||
	node->name == EQ ||
	node->name == EQ_TYPE ||
        node->name == FALSE ||
	node->name == FLOAT_TYPE ||
	node->name == FUNCTION_TYPE ||
	node->name == INDEX_VALUE ||
	node->name == INT ||
	node->name == INT_TYPE ||
	node->name == FLOAT ||
	node->name == GTE_INT ||
	node->name == GT_INT ||
	node->name == GTE_FLOAT ||
	node->name == GT_INT ||
	node->name == GT_FLOAT ||
	node->name == LIST_LITERAL ||
	node->name == LIST_TYPE ||
	node->name == LTE_INT ||
	node->name == LTE_FLOAT ||
	node->name == LT_INT ||
	node->name == LT_FLOAT ||
	node->name == MAP_LITERAL ||
	node->name == MAP_TYPE ||
	node->name == MEMBER_PROPERTY ||
	node->name == MEMBER_METHOD ||
	node->name == MUL_INT ||
	node->name == MUL_FLOAT ||
	node->name == MOD ||
	node->name == NAMED_ARG ||
	node->name == NAMED_ARG_NAME ||
	node->name == NAMED_ARGS ||
	node->name == NE ||
	node->name == NEG_INT ||
	node->name == NEG_FLOAT ||
	node->name == NON_QUOTE_CHAR ||
	node->name == NOT ||
	node->name == OR ||
	node->name == POS_INT ||
	node->name == POS_FLOAT ||
	node->name == PARAMS ||
	node->name == PRIVATE ||
	node->name == PROGRAM ||
	node->name == PUBLIC ||
	node->name == RAW_STRING ||
	node->name == READONLY ||
	node->name == RECORD_MEMBER_NAME ||
	node->name == RECORD_MEMBERS ||
	node->name == REGULAR_STRING ||
	node->name == STRING_TYPE ||
	node->name == SUB_INT ||
	node->name == SUB_FLOAT ||
	node->name == SWITCH ||
	node->name == TASK ||
	node->name == TASK_TYPE ||
	node->name == TOP_LEVEL_DEFS ||
	node->name == TRUE ||
	node->name == TUPLE_LITERAL ||
	node->name == TUPLE_TYPE ||
	node->name == TYPE ||
	node->name == TYPE_VARIABLE ||
	node->name == TYPE_VARIABLES ||
	node->name == TYPES) {
	// Do not assign a type variable
    } else if (node->name == EXPRS ||
	       node->name == IF ||
	       node->name == FUNCTION_CALL ||
	       node->name == LIST_LOOKUP ||
	       node->name == LIST_SLICE ||
	       node->name == LIST_UPDATE ||
	       node->name == MAP_KEY_VALUE ||
	       node->name == MAP_LOOKUP ||
	       node->name == MAP_UPDATE ||
	       node->name == NEW_RECORD ||
	       node->name == POSTFIX ||
	       node->name == RECORD_DOT ||
	       node->name == SLICE_LENGTH ||
	       node->name == WHEN) {
	// Assign a type variable
	type_t* type = type_new_type_variable(NULL);
	node->type = type;
    } else if (node->name == NAME) {
	// Names should already be in a symbol table or else it is an error
	node->type = symbol_tables_lookup(tables, node->value);
	if (node->type == NULL) {
	    SET_ERROR_MESSAGE(
		error, COMPONENT_COMPILER,
		"%d: '%s' has not been bound to a value",
		node->row, node->value);
	    return false;
	}
    } else if (node->name == PARAM_NAME ||
	       node->name == UNBOUND_NAME ||
	       node->name == FUNCTION_NAME) {
	if (node->type == NULL) {
	    // Assign a type variable
	    type_t* type = type_new_type_variable(NULL);
	    node->type = type;
	    // Add the type variable to the symbol table
	    symbol_tables_insert(tables, node->value, type);
	}
    } else if (node->name == FUNCTION_DEF) {
	uint32_t function_def_id = unique_id();
	symbol_table_t* table = symbol_table_new();
	symbol_tables_insert_table(tables, table, function_def_id);
	// Add type variables to the function definition children nodes
	uint16_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!type_variable_create(ast_get_child(node, i), tables,
				      function_def_id, error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	// Remove the symbol table created above and all nested symbol
	// tables created by bind expressions
	symbol_tables_delete_by_id(tables, function_def_id);
	traverse_children = false;
    } else if (node->name == FUNCTION_LITERAL) {
	// Create a new symbol table
	uint32_t function_literal_id = unique_id();
	symbol_table_t* table = symbol_table_new();
	symbol_tables_insert_table(tables, table, function_literal_id);
	// Add type variables to the function definition children nodes
	uint16_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!type_variable_create(ast_get_child(node, i), tables,
				      function_literal_id, error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	// Hoist the function name type variable
	ast_node_t* function_name_node = ast_get_child(node, 0);
	if (function_name_node->name == FUNCTION_NAME) {
	    symbol_tables_hoist(tables, function_name_node->value);
	}
	// Remove the symbol table created above and all nested symbol
	// tables created by bind expressions
	symbol_tables_delete_by_id(tables, function_literal_id);
	traverse_children = false;
    } else if (node->name == BIND) {
        // Extract all nodes constituting the bind expression
	uint16_t index = 0;
	ast_node_t* left_node = ast_get_child(node, index);
	ast_node_t* type_node = NULL;
	ast_node_t* as_node = NULL;
	ast_node_t* right_node;
	ast_node_t* child_node = ast_get_child(node, ++index);
	if (child_node->name == TYPE) {
	    type_node = child_node;
	    child_node = ast_get_child(node, ++index);
	}
	if (child_node->name == AS) {
	    as_node = child_node;
	    child_node = ast_get_child(node, ++index);
	}
	right_node = child_node;
	// Take care of the right node first
	if (!type_variable_create(right_node, tables, block_expr_id, error)) {
	    CLEAR_ERROR(error);
	    return false;
	}
        // Create a new symbol table
	symbol_table_t* table = symbol_table_new();
	block_expr_id = unique_id();
	symbol_tables_insert_table(tables, table, block_expr_id);
	// Take care of the left node
	if (!type_variable_create(left_node, tables, block_expr_id, error)) {
	    CLEAR_ERROR(error);
	    return false;
	}
	// Take care of the is node
	if (type_node != NULL) {
	    if (!type_variable_create(type_node, tables, block_expr_id,
				      error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	// Take care of the as node
	if (as_node != NULL) {
	    if (!type_variable_create(as_node, tables, block_expr_id, error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	traverse_children = false;
    } else if (node->name == BLOCK) {
	// Create a new symbol table
	symbol_table_t* table = symbol_table_new();
	block_expr_id = unique_id();
	symbol_tables_insert_table(tables, table, block_expr_id);
	// Add type variables to the block expression children nodes
	uint16_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!type_variable_create(ast_get_child(node, i), tables,
				      block_expr_id, error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	// Remove the symbol table created above and all sub symbol
	// tables created by bind expressions
	symbol_tables_delete_by_id(tables, block_expr_id);
	traverse_children = false;
    } else if (node->name == RECORD_DEF) {
	// Create a new symbol table
	uint32_t record_def_id = unique_id();
	symbol_table_t* table = symbol_table_new();
	symbol_tables_insert_table(tables, table, record_def_id);
	// Collect all member names
	uint16_t index = 0;
	ast_node_t* child_node = ast_get_child(node, ++index);
	if (child_node->name == TYPE_VARIABLES) {
	    child_node = ast_get_child(node, ++index);
	}
	ast_node_t* record_members_node = child_node;
	uint16_t n = ast_number_of_children(record_members_node);
	for (uint16_t i = 0; i < n; i++) {
	    ast_node_t* record_member_node =
		ast_get_child(record_members_node, i);
	    uint16_t m = ast_number_of_children(record_member_node);
	    ast_node_t* record_member_name_node = NULL;
	    // Look for member name nodes
	    for (uint16_t j = 0; j < m; j++) {
		ast_node_t* child_node =
		    ast_get_child(record_member_node, j);
		if (child_node->name == RECORD_MEMBER_NAME) {
		    record_member_name_node = child_node;
		    break;
		}
	    }
	    type_t* type_variable = type_new_type_variable(NULL);
	    record_member_name_node->type = type_variable;
	    symbol_tables_insert(tables, record_member_name_node->value,
				 type_variable);

	}
	// Add type variables to the block expression children nodes
	for (uint16_t i = 0; i < n; i++) {
	    if (!type_variable_create(ast_get_child(record_members_node, i),
				      tables, record_def_id, error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	// Remove the symbol table created above and all nested symbol
	// tables created by bind expressions
	symbol_tables_delete_by_id(tables, record_def_id);
	traverse_children = false;
    } else {
	LOG_ABORT("Not handled node: %s\n",
		  ast_node_name_to_string(node->name));
    }

    // Traverse children (if any)
    if (traverse_children) {
	uint16_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!type_variable_create(ast_get_child(node, i), tables,
				      block_expr_id, error)) {
		return false;
	    }
	}
    }

    CLEAR_ERROR(error);
    return true;
}

bool type_variable_associate(equations_t* equations,
			     types_t* type_variables_types, type_t* type,
			     satie_error_t* error) {
    uint16_t number_of_type_variables = types_size(type_variables_types);
    return associate(equations, type_variables_types, number_of_type_variables,
		     type, error);
}

//
// Local functions (alphabetical order)
//

static bool associate(equations_t* equations, types_t* type_variables_types,
		      uint16_t number_of_type_variables, type_t* type,
		      satie_error_t* error) {
    if (number_of_type_variables == 0) {
	CLEAR_ERROR(error);
	return true;
    }
    if (type->tag == TYPE_TAG_TYPE_VARIABLE) {
	for (uint16_t i = 0; i < number_of_type_variables; i++) {
	    type_t* type_variable_type = types_get(type_variables_types, i);
	    if (strcmp(type_variable_type->type_variable.name,
		       type->type_variable.name) == 0) {
		// Equation: Type variable
		equation_t equation =
		    equation_new(type_variable_type, type, NULL, NULL, true);
		equations_append(equations, &equation);
		return true;
	    }
	}
	SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
			  "Unknown type variable '%s'\n",
			  type->type_variable.name);
	return false;
    } else if (type->tag == TYPE_TAG_LIST_TYPE) {
	if (!associate(equations, type_variables_types,
		       number_of_type_variables, type->list_type, error)) {
	    return false;
	}
    } else if (type->tag == TYPE_TAG_FUNCTION_TYPE) {
	types_t* generic_types = type->function_type.generic_types;
	uint16_t n = types_size(generic_types);
	for (uint16_t i = 0; i < n; i++) {
	    if (!associate(equations, type_variables_types,
			   number_of_type_variables,
			   types_get(generic_types, i), error)) {
		return false;
	    }
	}
	types_t* arg_types = type->function_type.arg_types;
	n = types_size(arg_types);
	for (uint16_t i = 0; i < n; i++) {
	    if (!associate(equations, type_variables_types,
			   number_of_type_variables, types_get(arg_types, i),
			   error)) {
		return false;
	    }
	}
	type_t* return_type = type->function_type.return_type;
	if (!associate(equations, type_variables_types,
		       number_of_type_variables, return_type, error)) {
	    return false;
	}
    } else if (type->tag == TYPE_TAG_TUPLE_TYPE) {
	uint16_t n = types_size(type->tuple_types);
	for (uint16_t i = 0; i < n; i++) {
	    if (!associate(equations, type_variables_types,
			   number_of_type_variables,
			   types_get(type->tuple_types, i), error)) {
		return false;
	    }
	}
    } else if (type->tag == TYPE_TAG_MAP_TYPE) {
	if (!associate(equations, type_variables_types,
		       number_of_type_variables, type->map_type.key_type,
		       error)) {
	    return false;
	}
	if (!associate(equations, type_variables_types,
		       number_of_type_variables, type->map_type.value_type,
		       error)) {
	    return false;
	}
    } else if (type->tag == TYPE_TAG_CONSTRUCTOR_TYPE) {
	types_t* types = type->constructor_type.types;
	uint16_t n = types_size(types);
	for (uint16_t i = 0; i < n; i++) {
	    if (!associate(equations, type_variables_types,
			   number_of_type_variables, types_get(types, i),
			   error)) {
		return false;
	    }
	}
    }
    CLEAR_ERROR(error);
    return true;
}
