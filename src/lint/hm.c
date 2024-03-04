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
static void forward_declare_function_definitions(ast_node_t* node,
						 symbol_tables_t* tables);
static bool create_type_variables(ast_node_t* node, symbol_tables_t* tables,
				  uint32_t block_expr_id, satie_error_t* error);
static void create_type_equations(ast_node_t* node, symbol_tables_t* tables,
				  equations_t* equations);
static operator_types_t get_operator_types(node_name_t name);
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
    // Create type variables
    forward_declare_function_definitions(node, &tables);
    if (!create_type_variables(node, &tables, unique_id(), error)) {
	return false;
    }
    // Print parse tree
    ast_print(node, 0);
    printf("\n");
    // Create type equations
    equations_t equations;
    equations_init(&equations);
    create_type_equations(node, &tables, &equations);
    // Print equations
    equations_print(&equations);
    return true;
}

//
// Local functions
//

static bool is_valid(ast_node_t* parent_node, ast_node_t* node,
		     satie_error_t* error) {
    // Check that the := operator only is allowed as a top-level expression
    if (node->name == BIND &&
	parent_node != NULL && parent_node->name != BLOCK_EXPR) {
	SET_ERROR_MESSAGE(
	    error, COMPONENT_COMPILER,
	    "%d: The := operator is only allowed as a top-level expression",
	    node->row);
	return false;
    }

    // Traverse children (if any)
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

static void forward_declare_function_definitions(ast_node_t* node,
						 symbol_tables_t* tables) {
    ast_node_t* top_level_defs_node = ast_get_child(node, 0);
    size_t n = ast_number_of_children(top_level_defs_node);
    for (uint16_t i = 0; i < n; i++) {
	ast_node_t* child_node = ast_get_child(top_level_defs_node, i);
	if (child_node->name == FUNCTION_DEF) {
	    ast_node_t* function_name_node = ast_get_child(child_node, 0);
	    if (function_name_node->name == EXPORT) {
		function_name_node = ast_get_child(child_node, 1);
	    }
	    LOG_ASSERT(function_name_node->name == FUNCTION_NAME,
		       "Expected a FUNCTION_NAME node");
	    type_t* type = type_new_type_variable();
	    function_name_node->type = type;
	    symbol_tables_insert(tables, function_name_node->value, type);
	}
    }
}

static bool create_type_variables(ast_node_t* node, symbol_tables_t* tables,
				  uint32_t block_expr_id, satie_error_t* error) {
    bool traverse_children = true;
    if (node->name == ARGS ||
	node->name == ARG_TYPES ||
	node->name == ELSE ||
	node->name == EXPORT ||
	node->name == DEFAULT_PARAM ||
	node->name == DEFAULT_PARAMS ||
	node->name == EQ_TYPE ||
	node->name == IF ||
	node->name == NON_DEFAULT_PARAMS ||
	node->name == POSITIONAL_ARGS ||
	node->name == PARAMS ||
	node->name == PROGRAM ||
	node->name == TOP_LEVEL_DEFS ||
	node->name == TYPE) {
	// Do not assign a type variable
    } else if (node->name == ADD_INT ||
	       node->name == ADD_FLOAT ||
     	       node->name == AND ||
	       node->name == BITWISE_AND ||
	       node->name == BITWISE_OR ||
	       node->name == BOOL_TYPE ||
	       node->name == BSL ||
	       node->name == BSR ||
	       node->name == CHAR ||
	       node->name == CHAR_TYPE ||
	       node->name == CONS ||
	       node->name == CONCAT_LIST ||
	       node->name == CONCAT_MAP ||
	       node->name == CONCAT_STRING ||
	       node->name == DIV_INT ||
	       node->name == DIV_FLOAT ||
	       node->name == EMPTY_LIST_TYPE ||
	       node->name == EMPTY_MAP_TYPE ||
	       node->name == EMPTY_TUPLE_TYPE ||
	       node->name == EQ ||
	       node->name == ESCAPE_CHAR ||
	       node->name == EXP ||
	       node->name == FALSE ||
	       node->name == FLOAT ||
	       node->name == FLOAT_TYPE ||
	       node->name == FUNCTION_CALL ||
	       node->name == GTE_INT ||
	       node->name == GT_INT ||
	       node->name == GTE_FLOAT ||
	       node->name == GT_INT ||
	       node->name == GT_FLOAT ||
	       node->name == IF_EXPR ||
	       node->name == IN ||
	       node->name == INT ||
	       node->name == INT_TYPE ||
	       node->name == FUNCTION_TYPE ||
	       node->name == LIST_LITERAL ||
	       node->name == LIST_LOOKUP ||
	       node->name == LIST_TYPE ||
	       node->name == LTE_INT ||
	       node->name == LTE_FLOAT ||
	       node->name == LT_INT ||
	       node->name == LT_FLOAT ||
	       node->name == MAP_KEY_VALUE ||
	       node->name == MAP_LITERAL ||
	       node->name == MAP_TYPE ||
	       node->name == NE ||
	       node->name == NEG_INT ||
	       node->name == NEG_FLOAT ||
	       node->name == MUL_INT ||
	       node->name == MUL_FLOAT ||
	       node->name == NOT ||
	       node->name == MOD ||
	       node->name == NE ||
	       node->name == NON_QUOTE_CHAR ||
	       node->name == OR ||
	       node->name == POS_INT ||
	       node->name == POS_FLOAT ||
	       node->name == POSTFIX_EXPR ||
	       node->name == RAW_STRING ||
	       node->name == REGULAR_STRING ||
	       node->name == STRING_TYPE ||
	       node->name == SUB_INT ||
	       node->name == SUB_FLOAT ||
	       node->name == TASK_TYPE ||
	       node->name == TRUE ||
	       node->name == TUPLE_LITERAL ||
	       node->name == TUPLE_TYPE) {
	// Assign a type variable
	type_t* type = type_new_type_variable();
	node->type = type;
    } else if (node->name == NAME) {
	// Names should already be in a symbol table or else it is an error
	node->type = symbol_tables_lookup(tables, node->value);
	if (node->type == NULL) {
	    SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
			      "%d: '%s' has not been bound to a value",
			      node->row, node->value);
	    return false;
	}
    } else if (node->name == PARAM_NAME ||
	       node->name == UNBOUND_NAME ||
	       node->name == FUNCTION_NAME) {
	if (node->type == NULL) {
	    // Assign a type variable
	    type_t* type = type_new_type_variable();
	    node->type = type;
	    // Add the type variable to the symbol table
	    symbol_tables_insert(tables, node->value, type);
	}
    } else if (node->name == FUNCTION_DEF) {
	uint32_t block_expr_id = unique_id();
	symbol_table_t* table = symbol_table_new();
	symbol_tables_insert_table(tables, table, block_expr_id);
	// Add type variables to the function definition children nodes
	size_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!create_type_variables(ast_get_child(node, i), tables,
				       block_expr_id, error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	// Remove the symbol table created above and all nested symbol
	// tables created by bind expressions
	symbol_tables_delete_by_id(tables, block_expr_id);
	traverse_children = false;
    } else if (node->name == FUNCTION_LITERAL) {
	// Assign a type variable
	type_t* type = type_new_type_variable();
	node->type = type;
	// Create a new symbol table
	uint32_t block_expr_id = unique_id();
	symbol_table_t* table = symbol_table_new();
	symbol_tables_insert_table(tables, table, block_expr_id);
	// Add type variables to the function definition children nodes
	size_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!create_type_variables(ast_get_child(node, i), tables,
				       block_expr_id, error)) {
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
	symbol_tables_delete_by_id(tables, block_expr_id);
	traverse_children = false;
    } else if (node->name == BIND) {
	// Assign a type variable
	type_t* type = type_new_type_variable();
	node->type = type;
	// Take care of the right node first
	ast_node_t* right_node = ast_get_child(node, 1);
	if (!create_type_variables(right_node, tables, block_expr_id, error)) {
	    CLEAR_ERROR(error);
	    return false;
	}
        // Create a new symbol table
	symbol_table_t* table = symbol_table_new();
	block_expr_id = unique_id();
	symbol_tables_insert_table(tables, table, block_expr_id);
	// Take care of the left node
	ast_node_t* left_node = ast_get_child(node, 0);
	if (!create_type_variables(left_node, tables, block_expr_id, error)) {
	    CLEAR_ERROR(error);
	    return false;
	}
	traverse_children = false;
    } else if(node->name == BLOCK_EXPR) {
	// Assign a type variable
	type_t* type = type_new_type_variable();
	node->type = type;
	// Create a new symbol table
	symbol_table_t* table = symbol_table_new();
	block_expr_id = unique_id();
	symbol_tables_insert_table(tables, table, block_expr_id);
	// Add type variables to the block expression children nodes
	size_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    if (!create_type_variables(ast_get_child(node, i), tables,
				       block_expr_id, error)) {
		CLEAR_ERROR(error);
		return false;
	    }
	}
	// Remove the symbol table created above and all sub symbol
	// tables created by bind expressions
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
	    if (!create_type_variables(ast_get_child(node, i), tables,
				       block_expr_id, error)) {
		return false;
	    }
	}
    }

    CLEAR_ERROR(error);
    return true;
}

static void create_type_equations(ast_node_t *node, symbol_tables_t* tables,
				  equations_t* equations) {
    // Traverse children first (if any)
    size_t n = ast_number_of_children(node);
    for (uint16_t i = 0; i < n; i++) {
	create_type_equations(ast_get_child(node, i), tables, equations);
    }

    if (node->name == ARGS ||
	node->name == ARG_TYPES ||
	node->name == BOOL_TYPE ||
	node->name == CHAR_TYPE ||
	node->name == ELSE ||
	node->name == ESCAPE_CHAR ||
	node->name == EQ_TYPE ||
	node->name == EXPORT ||
	node->name == FLOAT_TYPE ||
	node->name == FUNCTION_CALL ||
	node->name == FUNCTION_NAME ||
	node->name == FUNCTION_TYPE ||
	node->name == IF ||
	node->name == INT_TYPE ||
	node->name == LIST_LOOKUP ||
	node->name == MAP_KEY_VALUE ||
	node->name == NON_DEFAULT_PARAMS ||
	node->name == NON_QUOTE_CHAR ||
	node->name == NAME ||
	node->name == PARAM_NAME ||
	node->name == POSITIONAL_ARGS ||
	node->name == PROGRAM ||
	node->name == STRING_TYPE ||
	node->name == TASK_TYPE ||
	node->name == TOP_LEVEL_DEFS ||
	node->name == TUPLE_TYPE) {
	// Do not create an equation
    } else if (node->name == TYPE) {
	// Equation: Type
	ast_node_t* actual_type_node = ast_get_child(node, 0);
	type_t* type = extract_type(actual_type_node);
	equation_t equation =
	    equation_new(actual_type_node->type, type, node, actual_type_node,
			 true);
	equations_add(equations, &equation);
    } else if (node->name == UNBOUND_NAME) {
	// Equation: Unbound name (if any)
	ast_node_t* type_node = ast_get_child(node, 0);
	if (type_node != NULL) {
	    ast_node_t* actual_type_node = ast_get_child(type_node, 0);
	    equation_t equation =
		equation_new(node->type, actual_type_node->type, node,
			     actual_type_node, false);
	    equations_add(equations, &equation);
	}
    } else if (node->name == FALSE || node->name == TRUE) {
	// Equation: Bool constant
	equation_t equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == INT) {
	// Equation: Int constant
	equation_t equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_INT),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == FLOAT) {
	// Equation: Float constant
	equation_t equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_FLOAT),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == CHAR) {
	// Equation: Char constant
	equation_t equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_CHAR),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == RAW_STRING) {
	// Equation: String constant
	equation_t equation =
	    equation_new(node->type,
			 type_new_basic_type(TYPE_BASIC_TYPE_STRING),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == REGULAR_STRING) {
	// Equation: String constant
	equation_t equation =
	    equation_new(node->type,
			 type_new_basic_type(TYPE_BASIC_TYPE_STRING),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == TASK) {
	// Equation: Task constant
	equation_t equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_TASK),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == CONSTRUCTOR_TYPE) {
	LOG_ABORT("Not implemented yet\n");
    } else if (node->name == FUNCTION_TYPE) {
	LOG_ABORT("Not implemented yet\n");
    } else if (node->name == LIST_TYPE) {
	// Equation: List
	ast_node_t* list_type_node = ast_get_child(node, 0);
	equation_t equation =
	    equation_new(node->type, type_new_list_type(list_type_node->type),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == EMPTY_LIST_TYPE) {
	// Equation: Empty list type
	equation_t equation =
	    equation_new(node->type, type_new_empty_list_type(), node, node,
			 false);
	equations_add(equations, &equation);
    } else if (node->name == MAP_TYPE) {
	// Equation: Map
	ast_node_t* key_type_node = ast_get_child(node, 0);
	ast_node_t* value_type_node = ast_get_child(node, 1);
	equation_t equation =
	    equation_new(node->type, type_new_map_type(key_type_node->type,
						       value_type_node->type),
			 node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == EMPTY_MAP_TYPE) {
	// Equation: Empty map type
	equation_t equation =
	    equation_new(node->type, type_new_empty_map_type(), node, node,
			 false);
	equations_add(equations, &equation);
    } else if (node->name == TUPLE_TYPE) {
	// Equation: Tuple
	types_t* types = types_new();
	size_t n = ast_number_of_children(node);
	for (uint16_t i = 0; i < n; i++) {
	    ast_node_t* type_node = ast_get_child(node, i);
	    types_add(types, type_node->type);
	}
	type_t* type = type_new_tuple_type(types);
	equation_t equation = equation_new(node->type, type, node, node, false);
	equations_add(equations, &equation);
    } else if (node->name == EMPTY_TUPLE_TYPE) {
	// Equation: Empty tuple type
	equation_t equation =
	    equation_new(node->type, type_new_empty_tuple_type(), node, node,
			 false);
	equations_add(equations, &equation);
    } else if (node->name == TYPE_VARIABLE) {
	LOG_ABORT("Not implemented yet\n");
    } else if (node->name == FUNCTION_LITERAL) {
        // Extract all nodes constituting the function literal
	uint16_t i = 0;
	ast_node_t* child_node = ast_get_child(node, i);
	if (child_node->name == FUNCTION_NAME) {
	    child_node = ast_get_child(node, ++i);
	}
	ast_node_t* non_default_params_node = NULL;
	ast_node_t* return_type_node = NULL;
	ast_node_t* block_expr_node;
	if (child_node->name == BLOCK_EXPR) {
	    block_expr_node = child_node;
	} else if (child_node->name == NON_DEFAULT_PARAMS) {
	    non_default_params_node = child_node;
	    child_node = ast_get_child(node, i + 1);
	    if (child_node->name == TYPE) {
		return_type_node = child_node;
		block_expr_node = ast_get_child(node, i + 2);
	    } else {
		block_expr_node = child_node;
	    }
	} else if (child_node->name == TYPE) {
	    return_type_node = child_node;
	    block_expr_node = ast_get_child(node, i + 1);
	} else {
	    block_expr_node = child_node;
	}
	// Extract all parameter types
	types_t* param_types = types_new();
	if (non_default_params_node != NULL) {
	    size_t n = ast_number_of_children(non_default_params_node);
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* param_node =
		    ast_get_child(non_default_params_node, i);
		LOG_ASSERT(param_node->name == PARAM_NAME,
			   "Expected a PARAM_NAME node");
		// Has a type been specified?
		if (ast_number_of_children(param_node) > 0) {
		    ast_node_t* param_type_node = ast_get_child(param_node, 0);
		    LOG_ASSERT(param_type_node->name == TYPE,
			       "Expected a TYPE node");
		    ast_node_t* actual_type_node =
			ast_get_child(param_type_node, 0);
                    // Equation: Parameter
		    equation_t param_type_equation =
			equation_new(param_node->type, actual_type_node->type,
				     node, param_node, true);
		    equations_add(equations, &param_type_equation);
		}
		types_add(param_types, param_node->type);
	    }
	}
	// Equation: Function
	equation_t function_equation =
	    equation_new(node->type,
			 type_new_function_type(param_types,
						block_expr_node->type),
			 node, node, false);
	equations_add(equations, &function_equation);
	// Has a return type been specified?
	if (return_type_node != NULL) {
	    // Equation: Return type
	    ast_node_t* type_node = ast_get_child(return_type_node, 0);
	    equation_t return_type_equation =
		equation_new(block_expr_node->type, type_node->type, node,
			     return_type_node, true);
	    equations_add(equations, &return_type_equation);
	}
    } else if (node->name == LIST_LITERAL) {
	size_t n = ast_number_of_children(node);
	if (n == 0) {
	    // Equation: Empty list literal
	    equation_t equation = equation_new(
		node->type, type_new_empty_list_type(), node, node, false);
	    equations_add(equations, &equation);
	} else {
	    // Extract first list element
	    ast_node_t* first_element_node = ast_get_child(node, 0);
	    // Equation: List literal
	    type_t* type = type_new_list_type(first_element_node->type);
	    equation_t equation = equation_new(node->type, type, node,
					       first_element_node, false);
	    equations_add(equations, &equation);
	    // Add equations for all list elements
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* element_node = ast_get_child(node, i);
		// Equation: List element
		equation_t equation =
		    equation_new(element_node->type, first_element_node->type,
				 node, element_node, false);
		equations_add(equations, &equation);
	    }
	}
    } else if (node->name == MAP_LITERAL) {
	size_t n = ast_number_of_children(node);
	if (n == 0) {
	    // Equation: Empty map literal
	    equation_t equation = equation_new(
		node->type, type_new_empty_map_type(), node, node, false);
	    equations_add(equations, &equation);
	} else {
	    // Extract first map key-value pair
	    ast_node_t* first_map_key_value_node = ast_get_child(node, 0);
	    LOG_ASSERT(first_map_key_value_node->name == MAP_KEY_VALUE,
		       "Expected a MAP_KEY_VALUE node");
	    ast_node_t* first_key_node =
		ast_get_child(first_map_key_value_node, 0);
	    ast_node_t* first_value_node =
		ast_get_child(first_map_key_value_node, 1);
	    // Equation: Map literal
	    type_t* type = type_new_map_type(
		first_key_node->type, first_value_node->type);
	    equation_t equation = equation_new(node->type, type, node,
					       first_map_key_value_node, false);
	    equations_add(equations, &equation);
	    // Add equations for all key-value pairs
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* map_key_value_node = ast_get_child(node, i);
		ast_node_t* key_node = ast_get_child(map_key_value_node, 0);
		ast_node_t* value_node = ast_get_child(map_key_value_node, 1);
		// Equation: Map key
		equation_t equation =
		    equation_new(key_node->type, first_key_node->type, node,
				 key_node, false);
		equations_add(equations, &equation);
		// Equation: Map value
		equation = equation_new(
		    value_node->type, first_value_node->type,
		    node, value_node, false);
		equations_add(equations, &equation);
	    }
	}
    } else if (node->name == TUPLE_LITERAL) {
	// Equation: Tuple literal
	size_t n = ast_number_of_children(node);
	if (n == 0) {
	    // Equation: Empty tuple literal
	    equation_t equation = equation_new(
		node->type, type_new_empty_tuple_type(), node, node, false);
	    equations_add(equations, &equation);
	} else {
	    types_t* types = types_new();
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* element_node = ast_get_child(node, i);
		types_add(types, element_node->type);
	    }
	    type_t* type = type_new_tuple_type(types);
	    equation_t equation = equation_new(node->type, type, node, node,
					       false);
	    equations_add(equations, &equation);
	}
    } else if (node->name == EQ || node->name == NE) {
	// Equation: Equal or Not Equal
	equation_t operator_equation =
	    equation_new(node->type, type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
			 node, node, false);
	equations_add(equations, &operator_equation);
        // Extract the type
	ast_node_t* type_node = ast_get_child(node, 1);
	type_t* type;
	if (type_node->name == EQ_TYPE) {
	    LOG_ABORT("Not implemented yet");
	} else {
	    LOG_ASSERT(type_node->name == INT_TYPE,
		       "Expected a INT_TYPE node");
	    type = type_node->type;
	}
        // Equation: Left operand
	ast_node_t* left_node = ast_get_child(node, 0);
	equation_t left_equation =
	    equation_new(left_node->type, type, node, left_node, false);
	equations_add(equations, &left_equation);
	// Equation: Right operand
	ast_node_t* right_node = ast_get_child(node, 2);
	equation_t right_equation =
	    equation_new(right_node->type, type, node, right_node, false);
	equations_add(equations, &right_equation);
    } else if (node->name == NEG_FLOAT ||
	       node->name == NEG_INT ||
	       node->name == NOT ||
	       node->name == POS_FLOAT ||
	       node->name == POS_INT) {
	operator_types_t types = get_operator_types(node->name);
	// Equation: Operator
	equation_t operator_equation =
	    equation_new(node->type, types.return_type, node, node, false);
	equations_add(equations, &operator_equation);
	// Equation: Operand
	ast_node_t* operand_node = ast_get_child(node, 0);
	equation_t operand_equation =
	    equation_new(operand_node->type, types.operand_type, node,
			 operand_node, false);
	equations_add(equations, &operand_equation);
    } else if (node->name == CONCAT_LIST) {
	ast_node_t* left_node = ast_get_child(node, 0);
	ast_node_t* right_node = ast_get_child(node, 1);
	// Equation: Operator
	equation_t operator_equation =
	    equation_new(node->type, left_node->type, node, left_node, false);
	equations_add(equations, &operator_equation);
	// Equation: Operands
	equation_t operands_equation =
	    equation_new(left_node->type, right_node->type, node, right_node,
			 false);
	equations_add(equations, &operands_equation);
    } else if (node->name == ADD_FLOAT ||
	       node->name == ADD_INT ||
	       node->name == AND ||
	       node->name == BITWISE_AND ||
	       node->name == BITWISE_OR ||
	       node->name == BSL ||
	       node->name == BSR ||
	       node->name == CONCAT_MAP ||
	       node->name == CONCAT_STRING ||
	       node->name == CONS ||
	       node->name == DIV_FLOAT ||
	       node->name == DIV_INT ||
	       node->name == EXP ||
	       node->name == GTE_FLOAT ||
	       node->name == GTE_INT ||
	       node->name == GT_FLOAT ||
	       node->name == GT_INT ||
	       node->name == LTE_FLOAT ||
	       node->name == LTE_INT ||
	       node->name == LT_FLOAT ||
	       node->name == LT_INT ||
	       node->name == MOD ||
	       node->name == MUL_FLOAT ||
	       node->name == MUL_INT ||
	       node->name == OR ||
	       node->name == SUB_INT ||
	       node->name == SUB_FLOAT) {
	operator_types_t types = get_operator_types(node->name);
	// Equation: Operator
	equation_t operator_equation =
	    equation_new(node->type, types.return_type, node, node, false);
	equations_add(equations, &operator_equation);
	// Equation: Left operand
	ast_node_t* left_node = ast_get_child(node, 0);
	equation_t left_equation =
	    equation_new(left_node->type, types.operand_type, node, left_node,
			 false);
	equations_add(equations, &left_equation);
	// Equation: Right operand
	ast_node_t* right_node = ast_get_child(node, 1);
	equation_t right_equation =
	    equation_new(right_node->type, types.operand_type, node,
			 right_node, false);
	equations_add(equations, &right_equation);
    } else if (node->name == CONS) {
	ast_node_t* left_node = ast_get_child(node, 0);
	// Equation: Operator
	equation_t operator_equation =
	    equation_new(node->type, type_new_list_type(left_node->type),
			 node, node, false);
	equations_add(equations, &operator_equation);
	// Equation: Right operand
	ast_node_t* right_node = ast_get_child(node, 1);
	equation_t right_equation =
	    equation_new(right_node->type, type_new_list_type(left_node->type),
			 node, right_node, false);
	equations_add(equations, &right_equation);
    } else if (node->name == IN) {
	ast_node_t* left_node = ast_get_child(node, 0);
	ast_node_t* right_node = ast_get_child(node, 1);
        // Equation: Operator
	equation_t operator_equation =
	    equation_new(right_node->type,
			 type_new_map_type(left_node->type, node->type),
			 node, node, false);
	equations_add(equations, &operator_equation);
    } else if (node->name == IF_EXPR) {
	// Extract all nodes constituting the if expression
	ast_node_t* if_node = ast_get_child(node, 0);
	LOG_ASSERT(if_node->name == IF, "Expected an IF node");
	ast_node_t* if_conditional_node = ast_get_child(if_node, 0);
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
	uint16_t i = 0;
	ast_node_t* child_node = ast_get_child(node, i);
	ast_node_t* function_name_node = NULL;
	if (child_node->name == EXPORT) {
	    function_name_node = ast_get_child(node, ++i);
	} else {
	    function_name_node = child_node;
	}
	LOG_ASSERT(function_name_node->name == FUNCTION_NAME,
		   "Expected a FUNCTION_NAME node");
	child_node = ast_get_child(node, i + 1);
	ast_node_t* non_default_params_node = NULL;
	ast_node_t* return_type_node = NULL;
	ast_node_t* block_expr_node;
	if (child_node->name == BLOCK_EXPR) {
	    block_expr_node = child_node;
	} else if (child_node->name == NON_DEFAULT_PARAMS) {
	    non_default_params_node = child_node;
	    child_node = ast_get_child(node, i + 2);
	    if (child_node->name == TYPE) {
		return_type_node = child_node;
		block_expr_node = ast_get_child(node, i + 3);
	    } else {
		block_expr_node = child_node;
	    }
	} else if (child_node->name == TYPE) {
	    return_type_node = child_node;
	    block_expr_node = ast_get_child(node, i + 2);
	} else {
	    block_expr_node = child_node;
	}
	// Extract all parameter types
	types_t* param_types = types_new();
	if (non_default_params_node != NULL) {
	    size_t n = ast_number_of_children(non_default_params_node);
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* param_node =
		    ast_get_child(non_default_params_node, i);
		LOG_ASSERT(param_node->name == PARAM_NAME,
			   "Expected a PARAM_NAME node");
		// Has a type been specified?
		if (ast_number_of_children(param_node) > 0) {
		    ast_node_t* param_type_node = ast_get_child(param_node, 0);
		    LOG_ASSERT(param_type_node->name == TYPE,
			       "Expected a TYPE node");
		    ast_node_t* actual_type_node =
			ast_get_child(param_type_node, 0);
                    // Equation: Parameter
		    equation_t param_type_equation =
			equation_new(param_node->type, actual_type_node->type,
				     node, param_node, true);
		    equations_add(equations, &param_type_equation);
		}
		types_add(param_types, param_node->type);
	    }
	}
	// Equation: Function
	equation_t function_equation =
	    equation_new(function_name_node->type,
			 type_new_function_type(param_types,
						block_expr_node->type),
			 node, node, false);
	equations_add(equations, &function_equation);
	// Has a return type been specified?
	if (return_type_node != NULL) {
	    // Equation: Return type
	    ast_node_t* type_node = ast_get_child(return_type_node, 0);
	    equation_t return_type_equation =
		equation_new(block_expr_node->type, type_node->type, node,
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
	// Equation: Bind
	LOG_ASSERT(right_node->type != NULL, "Expected a type");
	equation_t bind_equation =
	    equation_new(node->type, right_node->type, node, node, false);
	equations_add(equations, &bind_equation);
    } else if (node->name == POSTFIX_EXPR) {
	/*

    POSTFIX_EXPR:12::
          NAME:12:l3:
          LIST_LOOKUP:12::
            INT:12:0:
          LIST_LOOKUP:12::
            INT:12:1:
          FUNCTION_CALL:12::
            INT:12:1:
            ARGS:12::
              NAME:12:x:
	*/
        /*
	 * NOTE: This POSTFIX_EXPR is hardwired to only handle function calls
	 */
	// Extract all nodes constituting the function call
	ast_node_t* name_node = ast_get_child(node, 0);
	LOG_ASSERT(name_node->name == NAME, "Expected a NAME node");
	ast_node_t* function_call_node = ast_get_child(node, 1);
	LOG_ASSERT(function_call_node->name == FUNCTION_CALL,
		   "Expected a FUNCTION_CALL node");
	ast_node_t* args_node = ast_get_child(function_call_node, 0);
	// Extract all argument types
	types_t* arg_types = types_new();
	if (args_node != NULL) {
	    LOG_ASSERT(args_node->name == ARGS,
		       "Expected an ARGS node");
	    size_t n = ast_number_of_children(args_node);
	    for (uint16_t i = 0; i < n; i++) {
		ast_node_t* arg_node = ast_get_child(args_node, i);
		types_add(arg_types, arg_node->type);
	    }
	}
        // Equation: Function call
	equation_t function_equation =
	    equation_new(name_node->type,
			 type_new_function_type(arg_types,
						function_call_node->type),
			 node, node, false);
	equations_add(equations, &function_equation);
        // Equation: Postfix expression
	equation_t postfix_expr_equation =
	    equation_new(node->type, function_call_node->type, node, node,
			 false);
	equations_add(equations, &postfix_expr_equation);
    } else if (node->name == BLOCK_EXPR) {
	// Equation: Block expression
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
}

static type_t* extract_type(ast_node_t* type_node) {
    switch (type_node->name) {
	case BOOL_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_BOOL);
	case INT_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_INT);
	case FLOAT_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_FLOAT);
	case CHAR_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_CHAR);
	case STRING_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_STRING);
	case TASK_TYPE:
	    return type_new_basic_type(TYPE_BASIC_TYPE_TASK);
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
	case LIST_TYPE:
	    return type_new_list_type(
		extract_type(ast_get_child(type_node, 0)));
	case EMPTY_LIST_TYPE:
	    return type_new_empty_list_type();
	case MAP_TYPE: {
	    ast_node_t* key_type_node = ast_get_child(type_node, 0);
	    type_t* key_type = extract_type(key_type_node);
	    ast_node_t* value_type_node = ast_get_child(type_node, 1);
	    type_t* value_type = extract_type(value_type_node);
	    return type_new_map_type(key_type, value_type);
	}
	case EMPTY_MAP_TYPE:
	    return type_new_empty_map_type();
	case TUPLE_TYPE: {
	    types_t* tuple_types = types_new();
	    for (uint16_t i = 0; i < ast_number_of_children(type_node); i++) {
		ast_node_t* tuple_type_node = ast_get_child(type_node, i);
		type_t* tuple_type = extract_type(tuple_type_node);
		types_add(tuple_types, tuple_type);
	    }
	    return type_new_tuple_type(tuple_types);
	}
	case EMPTY_TUPLE_TYPE:
	    return type_new_empty_tuple_type();
	case TYPE_VARIABLE:
	    return type_new_type_variable();
	default:
	    LOG_ABORT("Unknown type node: %s",
		      ast_node_name_to_string(type_node->name));
    }
    assert(false);
}

static operator_types_t get_operator_types(node_name_t name) {
    switch (name) {
	case ADD_FLOAT:
	case EXP:
	case DIV_FLOAT:
	case SUB_FLOAT:
	case MUL_FLOAT:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_FLOAT),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_FLOAT)
	    };
	case ADD_INT:
	case DIV_INT:
	case MOD:
	case MUL_INT:
	case SUB_INT:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_INT),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_INT)
	    };
	case AND:
	case OR:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL)
	    };
	case BITWISE_AND:
	case BITWISE_OR:
	case BSL:
	case BSR:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_INT),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL)
	    };
	case CONCAT_MAP:
	    // FIXME: Add as own
	    break;
	case CONCAT_STRING:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_STRING),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_STRING)
	    };
	case GT_FLOAT:
	case GTE_FLOAT:
	case LT_FLOAT:
	case LTE_FLOAT:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_FLOAT),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL)
	    };
	case GT_INT:
	case GTE_INT:
	case LT_INT:
	case LTE_INT:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_INT),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL)
	    };
	case NEG_FLOAT:
	case POS_FLOAT:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_FLOAT),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_FLOAT)
	    };
	case NEG_INT:
	case POS_INT:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_INT),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_INT)
	    };
	case NOT:
	    return (operator_types_t) {
		.operand_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL),
		.return_type = type_new_basic_type(TYPE_BASIC_TYPE_BOOL)
	    };
	default:
	    assert(false);
    }
    assert(false);
}

static uint32_t unique_id(void) {
    static uint32_t id = 0;
    return id++;
}
