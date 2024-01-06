#include "equations.h"
#include "types.h"

// Forward declarations of local functions (alphabetical order)
static void print_type(type_t* type);

void equations_init(equations_t* equations) {
    dynarray_init(equations, NULL, 0, sizeof(equation_t));
}

void equations_add(equations_t* equations, equation_t* equation) {
    dynarray_append(equations, equation);
}

equation_t* equations_lookup(equations_t* equations, size_t i) {
    return dynarray_element(equations, i);
}

void print_equations(equations_t* equations) {
    for (uint16_t i = 0; i < equations->size; i++) {
	equation_t* equation = equations_lookup(equations, i);
	fprintf(stderr, "%s",
		ast_node_name_to_string(equation->origin_node->name));
	if (equation->node->value != NULL) {
	    fprintf(stderr, " (%s): ", equation->node->value);
	} else {
	    fprintf(stderr, ": ");
	}
	print_type(equation->arg_type);
	fprintf(stderr, " -> ");
	print_type(equation->return_type);
	fprintf(stderr, "\n");
    }
}

//
// Local functions (alphabetical order)
//

static void print_type(type_t* type) {
    switch (type->tag) {
	case TYPE_TAG_BASIC_TYPE:
	    switch (type->basic_type) {
		case TYPE_BASIC_TYPE_INTEGRAL:
		    fprintf(stderr, "integral");
		    break;
		case TYPE_BASIC_TYPE_BOOL:
		    fprintf(stderr, "bool");
		    break;
	    }
	    break;
	case TYPE_TAG_VARIABLE:
	    fprintf(stderr, "t%d", type->variable);
	    break;
	case TYPE_TAG_FUNCTION:
	    fprintf(stderr, "(");
	    if (type->function_type.arg_types->size > 1) {
		fprintf(stderr, "(");
	    }
	    for (size_t i = 0; i < type->function_type.arg_types->size; i++) {
		type_t* arg_type =
		    types_lookup(type->function_type.arg_types, i);
		print_type(arg_type);
		if (i < type->function_type.arg_types->size - 1) {
		    fprintf(stderr, ", ");
		}
	    }
	    if (type->function_type.arg_types->size > 1) {
		fprintf(stderr, ")");
	    }
	    fprintf(stderr, " -> ");
	    print_type(type->function_type.return_type);
	    fprintf(stderr, ")");
	    break;
    }
}
