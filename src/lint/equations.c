#include "equations.h"
#include "types.h"

#define MAX_COLS 30

void equations_init(equations_t* equations) {
    dynarray_init(equations, NULL, 0, sizeof(equation_t));
}

void equations_add(equations_t* equations, equation_t* equation) {
    dynarray_append(equations, equation);
}

equation_t* equations_lookup(equations_t* equations, size_t i) {
    return dynarray_element(equations, i);
}

void equations_print(equations_t* equations) {
    for (uint16_t i = 0; i < equations->size; i++) {
	equation_t* equation = equations_lookup(equations, i);
	uint16_t n = 0;
	n += printf("%s:%d:%d",
		    ast_node_name_to_string(equation->origin_node->name),
		    equation->origin_node->row,
		    equation->node->row);
	if (equation->node->value != NULL) {
	    n += printf(":%s:", equation->node->value);
	} else {
	    n += printf("::");
	}
	printf("%*s", MAX_COLS - n, "");
	printf("{");
	type_print_type(equation->left);
	printf(", ");
	type_print_type(equation->right);
	printf("}\n");
    }
}
