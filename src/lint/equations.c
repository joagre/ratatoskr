#include <log.h>

#include "equations.h"
#include "types.h"

#define MAX_COLS 40

void equations_init(equations_t* equations) {
    dynarray_init(equations, NULL, 0, sizeof(equation_t));
}

void equations_append(equations_t* equations, equation_t* equation) {
    dynarray_append(equations, equation);
}

equation_t* equations_get(equations_t* equations, uint16_t i) {
    return dynarray_element(equations, i);
}

void equations_print(equations_t* equations) {
    for (uint16_t i = 0; i < equations->size; i++) {
	equation_t* equation = equations_get(equations, i);
	uint16_t n = 0;
	if (equation->origin_node == NULL || equation->node == NULL) {
	    n += printf("GENERATED_TYPE:0:0:%d",  equation->user_defined);
	} else {
	    n += printf("%s:%d:%d:%d",
			ast_node_name_to_string(equation->origin_node->name),
			equation->origin_node->row,
			equation->node->row,
			equation->user_defined);
	}
	if (equation->node != NULL && equation->node->value != NULL) {
	    n += printf(":%s:", equation->node->value);
	} else {
	    n += printf("::");
	}
	printf("%*s", MAX_COLS - n, "");
	printf("{");
	fflush(stdout);
	type_print_type(equation->left);
	printf(", ");
	fflush(stdout);
	LOG_ASSERT(equation->right != NULL, "equation->right is NULL");
	type_print_type(equation->right);
	printf("}\n");
    }
}
