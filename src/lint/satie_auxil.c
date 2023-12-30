#include "satie_auxil.h"

static const char* type_strings[] = {
    FOREACH_TYPE(GENERATE_STRING)
};

satie_auxil_t* satie_auxil_new() {
    fprintf(stderr, "** satie_auxil_new\n");
    satie_auxil_t* auxil = malloc(sizeof(satie_auxil_t));
    auxil->row = 1;
    return auxil;
}

const char* type_to_string(node_type_t type) {
    return type_strings[type];
}

ast_node_t* new_node(satie_auxil_t* auxil, node_type_t type) {
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = type;
    node->type_variable = 0;
    node->value = NULL;
    node->row = auxil->row;
    node->column = 1;
    node->children = NULL;
    return node;
}

ast_node_t* retype_node(ast_node_t* node, node_type_t type) {
    fprintf(stderr, "** retype_node\n");
    if (node == NULL) {
        fprintf(stderr, "WARNING: An undefined node cannot be retyped to %s\n",
                type_to_string(type));
    } else {
        node->type = type;
    }
    return node;
}

ast_node_t* create_terminal(satie_auxil_t* auxil, node_type_t type,
			    const char* value) {
    fprintf(stderr, "** create_terminal: %s (%s)\n", value,
            type_to_string(type));
    ast_node_t* node = new_node(auxil, type);
    if (value != NULL) {
        node->value = strdup(value);
    } else {
        node->value = NULL;
    }
    return node;
}

ast_node_t* create_node(satie_auxil_t* auxil, node_type_t type,
			uint16_t n, ...) {
    fprintf(stderr, "** create_children_node: %s\n", type_to_string(type));
    ast_node_t* node = new_node(auxil, type);
    node->children = malloc(sizeof(node_array_t));
    dynarray_init(node->children, NULL, 0, sizeof(ast_node_t));
    va_list args;
    va_start(args, n);
    for (uint16_t i = 0; i < n; i++) {
        ast_node_t* child_node = va_arg(args, ast_node_t*);
        if (child_node != NULL) {
            if (child_node->type == POSTFIX_EXPR &&
                dynarray_size(child_node->children) == 1) {
                ast_node_t* grand_child_node =
                    dynarray_element(child_node->children, 0);
                free(child_node);
                child_node = grand_child_node;
            }
            dynarray_append(node->children, child_node);
        } else {
            fprintf(stderr,
                    "WARNING: An undefined child node %d is ignored by %s\n",
                    i, type_to_string(type));
        }
    }
    /*
    if (dynarray_size(node->children) == 0) {
        free(node->children);
        return NULL;
    }
    */
    return node;
}

void add_child(satie_auxil_t* auxil, ast_node_t* parent_node, ast_node_t* node) {
    fprintf(stderr, "** add_child\n");
    if (node == NULL) {
        fprintf(stderr, "WARNING: An undefined node cannot be appended to %s\n",
                type_to_string(parent_node->type));
    } else {
        fprintf(stderr, "** add_child1\n");
        if (node->type == POSTFIX_EXPR &&
            dynarray_size(node->children) == 1) {
            fprintf(stderr, "** add_child2\n");
            ast_node_t* child_node = dynarray_element(node->children, 0);
            fprintf(stderr, "** add_child3\n");
            free(node);
            fprintf(stderr, "** add_child4\n");
            node = child_node;
        }
        dynarray_append(parent_node->children, node);
    }
}

#define MAX_COLS 80

void print_ast(ast_node_t* node, uint16_t level) {
    if (node == NULL) {
        printf("Tree: NULL\n");
        return;
    }
    int cols = 0;
    if (level > 0) {
	printf("%*s", 2 * level, "");
	cols += 2 * level;
    }
    printf("%s", type_to_string(node->type));
    cols += strlen(type_to_string(node->type));
    if (node->value != NULL) {
        printf(": %s", node->value);
	cols += strlen(node->value) + 2;
    }
    if (node->type_variable != 0) {
	if (cols > MAX_COLS) {
	    printf("    t%d\n", node->type_variable);
	} else {
	    printf("%*st%d\n", MAX_COLS - cols - 4, "", node->type_variable);
	}
    } else {
	printf("\n");
    }
    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
            ast_node_t* child_node = dynarray_element(node->children, i);
            print_ast(child_node, level + 1);
        }
    }
}
