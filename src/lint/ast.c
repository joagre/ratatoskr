#define MUTE_LOG_DEBUG 1

#include "ast.h"

#define MAX_COLS 60

static const char* node_name_strings[] = {
    FOREACH_NODE_NAME(GENERATE_STRING)
};

const char* ast_node_name_to_string(node_name_t name) {
    return node_name_strings[name];
}

ast_node_t* ast_get_child(ast_node_t* node, uint32_t i) {
    ast_node_t* child_node = dynarray_element(node->children, i);
    return child_node;
}

uint16_t ast_number_of_children(ast_node_t* node) {
    if (node->children == NULL) {
	return 0;
    }
    return dynarray_size(node->children);
}

ast_node_t* ast_last_child(ast_node_t* node) {
    return dynarray_element(node->children, dynarray_size(node->children) - 1);
}

void ast_print(ast_node_t* node, uint16_t level) {
    if (node == NULL) {
        return;
    }
    int cols = 0;
    if (level > 0) {
	cols += printf("%*s", 2 * level, "");
    }
    cols += printf("%s:%d", ast_node_name_to_string(node->name), node->row);
    if (node->value != NULL) {
        cols += printf(":%s:", node->value);
    } else {
	cols += printf("::");
    }
    if (node->type != NULL) {
	uint16_t indent;
	if (cols > MAX_COLS) {
	    indent = 4;
	} else {
	    indent = MAX_COLS - cols - 4;
	}
	if (node->type->tag == TYPE_TAG_BASIC_TYPE) {
	    if (node->type->basic_type == TYPE_BASIC_TYPE_BOOL) {
		printf("%*sBool\n", indent, "");
	    } else if (node->type->basic_type == TYPE_BASIC_TYPE_INT) {
		printf("%*sInt\n", indent, "");
	    } else {
		assert(false);
	    }
	} else if (node->type->tag == TYPE_TAG_TYPE_VARIABLE) {
	    printf("%*st%d\n", indent, "", node->type->type_variable);
	} else {
	    assert(false);
	}
    } else {
	printf("\n");
    }
    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
            ast_node_t* child_node = dynarray_element(node->children, i);
            ast_print(child_node, level + 1);
        }
    }
}
