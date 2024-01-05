#define MUTE_LOG_DEBUG 1

#include "ast.h"

#define MAX_COLS 80

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

void ast_print(ast_node_t* node, uint16_t level) {
    if (node == NULL) {
        printf("Tree: NULL\n");
        return;
    }
    int cols = 0;
    if (level > 0) {
	printf("%*s", 2 * level, "");
	cols += 2 * level;
    }
    printf("%s", ast_node_name_to_string(node->name));
    cols += strlen(ast_node_name_to_string(node->name));
    if (node->value != NULL) {
        printf(": %s", node->value);
	cols += strlen(node->value) + 2;
    }
    if (node->type != NULL) {
	uint16_t indent;
	if (cols > MAX_COLS) {
	    indent = 4;
	} else {
	    indent = MAX_COLS - cols - 4;
	}
	if (node->type->tag == HM_TYPE_TAG_BASIC_TYPE) {
	    if (node->type->basic_type == HM_BASIC_TYPE_BOOL) {
		printf("%*sbool\n", indent, "");
	    } else if (node->type->basic_type == HM_BASIC_TYPE_INTEGRAL) {
		printf("%*sint\n", indent, "");
	    } else {
		assert(false);
	    }
	} else if (node->type->tag == HM_TYPE_TAG_VARIABLE) {
	    printf("%*st%d\n", indent, "", node->type->variable);
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
