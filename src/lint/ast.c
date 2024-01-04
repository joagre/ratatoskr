#define MUTE_LOG_DEBUG 1

#include "ast.h"

#define MAX_COLS 80

static const char* type_strings[] = {
    FOREACH_TYPE(GENERATE_STRING)
};

const char* ast_type_to_string(node_type_t type) {
    return type_strings[type];
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
    printf("%s", ast_type_to_string(node->type));
    cols += strlen(ast_type_to_string(node->type));
    if (node->value != NULL) {
        printf(": %s", node->value);
	cols += strlen(node->value) + 2;
    }
    if (node->hm_type != NULL) {
	uint16_t indent;
	if (cols > MAX_COLS) {
	    indent = 4;
	} else {
	    indent = MAX_COLS - cols - 4;
	}
	if (node->hm_type->tag == HM_TYPE_TAG_BASIC_TYPE) {
	    if (node->hm_type->basic_type == HM_TYPE_BOOL) {
		printf("%*sbool\n", indent, "");
	    } else if (node->hm_type->basic_type == HM_TYPE_INTEGRAL) {
		printf("%*sint\n", indent, "");
	    } else {
		assert(false);
	    }
	} else if (node->hm_type->tag == HM_TYPE_TAG_VARIABLE) {
	    printf("%*st%d\n", indent, "", node->hm_type->variable);
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
