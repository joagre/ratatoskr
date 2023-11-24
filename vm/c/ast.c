#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>
#include "ast.h"
#include "dlist.h"

ast_node_t* create_root_node() {
    fprintf(stderr, "create_root_node()\n");
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = AST_NODE_TYPE_ROOT;
    dlist_init(&(node->data.root.top_level_nodes));
    return node;
}

ast_node_t* create_import_node(ast_ctrl_t* ctrl, ast_node_t* alias_node,
                               ast_node_t* module_path_node) {

    fprintf(stderr, "create_import_node()\n");
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = AST_NODE_TYPE_IMPORT;
    node->data.import.alias_node = alias_node;
    node->data.import.module_path_node = module_path_node;
    dlist_insert_last(&ctrl->root_node->data.root.top_level_nodes, node);
    return node;
}

ast_node_t* create_identifier_node(ast_ctrl_t*ctrl, char* string) {
    fprintf(stderr, "create_identifier_node()\n");
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = AST_NODE_TYPE_IDENTIFIER;
    node->data.identifier.string = strdup(string);
    return node;
}

ast_node_t* create_module_path_node(ast_ctrl_t*ctrl,
                                    ast_node_t* identifier_node) {
    fprintf(stderr, "create_module_path_node()\n");
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = AST_NODE_TYPE_MODULE_PATH;
    dlist_init(&(node->data.module_path.identifier_nodes));
    dlist_insert_last(&(node->data.module_path.identifier_nodes), identifier_node);
    node->data.module_path.is_wildcard = false;
    return node;
}

ast_node_t* add_module_path_identifier(ast_ctrl_t*ctrl,
                                       ast_node_t* module_path_node,
                                       ast_node_t* identifier_node) {
    fprintf(stderr, "add_module_path_identifier()\n");
    dlist_insert_last(&(module_path_node->data.module_path.identifier_nodes),
                      identifier_node);
    return module_path_node;
}

ast_node_t* set_module_path_wildcard(ast_ctrl_t*ctrl,
                                     ast_node_t* module_path_node) {
    fprintf(stderr, "set_module_path_wildcard()\n");
    module_path_node->data.module_path.is_wildcard = true;
    return module_path_node;
}

/*
ast_node_t* create_binary_op(char op[OP_LEN], ast_node_t* left, ast_node_t* right) {
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = AST_NODE_TYPE_BINARY_OP;
    strcpy(node->data.binary_op.op, op);
    node->data.binary_op.left = left;
    node->data.binary_op.right = right;
    return node;
}

ast_node_t* create_integer(int value) {
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->type = AST_NODE_TYPE_INTEGER;
    node->data.integer.value = value;
    return node;
}

void free_ast_node(ast_node_t* node) {
    if (node == NULL) {
        return;
    }
    if (node->type == AST_NODE_TYPE_ROOT) {
        dlist_iter_t iter;
        dlist_iter_init(&iter, &(node->data.root.nodes));
        while(!dlist_iter_end(&iter)) {
            ast_node_t* current_node = (ast_node_t*)dlist_iter_current(&iter);
            dlist_iter_next(&iter);
            free_ast_node(current_node);
        }
    } else if (node->type == AST_NODE_TYPE_BINARY_OP) {
        free_ast_node(node->data.binary_op.left);
        free_ast_node(node->data.binary_op.right);
    }
    free(node);
}

void dump_ast(ast_node_t *node, uint8_t indent) {
    switch (node->type) {
    case AST_NODE_TYPE_ROOT:
        printf("%*sRootNode", indent, "");
        dlist_iter_t iter;
        dlist_iter_init(&iter, &(node->data.root.nodes));
        while(!dlist_iter_end(&iter)) {
            ast_node_t* next_node = (ast_node_t*)dlist_iter_current(&iter);
            dump_ast(next_node, indent + 4);
            dlist_iter_next(&iter);
        }
        break;
    case AST_NODE_TYPE_IMPORT:
        printf("%*sImport", indent, "");
    }
}
*/
