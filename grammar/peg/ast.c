#include <string.h>
#include <stdio.h>
#include "slist.h"
#include "ast.h"

ast_node_t* ast_create_node(char *rule_name) {
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->rule_name = strdup(rule_name);
    node->value = NULL;
    slist_init(&node->values);
    slist_init(&node->children);
    return node;
}

ast_node_t* ast_add_child(char *rule_name, ast_node_t *parent_node) {
    fprintf(stderr, "A2a1\n");
    ast_node_t* node = ast_create_node(rule_name);
    fprintf(stderr, "A2a2: %s\n", parent_node->rule_name);
    slist_insert_first(&parent_node->children, node);
    fprintf(stderr, "A2a3\n");
    return node;
}

void ast_add_value(char *rule_name, ast_node_t *parent_node, const char *value) {
    fprintf(stderr, "SNUS\n");
    ast_node_t* node = ast_create_node(rule_name);
    node->value = strdup(value);
    slist_insert_last(&parent_node->values, node);
}
