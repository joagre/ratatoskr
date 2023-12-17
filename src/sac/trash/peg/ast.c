#include <string.h>
#include <stdio.h>
#include "slist.h"
#include "ast.h"

ast_node_t* ast_create_node(char *rule_name) {
    fprintf(stderr, "ast_create_node: %s\n", rule_name);
    ast_node_t* node = malloc(sizeof(ast_node_t));
    node->rule_name = strdup(rule_name);
    node->value = NULL;
    slist_init(&node->values);
    slist_init(&node->children);
    return node;
}

ast_node_t* ast_create_value_node(char *rule_name, const char *value) {
    fprintf(stderr, "ast_create_vakue_node: %s, %s\n", rule_name, value);
    ast_node_t* node = ast_create_node(rule_name);
    node->rule_name = strdup(rule_name);
    return node;
}

ast_node_t* ast_add_child_node(char *rule_name, ast_node_t *parent_node) {
    fprintf(stderr, "ast_add_child_node: %s (%p)\n", rule_name, (void *)parent_node);
    ast_node_t* node = ast_create_node(rule_name);
    slist_insert_first(&parent_node->children, node);
    return node;
}

void ast_add_value_node(char *rule_name, ast_node_t *parent_node, const char *value) {
    fprintf(stderr, "ast_add_valie_node: %s (%p)\n", rule_name, (void *)parent_node);
    ast_node_t* node = ast_create_node(rule_name);
    node->value = strdup(value);
    slist_insert_last(&parent_node->values, node);
}
