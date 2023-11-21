#ifndef __AST_H__
#define __AST_H__

#include "slist.h"

typedef struct {
    char *rule_name;
    char *value;
    slist_t values;
    slist_t children;
} ast_node_t;

ast_node_t* ast_create_node(char *rule_name);
ast_node_t* ast_add_child(char *rule_name, ast_node_t *parent_node);
void ast_add_value(char *rule_name, ast_node_t *parent_node, const char *value);

#endif
