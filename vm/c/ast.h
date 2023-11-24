#ifndef __AST_H__
#define __AST_H__

#include <stdint.h>
#include <stdbool.h>
#include "dlist.h"

#define OP_LEN 8
#define ID_LEN 256
#define MODULE_PATH_LEN 1024

typedef enum {
    AST_NODE_TYPE_ROOT,
    AST_NODE_TYPE_IMPORT,
    AST_NODE_TYPE_IDENTIFIER,
    AST_NODE_TYPE_MODULE_PATH
} ast_node_type_t;

struct ast_node;

typedef struct ast_node {
    ast_node_type_t type;
    union {
        // AST_NODE_TYPE_ROOT
        struct {
            dlist_t top_level_nodes;
        } root;
        // AST_NODE_TYPE_IMPORT
        struct {
            struct ast_node* alias_node;
            struct ast_node* module_path_node;
        } import;
        // AST_NODE_TYPE_IDENTIFIER
        struct {
            char* string;
        } identifier;
        // AST_NODE_TYPE_MODULE_PATH
        struct {
            dlist_t identifier_nodes;
            bool is_wildcard;
        } module_path;
        // AST_NODE_TYPE_BINARY_OP
        struct {
            char op[OP_LEN];
            struct ast_node* left_node;
            struct ast_node* right_node;
        } binary_op;
        // AST_NODE_TYPE_INTEGER
        struct {
            uint32_t value;
        } integer;
    } data;
} ast_node_t;

typedef struct {
    uint32_t line;
    uint32_t column;
    ast_node_t* root_node;
    ast_node_t* managed_node;
} ast_ctrl_t;

ast_node_t* create_root_node();
ast_node_t* create_import_node(ast_ctrl_t* ctrl, ast_node_t* alias_node,
                               ast_node_t* module_path_node);
ast_node_t* create_identifier_node(ast_ctrl_t*ctrl, char* string);
ast_node_t* create_module_path_node(ast_ctrl_t*ctrl,
                                    ast_node_t* identifier_node);
ast_node_t* add_module_path_identifier(ast_ctrl_t*ctrl,
                                       ast_node_t* module_path_node,
                                       ast_node_t* identifier_node);
ast_node_t* set_module_path_wildcard(ast_ctrl_t*ctrl,
                                     ast_node_t* module_path_node);

#endif
