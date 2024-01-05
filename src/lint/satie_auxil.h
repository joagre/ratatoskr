#ifndef __SATIE_AUXIL_H__
#define __SATIE_AUXIL_H__

#include <stdint.h>
#include <stdarg.h>
#include "ast.h"

typedef struct {
    uint32_t row;
} satie_auxil_t;

satie_auxil_t* satie_auxil_new();
ast_node_t* satie_auxil_rename_node(ast_node_t* node, node_name_t name);
ast_node_t* satie_auxil_create_terminal(satie_auxil_t* auxil, node_name_t name,
					const char* value);
ast_node_t* satie_auxil_create_node(satie_auxil_t* auxil, node_name_t name,
				    uint16_t n, ...);
void satie_auxil_add_child(ast_node_t* parent_node, ast_node_t* node);

#define RN(node, name) satie_auxil_rename_node(node, name)
#define CT(name, value) satie_auxil_create_terminal(auxil, name, value)
#define CN(name, n, ...) satie_auxil_create_node(auxil, name, n, __VA_ARGS__)
#define AC(parent_node, node) satie_auxil_add_child(parent_node, node)

#endif
