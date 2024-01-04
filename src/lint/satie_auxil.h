#ifndef __SATIE_AUXIL_H__
#define __SATIE_AUXIL_H__

#include <stdint.h>
#include <stdarg.h>
#include "ast.h"

typedef struct {
    uint32_t row;
} satie_auxil_t;

satie_auxil_t* satie_auxil_new();
ast_node_t* satie_auxil_retype_node(ast_node_t* node, node_type_t type);
ast_node_t* satie_auxil_create_terminal(satie_auxil_t* auxil, node_type_t type,
					const char* value);
ast_node_t* satie_auxil_create_node(satie_auxil_t* auxil, node_type_t type,
				    uint16_t n, ...);
void satie_auxil_add_child(ast_node_t* parent_node, ast_node_t* node);

#define RN(node, type) satie_auxil_retype_node(node, type)
#define CT(type, value) satie_auxil_create_terminal(auxil, type, value)
#define CN(type, n, ...) satie_auxil_create_node(auxil, type, n, __VA_ARGS__)
#define AC(parent_node, node) satie_auxil_add_child(parent_node, node)

#endif
