#ifndef __EQUATION_H__
#define __EQUATION_H__

#include "type.h"
#include "ast.h"

typedef struct {
    type_t* arg_type;
    type_t* return_type;
    char* info;
    ast_node_t* origin_node;
    ast_node_t* node;
} equation_t;

#endif
