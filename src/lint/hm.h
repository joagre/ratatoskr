#ifndef __HM_H__
#define __HM_H__

#include <stdbool.h>
#include <satie_error.h>
#include "ast.h"

#define CONTEXT_LIST_LOOKUP 0x01
#define CONTEXT_LIST_SLICE  0x02
#define CONTEXT_BIND        0x03

typedef struct {
    type_t* operand_type;
    type_t* return_type;
} operator_types_t;

bool hm_infer_types(ast_node_t* node, satie_error_t* error);

#endif
