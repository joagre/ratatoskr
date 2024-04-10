#ifndef __HM_H__
#define __HM_H__

#include <satie_error.h>
#include <stdbool.h>

#include "ast.h"

#define CONTEXT_LIST_LOOKUP 0x01
#define CONTEXT_LIST_SLICE  0x02
#define CONTEXT_BIND        0x03
#define CONTEXT_AS          0x04
#define CONTEXT_CASE        0x05

#define MAX_UNBOUND_NAMES   128

typedef struct {
    type_t* operand_type;
    type_t* return_type;
} operator_types_t;

bool hm_infer_types(ast_node_t* node, satie_error_t* error);
uint32_t unique_id(void);

#endif
