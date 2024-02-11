#ifndef __HM_H__
#define __HM_H__

#include <stdbool.h>
#include <satie_error.h>
#include "ast.h"

bool hm_infer_types(ast_node_t* node, satie_error_t* error);

#endif
