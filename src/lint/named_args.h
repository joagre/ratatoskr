#ifndef LINT_NAMED_ARGS_H
#define LINT_NAMED_ARGS_H

#include <dynarr.h>
#include "types.h"
#include "named_arg.h"

typedef dynarray_t named_args_t;

named_args_t* named_args_new(void);
void named_args_add(named_args_t* args, char* name, type_t* type);
uint16_t named_args_size(named_args_t* args);
named_arg_t* named_args_get(named_args_t* args, uint16_t i);

#endif
