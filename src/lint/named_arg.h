#ifndef LINT_NAMED_ARG_H
#define LINT_NAMED_ARG_H

#include "type.h"

typedef struct {
    char* name;
    type_t* type;
} named_arg_t;

named_arg_t* named_arg_new(char* name, type_t* type);

#endif
