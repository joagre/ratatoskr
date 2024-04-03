#include <stdio.h>
#include "named_arg.h"

named_arg_t* named_arg_new(char* name, type_t* type) {
    named_arg_t* arg = malloc(sizeof(named_arg_t));
    arg->name = name;
    arg->type = type;
    return arg;
}
