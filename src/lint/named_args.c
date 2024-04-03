#include "named_args.h"

named_args_t* named_args_new(void) {
    named_args_t* args = malloc(sizeof(named_args_t));
    dynarray_init(args, NULL, 0, sizeof(named_arg_t));
    return args;
}

void named_args_add(named_args_t* args, char* name, type_t* type) {
    named_arg_t* arg = named_arg_new(name, type);
    dynarray_append(args, arg);
}

uint16_t named_args_size(named_args_t* args) {
    return dynarray_size(args);
}

named_arg_t* named_args_get(named_args_t* args, uint16_t i) {
    return dynarray_element(args, i);
}
