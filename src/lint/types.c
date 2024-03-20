#include "types.h"

types_t* types_new(void) {
    types_t* types = malloc(sizeof(types_t));
    dynarray_init(types, NULL, 0, sizeof(type_t));
    return types;
}

void types_add(types_t* types, type_t* type) {
    dynarray_append(types, type);
}

type_t* types_get(types_t* types, size_t i) {
    return dynarray_element(types, i);
}
