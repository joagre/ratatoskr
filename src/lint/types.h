#ifndef LINT_TYPES_H
#define LINT_TYPES_H

#include <dynarr.h>
#include "type.h"

typedef dynarray_t types_t;

types_t* types_new(void);
void types_add(types_t* types, type_t* type);
type_t* types_get(types_t* types, size_t i);

#endif
