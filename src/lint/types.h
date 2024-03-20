#ifndef LINT_TYPES_H
#define LINT_TYPES_H

#include <dynarr.h>
#include "type.h"

I need the follwing typedef in another header file:

typedef dynarray_t types_t;

But this leads to ccyclic header files. What is the beat apptoach?



types_t* types_new(void);
void types_add(types_t* types, type_t* type);
type_t* types_lookup(types_t* types, size_t i);

#endif
