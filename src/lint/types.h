#ifndef __TYPES_H__
#define __TYPES_H__

#include <dynarr.h>
#include "type.h"

typedef dynarray_t types_t;

types_t* types_new(void);
void types_add(types_t* types, type_t* type);
type_t* types_lookup(types_t* types, size_t i);

#endif
