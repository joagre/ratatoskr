#ifndef __EQUATIONS_H__
#define __EQUATIONS_H__

#include <dynarr.h>
#include "equation.h"

typedef dynarray_t equations_t;

void equations_init(equations_t* equations);
void equations_add(equations_t* equations, equation_t* equation);
equation_t* equations_lookup(equations_t* equations, size_t i);
void equations_print(equations_t* equations);

#endif
