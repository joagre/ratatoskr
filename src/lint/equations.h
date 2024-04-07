#ifndef LINT_EQUATIONS_H
#define LINT_EQUATIONS_H

#include <dynarr.h>

#include "equation.h"

typedef dynarray_t equations_t;

void equations_init(equations_t* equations);
void equations_append(equations_t* equations, equation_t* equation);
equation_t* equations_get(equations_t* equations, uint16_t i);
void equations_print(equations_t* equations);

#endif
