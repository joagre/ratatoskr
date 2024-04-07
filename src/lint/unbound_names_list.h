#ifndef LINT_UNBOUND_NAMES_LIST_H
#define LINT_UNBOUND_NAMES_LIST_H

#include <dynarr.h>

#include "unbound_names.h"

typedef dynarray_t unbound_names_list_t;

void unbound_names_list_init(unbound_names_list_t* list);
void unbound_names_list_clear(unbound_names_list_t* list);
void unbound_names_list_append(unbound_names_list_t* list,
			       unbound_names_t* names);
unbound_names_t* unbound_names_list_get(unbound_names_list_t* list, uint16_t i);
void unbound_names_list_print(unbound_names_list_t* list);

#endif
