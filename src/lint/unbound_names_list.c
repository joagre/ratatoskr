//#define MUTE_LOG_DEBUG 1

#include <stdint.h>
#include <stdio.h>
#include "unbound_names_list.h"

void unbound_names_list_init(unbound_names_list_t* list) {
    dynarray_init(list, NULL, 0, sizeof(unbound_names_t));
}

void unbound_names_list_clear(unbound_names_list_t* list) {
    uint16_t n = dynarray_size(list);
    for (uint16_t i = 0; i < n; i++) {
	unbound_names_t* names = dynarray_element(list, i);
	unbound_names_free(names);
    }
    dynarray_clear(list);
}

void unbound_names_list_append(unbound_names_list_t* list,
			       unbound_names_t* names) {
    dynarray_append(list, names);
}

unbound_names_t* unbound_names_list_get(unbound_names_list_t* list,
					uint16_t i) {
    return dynarray_element(list, i);
}

void unbound_names_list_print(unbound_names_list_t* list) {
    uint16_t n = dynarray_size(list);
    for (uint16_t i = 0; i < n; i++) {
	printf("** names:\n");
	unbound_names_t* names = dynarray_element(list, i);
	unbound_names_print(names);
    }
}
