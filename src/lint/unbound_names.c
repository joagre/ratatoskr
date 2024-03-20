//#define MUTE_LOG_DEBUG 1

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include "unbound_names.h"

unbound_names_t* unbound_names_new(void) {
    unbound_names_t* names = malloc(sizeof(unbound_names_t));
    dynarray_init(names, NULL, 0, sizeof(char *));
    return names;
}

// BY DESIGN: The actual memory for the strings is not freed
void unbound_names_free(unbound_names_t* names) {
    dynarray_clear(names);
}

void unbound_names_append(unbound_names_t* names, char* name) {
    dynarray_append(names, name);
}

char* unbound_names_get(unbound_names_t* names, uint16_t i) {
    return dynarray_element(names, i);
}

uint16_t unbound_names_size(unbound_names_t* names) {
    return dynarray_size(names);
}

bool unbound_names_member(unbound_names_t* names, char* name) {
    uint16_t n = dynarray_size(names);
    for (uint16_t i = 0; i < n; i++) {
	if (strcmp(dynarray_element(names, i), name) == 0) {
	    return true;
	}
    }
    return false;
}

void unbound_names_print(unbound_names_t* names) {
    for (uint16_t i = 0; i < dynarray_size(names); i++) {
	printf("%s\n", (char *)dynarray_element(names, i));
    }
}
