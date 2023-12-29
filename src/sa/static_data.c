#define MUTE_LOG_DEBUG 1

#include <utils.h>
#include "static_data.h"

void static_data_init(static_data_t* data) {
    data->data = NULL;
    data->size = 0;
    data->max_size = 0;
    dynarray_init(&data->indices, NULL, INITIAL_STATIC_DATA_SIZE,
                  sizeof(vm_stack_value_t));
}

void static_data_clear(static_data_t* data) {
    dynarray_clear(&data->indices);
}

vm_stack_value_t static_data_insert_string(static_data_t* data, char* string) {
    for (uint32_t i = 0; i < dynarray_size(&data->indices); i++) {
        vm_stack_value_t* data_index = dynarray_element(&data->indices, i);
        char* data_string =
            (char*)&data->data[*data_index + sizeof(vm_data_length_t)];
        if (strcmp(string, data_string) == 0) {
            return *data_index;
        }
    }
    vm_stack_value_t data_index = data->size;
    dynarray_append(&data->indices, &data_index);
    // Append string length
    vm_data_length_t data_length = strlen(string) + 1;
    uint8_t* data_length_bytes = (uint8_t*)&data_length;
    buf_append((uint8_t **)&data->data, &data->max_size, &data->size,
               data_length_bytes, sizeof(data_length));
    // Append string
    buf_append((uint8_t **)&data->data, &data->max_size, &data->size,
               (uint8_t*)string, data_length);
    return data_index;
}

uint8_t* static_data_lookup(static_data_t* static_data,
			    vm_stack_value_t index) {
    return (uint8_t*)(&static_data->data[index + sizeof(vm_data_length_t)]);
}

void static_data_pretty_print(static_data_t* static_data) {
    // Print static data via indices
    for (uint32_t i = 0; i < dynarray_size(&static_data->indices); i++) {
	vm_stack_value_t* data_index =
	    dynarray_element(&static_data->indices, i);
	vm_data_length_t data_length =
	    GET_VALUE(vm_data_length_t, &static_data->data[i]);
	char* data_string =
	    (char*)&static_data->data[*data_index + sizeof(data_length)];
	printf("  %u: %s\n", i, data_string);
    }
    // Print static data via data
    fprintf(stderr, "size: %u\n", static_data->size);
    uint32_t i = 0;
    while (i < static_data->size) {
	uint32_t j = GET_VALUE(vm_data_length_t, &static_data->data[i]);
	char* data_string =
	    (char*)&static_data->data[i + sizeof(vm_data_length_t)];
	printf("  data %u: %s\n", i, data_string);
	i += sizeof(vm_data_length_t) + j;
    }
}
