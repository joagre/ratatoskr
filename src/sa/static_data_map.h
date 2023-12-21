#ifndef __STATIC_DATA_MAP_H__
#define __STATIC_DATA_MAP_H__

#include <lhash_kv.h>
#include "vm.h"

typedef lhash_kv_t static_data_map_t;

void static_data_map_init(static_data_map_t* map);
void static_data_map_clear(static_data_map_t* map);
void static_data_map_insert(static_data_map_t* map, vm_stack_value_t index,
                            vm_stack_value_t resolved_index);
vm_stack_value_t static_data_map_lookup(static_data_map_t* map,
                                        vm_stack_value_t index);
void static_data_map_iterate(static_data_map_t* map,
                             void (*callback)(vm_stack_value_t,
                                              vm_stack_value_t));

#endif
