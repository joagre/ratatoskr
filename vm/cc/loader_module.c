#include "loader_module.h"

module_t* loader_module_new(address_type_t start_address) {
    module_t* module = malloc(sizeof(module_t));
    module->start_address = start_address;
    lhash_kv_init(&module->jump_table, NULL, NULL, NULL);
    return module;
}

void loader_module_free(module_t *) {
    // FIXME
}
