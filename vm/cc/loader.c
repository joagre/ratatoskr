#define _GNU_SOURCE
#include <stdio.h>
#include <errno.h>
#include "clib/lhash_kv.h"
#include "loader.h"
#include "vm.h"
#include "module.h"

void loader_init(loader_t* loader, const char* load_path) {
    loader->byte_code = NULL;
    loader->byte_code_size = 0;
    loader->load_path = load_path;
    lhash_kv_init(&loader->modules, NULL, NULL, NULL);
}

void loader_free(loader_t* loader) {
    free(loader->byte_code);
    lhash_kv_clear(&loader->modules);
}

loader_result_t loader_load_module(loader_t *loader, const char* module_name) {
    char* file_path;
    asprintf(&file_path, "%s/%s.posm", loader->load_path, module_name);
    FILE* file;
    if ((file = fopen(file_path, "r")) == NULL) {
        free(file_path);
        return (loader_result_t){
            .success = false,
            .errno_value = errno
        };
    }
    module_t* module = module_new((address_type_t)loader->byte_code_size);
    loader_result_t result = loader_generate_byte_code(module);
    free(file_path);
    fclose(file);
    if (result.success) {
        module->stop_address = (address_type_t)loader->byte_code_size - 1;
        lhash_kv_insert(&loader->modules, (char *)module_name, module);
        return (loader_result_t){
            .success = true
        };
    } else {
        module_free(module);
        return result;
    }
}

loader_result_t loader_generate_byte_code(module_t*) {
    return (loader_result_t){
        .success = true
    };
}
