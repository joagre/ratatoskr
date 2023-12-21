//#define MUTE_LOG_DEBUG 1

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include <utils.h>
#include <log.h>
#include "loader.h"
#include "pretty_print.h"

// Forward declarations of local functions (alphabetical order)
static void append_bytes(loader_t* loader, uint8_t* bytes,  uint16_t n);
static int key_cmp(void* key1, void* key2, void*);
static size_t key_hash(void* key, void*);
static void load_bytecode(loader_t* loader, module_t* module, FILE* file,
                          satie_error_t* error);
static void resolve_label(uint8_t* bytecode, module_t* module,
                          vm_address_t first_operand, uint16_t operand_offset);
static uint16_t size_of_operands(opcode_t opcode);

void loader_init(loader_t* loader, char* load_path) {
    loader->bytecode = NULL;
    loader->bytecode_size = 0;
    loader->max_bytecode_size = 0;
    loader->load_path = load_path;
    lhash_kv_init(&loader->modules, NULL, key_hash, key_cmp);
    static_data_init(&loader->static_data);
    static_data_map_init(&loader->static_data_map);
}

void loader_clear(loader_t* loader) {
    free(loader->bytecode);
    lhash_kv_clear(&loader->modules);
    static_data_clear(&loader->static_data);
    static_data_map_clear(&loader->static_data_map);
}

bool loader_is_module_loaded(loader_t* loader, char *module_name) {
    module_t* module;
    return lhash_kv_find(&loader->modules, module_name, (void**)&module) != 0;
}

vm_address_t loader_lookup_address(loader_t* loader, char *module_name,
                                   vm_label_t label) {
    module_t* module;
    lhash_kv_find(&loader->modules, module_name, (void**)&module);
    return module_lookup_address(module, label);
}

void loader_load_module(loader_t *loader, char* module_name,
                        satie_error_t* error) {
    // Open bytecode file
    uint16_t filename_length =
        strlen(loader->load_path) +
        strlen("/") +
        strlen(module_name) +
        strlen(".sab");
    char filename[filename_length];
    sprintf(filename, "%s/%s.sab", loader->load_path, module_name);
    FILE* file;
    if ((file = fopen(filename, "r")) == NULL) {
        SET_ERROR_ERRNO(error, COMPONENT_LOADER);
        return;
    }

    // Load bytecode
    uint32_t old_bytecode_size = loader->bytecode_size;
    module_t* module = module_new(loader->bytecode_size);
    load_bytecode(loader, module, file, error);
    fclose(file);

    if (error->failed) {
        loader->bytecode_size = old_bytecode_size;
        module_free(module);
    } else {
        module->stop_address = loader->bytecode_size - 1;
        lhash_kv_insert(&loader->modules, (char *)module_name, module);
        CLEAR_ERROR(error);
        LOG_DEBUG("%s has been loaded...", filename);
        pretty_print_module(loader, module_name);
    }
}

void pretty_print(loader_t* loader) {
    vm_address_t address = 0;
    while (address < loader->bytecode_size) {
        fprintf(stderr, "%d: ", address);
        address += 1 + print_instruction(&loader->bytecode[address],
                                         &loader->static_data);
    }
}

void pretty_print_module(loader_t* loader, char* module_name) {
    module_t* module;
    lhash_kv_find(&loader->modules, module_name, (void**)&module);
    vm_address_t address = module->start_address;
    while (address <= module->stop_address) {
        fprintf(stderr, "%d: ", address);
        address += 1 + print_instruction(&loader->bytecode[address],
                                         &loader->static_data);
    }
}

//
// Local functions (alphabetical order)
//

static void append_bytes(loader_t* loader, uint8_t* bytes, uint16_t n) {
    buf_append(&loader->bytecode, &loader->max_bytecode_size,
               &loader->bytecode_size, bytes, n);
}

static int key_cmp(void* key1, void* key2, void* arg) {
    (void*)arg;
    return (key1 == key2);
};

static size_t key_hash(void* key, void* arg) {
    (void*)arg;
    return (size_t)key;
};

static void load_bytecode(loader_t* loader, module_t* module,
			  FILE* file, satie_error_t* error) {
    // NOTE: Bytecode format is defined in sac/compiler.c
    // Read static data size
    uint32_t static_data_size = 0;
    size_t n = fread(&static_data_size, sizeof(static_data_size), 1, file);
    if (n != 1) {
        SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                          "Failed to read static data size");
        return;
    }
    LOG_DEBUG("Static data size: %u", static_data_size);
    // Read static data
    if (static_data_size > 0) {
        vm_stack_value_t local_data_index = 0;
        do {
            LOG_DEBUG("local_data_index = %u", local_data_index);
            vm_data_length_t data_length;
            n = fread(&data_length, sizeof(data_length), 1, file);
            if (n != 1) {
                SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                                  "Failed to read data length");
                return;
            }
            LOG_DEBUG("Data length: %u", data_length);
            char string[data_length];
            n = fread(string, data_length, 1, file);
            if (n != 1) {
                SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                                  "Failed to read string");
                return;
            }
            LOG_DEBUG("String: %s", string);
            vm_stack_value_t data_index =
                static_data_insert_string(&loader->static_data, string);
            LOG_DEBUG("Map %u -> %u", local_data_index, data_index);
            static_data_map_insert(&loader->static_data_map,
                                   local_data_index, data_index);
            local_data_index += data_length;
            static_data_size -= sizeof(data_length) + data_length;
        } while (static_data_size > 0);
    }
    // Read jump table size
    uint32_t jump_table_size = 0;
    n = fread(&jump_table_size, sizeof(jump_table_size), 1, file);
    if (n != 1) {
        SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                          "Failed to read jump table size");
        return;
    }
    LOG_DEBUG("Jump table size: %u", jump_table_size);
    // Read jump table
    for (uint32_t i = 0; i < jump_table_size; i++) {
        vm_label_t label = 0;
        vm_address_t address = 0;
        n = fread(&label, sizeof(label), 1, file);
        if (n != 1) {
            SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                              "Failed to read label");
            return;
        }
        n = fread(&address, sizeof(address), 1, file);
        if (n != 1) {
            SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                              "Failed to read address");
            return;
        }
        LOG_DEBUG("Jump table insertion: %u -> %u", label, address);
        module_insert(module, label, address);
    }
    // Read byte code size
    uint32_t byte_code_size = 0;
    n = fread(&byte_code_size, sizeof(byte_code_size), 1, file);
    if (n != 1) {
        SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                          "Failed to read byte code size");
        return;
    }
    LOG_DEBUG("Byte code size: %u", byte_code_size);
    // Read byte code
    uint8_t buf[BUFSIZ];
    do {
        n = fread(buf, 1, BUFSIZ, file);
        if (n == 0) {
            break;
        }
        append_bytes(loader, buf, n);
    } while (true);

    // Resolve labels and static data
    vm_address_t address = module->start_address;
    LOG_DEBUG("start_address = %u\n", address);
    while (address < loader->bytecode_size) {
        opcode_t opcode = loader->bytecode[address];
        vm_address_t operand_address = address + (vm_address_t)OPCODE_SIZE;
        if (opcode == OPCODE_JMPRINEQ) {
            resolve_label(loader->bytecode, module, operand_address,
                          sizeof(vm_register_t) + sizeof(vm_immediate_value_t));
            address += size_of_operands(OPCODE_JMPRINEQ);
        } else if (opcode == OPCODE_JMPRNZE) {
            resolve_label(loader->bytecode, module, operand_address,
                          sizeof(vm_register_t));
            address += size_of_operands(OPCODE_JMPRNZE);
        } else if (opcode == OPCODE_JMPRINGT) {
            resolve_label(loader->bytecode, module, operand_address,
                          sizeof(vm_register_t) + sizeof(vm_immediate_value_t));
            address += size_of_operands(OPCODE_JMPRINGT);
        } else if (opcode == OPCODE_CALL) {
            resolve_label(loader->bytecode, module, operand_address, 0);
            address += size_of_operands(OPCODE_CALL);
        } else if (opcode == OPCODE_JMP) {
            resolve_label(loader->bytecode, module, operand_address, 0);
            address += size_of_operands(OPCODE_JMP);
        } else if (opcode == OPCODE_SPAWN) {
            resolve_label(loader->bytecode, module, operand_address, 0);
            address += size_of_operands(OPCODE_SPAWN);
        } else if (opcode == OPCODE_PUSHSTR) {
            vm_stack_value_t value =
                GET_VALUE(vm_stack_value_t, &loader->bytecode[operand_address]);
            value = static_data_map_lookup(&loader->static_data_map, value);
            SET_VALUE(vm_stack_value_t, value,
                      &loader->bytecode[operand_address]);
            address += size_of_operands(OPCODE_PUSHSTR);
        } else {
            address += size_of_operands((opcode_t)opcode);
        }
        address++;
    }
    CLEAR_ERROR(error);
}

static void resolve_label(uint8_t* bytecode, module_t* module,
                          vm_address_t first_operand, uint16_t operand_offset) {
    vm_address_t label_address = first_operand + operand_offset;
    vm_label_t label = GET_VALUE(vm_label_t, &bytecode[label_address]);
    vm_address_t address = module_lookup_address(module, label);
    LOG_DEBUG("Jump table lookup: %u -> %u", label, address);
    SET_VALUE(vm_address_t, address, &bytecode[label_address]);
}

static uint16_t size_of_operands(opcode_t opcode) {
    opcode_info_t* opcode_info = opcode_to_opcode_info(opcode);
    uint16_t size = 0;
    for (uint8_t i = 0; i < opcode_info->number_of_operands; i++) {
        operand_t operand_type = opcode_info->operands[i];
        switch (operand_type) {
	    case OPERAND_STACK_VALUE:
		size += sizeof(vm_stack_value_t);
		break;
	    case OPERAND_REGISTER:
		size += sizeof(vm_register_t);
		break;
	    case OPERAND_LABEL:
		size += sizeof(vm_label_t);
		break;
	    case OPERAND_IMMEDIATE_VALUE:
		size += sizeof(vm_immediate_value_t);
		break;
	    case OPERAND_STACK_OFFSET:
		size += sizeof(vm_stack_offset_t);
		break;
	    case OPERAND_ARITY:
		size += sizeof(vm_arity_t);
		break;
	    case OPERAND_SYSTEM_CALL:
		size += sizeof(vm_system_call_t);
		break;
	    case OPERAND_STRING:
		size += sizeof(vm_stack_value_t);
		break;
        }
    }
    return size;
}

//
// Unit test
//

void loader_unit_test(void) {
    loader_t loader;
    loader_init(&loader, "../../examples/sai");
    // Verify that loader->modules is functional
    module_t* module = module_new(42);
    lhash_kv_insert(&loader.modules, "foo", module);
    module_t* module2;
    lhash_kv_find(&loader.modules, "foo", (void**)&module2);
    LOG_ASSERT(module2->start_address == 42, "Wrong start address");
    LOG_INFO("Unit test passed");
}
