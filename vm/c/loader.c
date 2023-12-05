#define MUTE_LOG_DEBUG 1

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>
#include "loader.h"
#include "log.h"
#include "util.h"
#include "pretty_print.h"

// Forward declarations of local functions (alphabetical order)
static void append_bytes(loader_t* loader, uint16_t n, uint8_t* bytes);
static void append_operands(loader_t* loader, opcode_info_t *opcode_info,
                            char operands[][MAX_OPERAND_STRING_SIZE],
                            uint8_t number_of_operands, satie_error_t* error);
static void generate_byte_code(loader_t* loader, module_t* module,
                               FILE* file, satie_error_t* error);
static int key_cmp(void* key1, void* key2, void*);
static size_t key_hash(void* key, void*);
static void purge_line(char *purged_line, char *line);
static void resolve_label(uint8_t* byte_code, module_t* module,
                          vm_address_t first_operand, uint16_t operand_offset);
static uint16_t size_of_operands(opcode_t opcode);

void loader_init(loader_t* loader, char* load_path) {
    loader->byte_code = NULL;
    loader->byte_code_size = 0;
    loader->max_byte_code_size = 0;
    loader->load_path = load_path;
    lhash_kv_init(&loader->modules, NULL, key_hash, key_cmp);
}

void loader_free(loader_t* loader) {
    free(loader->byte_code);
    lhash_kv_clear(&loader->modules);
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
    // Open file
    uint16_t file_path_length =
        strlen(loader->load_path) +
        strlen(module_name) +
        strlen(".posm");
    char file_path[file_path_length];
    sprintf(file_path, "%s/%s.posm", loader->load_path, module_name);
    FILE* file;
    if ((file = fopen(file_path, "r")) == NULL) {
        SET_ERROR_ERRNO(error, COMPONENT_LOADER);
        return;
    }

    // Generate byte code
    uint32_t old_byte_code_size = loader->byte_code_size;
    module_t* module = module_new(loader->byte_code_size);
    generate_byte_code(loader, module, file, error);
    fclose(file);
    if (error->failed) {
        loader->byte_code_size = old_byte_code_size;
        module_free(module);
    } else {
        module->stop_address = loader->byte_code_size - 1;
        lhash_kv_insert(&loader->modules, (char *)module_name, module);
        CLEAR_ERROR(error);
    }
}

void pretty_print(loader_t* loader) {
    vm_address_t address = 0;
    while (address < loader->byte_code_size) {
        fprintf(stderr, "%d: ", address);
        address += 1 + print_instruction(&loader->byte_code[address]);
    }
}

void pretty_print_module(loader_t* loader, char* module_name) {
    module_t* module;
    lhash_kv_find(&loader->modules, module_name, (void**)&module);
    vm_address_t address = module->start_address;
    while (address <= module->stop_address) {
        fprintf(stderr, "%d: ", address);
        address += 1 + print_instruction(&loader->byte_code[address]);
    }
}

//
// Local functions (alphabetical order)
//

static void append_bytes(loader_t* loader, uint16_t n, uint8_t* bytes) {
    if (loader->max_byte_code_size == 0) {
        // First allocation
        loader->byte_code = malloc(INITIAL_BYTE_CODE_SIZE);
        loader->max_byte_code_size = INITIAL_BYTE_CODE_SIZE;
    } else if (loader->byte_code_size + n > loader->max_byte_code_size) {
        // Reallocate
        loader->max_byte_code_size *= 2;
        loader->byte_code =
            realloc(loader->byte_code, loader->max_byte_code_size);
    }
    // Append bytes
    memcpy(loader->byte_code + loader->byte_code_size, bytes, n);
    loader->byte_code_size += n;
}

static void append_operands(loader_t* loader, opcode_info_t *opcode_info,
                            char operands[][MAX_OPERAND_STRING_SIZE],
                            uint8_t number_of_operands, satie_error_t* error) {
    // Check number of operands (special handling of pushs and ret)
    if (!(opcode_info->opcode == OPCODE_PUSHS ||
          opcode_info->opcode == OPCODE_RET ||
          number_of_operands == opcode_info->number_of_operands)) {
        SET_ERROR_MESSAGE(error, COMPONENT_LOADER, "Wrong number of operands");
        return;
    }

    // Append operands
    for (uint8_t i = 0; i < opcode_info->number_of_operands; i++) {
        switch (opcode_info->operands[i]) {
        case OPERAND_STACK_VALUE: {
            vm_stack_value_t stack_value = string_to_long(operands[i], error);
            if (error->failed) {
                return;
            }
            APPEND_VALUE(loader, vm_stack_value_t, stack_value);
            break;
        }
        case OPERAND_REGISTER:
            // Register values are prefixed with 'r'
            if (operands[i][0] != 'r') {
                SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                                  "Invalid register %s", operands[i]);
                return;
            }
            vm_register_t register_ = string_to_long(operands[i] + 1, error);
            if (error->failed) {
                return;
            }
            APPEND_VALUE(loader, vm_register_t, register_);
            break;
        case OPERAND_LABEL:
            vm_label_t label = string_to_long(operands[i], error);
            if (error->failed) {
                return;
            }
            APPEND_VALUE(loader, vm_label_t, label);
            break;
        case OPERAND_IMMEDIATE_VALUE:
            // Immediate values are prefixed with '#'
            if (operands[i][0] != '#') {
                SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                                  "Invalid immediate value %s", operands[i]);
                return;
            }
            vm_immediate_value_t immediate_value =
                string_to_long(operands[i] + 1, error);
            if (error->failed) {
                return;
            }
            APPEND_VALUE(loader, vm_immediate_value_t, immediate_value);
            break;
        case OPERAND_STACK_OFFSET:
            // Stack offsets are prefixed with '@'
            if (operands[i][0] != '@') {
                SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                                  "Invalid stack offset %s", operands[i]);
                return;
            }
            vm_stack_offset_t stack_offset =
                string_to_long(operands[i] + 1, error);
            if (error->failed) {
                return;
            }
            APPEND_VALUE(loader, vm_stack_offset_t, stack_offset);
            break;
        case OPERAND_ARITY:
            vm_arity_t arity = string_to_long(operands[i], error);
            if (error->failed) {
                return;
            }
            APPEND_VALUE(loader, vm_arity_t, arity);
            break;
        case OPERAND_RETURN_MODE:
            if (number_of_operands == 0) {
                // Default return mode is value
                vm_return_mode_t return_mode = RETURN_MODE_VALUE;
                APPEND_VALUE(loader, vm_return_mode_t, return_mode);
            } else if (strcmp(operands[0], "copy") == 0) {
                vm_return_mode_t return_mode = RETURN_MODE_COPY;
                APPEND_VALUE(loader, vm_return_mode_t, return_mode);
            }
            break;
        case OPERAND_SYSTEM_CALL:
            system_call_t system_call =
                string_to_system_call(operands[i], error);
            if (error->failed) {
                return;
            }
            APPEND_VALUE(loader, vm_system_call_t, system_call);
            break;
        case OPERAND_STRING:
            // Strings are prefixed and suffixed with '"'
            if (operands[i][0] != '"' ||
                operands[i][strlen(operands[i]) - 1] != '"') {
                SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                                  "Invalid string '%s'", operands[i]);
                return;
            }
            // Remove quotes
            char* naked_string = operands[i] + 1;
            naked_string[strlen(naked_string) - 1] = '\0';
            // Append string length
            uint16_t string_length = strlen(naked_string);
            APPEND_VALUE(loader, vm_data_length_t, string_length);
            // Append string
            append_bytes(loader, string_length, (uint8_t*)naked_string);
            break;
        }
    }
    CLEAR_ERROR(error);
}

static void generate_byte_code(loader_t* loader, module_t* module,
                               FILE* file, satie_error_t* error) {
    char opcode_string[MAX_OPCODE_STRING_SIZE];
    char operands_string[MAX_OPERANDS_STRING_SIZE] = "";
    char operands[MAX_OPERANDS][MAX_OPERAND_STRING_SIZE];
    char *line = NULL;
    size_t line_size = 0;
    char purged_line[MAX_LINE_LENGTH];

    while (true) {
        // Read line
        ssize_t read = getline(&line, &line_size, file);
        if (read == -1) {
            break;
        }

        // Purge line
        purge_line(purged_line, line);
        if (strlen(purged_line) == 0) {
            continue;
        }

        // Parse line
        LOG_DEBUG("Instruction: '%s'", purged_line);
        char *first_blank = strchr(purged_line, ' ');
        uint8_t number_of_operands = 0;
        if (first_blank == NULL) {
            strcpy(opcode_string, purged_line);
            //LOG_DEBUG("opcode_string: '%s'", opcode_string);
        } else {
            strcpy(operands_string, first_blank + 1);
            strcpy(opcode_string, strtok(purged_line, " "));
            //LOG_DEBUG("opcode_string: '%s'", opcode_string);
            //LOG_DEBUG("operands_string: '%s'", operands_string);
            uint8_t i = 0;
            while (i < MAX_OPERANDS) {
                char *token = strtok(NULL, " ");
                if (token == NULL) {
                    break;
                }
                strcpy(operands[i], token);
                //LOG_DEBUG("operands[%u]: %s", i, operands[i]);
                number_of_operands++;
                i++;
            }
        }

        // Special treatment of labels
        if (strcmp(opcode_string, "label") == 0) {
            if (number_of_operands != 1) {
                free(line);
                SET_ERROR_MESSAGE(error, COMPONENT_LOADER,
                                  "Wrong number of label operands");
                return;
            }
            vm_label_t label = string_to_long(operands[0], error);
            if (error->failed) {
                free(line);
                return;
            }
            LOG_DEBUG("Jump table insertion: %u -> %u", label, loader->byte_code_size);
            module_insert(module, label, loader->byte_code_size);
            continue;
        }

        // Look for opcode info
        opcode_info_t* opcode_info =
            string_to_opcode_info(opcode_string, error);
        if (error->failed) {
            free(line);
            return;
        }

        // Add opcode byte to byte code
        append_bytes(loader, 1, (uint8_t*)&opcode_info->opcode);

        // Add operand bytes to byte code
        append_operands(loader, opcode_info, operands, number_of_operands,
                        error);
        if (error->failed) {
            free(line);
            return;
        }
    }
    free(line);

    // Resolve labels to addresses
    vm_address_t address = module->start_address;
    LOG_DEBUG("start_address = %u\n", address);
    while (address < loader->byte_code_size) {
        opcode_t opcode = loader->byte_code[address];
        vm_address_t operand_address = address + (vm_address_t)OPCODE_SIZE;

        // Resolve register machine labels
        if (opcode == OPCODE_JMPRNZE) {
            resolve_label(loader->byte_code, module, operand_address,
                          sizeof(vm_register_t));
            address += size_of_operands(OPCODE_JMPRNZE);
        } else if (opcode == OPCODE_JMPRINGT) {
            resolve_label(loader->byte_code, module, operand_address,
                          sizeof(vm_register_t) + sizeof(vm_immediate_value_t));
            address += size_of_operands(OPCODE_JMPRINGT);
        } else if (opcode == OPCODE_RCALL) {
            resolve_label(loader->byte_code, module, operand_address, 0);
            address += size_of_operands(OPCODE_RCALL);
        } else if (opcode == OPCODE_JMP) {
            resolve_label(loader->byte_code, module, operand_address, 0);
            address += size_of_operands(OPCODE_JMP);
            // Resolve stack machine labels
        } else if (opcode == OPCODE_PUSHS) {
            vm_data_length_t length =
                GET_VALUE(vm_data_length_t, &loader->byte_code[operand_address]);
            address += sizeof(vm_data_length_t) + length;
        } else if (opcode == OPCODE_JUMP) {
            resolve_label(loader->byte_code, module, operand_address, 0);
            address += size_of_operands(OPCODE_JUMP);
        } else if (opcode == OPCODE_CJUMP) {
            resolve_label(loader->byte_code, module, operand_address, 0);
            address += size_of_operands(OPCODE_CJUMP);
        } else if (opcode == OPCODE_CALL) {
            resolve_label(loader->byte_code, module, operand_address, 0);
            address += size_of_operands(OPCODE_CALL);
        } else if (opcode == OPCODE_SPAWN) {
            resolve_label(loader->byte_code, module, operand_address, 0);
            address += size_of_operands(OPCODE_SPAWN);
        } else {
            address += size_of_operands((opcode_t)opcode);
        }
        address++;
    }

    CLEAR_ERROR(error);
}

static int key_cmp(void* key1, void* key2, void*) {
    return (key1 == key2);
};

static size_t key_hash(void* key, void*) {
    return (size_t)key;
};

static void purge_line(char *purged_line, char *line) {
    // Remove heading whitespaces and comment rows
    uint16_t i = 0;
    while (line[i] == '\t' || line[i] == '\r' || line[i] == ' ') {
        i++;
    }

    // Nothing of value
    if (line[i] == '\n' || line[i] == ';') {
        purged_line[0] = '\0';
        return;
    }

    // Whitespace purging
    bool is_space = false;
    uint16_t j = 0;
    while (line[i] != ';' && line[i] != '\n') {
        // Replace \t and \r with spaces and remove multiple spaces
        if (line[i] == '\t' || line[i] == '\r' || line[i] == ' ') {
            if (!is_space) {
                purged_line[j++] = ' ';
                is_space = true;
            }
        } else {
            purged_line[j++] = line[i];
            is_space = false;
        }
        i++;
    }

    // Truncate line
    if (purged_line[j - 1] == ' ') {
        // Remove trailing space
        purged_line[j - 1] = '\0';
    } else {
        purged_line[j] = '\0';
    }
}

static void resolve_label(uint8_t* byte_code, module_t* module,
                          vm_address_t first_operand, uint16_t operand_offset) {
    vm_address_t label_address = first_operand + operand_offset;
    vm_label_t label = GET_VALUE(vm_label_t, &byte_code[label_address]);
    vm_address_t address = module_lookup_address(module, label);
    LOG_DEBUG("Jump table lookup: %u -> %u", label, address);
    SET_VALUE(vm_address_t, address, &byte_code[label_address]);
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
        case OPERAND_RETURN_MODE:
            size += sizeof(vm_return_mode_t);
            break;
        case OPERAND_SYSTEM_CALL:
            size += sizeof(system_call_t);
            break;
        case OPERAND_STRING:
            // String length calculation is taken care of elsewhere
        }
    }
    return size;
}

//
// Unit test
//

void loader_unit_test(void) {
    loader_t loader;
    loader_init(&loader, "../examples");

    // Verify that loader->modules is functional
    module_t* module = module_new(42);
    lhash_kv_insert(&loader.modules, "foo", module);
    module_t* module2;
    lhash_kv_find(&loader.modules, "foo", (void**)&module2);
    LOG_ASSERT(module2->start_address == 42, "Wrong start address");

    // loader_load_module
    satie_error_t error;
    loader_load_module(&loader, "ackermannr", &error);
    LOG_ASSERT(!error.failed, "Failed to load module");

    // loader_is_module_loaded
    LOG_ASSERT(loader_is_module_loaded(&loader, "ackermannr"),
               "loader_is_module_loaded");

    LOG_INFO("Unit test passed");
}
