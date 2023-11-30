#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include "loader.h"
#include "log.h"
#include "util.h"


        /*
          module_t* ptr;
          lhash_find(&loader->modules, (char *)module_name, (void**) &ptr);
          printf("%s -> %d\n", module_name, ptr->start_address);
        */


static void append_bytes(loader_t* loader, size_t n, const uint8_t* bytes);
static size_t append_operands(loader_t* loader,
                              const opcode_info_t *opcode_info,
                              char** operands, size_t number_of_operands,
                              satie_error_t* satie_error);
static void purge_line(char *purged_line,  const char *line);
static size_t key_hash(void* key, void*);
static int key_cmp(void* key1, void* key2, void*);

void loader_init(loader_t* loader, const char* load_path) {
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

static void generate_byte_code(loader_t* loader, module_t* module,
                               FILE* file, satie_error_t* satie_error) {
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
        SATIE_LOG(LOG_LEVEL_DEBUG, "purged_line: '%s'", purged_line);
        if (strlen(purged_line) == 0) {
            continue;
        }
        
        // Parse line
        char *first_blank = strchr(purged_line, ' ');
        size_t number_of_operands = 0;
        if (first_blank == NULL) {
            strcpy(opcode_string, purged_line);
            SATIE_LOG(LOG_LEVEL_DEBUG, "opcode_string: '%s'", opcode_string);
        } else {
            strcpy(operands_string, first_blank + 1);
            strcpy(opcode_string, strtok(purged_line, " "));
            SATIE_LOG(LOG_LEVEL_DEBUG, "opcode_string: '%s'", opcode_string);
            SATIE_LOG(LOG_LEVEL_DEBUG, "operands_string: '%s'", operands_string);
            size_t i = 0;
            while (i < MAX_OPERANDS) {
                char *token = strtok(NULL, " ");
                if (token == NULL) {
                    break;
                }
                strcpy(operands[i], token);
                SATIE_LOG(LOG_LEVEL_DEBUG, "operands[%d]: %s", i, operands[i]);
                number_of_operands++;
                i++;
            }
        }

        // Special treatment of labels
        if (strcmp(opcode_string, "label") == 0) {
            if (number_of_operands != 1) {
                free(line);
                SET_ERROR(satie_error, ERROR_TYPE_MESSAGE, COMPONENT_LOADER);
                satie_error->message = "Invalid label";
                return;
            }
            vm_label_t label = string_to_long(operands[0], satie_error);
            if (satie_error->failed) {
                free(line);
                return;
            }
            SATIE_LOG(LOG_LEVEL_DEBUG, "label %ld -> %d", label,
                      loader->byte_code_size);
            module_insert_label(module, label, loader->byte_code_size);
            continue;
        }

        // Look for opcode info
        const opcode_info_t* opcode_info =
            string_to_opcode_info(opcode_string, satie_error);
        if (satie_error->failed) {
            free(line);
            return;
        }

        // Add opcode byte to byte code
        append_bytes(loader, 1, (uint8_t*)&opcode_info->opcode);
        
        // Add operand bytes to byte code
        append_operands(loader, opcode_info, (char **)operands,
                        number_of_operands, satie_error);
        if (satie_error->failed) {
            free(line);
            return;
        }
    }
    free(line);
    CLEAR_ERROR(satie_error);
}

static void append_bytes(loader_t* loader, size_t n, const uint8_t* bytes) {
    if (loader->max_byte_code_size == 0) {
        loader->byte_code = malloc(INITIAL_BYTE_CODE_SIZE);
        loader->max_byte_code_size = INITIAL_BYTE_CODE_SIZE;
    } else if (loader->byte_code_size + n > loader->max_byte_code_size) {
        loader->max_byte_code_size *= 2;
        loader->byte_code =
            realloc(loader->byte_code, loader->max_byte_code_size);
    }
    memcpy(loader->byte_code + loader->byte_code_size, bytes, n);
    loader->byte_code_size += n;
}

static size_t append_operands(loader_t* loader,
                              const opcode_info_t *opcode_info,
                              char** operands, size_t number_of_operands,
                              satie_error_t* satie_error) {
    if (!(opcode_info->opcode == OPCODE_PUSHS || 
          opcode_info->opcode == OPCODE_RET || 
          number_of_operands == opcode_info->number_of_operands)) {
        SET_ERROR(satie_error, ERROR_TYPE_MESSAGE, COMPONENT_LOADER);
        satie_error->message = "Wrong number of operands";
        return 0;
    }
    
    size_t n = 0;
    for (size_t i = 0; i < opcode_info->number_of_operands; i++) {
        switch (opcode_info->operands[i]) {
        case OPERAND_STACK_VALUE: {
            vm_stack_value_t stack_value =
                string_to_long(operands[i], satie_error);
            if (satie_error->failed) {
                return 0;
            }
            uint8_t bytes[sizeof(vm_stack_value_t)];
            memcpy(bytes, &stack_value, sizeof(vm_stack_value_t));
            append_bytes(loader, sizeof(vm_stack_value_t), bytes);
            n += sizeof(vm_stack_value_t);
            break;
        }
        case OPERAND_REGISTER: {
            if (operands[i][0] != 'r') {
                SET_ERROR(satie_error, ERROR_TYPE_MESSAGE, COMPONENT_LOADER);
                satie_error->message = "Invalid register";
                return 0;
            }
            vm_register_t register_ = string_to_long(operands[i] + 1,
                                                     satie_error);
            if (satie_error->failed) {
                return 0;
            }
            uint8_t bytes[sizeof(vm_register_t)];
            memcpy(bytes, &register_, sizeof(vm_register_t));
            append_bytes(loader, sizeof(vm_register_t), bytes);
            n += sizeof(vm_register_t);
            break;
        }
        case OPERAND_LABEL: {
            vm_label_t label = string_to_long(operands[i], satie_error);
            if (satie_error->failed) {
                return 0;
            }
            uint8_t bytes[sizeof(vm_label_t)];
            memcpy(bytes, &label, sizeof(vm_label_t));
            append_bytes(loader, sizeof(vm_label_t), bytes);
            n += sizeof(vm_label_t);
            break;
        }
        case OPERAND_IMMEDIATE_VALUE: {
            if (operands[i][0] != '#') {
                SET_ERROR(satie_error, ERROR_TYPE_MESSAGE, COMPONENT_LOADER);
                satie_error->message = "Invalid immediate value";
                return 0;
            }
            vm_immediate_value_t immediate_value =
                string_to_long(operands[i] + 1, satie_error);
            if (satie_error->failed) {
                return 0;
            }
            uint8_t bytes[sizeof(vm_immediate_value_t)];
            memcpy(bytes, &immediate_value, sizeof(vm_immediate_value_t));
            append_bytes(loader, sizeof(vm_immediate_value_t), bytes);
            n += sizeof(vm_immediate_value_t);
            break;
        }
        case OPERAND_STACK_OFFSET: {
            if (operands[i][0] != '@') {
                SET_ERROR(satie_error, ERROR_TYPE_MESSAGE, COMPONENT_LOADER);
                satie_error->message = "Invalid stack offset";
                return 0;
            }
            vm_stack_offset_t stack_offset =
                string_to_long(operands[i] + 1, satie_error);
            if (satie_error->failed) {
                return 0;
            }
            uint8_t bytes[sizeof(vm_stack_offset_t)];
            memcpy(bytes, &stack_offset, sizeof(vm_stack_offset_t));
            append_bytes(loader, sizeof(vm_stack_offset_t), bytes);
            n += sizeof(vm_stack_offset_t);
            break;
        }
        case OPERAND_ARITY: {
            vm_arity_t arity = string_to_long(operands[i], satie_error);
            if (satie_error->failed) {
                return 0;
            }
            uint8_t bytes[sizeof(vm_arity_t)];
            memcpy(bytes, &arity, sizeof(vm_arity_t));
            append_bytes(loader, sizeof(vm_arity_t), bytes);
            n += sizeof(vm_arity_t);
            break;
        }
        case OPERAND_RETURN_MODE: {
            if (number_of_operands == 0) {
                uint8_t bytes[sizeof(vm_return_mode_t)];
                vm_return_mode_t return_mode = RETURN_MODE_VALUE;
                memcpy(bytes, &return_mode, sizeof(vm_return_mode_t));
                append_bytes(loader, sizeof(vm_return_mode_t), bytes);
                n += sizeof(vm_return_mode_t);
            } else if (strcmp(operands[0], "copy") == 0) {
                uint8_t bytes[sizeof(vm_return_mode_t)];
                vm_return_mode_t return_mode = RETURN_MODE_COPY;
                memcpy(bytes, &return_mode, sizeof(vm_return_mode_t));
                append_bytes(loader, sizeof(vm_return_mode_t), bytes);
                n += sizeof(vm_return_mode_t);
            }
            break;
        }
        case OPERAND_SYSTEM_CALL: {
            system_call_t system_call =
                string_to_system_call(operands[i], satie_error);
            if (satie_error->failed) {
                return 0;
            }
            uint8_t bytes[sizeof(system_call_t)];
            memcpy(bytes, &system_call, sizeof(system_call_t));
            append_bytes(loader, sizeof(system_call_t), bytes);
            n += sizeof(system_call_t);
            break;
        }
        case OPERAND_STRING: {
            if (operands[i][0] != '"' ||
                operands[i][strlen(operands[i]) - 1] != '"') {
                SET_ERROR(satie_error, ERROR_TYPE_MESSAGE, COMPONENT_LOADER);
                satie_error->message = "Invalid string";
                return 0;
            }
            char* naked_string = operands[i] + 1;
            naked_string[strlen(naked_string) - 1] = '\0';
            size_t string_length = strlen(naked_string);
            uint8_t bytes[sizeof(vm_data_length_t)];
            memcpy(bytes, &string_length, sizeof(vm_data_length_t));
            append_bytes(loader, sizeof(vm_data_length_t), bytes);
            n += sizeof(vm_data_length_t);
            append_bytes(loader, string_length, (uint8_t*)naked_string);
            n += string_length;
            break;
        }
        }
    }
    CLEAR_ERROR(satie_error);
    return n;
}

void loader_load_module(loader_t *loader, const char* module_name,
                        satie_error_t* satie_error) {
    // Open file
    size_t file_path_length =
        strlen(loader->load_path) +
        strlen(module_name) +
        strlen(".posm");
    char file_path[file_path_length];
    sprintf(file_path, "%s/%s.posm", loader->load_path, module_name);
    FILE* file;
    if ((file = fopen(file_path, "r")) == NULL) {
        SET_ERROR(satie_error, ERROR_TYPE_CODE, COMPONENT_LOADER);
        satie_error->code = errno;
        return;
    }

    // Generate byte code
    size_t old_byte_code_size = loader->byte_code_size;
    module_t* module = module_new((vm_address_t)loader->byte_code_size);
    generate_byte_code(loader, module, file, satie_error);
    fclose(file);
    if (satie_error->failed) {
        loader->byte_code_size = old_byte_code_size;
        module_free(module);
    } else {
        module->stop_address = (vm_address_t)loader->byte_code_size - 1;
        lhash_kv_insert(&loader->modules, (char *)module_name, module);
        CLEAR_ERROR(satie_error);
    }
}

static void purge_line(char *purged_line,  const char *line) {
    // Remove heading whitespaces and comment rows
    size_t i = 0;
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
    size_t j = 0;
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

static size_t key_hash(void* key, void*) {
    return (size_t)key;
};

static int key_cmp(void* key1, void* key2, void*) {
    return (key1 == key2);
};

