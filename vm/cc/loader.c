#define _GNU_SOURCE
#include <stdio.h>
#include <errno.h>
#include "clib/lhash_kv.h"
#include "loader.h"
#include "vm.h"
#include "log.h"
#include "loader_module.h"

static size_t key_hash(void* key, void*) {
    return (size_t)key;
};

static int key_cmp(void* key1, void* key2, void*) {
    return (key1 == key2);
};

void loader_init(loader_t* loader, const char* load_path) {
    loader->byte_code = NULL;
    loader->byte_code_size = 0;
    loader->load_path = load_path;
    lhash_kv_init(&loader->modules, NULL, key_hash, key_cmp);
}

void loader_free(loader_t* loader) {
    free(loader->byte_code);
    lhash_kv_clear(&loader->modules);
}

loader_result_t loader_load_module(loader_t *loader, const char* module_name) {
    // Open file
    char file_path[strlen(loader->load_path) +
                   strlen(module_name) +
                   strlen(".posm")];
    sprintf(file_path, "%s/%s.posm", loader->load_path, module_name);
    FILE* file;
    if ((file = fopen(file_path, "r")) == NULL) {
        return (loader_result_t){ .success = false, .errno_value = errno };
    }

    // Generate byte code
    size_t old_byte_code_size = loader->byte_code_size;
    module_t* module = loader_module_new((vm_address_t)loader->byte_code_size);
    loader_result_t result = loader_generate_byte_code(module, file);
    fclose(file);
    if (result.success) {
        module->stop_address = (vm_address_t)loader->byte_code_size - 1;
        lhash_kv_insert(&loader->modules, (char *)module_name, module);
        /*
        module_t* ptr;
	lhash_find(&loader->modules, (char *)module_name, (void**) &ptr);
	printf("%s -> %d\n", module_name, ptr->start_address);
        */
        return (loader_result_t){ .success = true };
    } else {
        loader->byte_code_size = old_byte_code_size;
        loader_module_free(module);
        return result;
    }
}

loader_result_t loader_generate_byte_code(module_t*, FILE* file) {
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
                return (loader_result_t){
                    .success = false,
                    .error_message = "Bad label definition"
                };
            }
            //module_.insertLabel(parse!LabelType(operands[0], line),
            //                        cast(AddressType)byteCode.length);
            continue;
        }
    }
    free(line);

    return (loader_result_t){ .success = true };
}

void purge_line(char *purged_line,  const char *line) {
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
