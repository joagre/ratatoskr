#define _GNU_SOURCE
#include <stdio.h>
#include <errno.h>
#include "clib/lhash_kv.h"
#include "loader.h"
#include "vm.h"
#include "log.h"
#include "loader_module.h"

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
    // Open file
    char* file_path;
    asprintf(&file_path, "%s/%s.posm", loader->load_path, module_name);
    FILE* file;
    if ((file = fopen(file_path, "r")) == NULL) {
        free(file_path);
        return (loader_result_t){ .success = false, .errno_value = errno };
    }

    // Generate byte code
    size_t old_byte_code_size = loader->byte_code_size;
    module_t* module = loader_module_new((vm_address_t)loader->byte_code_size);
    loader_result_t result = loader_generate_byte_code(module, file);
    free(file_path);
    fclose(file);
    if (result.success) {
        module->stop_address = (vm_address_t)loader->byte_code_size - 1;
        //lhash_kv_insert(&loader->modules, (char *)module_name, module);
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

    while (true) {
        // Reset operands
        for (int i = 0; i < MAX_OPERANDS; i++) {
            operands[i][0] = '\0';
        }

        // Read line
        char *line = NULL;
        size_t n = 0;
        ssize_t read = getline(&line, &n, file);
        if (read == -1) {
            free(line);
            break;
        }

        // Purge line
        char* purged_line = purge_line(line);
        SATIE_LOG(LOG_LEVEL_DEBUG, "purged_line: '%s'", purged_line);
        if (strlen(purged_line) == 0) {
            free(line);
            free(purged_line);
            continue;
        }

        // Parse line
        char *first_blank = strchr(purged_line, ' ');
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
                i++;
            }
        }
        free(line);
        free(purged_line);

        /*
        // Special treatment of labels
        if (strcmp(opcode_string, "label") == 0) {
            if (operands[1] != NULL) {
                return (loader_result_t){
                    .success = false,
                    .error_message = "Bad label definition"
                }
            }
            module_.insertLabel(parse!LabelType(operands[0], line),
                                    cast(AddressType)byteCode.length);
            continue;
        }
        */
    }

    return (loader_result_t){ .success = true };
}

char *purge_line(char *line) {
    size_t i = 0;
    // Remove heading whitespaces and comment rows
    while (line[i] == '\t' || line[i] == '\r' || line[i] == ' ') {
        i++;
    }
    if (line[i] == '\n' || line[i] == ';') {
        return "";
    }
    char *purged_line = strdup(line);
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
    if (purged_line[j-1] == ' ') {
        // Remove trailing space
        purged_line[j-1] = '\0';
    } else {
        purged_line[j] = '\0';
    }
    return purged_line;
}
