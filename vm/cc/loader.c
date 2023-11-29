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


static void append_byte_code(loader_t* loader, size_t n, uint8_t* bytes);
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
            SET_ERROR(satie_error, ERROR_TYPE_MESSAGE, COMPONENT_LOADER);
            satie_error->message = "Invalid opcode";
            free(line);
            return;
        }

        // Add opcode byte to byte code
        append_byte_code(loader, 1, (uint8_t*)&opcode_info->opcode);
        
        // Add operand bytes to byte code
        //uint8_t bytes[MAX_OPERANDS_BYTES_SIZE];
        //size_t n = get_operands_as_bytes(opcode_info, operands, line, bytes);
        //append_byte_code(loader, n, bytes);
    }
    free(line);
    CLEAR_ERROR(satie_error);
}

static void append_byte_code(loader_t* loader, size_t n, uint8_t* bytes) {
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

