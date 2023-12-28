//#define MUTE_LOG_DEBUG 1

#include <errno.h>
#include <stdlib.h>
#include <utils.h>
#include <log.h>
#include <module.h>
#include "pretty_print.h"
#include "compiler.h"

// Forward declarations of local functions (alphabetical order)
static void append_bytes(compiler_t* compiler, uint16_t n, uint8_t* bytes);
static void append_operands(compiler_t* compiler, opcode_info_t *opcode_info,
			    char* operands_string,
			    char operands[][MAX_OPERAND_STRING_SIZE],
                            satie_error_t* error);
static void generate_bytecode(compiler_t* compiler, FILE* file,
                              satie_error_t* error);
//static void pretty_print(compiler_t* compiler);
static void purge_line(char *purged_line, char *line);

void compiler_init(compiler_t* compiler) {
    compiler->bytecode = NULL;
    compiler->bytecode_size = 0;
    compiler->max_bytecode_size = 0;
    static_data_init(&compiler->static_data);
    module_init(&compiler->module, 0);
}

void compiler_clear(compiler_t* compiler) {
    free(compiler->bytecode);
    static_data_clear(&compiler->static_data);
    module_clear(&compiler->module);
}

void compiler_compile(compiler_t* compiler, char* input_filename,
                      char *output_directory, satie_error_t* error) {
    // Open input file
    FILE* input_file;
    if ((input_file = fopen(input_filename, "r")) == NULL) {
        SET_ERROR_ERRNO(error, COMPONENT_COMPILER);
        return;
    }

    // Open output file
    char* module_name = strip_extension(basename(input_filename));
    uint16_t output_filename_length =
        strlen(output_directory) +
        strlen("/") +
        strlen(module_name) +
        strlen(".sab");
    char output_filename[output_filename_length];
    snprintf(output_filename, MAX_ERROR_MESSAGE_SIZE, "%s/%s.sab",
             output_directory, module_name);
    FILE* output_file;
    if ((output_file = fopen(output_filename, "w")) == NULL) {
        SET_ERROR_ERRNO(error, COMPONENT_COMPILER);
        fclose(input_file);
        return;
    }

    // Generate byte code
    generate_bytecode(compiler, input_file, error);
    fclose(input_file);

    if (error->failed) {
        // Free resources
        fclose(output_file);
        remove(output_filename);
        return;
    } else {
        /*
          Bytecode format:

          static_data_size: sizeof(uint32_t) bytes
          static_data: static_data_size bytes
          ...

          jump_table_size: sizeof(uint32_t) bytes
          label: sizeof(vm_label_t) bytes, address: sizeof(vm_address_t) bytes
          ...

          bytecode_size: sizeof(uint32_t) bytes
          bytecode: bytecode_size bytes
        */
        // Write static data size
        uint32_t data_size = compiler->static_data.size;
	LOG_DEBUG("data_size = %u", data_size);
        fwrite(&data_size, sizeof(data_size), 1, output_file);
        // Write static data
        fwrite(compiler->static_data.data, data_size, 1, output_file);
        // Write jump table size
        uint32_t jump_table_size = module_jump_table_size(&compiler->module);
	LOG_DEBUG("jump_table_size = %u", jump_table_size);
        fwrite(&jump_table_size, sizeof(jump_table_size), 1, output_file);
        // Write jump table
        void write_jump_table_entry(vm_label_t label, vm_address_t address) {
	    LOG_DEBUG("label = %u, address = %u", label, address);
            fwrite(&label, sizeof(label), 1, output_file);
            fwrite(&address, sizeof(address), 1, output_file);
        }
        module_iterate(&compiler->module, write_jump_table_entry);
        // Write bytecode size
	LOG_DEBUG("bytecode_size = %u", compiler->bytecode_size);
        fwrite(&compiler->bytecode_size, sizeof(compiler->bytecode_size), 1,
               output_file);
        // Write bytecode
        fwrite(compiler->bytecode, compiler->bytecode_size, 1, output_file);
        // Free resources
        fclose(output_file);
        //pretty_print(compiler);
        CLEAR_ERROR(error);
    }
}

//
// Local functions (alphabetical order)
//

static void append_bytes(compiler_t* compiler, uint16_t n, uint8_t* bytes) {
    buf_append(&compiler->bytecode, &compiler->max_bytecode_size,
               &compiler->bytecode_size, bytes, n);
}

static void append_operands(compiler_t* compiler, opcode_info_t *opcode_info,
			    char* operands_string,
			    char operands[][MAX_OPERAND_STRING_SIZE],
                            satie_error_t* error) {
    // Append operands
    for (uint8_t i = 0; i < opcode_info->number_of_operands; i++) {
        switch (opcode_info->operands[i]) {
	    case OPERAND_STACK_VALUE: {
		vm_stack_value_t value = string_to_long(operands[i], error);
		if (error->failed) {
		    return;
		}
		APPEND_VALUE(compiler, vm_stack_value_t, value);
		break;
	    }
	    case OPERAND_REGISTER: {
		// Register values are prefixed with 'r'
		if (operands[i][0] != 'r') {
		    SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
				      "Invalid register %s", operands[i]);
		    return;
		}
		vm_register_t register_ =
		    string_to_long(operands[i] + 1, error);
		if (error->failed) {
		    return;
		}
		APPEND_VALUE(compiler, vm_register_t, register_);
		break;
	    }
	    case OPERAND_LABEL: {
		vm_label_t label = string_to_long(operands[i], error);
		if (error->failed) {
		    return;
		}
		APPEND_VALUE(compiler, vm_label_t, label);
		break;
	    }
	    case OPERAND_IMMEDIATE_VALUE: {
		// Immediate values are prefixed with '#'
		if (operands[i][0] != '#') {
		    SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
				      "Invalid immediate value %s",
				      operands[i]);
		    return;
		}
		vm_immediate_value_t value =
		    string_to_long(operands[i] + 1, error);
		if (error->failed) {
		    return;
		}
		APPEND_VALUE(compiler, vm_immediate_value_t, value);
		break;
	    }
	    case OPERAND_STACK_OFFSET: {
		// Stack offsets are prefixed with '@'
		if (operands[i][0] != '@') {
		    SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
				      "Invalid stack offset %s", operands[i]);
		    return;
		}
		vm_stack_offset_t stack_offset =
		    string_to_long(operands[i] + 1, error);
		if (error->failed) {
		    return;
		}
		stack_offset += STACK_FRAME_HEADER_SIZE;
		APPEND_VALUE(compiler, vm_stack_offset_t, stack_offset);
		break;
	    }
	    case OPERAND_ARITY: {
		vm_arity_t arity = string_to_long(operands[i], error);
		if (error->failed) {
		    return;
		}
		APPEND_VALUE(compiler, vm_arity_t, arity);
		break;
	    }
	    case OPERAND_SYSTEM_CALL: {
		system_call_t system_call =
		    string_to_system_call(operands[i], error);
		if (error->failed) {
		    return;
		}
		APPEND_VALUE(compiler, vm_system_call_t, system_call);
		break;
	    }
	    case OPERAND_STRING: {
		// Strings are prefixed and suffixed with '"'
		if (operands_string[0] != '"' ||
		    operands_string[strlen(operands_string) - 1] != '"') {
		    SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
				      "Invalid string '%s'", operands_string);
		    return;
		}
		// Remove quotes
		char* naked_string = operands_string + 1;
		naked_string[strlen(operands_string) - 2] = '\0';
		vm_stack_value_t value =
		    static_data_insert_string(&compiler->static_data,
					      naked_string);
		LOG_DEBUG("Static data %u -> %s", value, naked_string);
		APPEND_VALUE(compiler, vm_stack_value_t, value);
		break;
	    }
        }
    }
    CLEAR_ERROR(error);
}

static void generate_bytecode(compiler_t* compiler, FILE* file,
                              satie_error_t* error) {
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
                SET_ERROR_MESSAGE(error, COMPONENT_COMPILER,
                                  "Wrong number of label operands");
                return;
            }
            vm_label_t label = string_to_long(operands[0], error);
            if (error->failed) {
                free(line);
                return;
            }
            LOG_DEBUG("Jump table insertion: %u -> %u", label,
                      compiler->bytecode_size);
            module_insert(&compiler->module, label, compiler->bytecode_size);
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
        append_bytes(compiler, 1, (uint8_t*)&opcode_info->opcode);

        // Add operand bytes to byte code
        append_operands(compiler, opcode_info, operands_string, operands,
			error);
        if (error->failed) {
            free(line);
            return;
        }
    }
    free(line);
    CLEAR_ERROR(error);
}

/*
static void pretty_print(compiler_t* compiler) {
    vm_address_t address = 0;
    while (address < compiler->bytecode_size) {
        fprintf(stderr, "%d: ", address);
        address += 1 + print_instruction(&compiler->bytecode[address],
                                         &compiler->static_data);
    }
}
*/

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
