#include <stdio.h>
#include "satie.h"

static const char* satie_error_type_to_string(satie_error_type_t type);
static const char* satie_component_to_string(satie_component_t component);

void satie_print_error(const satie_error_t *error) {
    if (error) {
        if (error->failed) {
            fprintf(stderr, "An error occurred...\n");
            fprintf(stderr, "Error Type: %s\n",
                    satie_error_type_to_string(GET_ERROR_TYPE(error)));
            fprintf(stderr, "Component: %s\n",
                    satie_component_to_string(GET_COMPONENT(error)));
            if (IS_ERROR_TYPE(error, ERROR_TYPE_CODE)) {
                fprintf(stderr, "Error Code: %d\n", error->code);
            }
            if (IS_ERROR_TYPE(error, ERROR_TYPE_MESSAGE)) {
                fprintf(stderr, "Error Message: %s\n", error->message);
            }
        } else {
            fprintf(stderr, "No error\n");
        }
    }
}

static const char* satie_error_type_to_string(satie_error_type_t type) {
    switch (type) {
    case ERROR_TYPE_NONE:
        return "None";
    case ERROR_TYPE_CODE:
        return "Code";
    case ERROR_TYPE_MESSAGE:
        return "Message";
    default:
        return "Unknown";
    }
}

static const char* satie_component_to_string(satie_component_t component) {
    switch (component) {
    case COMPONENT_GENERAL:
        return "General";
    case COMPONENT_VM:
        return "VM";
    case COMPONENT_LOADER:
        return "Loader";
    case COMPONENT_INTERPRETER:
        return "Interpreter";
    default:
        return "Unknown";
    }
}
