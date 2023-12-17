#include <stdio.h>
#include <string.h>
#include "satie_error.h"

// Used to store error messages (see satie.h)
char satie_error_message[MAX_ERROR_MESSAGE_SIZE];

char* satie_error_type_to_string(satie_error_type_t type) {
    switch (type) {
    case ERROR_TYPE_NONE:
        return "None";
    case ERROR_TYPE_CODE:
        return "Code";
    case ERROR_TYPE_ERRNO:
        return "Errno";
    case ERROR_TYPE_MESSAGE:
        return "Message";
    default:
        return "Unknown";
    }
}

char* satie_component_to_string(satie_component_t component) {
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

void satie_print_error(satie_error_t *error) {
    if (error->failed) {
        fprintf(stderr, "Error: ");
        switch (GET_ERROR_TYPE(error)) {
        case ERROR_TYPE_NONE:
            fprintf(stderr, "None\n");
            break;
        case ERROR_TYPE_CODE:
            fprintf(stderr, "%d\n", error->code);
            break;
        case ERROR_TYPE_ERRNO:
            fprintf(stderr, "%s\n", strerror(error->errno_value));
            break;
        case ERROR_TYPE_MESSAGE:
            fprintf(stderr, "%s\n", satie_error_message);
            break;
        default:
            fprintf(stderr, "Unknown\n");
            break;
        }
        fprintf(stderr, "Type: %s\n", satie_error_type_to_string(GET_ERROR_TYPE(error)));
        fprintf(stderr, "Component: %s\n", satie_component_to_string(GET_COMPONENT(error)));
    } else {
        fprintf(stderr, "No error\n");
    }
}
