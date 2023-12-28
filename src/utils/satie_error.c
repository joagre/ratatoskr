#include <stdio.h>
#include <string.h>
#include "satie_error.h"

// Used to store error messages (see satie.h)
char satie_error_message[MAX_ERROR_MESSAGE_SIZE];

char* satie_error_type_to_string(satie_error_type_t type) {
    switch (type) {
	case ERROR_TYPE_NONE:
	    return "none";
	case ERROR_TYPE_CODE:
	    return "code";
	case ERROR_TYPE_ERRNO:
	    return "errno";
	case ERROR_TYPE_MESSAGE:
	    return "message";
	default:
	    return "unknown";
    }
}

char* satie_component_to_string(satie_component_t component) {
    switch (component) {
	case COMPONENT_GENERAL:
	    return "general";
	case COMPONENT_VM:
	    return "vm";
	case COMPONENT_LOADER:
	    return "loader";
	case COMPONENT_INTERPRETER:
	    return "interpreter";
	case COMPONENT_COMPILER:
	    return "compiler";
	case COMPONENT_SCHEDULER:
	    return "scheduler";
	default:
	    return "unknown";
    }
}

char* satie_error_to_string(satie_error_t *error, char* buf, size_t bufsiz) {
    if (error->failed) {
        switch (GET_ERROR_TYPE(error)) {
	    case ERROR_TYPE_NONE:
		snprintf(buf, bufsiz, "Not specified (%s)",
			 satie_component_to_string(GET_COMPONENT(error)));
		break;
	    case ERROR_TYPE_CODE:
		snprintf(buf, bufsiz, "%d (%s)", error->code,
			 satie_component_to_string(GET_COMPONENT(error)));
		break;
	    case ERROR_TYPE_ERRNO:
		snprintf(buf, bufsiz, "%s (%s)", strerror(error->errno_value),
			 satie_component_to_string(GET_COMPONENT(error)));
		break;
	    case ERROR_TYPE_MESSAGE:
		snprintf(buf, bufsiz, "%s (%s)", satie_error_message,
			 satie_component_to_string(GET_COMPONENT(error)));
		break;
	    default:
		fprintf(stderr, "Unknown (%s)",
			satie_component_to_string(GET_COMPONENT(error)));
		break;
        }
    } else {
        snprintf(buf, bufsiz, "No error");
    }
    return buf;
}
