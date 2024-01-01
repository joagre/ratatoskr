#include <stdio.h>
#include <string.h>
#include "satie_error.h"

// Used to store error messages (see satie.h)
char satie_error_message[MAX_ERROR_MESSAGE_SIZE];

static const char* error_type_strings[] = {
    FOREACH_ERROR_TYPE(GENERATE_STRING)
};

static const char* component_strings[] = {
    FOREACH_COMPONENT(GENERATE_STRING)
};

const char* satie_error_type_to_string(satie_error_type_t type) {
    return error_type_strings[type];
}

const char* satie_component_to_string(satie_component_t component) {
    return component_strings[component];
}

char* satie_error_to_string(satie_error_t *error, char* buf, size_t bufsiz) {
    if (error->failed) {
	const char* component = satie_component_to_string(GET_COMPONENT(error));
        switch (GET_ERROR_TYPE(error)) {
	    case ERROR_TYPE_NONE:
		snprintf(buf, bufsiz, "No error type specified (%s)",
			 component);
		break;
	    case ERROR_TYPE_CODE:
		snprintf(buf, bufsiz, "%d (%s)", error->code, component);
		break;
	    case ERROR_TYPE_ERRNO:
		snprintf(buf, bufsiz, "%s (%s)", strerror(error->errno_value),
			 component);
		break;
	    case ERROR_TYPE_MESSAGE:
		snprintf(buf, bufsiz, "%s (%s)", satie_error_message,
			 component);
		break;
	    default:
		snprintf(buf, bufsiz, "Unknown error type (%s)", component);
		break;
	}
    } else {
        snprintf(buf, bufsiz, "No error");
    }
    return buf;
}
