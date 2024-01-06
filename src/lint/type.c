#include "type.h"

// Forward declarations of local functions (alphabetical order)
static type_variable_t next_type_variable(void);

type_t* type_new_basic_type(type_basic_type_t basic_type) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_BASIC_TYPE;
    type->basic_type = basic_type;
    return type;
}

type_t* type_new_variable(void) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_VARIABLE;
    type->variable = next_type_variable();
    return type;
}

type_t* type_new_function(types_t* arg_types, type_t* return_type) {
    type_t* type = malloc(sizeof(type_t));
    type->tag = TYPE_TAG_FUNCTION;
    type->function_type.arg_types = arg_types;
    type->function_type.return_type = return_type;
    return type;
}

//
// Local functions (alphabetical order)
//

static type_variable_t next_type_variable(void) {
    static type_variable_t next_type_variable = 0;
    return next_type_variable++;
}
