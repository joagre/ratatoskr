#include <assert.h>
#include "hm.h"

void add_type_variables(ast_node_t *node, symbol_table_t *symbol_table) {
    if (node->type == NAME) {
	node->type_variable = symbol_table_lookup(symbol_table, node->value);
	assert(node->type_variable != 0);
    }
}

/*


	}
	if (node->type == AST_IDENTIFIER) {






		node->type_variable = malloc(sizeof(type_variable_t));
		node->type_variable->type = TYPE_VARIABLE;
		node->type_variable->name = malloc(sizeof(char) * 10);
		sprintf(node->type_variable->name, "t%d", type_variable_counter++);
	}







    if (node->children != NULL) {
        for (uint16_t i = 0; i < dynarray_size(node->children); i++) {
            ast_node_t* child_node = dynarray_element(node->children, i);
            print_ast(child_node, level + 1);
        }
    }














    if (node == NULL) {
		return;
	}
	if (node->type == AST_IDENTIFIER) {
		node->type_variable = malloc(sizeof(type_variable_t));
		node->type_variable->type = TYPE_VARIABLE;
		node->type_variable->name = malloc(sizeof(char) * 10);
		sprintf(node->type_variable->name, "t%d", type_variable_counter++);
	}
	add_type_variables(node->left);
	add_type_variables(node->right);
}





if isinstance(node, ast.Identifier):
        # Identifier nodes are treated specially, as they have to refer to
        # previously defined identifiers in the symbol table.
        if node.name in symtab:
            node._type = symtab[node.name]
        else:
            raise TypingError('unbound name "{}"'.format(node.name))
    elif isinstance(node, ast.LambdaExpr):
        node._type = TypeVar(_get_fresh_typename())
        local_symtab = dict()
        for argname in node.argnames:
            typename = _get_fresh_typename()
            local_symtab[argname] = TypeVar(typename)
        node._arg_types = local_symtab
        assign_typenames(node.expr, {**symtab, **local_symtab})
    elif isinstance(node, ast.OpExpr):
        node._type = TypeVar(_get_fresh_typename())
        node.visit_children(lambda c: assign_typenames(c, symtab))
    elif isinstance(node, ast.IfExpr):
        node._type = TypeVar(_get_fresh_typename())
        node.visit_children(lambda c: assign_typenames(c, symtab))
    elif isinstance(node, ast.AppExpr):
        node._type = TypeVar(_get_fresh_typename())
        node.visit_children(lambda c: assign_typenames(c, symtab))
    elif isinstance(node, ast.IntConstant):
        node._type = IntType()
    elif isinstance(node, ast.BoolConstant):
        node._type = BoolType()
    else:
        raise TypingError('unknown node {}', type(node))
*/
