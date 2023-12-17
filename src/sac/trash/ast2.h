typedef enum {
    BINARY_OP_NODE,
    INTEGER_NODE,
} NodeType;

struct AstNode;

typedef struct {
    NodeType type;
    union {
        struct {
            char op;  // Operator (+, -, *, /, etc.)
            struct AstNode* left;
            struct AstNode* right;
        } binary_op;

        struct {
            int value;  // Integer value
        } integer;

        // Add more union members for other node types
    } data;
} AstNode;

// Create functions for constructing specific node types
AstNode* createBinaryOpNode(char op, AstNode* left, AstNode* right) {
    AstNode* node = malloc(sizeof(AstNode));
    node->type = BINARY_OP_NODE;
    node->data.binary_op.op = op;
    node->data.binary_op.left = left;
    node->data.binary_op.right = right;
    return node;
}

AstNode* createIntegerNode(int value) {
    AstNode* node = malloc(sizeof(AstNode));
    node->type = INTEGER_NODE;
    node->data.integer.value = value;
    return node;
}

// Free function for deallocating an AST node and its children
void freeAstNode(AstNode* node) {
    if (node == NULL) {
        return;
    }

    // Recursively free child nodes if applicable
    if (node->type == BINARY_OP_NODE) {
        freeAstNode(node->data.binary_op.left);
        freeAstNode(node->data.binary_op.right);
    }

    // Free the node itself
    free(node);
}
