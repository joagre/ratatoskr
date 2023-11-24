#include <stdio.h>
#include <stdlib.h>

typedef enum {
    TOKEN_EOF = 0,
    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_NUMBER,
    TOKEN_LPAREN,
    TOKEN_RPAREN
} TokenType;

typedef struct {
    TokenType type;
    int value;
} Token;

typedef struct {
    Token current_token;
    const char* input;
} ParserState;

int expression(ParserState* state, int rbp);
int get_precedence(TokenType type);

void advance(ParserState* state) {
    while (*state->input == ' ') {
        state->input++;
    }

    switch (*state->input) {
    case '\0':
        state->current_token.type = TOKEN_EOF;
        return;
    case '+':
        state->current_token.type = TOKEN_PLUS;
        break;
    case '-':
        state->current_token.type = TOKEN_MINUS;
        break;
    case '*':
        state->current_token.type = TOKEN_STAR;
        break;
    case '/':
        state->current_token.type = TOKEN_SLASH;
        break;
    case '(':
        state->current_token.type = TOKEN_LPAREN;
        break;
    case ')':
        state->current_token.type = TOKEN_RPAREN;
        break;
    default:
        if (*state->input >= '0' && *state->input <= '9') {
            state->current_token.type = TOKEN_NUMBER;
            state->current_token.value = 0;
            while (*state->input >= '0' && *state->input <= '9') {
                state->current_token.value =
                    state->current_token.value * 10 + (*state->input - '0');
                state->input++;
            }
            return;
        }
        printf("Unexpected character: %c\n", *state->input);
        exit(EXIT_FAILURE);
    }
    state->input++;
    printf("Next token: Type %d, Value %d\n", state->current_token.type, state->current_token.value);
}

// Function to match the current token
void match(ParserState* state, TokenType expected) {
    if (state->current_token.type == expected) {
        advance(state);
    } else {
        printf("Unexpected token: %d\n", state->current_token.type);
        exit(EXIT_FAILURE);
    }
}

int nud(Token token, ParserState* state) {
    printf("Processing NUD for token: Type %d, Value %d\n", state->current_token.type, state->current_token.value);
    int value;
    switch (state->current_token.type) {
    case TOKEN_NUMBER:
        value = state->current_token.value;
        match(state, TOKEN_NUMBER);
        return value;
    case TOKEN_LPAREN:
        match(state, TOKEN_LPAREN);
        value = expression(state, 0);
        match(state, TOKEN_RPAREN);
        return value;
    default:
        printf("Unexpected token in nud: %d\n", state->current_token.type);
        exit(EXIT_FAILURE);
    }
}

int led(ParserState* state, int left) {
    int right;
    TokenType operatorType = state->current_token.type;

    switch (operatorType) {
    case TOKEN_PLUS:
        match(state, TOKEN_PLUS);
        right = expression(state, get_precedence(TOKEN_PLUS));
        return left + right;
    case TOKEN_MINUS:
        match(state, TOKEN_MINUS);
        right = expression(state, get_precedence(TOKEN_MINUS));
        return left - right;
    case TOKEN_STAR:
        match(state, TOKEN_STAR);
        right = expression(state, get_precedence(TOKEN_STAR));
        return left * right;
    case TOKEN_SLASH:
        match(state, TOKEN_SLASH);
        right = expression(state, get_precedence(TOKEN_SLASH));
        if (right == 0) {
            printf("Division by zero.\n");
            exit(EXIT_FAILURE);
        }
        return left / right;
    default:
        printf("Unexpected token in led: %d\n", operatorType);
        exit(EXIT_FAILURE);
    }
}

int get_precedence(TokenType type) {
    switch (type) {
    case TOKEN_PLUS:
    case TOKEN_MINUS:
        return 10;
    case TOKEN_STAR:
    case TOKEN_SLASH:
        return 20;
    default:
        return -1;
    }
}

int expression(ParserState* state, int rbp) {
    printf("Entered expression with token: Type %d, Value %d\n", state->current_token.type, state->current_token.value);
    Token token = state->current_token;
    advance(state); // Advance to the next token before processing nud
    int left = nud(token, state); // Process the primary expression (numbers, parentheses)

    while (rbp < get_precedence(state->current_token.type)) {
        if (state->current_token.type == TOKEN_NUMBER) {
            printf("Error: Number not expected here.\n");
            exit(EXIT_FAILURE);
        }
        token = state->current_token;
        advance(state);
        left = led(state, left);
    }
    printf("Exiting expression with result: %d\n", left);
    return left;
}

int main() {
    const char* input = "1 + 2 * (3 + 4)";
    ParserState state = {.input = input};
    advance(&state);
    printf("First token: Type %d, Value %d\n", state.current_token.type, state.current_token.value);
    int result = expression(&state, 0);
    printf("Result: %d\n", result);
    return 0;
}
