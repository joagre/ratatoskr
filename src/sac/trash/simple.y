%token NUMBER SYMBOL
%token OPEN_BRACKET CLOSE_BRACKET OPEN_PAREN CLOSE_PAREN EQUAL COMMA
%left '+' '*'

%%

Program
    : TopLevelExpr EOF
    ;

TopLevelExpr
    : TopLevelExpr WS COMMA WS Binding
    | Binding
    | TopLevelExpr WS COMMA WS Expr
    | Expr
    ;

Binding
    : MatchPattern EQUAL Expr
    ;

MatchPattern
    : Literal
    | FieldAccess
    | SYMBOL
    ;

Expr
    : Add
    ;

Add
    : Add '+' Multiplicate
    | Multiplicate
    ;

Multiplicate
    : Multiplicate '*' Indexing
    | Indexing
    ;

Indexing
    : Symbol OPEN_BRACKET Expr CLOSE_BRACKET
    | FunctionCall
    ;

FunctionCall
    : SYMBOL OPEN_PAREN ExprSequence CLOSE_PAREN
    | FieldAccess
    ;

FieldAccess
    : HasField
    | FieldAccess '.' HasField
    ;

HasField
    : Literal
    | Indexing
    | FunctionCall
    | SYMBOL
    ;

Primary
    : Literal
    | SYMBOL
    ;

ExprSequence
    : Expr
    | ExprSequence COMMA Expr
    ;

Literal
    : NUMBER
    ;

Symbol
    : SYMBOL
    ;

%%

/* C code for lexer, auxiliary functions, etc. */
