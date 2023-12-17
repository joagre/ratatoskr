grammar YourLanguage;

program : topLevelExpr EOF;

topLevelExpr : expr ;

expr: addExpr ;

addExpr : mulExpr (PLUS mulExpr)* ;
mulExpr : primary (MUL primary)* ;
primary : NUMBER | SYMBOL ;



// Parser rules
// program : topLevelExpr EOF;

// topLevelExpr : expr | binding;

// // Expressions with precedence
// expr : addExpr;
// addExpr : addExpr PLUS multExpr | multExpr;
// multExpr : multExpr MULT primaryExpr | primaryExpr;
// primaryExpr : functionCall | indexing | fieldAccess | literal | SYMBOL;

// // Specific expression forms
// functionCall : functionName LPAREN exprSequence? RPAREN;
// indexing : primaryExpr LSQUARE expr RSQUARE;
// fieldAccess : primaryExpr DOT primaryExpr;

// // Binding and matches
// binding : matchPattern EQUALS expr;
// matchPattern : literal | fieldAccess | SYMBOL;

// // Expression sequences
// exprSequence : expr (COMMA expr)*;

// // Primary expressions and literals
// primary : literal | SYMBOL;
// literal : NUMBER;

// // Symbols and function names
SYMBOL : [a-zA-Z_][a-zA-Z_0-9]*;
// functionName : SYMBOL;

// Lexer rules
NUMBER : [0-9]+;
PLUS : '+';
MULT : '*';
DOT : '.';
LSQUARE : '[';
RSQUARE : ']';
LPAREN : '(';
RPAREN : ')';
COMMA : ',';
EQUALS : '=';

// Whitespace and EOF handling
WS : [ \t\r\n]+ -> skip;
