grammar YourLanguage;

program : topLevelExpr EOF;

topLevelExpr : expr | binding;

expr : addExpr;
addExpr : addExpr PLUS multExpr | multExpr;
multExpr : multExpr MULT primary | primary;

primary : functionCall | indexing | fieldAccess | literal | SYMBOL;

functionCall : functionName LPAREN exprSequence? RPAREN;
exprSequence : expr (COMMA expr)*;
functionName : SYMBOL;
SYMBOL : [a-zA-Z_][a-zA-Z_0-9]*;

indexing : SYMBOL LSQUARE expr RSQUARE;

fieldAccess : SYMBOL DOT (SYMBOL | functionName);

literal : NUMBER;
NUMBER : [0-9]+;

binding : matchPattern EQUALS expr;
matchPattern : literal | fieldAccess | SYMBOL;


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
