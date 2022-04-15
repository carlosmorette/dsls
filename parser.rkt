#lang brag

ex-racket-program: (function-definition | operation | variable-definition | print-function | comment | invoke-function)*
function-definition: DEF-FUNC REFERENCE-VAR LEFT-PAREN REFERENCE-VAR* RIGHT-PAREN DO-SCOPE body-function* END-SCOPE
body-function: (operation | print-function)*
operation: (NUMBER | REFERENCE-VAR) NUMBER-OPERATOR (NUMBER | REFERENCE-VAR)
variable-definition: REFERENCE-VAR RECEIVE-OPERATOR (NUMBER | STRING)
print-function: PRINT-FUNCTION LEFT-PAREN (REFERENCE-VAR | NUMBER | STRING) RIGHT-PAREN
comment: START-COMMENT STRING
invoke-function: REFERENCE-VAR LEFT-PAREN (REFERENCE-VAR | STRING | NUMBER)* RIGHT-PAREN
