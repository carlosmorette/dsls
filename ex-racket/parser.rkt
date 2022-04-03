#lang brag

ex-racket-program: (function-definition | operation | variable-definition)*
function-definition: DEF-FUNC REFERENCE-VAR LEFT-PAREN REFERENCE-VAR* RIGHT-PAREN DO-SCOPE operation* END-SCOPE
operation: (NUMBER | REFERENCE-VAR) NUMBER-OPERATOR (NUMBER | REFERENCE-VAR)
variable-definition: REFERENCE-VAR RECEIVE-OPERATOR (NUMBER | STRING)
