#lang brag

ex-racket-program: (function-definition | operation | variable-definition | print-function)*
function-definition: /"def" REFERENCE-VAR /"(" param? /")" /"do" body-function* /"end"
/param: REFERENCE-VAR (/"," REFERENCE-VAR)*
body-function: (operation | print-function)*
operation: (NUMBER | REFERENCE-VAR) NUMBER-OPERATOR (NUMBER | REFERENCE-VAR)
variable-definition: REFERENCE-VAR /"=" (NUMBER | STRING)
print-function: PRINT-FUNCTION /"(" (REFERENCE-VAR | NUMBER | STRING) /")"
