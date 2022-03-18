#lang brag

function-definition: DEF-FUNC REFERENCE-VAR LEFT-PAREN parameter* RIGHT-PAREN DO-SCOPE operation* END-SCOPE
operation: NUMBER NUMBER-OPERATOR NUMBER
parameter: REFERENCE-VAR

