#lang brag

ex-racket-program: function-definition* | operation*
function-definition: DEF-FUNC REFERENCE-VAR LEFT-PAREN parameter* RIGHT-PAREN DO-SCOPE (operation* | value) END-SCOPE
operation: NUMBER NUMBER-OPERATOR NUMBER
parameter: REFERENCE-VAR
value: STRING | NUMBER

