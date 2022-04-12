#lang racket

(require brag/support)
(require br-parser-tools/lex)
(require br-parser-tools/lex-sre)

(provide tokenize)

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [(from/to "\"" "\"") (token 'STRING lexeme)]
     ["def" (token 'DEF-FUNC lexeme)]
     ["(" (token 'LEFT-PAREN lexeme)]
     [")" (token 'RIGHT-PAREN lexeme)]
     ["do" (token 'DO-SCOPE lexeme)]
     ["end" (token 'END-SCOPE lexeme)]
     ["=" (token 'RECEIVE-OPERATOR lexeme)]
     ["print" (token 'PRINT-FUNCTION lexeme)]
     ["#" (token 'START-COMMENT lexeme)]
     [(repetition 1 +inf.0 alphabetic) (token 'REFERENCE-VAR lexeme)]
     [(repetition 1 +inf.0 numeric) (token 'NUMBER (string->number lexeme))]
     [(union "+" "-" "*" "/") (token 'NUMBER-OPERATOR lexeme)]
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof) (void)]))
  (define (next-token) (my-lexer ip))
  next-token)
