#lang racket

(require brag/support)
(require br-parser-tools/lex)
(require br-parser-tools/lex-sre)
(require "parser.rkt")

(provide tokenize)

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     ["def" (token 'DEF-FUNC lexeme)]
     ["(" (token 'LEFT-PAREN lexeme)]
     [")" (token 'RIGHT-PAREN lexeme)]
     ["do" (token 'DO-SCOPE lexeme)]
     ["end" (token 'END-SCOPE lexeme)]
     [(repetition 1 +inf.0 alphabetic) (token 'REFERENCE-VAR lexeme)]
     [numeric (token 'NUMBER (string->number lexeme))]
     [(union "+" "-" "*" "/") (token 'NUMBER-OPERATOR lexeme)]
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof) (void)]))
  (define (next-token) (my-lexer ip))
  next-token)
