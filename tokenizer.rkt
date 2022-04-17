#lang racket

(require brag/support)
(require br-parser-tools/lex)
(require br-parser-tools/lex-sre)

(provide tokenize)

(define-lex-abbrev reserved-terms
  (:or "def" "(" ")" "do" "end" "," "="))

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [(from/to "\"" "\"") (token 'STRING lexeme)]
     [(from/stop-before "#" "\n") (token 'COMMENT #:skip? #t)]
     [reserved-terms (token lexeme (string->symbol lexeme))]
     ["print" (token 'PRINT-FUNCTION lexeme)]
     [(repetition 1 +inf.0 alphabetic) (token 'REFERENCE-VAR lexeme)]
     [(repetition 1 +inf.0 numeric) (token 'NUMBER (string->number lexeme))]
     [(union "+" "-" "*" "/") (token 'NUMBER-OPERATOR lexeme)]
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof) (void)]))
  (define (next-token) (my-lexer ip))
  next-token)
