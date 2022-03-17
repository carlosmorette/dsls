#lang racket

(require brag/support)
(require br-parser-tools/lex)
(require syntax/parse)
(require "concat.rkt")

(provide interpret-concat-program interpret-concat-sequence)

;; input -> tokenize -> parse -> interpret

(define a-parsed-value
  (parse (list
          (token 'STRING "carlos ")
          (token 'CONCAT-SYMBOL "<>")
          (token 'STRING "morette"))))

;; Lexer/ Tokenize
(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     [(repetition 1 +inf.0 alphabetic) (token 'STRING lexeme)]
     ["<>" (token 'CONCAT-SYMBOL lexeme)]
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof) (void)]))
  (define (next-token) (my-lexer ip))
  next-token)

(provide tokenize)


(define (interpret-concat-program concat-stx)
  (syntax-parse concat-stx
    [({~literal concat-program} concat-sequences-stx ...)
     (for ([cs (syntax->list #'(concat-sequences-stx ...))])
       (print (interpret-concat-sequence cs))
       (newline))]))

(define (interpret-concat-sequence concat-sequence-stx)
  (syntax-parse concat-sequence-stx
    [({~literal concat-sequence} s1 _ s2)
     (string-append (syntax-e #'s1) (syntax-e #'s2))]))
