#lang racket

(require syntax/strip-context)
(require "main.rkt" "parser.rkt")

(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))

(define (my-read in)
  (syntax->datum
   (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (define parsed (interpret-program (parse (tokenize in))))
  (strip-context
   `(module ex-racket-mod racket
      ,@parsed
      )))
