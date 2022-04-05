#lang racket

(require syntax/strip-context)
(require "parser.rkt" "tokenizer.rkt")

(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))

(define (my-read in)
  (syntax->datum
   (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (define parsed (parse (tokenize in)))
  (strip-context
   (datum->syntax #f `(module ex-racket-module "expander.rkt"
                        ,parsed
                        ))))
