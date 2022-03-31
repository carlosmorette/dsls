#lang racket

(require (for-syntax
          syntax/parse
          racket/syntax))

(provide (rename-out (ex-racket-program #%module-begin)))

(provide function-definition operation)

(define-syntax (ex-racket-program stx)
  (syntax-parse stx
    [(_ (uerp ...))
     (syntax-parse #'(uerp ...)
       [(_ program ...)
        #'(#%module-begin
           program ...)])]))

(define-for-syntax (eval-operation operator n1 n2)
  (apply operator (list (syntax-e #'n1) (syntax-e #'n2))))

(define-syntax (operation stx)
  (syntax-parse stx
    [(operation n1 "+" n2)
     #'(+ (syntax-e #'n1) (syntax-e #'n2))]

    [(operation n1 "-" n2)
     #'(- (syntax-e #'n1) (syntax-e #'n2))]
    
    [(operation n1 "*" n2)
     #'(* (syntax-e #'n1) (syntax-e #'n2))]
    
    [(operation n1 "/" n2)
     #'(/ (syntax-e #'n1) (syntax-e #'n2))]))

(define-for-syntax (datum->identifier datum)
  (format-id datum "~a" (format "~a" (syntax->datum datum))))

(define-syntax (function-definition stx)
  (syntax-case stx ()
    [(function-definition _ function-name _ _ _ _)
     (with-syntax ([name (datum->identifier #'function-name)])
       #'(begin
           (define (name) (void))
           (provide name)))]
    
    [(function-definition _ function-name _ _ _ body ... _)
     (with-syntax ([name (datum->identifier #'function-name)])
       #'(begin
           (define (name) body ...)
           (provide name)))]))

;; provavelmente isso aqui vai mudar
(define-for-syntax (evaluate-parameters parameters-stx function-name)
  (displayln parameters-stx)
  (syntax-parse parameters-stx
    [({~literal parameter} param-name ...)
     (append (list function-name)
             (for/fold ([params (list)]) ([p (syntax->list #'(param-name ...))])
               (append params p)))]))

