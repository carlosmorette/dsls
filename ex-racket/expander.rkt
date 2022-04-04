#lang racket

(require (for-syntax
          syntax/parse
          racket/syntax))

(provide
 function-definition
 operation
 variable-definition
 print-function
 body-function
 (rename-out (ex-racket-program #%module-begin)))

(define-for-syntax (check-value value)
  (cond
    [(number? (syntax-e value)) (syntax-e value)]
    [(string? (syntax-e value)) (syntax-e value)]
    [else
     (make-identifier value)]))

(define-for-syntax (make-identifier value)
  (format-id value "~a" (format "~a" (syntax-e value))))

(define-for-syntax (make-head-function func-name parameters)
  (append (list func-name)
          (map (lambda (i)
                 (make-identifier i))
               parameters)))

(define-syntax (ex-racket-program stx)
  (syntax-parse stx
    [(_ (uerp ...))
     (syntax-parse #'(uerp ...)
       [(_ program ...)
        #'(#%module-begin
           program ...)])]))

(define-syntax (function-definition stx)
  (syntax-parse stx    
    [({~literal function-definition} "def" function-name _ _ _ _)
     (with-syntax ([name (make-identifier #'function-name)])
       #'(begin
           (define (name) (void))
           (provide name)))]

    [({~literal function-definition} "def" function-name _ params ... ")" _ body ... _)
     (with-syntax* ([name (make-identifier #'function-name)]
                    [name-and-params (make-head-function #'name (syntax->list #'(params ...)))])
       #'(begin
           (define name-and-params body ...)
           (provide name)))]
    
    [({~literal function-definition} "def" function-name _ _ _ body ... _)
     (with-syntax ([name (make-identifier #'function-name)])
       #'(begin
           (define (name) body ...)
           (provide name)))]))

(define-syntax (body-function stx)
  (syntax-parse stx
    [({~literal body-function} body ...) #'(begin body ...)]))

(define-syntax (operation stx)
  (syntax-parse stx
    [(operation n1 "+" n2)  
     (with-syntax ([nc1 (check-value #'n1)]  
                   [nc2 (check-value #'n2)])
       #'(+ nc1 nc2))]

    [(operation n1 "-" n2)  
     (with-syntax ([nc1 (check-value #'n1)]  
                   [nc2 (check-value #'n2)])
       #'(- nc1 nc2))]
    
    [(operation n1 "*" n2)  
     (with-syntax ([nc1 (check-value #'n1)]  
                   [nc2 (check-value #'n2)])
       #'(* nc1 nc2))]
    
    [(operation n1 "/" n2)  
     (with-syntax ([nc1 (check-value #'n1)]  
                   [nc2 (check-value #'n2)])
       #'(/ nc1 nc2))]))

(define-syntax (variable-definition stx)
  (syntax-parse stx
    [({~literal variable-definition} var-name "=" var-value)
     (with-syntax ([name (make-identifier #'var-name)]
                   [value (syntax-e #'var-value)])
       #'(begin
           (define name value)
           (provide name)))]))

(define-syntax (print-function stx)
  (syntax-parse stx
    [({~literal print-function} "print" _ value _)
     (with-syntax ([p-value (check-value #'value)])
       #'(displayln p-value))]))
