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


(define-syntax (operation stx) 
  (syntax-parse stx
    [(operation n1 "+" n2)  
     (with-syntax ([nc1 (check-operation-value #'n1)]  
                   [nc2 (check-operation-value #'n2)])
       #'(+ nc1 nc2))]

    [(operation n1 "-" n2)  
     (with-syntax ([nc1 (check-operation-value #'n1)]  
                   [nc2 (check-operation-value #'n2)])
       #'(- nc1 nc2))]
    
    [(operation n1 "*" n2)  
     (with-syntax ([nc1 (check-operation-value #'n1)]  
                   [nc2 (check-operation-value #'n2)])
       #'(* nc1 nc2))]
    
    [(operation n1 "/" n2)  
     (with-syntax ([nc1 (check-operation-value #'n1)]  
                   [nc2 (check-operation-value #'n2)])
       #'(/ nc1 nc2))]))

(define-for-syntax (check-operation-value value)
  (let ([r (syntax-e value)])
    (if (number? r)
        r
        (make-identifier value))))

(define-for-syntax (make-identifier value)
  (format-id value "~a" (format "~a" (syntax-e value))))

(define-for-syntax (make-head-function func-name parameters)
  (append (list func-name)
          (map (lambda (i)
                 (make-identifier i))
               parameters)))

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

