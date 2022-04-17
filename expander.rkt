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
 ex-racket-program
 #%module-begin)

(define-for-syntax (eval-value value)
  (let ([rv (syntax-e value)])
    (cond
      [(number? rv) rv]
      [(regexp-match #rx"\".*\"" rv) rv]
      [else (make-identifier value)])))

(define-for-syntax (eval-param-list params-list)
  (map (lambda (i)
         (eval-value i)) params-list))

(define-for-syntax (make-identifier value)
  (format-id value "~a" (format "~a" (syntax-e value))))

(define-for-syntax (make-head-function func-name parameters)
  (append (list func-name)
          (map (lambda (i)
                 (make-identifier i))
               parameters)))

(define-syntax (ex-racket-program stx)
  (syntax-parse stx
    [({~literal ex-racket-program} program ...)
     #'(begin
         program ...)]))

(define-syntax (function-definition stx)
  (syntax-parse stx    
    [({~literal function-definition} function-name)
     (with-syntax ([name (make-identifier #'function-name)])
       #'(begin
           (define (name) (void))
           (provide name)))]

    [({~literal function-definition} function-name (params ...) body ...)
     (with-syntax* ([name (make-identifier #'function-name)]
                    [name-and-params (make-head-function #'name (syntax->list #'(params ...)))])
       #'(begin
           (define name-and-params body ...)
           (provide name)))]
    
    [({~literal function-definition} function-name body ...)
     (with-syntax ([name (make-identifier #'function-name)])
       #'(begin
           (define (name) body ...)
           (provide name)))]))

(define-syntax (body-function stx)
  (syntax-parse stx
    [({~literal body-function} body ...)
     #'(begin body ...)]))

(define-syntax (operation stx)
  (syntax-parse stx
    [(operation n1 "+" n2)  
     (with-syntax ([nc1 (eval-value #'n1)]  
                   [nc2 (eval-value #'n2)])
       #'(+ nc1 nc2))]

    [(operation n1 "-" n2)  
     (with-syntax ([nc1 (eval-value #'n1)]  
                   [nc2 (eval-value #'n2)])
       #'(- nc1 nc2))]
    
    [(operation n1 "*" n2)  
     (with-syntax ([nc1 (eval-value #'n1)]  
                   [nc2 (eval-value #'n2)])
       #'(* nc1 nc2))]
    
    [(operation n1 "/" n2)  
     (with-syntax ([nc1 (eval-value #'n1)]  
                   [nc2 (eval-value #'n2)])
       #'(/ nc1 nc2))]))

(define-syntax (variable-definition stx)
  (syntax-parse stx
    [({~literal variable-definition} var-name var-value)
     (with-syntax ([name (make-identifier #'var-name)]
                   [value (syntax-e #'var-value)])
       #'(begin
           (define name value)
           (provide name)))]))

(define-syntax (print-function stx)
  (syntax-parse stx
    [({~literal print-function} "print" value)
     (with-syntax ([p-value (eval-value #'value)])
       #'(displayln p-value))]))

