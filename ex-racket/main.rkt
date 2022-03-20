#lang racket

(require brag/support)
(require br-parser-tools/lex)
(require br-parser-tools/lex-sre)
(require syntax/parse)
(require "parser.rkt")

(provide tokenize interpret-program)

(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos
     ["def" (token 'DEF-FUNC lexeme)]
     ["(" (token 'LEFT-PAREN lexeme)]
     [")" (token 'RIGHT-PAREN lexeme)]
     ["do" (token 'DO-SCOPE lexeme)]
     ["end" (token 'END-SCOPE lexeme)]
     [(:or (from/to "\"" "\"") (from/to "'" "'")) (token 'STRING lexeme)]
     [(repetition 1 +inf.0 alphabetic) (token 'REFERENCE-VAR lexeme)]
     [numeric (token 'NUMBER (string->number lexeme))]
     [(union "+" "-" "*" "/") (token 'NUMBER-OPERATOR lexeme)]
     [whitespace (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof) (void)]))
  (define (next-token) (my-lexer ip))
  next-token)

(define (interpret-program stx)
  (syntax-parse stx
    [({~literal ex-racket-program} program ...)
     (for/fold ([acc-program (list)]) ([erp (syntax->list #'(program ...))])
       (append acc-program (list (interpret-racket-program erp))))]))

(define (interpret-racket-program stx)
  (syntax-parse stx
    [({~literal operation} op ...)
     (evaluate-operation #'(op ...))]
    
    [({~literal function-definition} func-d ...)
     (evaluate-function-definition #'(func-d ...))]))

(define (evaluate-operation stx)
  (syntax-parse stx
    [(n1 "+" n2)
     #'(+ n1 n2)]
    
    [(n1 "-" n2)
     #'(- n1 n2)]
    
    [(n1 "*" n2)
     #'(* n1 n2)]

    [(n1 "/" n2)
     #'(/ n1 n2)]))

(define (evaluate-function-definition stx)
  (syntax-parse stx
    [(_ function-name _ parameters ... _ _ body ... _)
     #'(define
         (evaluate-parameters #'(parameters ...) #'function-name)
         (evaluate-operation #'(body ...)))]
    
    [(_ function-name _ parameter _ _ _)
     (print "NO BODY")]))

(define (evaluate-parameters parameters-stx function-name)
  (displayln parameters-stx)
  (syntax-parse parameters-stx
    [({~literal parameter} param-name ...)
     (append (list function-name)
             (for/fold ([params (list)]) ([p (syntax->list #'(param-name ...))])
               (append params p)))]))

(provide evaluate-parameters)
