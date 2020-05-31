#lang plai

(require (file "./grammars.rkt"))


;; -Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta WAE.
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> WAE
;; parse: s-expression -> WAE
(define (parse sexp)
  (match sexp
    [(? symbol?) (case sexp 
                   ['true (boolS #t)] 
                   ['false  (boolS #f)]
                   [else (idS sexp)])] 
    [(? number?) (numS sexp)] 
    [(list 'with bindings body) 
     (withS (listBindings bindings) (parse body))]
    [(list 'with* bindings body)
     (withS* (listBindings bindings) (parse body))]
    [(list 'fun (cons x xs) body) 
     (funS (cons x xs) (parse body))]
    [(list (list 'fun fun-params fun-body) params) 
     (appS (funS fun-params (parse fun-body)) (map parse params))]
    [(list 'if cond-expr then-expr else-expr) 
     (iFS (parse cond-expr) (parse then-expr) (parse else-expr))]
    [(cons 'cond conditions) 
     (condS (listConds conditions))]
    [(cons x xs) 
     (case x 
       [(+ - * / % min max pow not and or < > <=  >= = /=) (opS (map-op x) (map parse xs))]
       [(number? symbol?) (opS (map-type x) (map parse xs))]
       [else (appS (parse x) (map parse xs))])]))


;; -Función que crea una lista de Bindings
;; list list symbol -> list Binding
(define (listBindings bindings)
  (map (λ (v) (binding (first v) (map-type (second v)) (parse (second v)))) bindings))


;; -Función que maneja los resultados de las conditions.
;; list symbol -> list Condition
(define (listConds conditions)
  (let ([conditions-map (λ (c) (match c
                                 [(list 'else else-expr) (else-cond (parse else-expr))]
                                 [(list expr then-expr) (condition (parse expr) (parse then-expr))]))])
    (map (λ (v) (conditions-map v)) conditions)))


;; -Función auxiliar que hace un mapeo entre los símbolos
;; de función en sintaxis concreta y las funciones de Racket.
;; map-op: symbol -> procedure
(define (map-op sexp)
  (match sexp
    ['add1 add1]
    ['sub1 sub1]
    ['modulo modulo]
    ['expt expt]
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ['not not]
    ['< <]
    ['> >]
    ['<= <=]
    ['>= >=]
    ['= =]
    [else error "Función no definida."]))


(define (map-type symbol)
  (match symbol
    [(? symbol?) (booleanT)]
    [(? number?) (numberT)]
    [else error "Error de tipo."]
    ))




