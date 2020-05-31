#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
;; (define (desugar sexpr))
(define (desugar expr)
  (match expr
    [(idS i) (id i)]
    [(numS n) (num n)]
    [(boolS b) (bool b)]
    [(opS f args) (op f (map desugar args))]
    [(iFS expr then-expr else-expr) (iF (desugar expr) (desugar then-expr) (desugar else-expr))]
    [(condS (cons x xs)) (match x
                           [(condition expr then-expr) (desugar (iFS expr then-expr (condS xs)))]
                           [(else-cond else-expr)  (desugar else-expr)])]
    [(withS bindings body) (app (fun (map binding-id bindings) (desugar body)) (map (λ (v) (desugar (binding-value v))) bindings))]
    [(withS* (cons x xs) body) (desugar (withS (list x) (if (empty? xs) body (withS* xs body))))]
    [(funS params body) (fun params (desugar body))]
    [(appS fun-expr args) (app (desugar fun-expr) (map desugar args))]))