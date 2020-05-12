#lang plai

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; -Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWAE DefrdSub-> CFWAE-Value
(define (interp expr ds)
  (match expr
    [(id i) (lookup i ds)]
    [(num n) (numV n)]
    [(bool b) (boolV b)]
    [(op f args) (opf f (map (λ (v) (strict (interp v ds))) args))]
    [(iF expr then-expr else-expr) (if (boolV-b (strict (interp expr ds)))
                                       (interp then-expr ds)
                                       (interp else-expr ds))]
    [(fun params body) (closureV params body ds)]
    [(app fun-expr args)
     (let [(fun-val (strict (interp fun-expr ds)))]
       (interp
        (closureV-body fun-val)
        (create-env (closureV-params fun-val) args ds)))]))



;; -Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWAE
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()
           (error 'lookup "Identificador Libre")]
    [aSub (id value rest-ds)
          (if (symbol=? id name)
              value
              (lookup name rest-ds))]))


;; -Función auxiliar que actualiza todos los valores de los parámetros
;; formales de una función y los agrega al ambiente.
;; actualiza-ds: listofSymbol listofNum DefrdSub -> DefrdSub 
(define (create-env params args ds)
  (match params
    ['() ds]
    [(cons x xs)
     (if (empty? args)
         (error "Missing arguments")
         (create-env xs (cdr args) (aSub x (exprV (car args) ds) ds)))]))


;; -Función para verificar si una operacíon está en las operaciones aceptadas
;; list-contain: list -> Bool
(define (contiene? l e)
  (match l
    ['() #f]
    [(cons x xs) (if (equal? e x) #t (contiene? xs e))]))


;; -Función aplica una operación a los argumentos
;; opf: procedure -> list -> CFBAE/L-Value
(define (opf f l)
  (let ([result (apply f (map (λ (v) (match v
                                       [(? numV?) (numV-n v)]
                                       [(? boolV?) (boolV-b v)])) l))])
    (if (contiene? (list + - * /) f)
        (numV result)
        (boolV result))))


;; -Función auxiliar para evitar usar (require cond-strict) 
;; strict: CFwBAE/L-Value -> CFwBAE/L-Value
(define (strict e)
  (match e
    [(exprV expr env) (strict (interp expr env))]
    [else e]))
