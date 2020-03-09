#lang plai

;; Funcion que calcula el area de un cono de base circular
;; area-cono: number number -> number
(define (area-cono d g) (+ (* 3.14159 (* (/ d 2) g)) (* 3.14159 (* (/ d 2) (/ d 2)))))


;; Funcion que eleva el numero a, a la potencia b
;; potencia: number number
(define (potencia a b) (cond [(equal? b 0) 1]
                             [(neg? b) (/ (potencia a (+ b 1)) a)]
                             [else (* a (potencia a (- b 1)))]))


;; Funcion que calcula la distancia entre dos puntos en el plano
;; distancia: (pairof number) (pairof number) -> number
(define (distancia p q) (sqrt (+ (potencia (- (list-ref q 0) (list-ref p 0)) 2) (potencia (- (list-ref q 1) (list-ref p 1)) 2))))


;; Predicado que nos dice si un numero es negativo
;; neg?: number -> Boolean

(define (neg? a) (cond [(> a -1) #f]
                       [else #t]))


;; Funcion que nos devuelve el valor absoluto de un nÃ o mero
;; absoluto: number -> number

(define (absoluto n) (cond [(equal? (neg? n) #t) (* n -1)]
                           [else n]))


;; Predicado que nos dice si un numero m es divisor de otro numero n
;; divisor?: number number -> number
(define (divisor? m n) (cond [(equal? m 0) (error "Error m=0")]
                             [(> m n) #f]
                             [(< m n) (divisor? m (- n m))]
                             [(equal? n m) #t]
                             [(< n 0) #f]))


;; Funcion que nos da la longitud de una lista
;; longitud: (listof a) -> number

(define (longitud lista) (cond [(empty? lista) 0]
                               [else (+ 1 (longitud (cdr lista)))]))


;; Funcion que nos da el elemento maximo de una lista
;; maximo: (listof a) -> number



(define (maximo lista) (define maxi 0) (map (lambda (i)
         (if(>= i maxi) (set! maxi i) (set! maxi maxi)))
       lista) maxi)

;; Funcion que nos da una lista invertida de la lista pasada como parametro
;; reversa-lista: (listof a) -> (listof a)



(define (reversa-lista lista) (define reversa '()) (map (lambda (i)
         (set! reversa (append (list i) reversa)))
       lista) reversa)


;; Predicado que nos dice si una lista contiene elementos que forman un palindromo
;; palindromo-lista?: (listof a) -> Boolean


(define (palindromo? lista) (cond [(equal? lista (reversa-lista lista)) #t]
                                  [else #f]))

;; Funcion que nos da el una lista de divisores de un numero pasado como parametro
;; divisores: number -> (listof number)



(define (divisores n) (define divisoresList '()) (for ([i n ])
                        (if(divisor? (+ i 1) n) (set! divisoresList (append divisoresList (list (+ i 1)))) (set! divisoresList divisoresList))) divisoresList)



(test (area-cono 10 15) 314.1592653589793)
(test (potencia 2 -3) 1/8)
(test (distancia '(2 -2) '(5 5)) 7.615773105863909)
(test (neg? -3) #t)
(test (absoluto -5) 5)
(test (divisor? 3 81) #t)
(test (longitud (list 1 "a" "Ejemplo chafa" 3.4 absoluto)) 5)
(test (maximo '(-10 3 4 1 0.0 7.99 -5)) 7.99)
(test (reversa-lista (list 1 2 3 4)) '(4 3 2 1))
(test (palindromo? '(1 "hola" 1)) #t)
(test (divisores 65) '(1 5 13 65))
