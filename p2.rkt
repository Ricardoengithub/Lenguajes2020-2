#lang plai

;; Función que recibe una lista y devuelve el conjunto potencia de los
;; elementos de dicha lista.
;; conjunto-potencia:(listof number) -> (listof (pairof number))


(define (cpAux e lista) (cond [(null? lista) #f]
                              [(equal? e (car lista)) #t]
                              [else (cpAux e (cdr lista))]))

(define (conjunto lista) (define listaa '()) (map (lambda (i)
                (cond [(equal? (cpAux i (cdr lista)) #f) (set! lista (cdr lista)) (set! listaa (append listaa (list i)))]
                      [else (set! listaa listaa) (set! lista (cdr lista))])
                )
       lista) listaa)

(define (conjunto-potencia lista) (define potencia '())(map (lambda (i)
                                        (map (lambda (j)
                                               (set! potencia (append potencia (list (list i j))))
                                               )
       (conjunto lista))
                )
       (conjunto lista)) potencia)

;; Función que calcula el cambio que tenemos que devovler según el
;; monto a cobrar y el monto pagado. Devuelve la cantidad de monedas de las
;; denominaciones $50, $20, $10, $5, $2, $1.
;; area-cono: number number -> (number number number number number number)
(define (cambioAux t p) (cond [(>= (- t p) 0) (+ 1 (cambioAux (- t p) p))]
                              [else 0]))


(define (cambio total pago) (define c (- pago total)) (define n 0)(map (lambda (i)
                (cond [(>= c 0) (set! n (cambioAux c i)) (set! c (- c (* i (cambioAux c i)))) n]
                      [else 0])
                )
       '(50 20 10 5 2 1)))





;; Función que calcula la descomposición en factores primos de un número
;; descomposicion-primos: number -> (listof (pairof number))

(define (divisor? m n) (cond [(equal? m 0) (error "Error m=0")]
                             [(> m n) #f]
                             [(< m n) (divisor? m (- n m))]
                             [(equal? n m) #t]
                             [(< n 0) #f]))


(define (longitud lista) (cond [(empty? lista) 0]
                               [else (+ 1 (longitud (cdr lista)))]))

(define (divisores n) (define divisoresList '()) (for ([i n ])
                        (if(divisor? (+ i 1) n) (set! divisoresList (append divisoresList (list (+ i 1)))) (set! divisoresList divisoresList))) divisoresList)

(define (numerosprimos n) (define primosList '()) (for ([i n])
                             (if (equal? (longitud (divisores (+ i 1))) 2) (set! primosList (append primosList (list (+ i 1))))
                                 (set! primosList primosList)                                 
                                 )) primosList)


(define (nprimosAux n i) (cond [(integer? (/ n i)) (+ 1 (nprimosAux (/ n i) i))]
                               [else 0]))



(define (descomposicion-primos n) (define lista '()) (map (lambda (i)
                (cond [(> (nprimosAux n i) 0) (set! lista (append lista (list (list i (nprimosAux n i)))))]
                      [else (set! lista lista)])
                )
       (numerosprimos n)) lista)




;; Función que recibe n, r y devuelve el conjunto de múltiplos n,
;; en el rango n y r.
;; multiplos: number number -> (listof number)


(define (multiplos n r) (define listaa '()) (map (lambda (i)
         (if(<= (* i n) r) (set! listaa (append listaa (list (* i n)))) (set! listaa listaa)))
       (build-list r values)) (cdr listaa))



;; Define un tipo abstracto de datos para crear figuras geométricas.
( define-type Figura
   [Circulo (diametro number?)]
   [Cuadrado (lado number?)]
   [Rectangulo (base number?) (altura number?)]
   [Triangulo (base number?) (altura number?)])

;; Función que recibe una figura y calcula su área.
;; perimetro: Figura -> number
(define (perimetro Figura) (cond [(Circulo? Figura) (* 3.1416 (Circulo-diametro Figura))]
                                 [(Cuadrado? Figura) (* (Cuadrado-lado Figura) 4)]
                                 [(Rectangulo? Figura) (+ (* (Rectangulo-base Figura) 2) (* (Rectangulo-altura Figura) 2))]
                                 [(Triangulo? Figura) (/ (* (Triangulo-base Figura) (Triangulo-altura Figura)) 2)]))

;; Función que recibe una figura y calcula su perímetro.
;; area Figura -> number
(define (area Figura) (cond [(Circulo? Figura) (* 3.1416 (* (/ (Circulo-diametro Figura) 2) (/ (Circulo-diametro Figura) 2)))]
                                 [(Cuadrado? Figura) (* (Cuadrado-lado Figura) (Cuadrado-lado Figura))]
                                 [(Rectangulo? Figura) (* (Rectangulo-base Figura) (Rectangulo-altura Figura))]
                                 [(Triangulo? Figura) (/ (* (Triangulo-base Figura) (Triangulo-altura Figura)) 2)]))

( define (any? a ) #t )

( define-type Arbol
   [vacio]
   [hoja (a any?)]
   [nodo (n number?) (t1 Arbol?) (t2 Arbol?)])

(define (agrega n arbol) (arbol))


(test (conjunto-potencia (list 1 2 3 3)) '((1 1)(1 2)(1 3)(2 1)(2 2)(2 3)(3 1)(3 2)(3 3)))
(test (cambio 124 200) '(1 1 0 1 0 1))
(test (descomposicion-primos 14175) '((3 4) (5 2) (7 1)))
(test (multiplos 73 1000) '(73 146 219 292 365 438 511 584 657 730 803 876 949))
(test (perimetro (Cuadrado 2.4)) 9.6)
(test (area (Circulo 4)) 12.566370614359172)
;;(test (agrega 6(agrega 4 (agrega 5 (agrega 3 vacio)))) (nodo 3 (vacio) (nodo 5 (hoja 4) (hoja 6))))
