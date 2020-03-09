#lang plai

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




(test (multiplos 73 1000) '(73 146 219 292 365 438 511 584 657 730 803 876 949))
(test (perimetro (Cuadrado 2.4)) 9.6)
(test (area (Circulo 4)) 12.566370614359172)
(test (agrega 6(agrega 4 (agrega 5 (agrega 3 vacio)))) (nodo 3 (vacio) (nodo 5 (hoja 4) (hoja 6))))