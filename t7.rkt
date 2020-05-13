#lang plai

 ( define ( fibo n )
 ( fibo-memo n ( make-hash ( list ( cons 0 1) ( cons 1 1)))))

 ;; fibo-memo : number hash-table → number
 ( define ( fibo-memo n tbl )
 (let ([ busqueda ( hash-ref! tbl n 'vacia )])
   ( cond [( equal? busqueda 'vacia )
           ( define nuevo
            (+ ( fibo-memo (- n 1) tbl ) ( fibo-memo (- n 2) tbl )))
           ( hash-set! tbl n nuevo ) nuevo]

          [ else busqueda ])))



( define tbl ( make-hash (list (cons 2 #t) )))

 ( define ( primo? n)
 (let ([ busqueda ( hash-ref! tbl n 'vacia )])
   ( cond [( equal? busqueda 'vacia )
           ( define nuevo
                   ( aux? n 2 ( sub1 n )))
           ( hash-set! tbl n nuevo ) nuevo]

          [ else busqueda ])))



 ( define ( aux? n i j)
 (let ([ busqueda ( hash-ref! tbl n 'vacia )])
   ( cond [( equal? busqueda 'vacia )
           ( define nuevo
                   ( cond
                          [( > i j ) #t ]
                          [( zero? ( modulo n i )) #f ]
                          [ else ( aux? n ( add1 i ) j )]))
           ( hash-set! tbl n nuevo ) nuevo]

          [ else busqueda ])))