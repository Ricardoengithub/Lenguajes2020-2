#lang plai

;; Definición del tipo SCFWBAE
(define-type SCFWBAE/L
  [idS    (i symbol?)]
  [numS   (n number?)]
  [boolS  (b boolean?)]
  [iFS    (condicion SCFWBAE/L?) (then SCFWBAE/L?) (else SCFWBAE/L?)]
  [opS    (f procedure?) (args (listof SCFWBAE/L?))]
  [condS  (cases (listof Condition?))]
  [withS  (bindings (listof binding?)) (body SCFWBAE/L?)]
  [withS* (bindings (listof binding?)) (body SCFWBAE/L?)]
  [funS   (params (listof symbol?)) (body SCFWBAE/L?)]
  [appS   (fun SCFWBAE/L?) (args (listof SCFWBAE/L?))])

;; -Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value SCFWBAE/L?)])

;; -Definición del tipo CFWAE
(define-type CFWBAE/L
  [id   (i symbol?)]
  [num  (n number?)]
  [bool (b boolean?)]
  [iF   (condicion CFWBAE/L?) (then CFWBAE/L?) (else CFWBAE/L?)]
  [op   (f procedure?) (args (listof CFWBAE/L?))]
  [fun  (params (listof symbol?)) (body CFWBAE/L?)]
  [app  (fun CFWBAE/L?) (args (listof CFWBAE/L?))])

;; -Data-type que representa un caché de sustituciones
(define-type DefrdSub
   [mtSub]
   [aSub (name symbol?) (value CFWBAE/L-Value?) (env DefrdSub?)])

;; -Data-type que representa la sintaxis abstracta de CFWAE-Value
(define-type CFWBAE/L-Value
   [numV (n number?)]
   [boolV (b boolean?)]
   [closureV (params (listof symbol?)) (body CFWBAE/L?) (env DefrdSub?)]
   [exprV (expr CFWBAE/L?) (env DefrdSub?)])

;; -Data-type que representa condiciones.
(define-type Condition
   [condition (expr SCFWBAE/L?) (then-expr SCFWBAE/L?)]
   [else-cond (else-expr SCFWBAE/L?)])
