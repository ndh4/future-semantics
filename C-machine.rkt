#lang racket

(require redex)

(define-language Λa
  (S ::= M error)
  (M N ::= V
        (let (x V) M) ; Might need to remove this line
        (let (x (future M)) M)
        (let (x (car V)) M)
        (let (x (cdr V)) M)
        (let (x (if V M M)) M)
        (let (x (apply V V)) M)
        (let (x M) M))
  (V ::= c x (λ (x) M) (cons x y))
  (F ::= V error)
  (A ::= c procedure (cons A A))
  (c ::= nil natural)
  (E ::= hole
         (let (x E) M)
         (let (x (future E)) M))
  (x y z ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ (x) M #:refers-to x)
  (let (x V) M #:refers-to x)
  (let (x (future M_1)) M_2 #:refers-to x)
  (let (x (car V)) M #:refers-to x)
  (let (x (cdr V)) M #:refers-to x)
  (let (x (if V M_1 M_2)) M_3 #:refers-to x)
  (let (x (apply V_1 V_2)) M #:refers-to x)
  (let (x M_1) M_2 #:refers-to x)
  )

(redex-match Λa (let (x M_1) M_2) (term (let (x 3) x)))
(redex-match Λa (let (x V) M_2) (term (let (x 3) x)))

(define -->c
  (reduction-relation
   Λa
   [--> (in-hole E (let (x V) M))
        (in-hole E (substitute M x V))
        bind]
   [--> (in-hole E (let (x (future V)) M))
        (in-hole E (substitute M x V))
        future-id]
   
   [--> (in-hole E (let (x (car (cons V_1 V_2))) M))
        (in-hole E (substitute M x V_1))
        car]
   [--> (in-hole E (let (x (car V)) M))
        error
        (side-condition
         (not (redex-match? Λa (cons V_1 V_2) (term V))))
        car-error]
   
   [--> (in-hole E (let (x (cdr (cons V_1 V_2))) M))
        (in-hole E (substitute M x V_2))
        cdr]
   [--> (in-hole E (let (x (cdr V)) M))
        error
        (side-condition
         (not (redex-match? Λa (cons V_1 V_2) (term V))))
        cdr-error]

   [--> (in-hole E (let (x (if V M_1 M_2)) M))
        (in-hole E (let (x M_1) M))
        (side-condition
         (not (redex-match? Λa nil (term V))))
        if-first]
   [--> (in-hole E (let (x (if nil M_1 M_2)) M))
        (in-hole E (let (x M_2) M))
        if-second]
   
   [--> (in-hole E (let (x (apply (λ (y) N) V_2)) M))
        (in-hole E (let (x (substitute N y V_2)) M))
        apply]
   [--> (in-hole E (let (x (apply V_1 V_2)) M))
        error
        (side-condition
         (not (redex-match? Λa (λ (y) N) (term V_1))))
        apply-error]))

(define (load-C p)
  (cond
    [(redex-match? Λa M p) (term (term (,p hole)))]
    [else (raise "load: expected a valid Λa program")]))

(define-metafunction Λa unload : s -> A
  [(unload c) c]
  [(unload (λ (x) M)) procedure]
  [(unload (cons V_1 V_2)) (cons (unload V_1) (unload V_2))])

