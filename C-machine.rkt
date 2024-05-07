#lang racket

(require redex)

;; ??? Do we need to define keywords (let, future, car, cdr, if, apply, cons) somewhere else? A: No

(define-language Λa
  (S ::= M error)
  (M ::= V
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
         (let (x (future E)) M)
;; ??? Should error be a type of evaluation context?
         )
  (x y z ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ (x) M #:refers-to x)
  (let (x V) M #:refers-to x)
  (let (x M) M #:refers-to x)
  
;; ??? How do we deal with non-lambda binding forms? (let)
  
  ;#:binding-forms
  ;(let (x V) M #:refers-to x)
  )

(redex-match Λa (let (x M_1) M_2) (term (let (x 3) x)))
(redex-match Λa (let (x V) M_2) (term (let (x 3) x)))

(define -->c
  (reduction-relation
   Λa
   [--> (in-hole E (let (x V) M))
        (in-hole E (substitute M x V))
        bind]))

