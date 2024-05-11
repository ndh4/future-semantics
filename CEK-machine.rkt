#lang racket

(require redex)

(define-language PCEK
    (S ::= (M E K) error (f-let (p S) S))
    (M ::= V ; copied from `C-machine.rkt`
        (let (x V) M) ; Might need to remove this line
        (let (x (future M)) M)
        (let (x (car V)) M)
        (let (x (cdr V)) M)
        (let (x (if V M M)) M)
        (let (x (apply V V)) M)
        (let (x M) M))
    (E ::= ((x V) ...)) ; i think this is right
    (V ::= PValue Ph-Obj)
    (PValueUCirc ::= PValue ∘) ; i think this is needed for the `touch` definition
    (PValue ::= c x Cl Pair)
    (CL ::= ((λ (x) M) E))
    (Pair ::= (cons V V))
    (Ph-Obj ::= (ph p ∘) (ph p V))
    (K ::= ϵ ((ar x M E) K) ((ar† x M E) K))
    (F (x E ϵ) error)
    (A ::= c procedure (cons A A))
    #:binding-forms
    (λ (x) M #:refers-to x)
    (let (x V) M #:refers-to x)
    (let (x (future M_1)) M_2 #:refers-to x)
    (let (x (car V)) M #:refers-to x)
    (let (x (cdr V)) M #:refers-to x)
    (let (x (if V M_1 M_2)) M_3 #:refers-to x)
    (let (x (apply V_1 V_2)) M #:refers-to x)
    (let (x M_1) M_2 #:refers-to x))

(define-metafunction PCEK
    unload : V -> A
    
    [(unload c) c]
    [(unload ((λ (x) M) E)) procedure]
    [(unload (cons V_1 V_2))
        (cons
            (unload V_1)
            (unload V_2))]
    [(unload (ph p V)) V])

(define-metafunction PCEK
    touch V -> PValueUCirc ; this uses the `PValueUCirc` type above
    
    [(touch PValue) PValue] ; i think this is a better definition of the third `touch` rule
    [(touch (ph p ∘)) ∘]
    [(touch (ph p V)) V])
