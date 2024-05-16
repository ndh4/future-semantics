#lang racket

(require redex)

;; language definition

(define-language PCEK
    (S ::= (M E K) error (f-let (p S) S))
    (M N ::= V ; copied from `C-machine.rkt`
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
    (Cl ::= ((λ (x) M) E))
    (Pair ::= (cons V V))
    (Ph-Obj ::= (ph p ∘) (ph p V))
    (c ::= nil natural)
    (K ::= ϵ ((ar x M E) K) ((ar† x M E) K))
    (F (x E ϵ) error)
    (A ::= c procedure (cons A A))
    (p x y z ::= variable-not-otherwise-mentioned)
    #:binding-forms
    (λ (x) M #:refers-to x)
    (let (x V) M #:refers-to x)
    (let (x (future M_1)) M_2 #:refers-to x)
    (let (x (car V)) M #:refers-to x)
    (let (x (cdr V)) M #:refers-to x)
    (let (x (if V M_1 M_2)) M_3 #:refers-to x)
    (let (x (apply V_1 V_2)) M #:refers-to x)
    (let (x M_1) M_2 #:refers-to x))

;; paper meta-functions

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
    touch : V -> PValueUCirc ; this uses the `PValueUCirc` type above
    
    [(touch (ph p ∘)) ∘]
    [(touch (ph p V)) V]
    [(touch PValue) PValue]) ; i think this is a better definition of the third `touch` rule

;; environment meta-functions

(define-metafunction PCEK
    extend : x V E -> E
    
    [(extend x V ((x_1 V_1) ...)) ((x V) (x_1 V_1) ...)])

(define-metafunction PCEK
    lookup : x E -> V
    
    [(lookup x ((x V) (x_1 V_1) ...)) V]
    [(lookup x ((y V) (x_1 V_1) ...))
        (lookup x ((x_1 V_1) ...))])

;; placeholder substitution

; (define-metafunction PCEK
;     substitute-S : S 
    
;     )

;; side conditions

(define-metafunction PCEK
    is-not-cons-or-circ : V -> boolean
    
    [(is-not-cons-or-circ (cons V_1 V_2)) #false]
    [(is-not-cons-or-circ ∘) #false]
    [(is-not-cons-or-circ V) #true])

(define-metafunction PCEK
    is-not-nil-or-circ : V -> boolean
    
    [(is-not-nil-or-circ nil) #false]
    [(is-not-nil-or-circ ∘) #false]
    [(is-not-nil-or-circ V) #true])

(define-metafunction PCEK
    is-not-cl-or-circ : V -> boolean
    
    [(is-not-cl-or-circ Cl) #false]
    [(is-not-cl-or-circ ∘) #false]
    [(is-not-cl-or-circ V) #true])

;; judgment forms

(define-judgment-form PCEK
    #:mode (-> I O)
    #:contract (-> S S)

    [
        ---"bind-const"
        (->
            ((let (x c) M) E K)
            (M (extend x c E) K))]
    
    [
        ---"bind-var"
        (->
            ((let (x y) M) E K)
            (M (extend x (lookup y E) E) K))]
    
    [
        ---"bind-lam"
        (->
            ((let (x (λ (y) M)) N) E K)
            (N (extend x ((λ (y) M) E) E) K))]

    [
        ---"bind-cons"
        (->
            ((let (x (cons y z)) M) E K)
            (M (extend x (cons (lookup y E) (lookup z E)) E) K))]

    [
        ---"return"
        (->
            (x E_1 ((ar y M E_2) K))
            (M (extend y (lookup x E_1) E_1) K))]

    [
        (where (cons V_1 V_2) (lookup y E))
        ---"car"
        (->
            ((let (x (car y)) M) E K)
            (M (extend x V_1 E) K))]
    [
        (side-condition (is-not-cons-or-circ (lookup y E)))
        ---"car-error"
        (->
            ((let (x (car y)) M) E K)
            error)]

    [
        (where (cons V_1 V_2) (lookup y E))
        ---"cdr"
        (->
            ((let (x (cdr y)) M) E K)
            (M (extend x V_2 E) K))]
    [
        (side-condition (is-not-cons-or-circ (lookup y E)))
        ---"cdr-error"
        (->
            ((let (x (cdr y)) M) E K)
            error)]

    [
        (side-condition (is-not-nil-or-circ (lookup y E)))
        ---"if-true"
        (->
            ((let (x (if y M_1 M_2)) M_3) E K)
            (M_1 E ((ar x M_3 E) K)))]
    [
        (where nil (lookup y E))
        ---"if-false"
        (->
            ((let (x (if y M_1 M_2)) M_3) E K)
            (M_2 E ((ar x M_3 E) K)))]

    [
        (where ((λ (x) N) E_2) (lookup y E_1))
        ---"apply"
        (->
            ((let (x (apply y z)) M) E_1 K)
            (N (extend x (lookup z E_1) E_2) ((ar x M E_1) K)))]

    [
        (side-condition (is-not-cl-or-circ (lookup y E)))
        ---"apply-error"
        (->
            ((let (x (apply y z)) M) E K)
            error)]

    [
        ---"future"
        (->
            ((let (x (future N)) M) E K)
            (N E ((ar† x M E) K)))]
    
    [
        ---"future-id"
        (->
            (x E_1 ((ar† y M E_2) K))
            (M (extend y (lookup x E_1) E_2) K))]

    ; [---"fork"]

    ; [---"join"]

    [
        ---"join-error"
        (->
            (f-let (p error) S)
            error)]
    
    ; [---"lift"]
            )

;; load function

(define (load-PCEK p)
    (cond
        [(redex-match? PCEK M p)
            (term (,p () ϵ))]
        [else
            (raise (format "load-PCEK: expected a valid PCEK program, got: ~a" p))]))

; (traces
;     ->
;     (load-PCEK
;         (term
;             (let (x 3)
;                 (let (y 4)
;                     (cons x y))))))

; (traces
;     ->
;     (load-PCEK (term
;         (let (x 1)
;             (let (y 2)
;                 (let (z (cons x y))
;                     (let (w (car z))
;                         w)))))))

; (traces
;     ->
;     (load-PCEK (term
;         (let (x 1)
;             (let (y 2)
;                 (let (z (cons x y))
;                     (let (w (car x))
;                         w)))))))

; (traces
;     ->
;     (load-PCEK (term
;         (let (x 1)
;             (let (y 2)
;                 (let (z (cons x y))
;                     (let (w (cdr z))
;                         w)))))))

; (traces
;     ->
;     (load-PCEK (term
;         (let (x 1)
;             (let (y 2)
;                 (let (z (cons x y))
;                     (let (w (cdr x))
;                         w)))))))

; (traces
;     ->
;     (load-PCEK (term
;         (let (x nil)
;             (let (y (if x 1 2))
;                 y)))))

; (traces
;     ->
;     (load-PCEK (term
;         (let (x 2)
;             (let (y (if x 1 2))
;                 y)))))
