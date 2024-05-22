#lang racket

(require redex)

;; language definition

(define-language PCEK
    (S ::= (M E K) error (f-let (p S) S))
    (M N ::= ; copied from `C-machine.rkt`
        x
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
    (K ::= ϵ (kappa K))
    (kappa ::= (ar x M E) (ar† x M E))
    (F ::= (x E ϵ) error)
    (A ::= c procedure (cons A A))
    (p ::= number)
    (x y z ::= variable-not-otherwise-mentioned)
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
    unload-state : F -> A
    
    [(unload-state error) error]
    [(unload-state (x E K))
        (unload (lookup x E))])

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

(define-metafunction PCEK
    substitute-S : p V S -> S
    
    [(substitute-S p V (f-let (p S_1) S_2))
        (f-let (p (substitute-S p V S_1)) S_2)]
    [(substitute-S p_1 V (f-let (p_2 S_1) S_2))
        (f-let (p_2 (substitute-S p V S_1)) (substitute-S p V S_2))]
        
    [(substitute-S p V (M E K)) (M (substitute-E p V E) (substitute-K p V E))]

    [(substitute-S p V error) error])

(define-metafunction PCEK
    substitute-E : p V E -> E
    
    [(substitute-E p V ϵ) ϵ]
    
    [(substitute-E p V E)
        (ph p V)
        (where (ph p ∘) (lookup x E))
        or
        (lookup x E)])

(define-metafunction PCEK
    substitute-K : p V K -> K

    [(substitute-K p V ((ar x M E) K))
        ((ar x M (substitute-E p V E)) (substitute-K p V K))]

    [(substitute-K p V ((ar† x M E) K))
        ((ar† x M (substitute-E p V E)) (substitute-K p V K))])

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

(define-metafunction PCEK
    not-in-FP : p S -> boolean
    
    [(not-in-FP p S)
        (not (exists-in-FP p (FP S)))])

(define-metafunction PCEK
    exists-in-FP : p (p ...) -> boolean
    
    [(exists-in-FP p (p p_1 ...)) #true]
    [(exists-in-FP p ()) #false]
    [(exists-in-FP p (p_1 p_2 ...))
        (exists-in-FP p (p_2 ...))])

(define-metafunction PCEK
    FP : S -> (p ...)
    
    [(FP error) ()]
    [(FP (f-let (p S_1) S_2))
        (union-FP
            (FP S_1)
            (minus-FP
                p
                (FP S_2)))]
    [(FP (M E K))
        (union-FP
            (FP-M M E)
            (FP-K K))])

(define-metafunction PCEK
    union-FP : (p ...) (p ...) -> (p ...)
    
    [(union-FP (p ...) ()) (p ...)]
    [(union-FP (p_1 ...) (p_2 p_3 ...))
        (union-FP (p_1 ... p_2) (p_3 ...))])

(define-metafunction PCEK
    minus-FP : p (p ...) -> (p ...)
    
    [(minus-FP p ()) ()]
    [(minus-FP p (p_1 ... p p_2 ...)) (p_1 ... p_2 ...)]
    [(minus-FP p (p_1 ...)) (p_1 ...)])

(define-metafunction PCEK
    FP-M : M E -> (p ...)
    
    [(FP-M x E) (FP-V (lookup x E))]
    [(FP-M (let (x V) M))
        (union-FP
            (FP-V V)
            (FP-M M))]
    [(FP-M (let (x (future M_1)) M_2))
        (union-FP
            (FP-M M_1)
            (FP-M M_2))]
    [(FP-M (let (x (car V)) M))
        (union-FP
            (FP-V V)
            (FP-M M))]
    [(FP-M (let (x (cdr V)) M))
        (union-FP
            (FP-V V)
            (FP-M M))]
    [(FP-M (let (x (if V M_1 M_2)) M_3))
        (union-FP
            (FP-V V)
            (FP-M M_3))]
    [(FP-M (let (x (apply V_1 V_2)) M))
        (union-FP
            (union-FP
                (FP-V V_1)
                (FP-V V_2))
            (FP-M M))]
    [(FP-M (let (x M) M))
        (union-FP
            (FP-V V)
            (FP-M M))])

(define-metafunction PCEK
    FP-V : V -> (p ...)
    
    [(FP-V PValueUCirc) ()]
    [(FP-V Ph-Obj)
        (FP-Ph-Obj Ph-Obj)])

(define-metafunction PCEK
    FP-Ph-Obj : Ph-Obj -> (p ...)
    
    [(FP-Ph-Obj (ph p ∘)) (p)]
    [(FP-Ph-Obj (ph p V))
        (union-FP
            (FP-V V)
            (p))])

(define-metafunction PCEK
    FP-K : K -> (p... )
    
    [(FP-K ϵ) ()]
    [(FP-K (kappa K))
        (union-FP
            (FP-kappa kappa)
            (FP-K K))])

(define-metafunction PCEK
    FP-kappa : kappa -> (p ...)
    
    [(FP-kappa (ar† x M E))
        (union-FP
            (FP-V (lookup x E))
            (FP-M M E))]
    [(FP-kappa (ar x M E))
        (union-FP
            (FP-V (lookup x E))
            (FP-M M E))])

(define-metafunction PCEK
    placeholder-not-in-FP : (p ...) -> p
    
    [(placeholder-not-in-FP ()) 0]
    [(placeholder-not-in-FP (p ...))
        ,(+ (apply max (term (p ...))) 1)])

(define-metafunction PCEK
    maximum-in-FP : p (p ...) -> p
    
    [(maximum-in-FP p ()) p]
    [(maximum-in-FP p_1 (p_2 p_3 ...))
        (maximum-in-FP p_2 (p_3 ...))
        (side-condition (> (term p_2) (term p_1)))
        
        or
        
        (maximum-in-FP p_1 (p_3 ...))])

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

    [
        (where p (placeholder-not-in-FP (E_2 K_2)))
        ---"fork"
        (->
            (M E_1 (kappa_1 ((ar† x N E_2) K_2)))
            (f-let
                (p (M E_1 (kappa_1 ϵ)))
                (N (extend x (ph p ∘) E_2) K_2)))]

    [
        ---"join"
        (->
            (f-let (p (x E ϵ)) S)
            (substitute-S p (lookup x E) S))]

    [
        ---"join-error"
        (->
            (f-let (p error) S)
            error)]
    
    [
        (side-condition (not-in-FP p_1 S_2))
        ---"lift"
        (->
            (f-let (p_2 (f-let (p_1 S_1) S_2)) S_3)
            (f-let (p_1 S_1) (f-let (p_2 S_2) S_3)))]

    [
        (-> S_1 S_3)
        (-> S_2 S_4)
        ---"parallel-both"
        (->
            (f-let (p S_1) S_2)
            (f-let (p S_3) S_4))]

    [
        (-> S_1 S_3)
        ---"parallel-inner"
        (->
            (f-let (p S_1) S_2)
            (f-let (p S_3) S_2))]

    [
        (-> S_2 S_4)
        ---"parallel-outer"
        (->
            (f-let (p S_1) S_2)
            (f-let (p S_1) S_4))])

;; load function

(define (load-PCEK p)
    (cond
        [(redex-match? PCEK M p)
            (term (,p () ϵ))]
        [else
            (raise (format "load-PCEK: expected a valid PCEK program, got: ~a" p))]))

(traces
    ->
    (load-PCEK
        (term
            (let (x 3) (let (y 4) (let (z (cons x y))
                z))))))

(traces
    ->
    (load-PCEK
        (term
            (let (x 3) (let (y 4) (let (z (cons x y))
                (let (w (car z))
                    w)))))))

(traces
    ->
    (load-PCEK
        (term
            (let (x 3) (let (y 4) (let (z (cons x y))
                (let (w (cdr z))
                    w)))))))

(traces
    ->
    (load-PCEK
        (term
            (let (x 3) (let (y 4) (let (z (cons x y))
                (let (w (car x))
                    w)))))))

(traces
    ->
    (load-PCEK
        (term
            (let (x 3) (let (y 4) (let (z (cons x y))
                (let (w (cdr x))
                    w)))))))

(traces
    ->
    (load-PCEK
        (term
            (let (x 1) (let (y 2) (let (z nil)
                (let (w (if z x y))
                    w)))))))


(traces
    ->
    (load-PCEK
        (term
            (let (x 1) (let (y 2) (let (z 1)
                (let (w (if z x y))
                    w)))))))


(traces
    ->
    (load-PCEK
        (term
            (let (x 1) (let (y 2) (let (z nil)
                (let (w (if z x y))
                    (let (a (if w y x))
                        a))))))))

(traces
    ->
    (load-PCEK
        (term
            (let (x 3) (let (y 4) (let (z (cons x y))
                (let (w (future (let (a (car z)) a)))
                    w)))))))

(define (eval program reduce)
    (let ([results (apply-reduction-relation* reduce (load-PCEK program))])
        (cond
            [(empty? results) 'diverges]
            [else
            (unless (= (length results) 1)
                (writeln (format "WARNING: multiple results returned")))
            (term
                (unload-state
                    ,(first results)))])))

(define-term program
    (let (x 3) (let (y 4) (let (z (cons x y))
                (let (w (future (let (a (car z)) a)))
                    w)))))

(writeln
    (apply-reduction-relation* -> (load-PCEK (term program))))

(writeln
    (eval (term program) ->))

(writeln (term (FP-V
    (ph 2 (ph 5 ∘)))))

(writeln (term (placeholder-not-in-FP (FP-V
    (ph 2 (ph 5 ∘))))))

(writeln (term (maximum-in-FP -1 (FP-V
    (ph 2 (ph 5 ∘))))))
