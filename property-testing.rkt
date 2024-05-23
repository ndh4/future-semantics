#lang racket


(require redex
         "./input-language.rkt"
         "./C-machine.rkt"
         "./PCEK-machine.rkt")

(struct testp (in out))

(define testing-pairs
  (list (testp (term (let (x 3) x))
               3)
        (testp (term (let (x 12)
                       (let (y (λ (z) z))
                         (let (z (apply y x))
                           z))))
               12)
        ))

(for ([pair testing-pairs])
  (define in (testp-in pair))
  (define out (testp-out pair))
  (test-equal (C-eval in) out)
  (test-equal (PCEK-eval in) out))
  
#;(C-eval (term (let
                    ; (f
                    (f (future
                        (let
                            (g (future
                                (let
                                    (h (future
                                        (let (i 1) i)))
                                    h)))
                            g)))
                    f)))
#;(test-equal (PCEK-eval (term term1)) 3)

#;(test-equal (C-eval ) 12)

(test-equal (C-eval (term (let (x 12)
              (let (y 13)
                (let (z nil)
                  (let (q (cons y z))
                    (let (r (cons x q))
                      (let (other (if z x y)) other)))))))) 13)

(test-equal (C-eval (term (let (x 3)
              (let (y (future x)) y)))) 3)

(test-equal (C-eval (term (let (x 3)
              (let (y (future x)) y)))) 3)

(test-equal (C-eval (term (let (x (future (let (a 3) a))) x))) 3)

(test-equal (C-eval (term (let (x 12)
              (let (y 13)
                (let (z nil)
                  (let
                      (q
                       (future
                        (let
                            (x
                             (future
                              (let
                                  (a
                                   (cons y z)) a))) x)))
                    (let (r (cons x q))
                      (let (other (if z x y)) other)))))))) 13)

(test-equal (C-eval (term (let (x 12)
              (let (y 13)
                (let (z (cons y y))
                  (let
                      (q
                       (future
                        (let
                            (x
                             (future
                              (let
                                  (a
                                   (cons y z)) a))) x)))
                    (let (r (cons x q))
                      (let (other (if z x y)) other)))))))) 12)

(test-equal (C-eval (term (let (x (λ (y) z)) x))) 'procedure)

(test-equal
 (C-eval
  (term
   (let (x (λ (y) z))
     (let (y (cons x x))
        (let (z (cons y y)) z)))))
 '(cons (cons procedure procedure) (cons procedure procedure)))

(test-equal
 (C-eval
  (term
   (let (x 42)
     (let (y (cons x x))
        (let (z (cons y y)) z)))))
 '(cons (cons 42 42) (cons 42 42)))

(define-term program
    (let (x 3) (let (y 4) (let (z (cons x y))
                (let (w (future (let (a (car z)) a)))
                    w)))))

; (writeln
;     (apply-reduction-relation* -> (load-PCEK (term program))))

(writeln
    (PCEK-eval (term program)))

(writeln (term (FP-V
    (ph -2 (ph -1 ∘)))))

(writeln (term (placeholder-not-in-FP (FP-V
    (ph -2 (ph -1 ∘))))))


(writeln (term (minus-FP
    -2
    (FP-V
        (ph -3 (ph -2 (ph -1 ∘)))))))

(define-term future-program-long
    (let (a 1)
        (let (b 2)
            (let (c 3)
                (let (d 4)
                    (let (e 5)
                        (let (NIL nil)
                            (let
                                (f (future
                                    (let
                                        (g (future
                                            (let (h (cons a b))
                                                h)))
                                        g)))
                                (let
                                    (i (future
                                        (let (j (cons c d))
                                            j)))
                                    (let (k (car f))
                                        (let (l (cdr i))
                                            (let (m (cons k l))
                                                m))))))))))))

(define-term future-program
    (let (a 1)
        (let (b 2)
            (let (c 3)
                (let (d 4)
                    (let (e 5)
                        (let (NIL nil)
                            (let
                                (f (future
                                    (let
                                        (g (future
                                            (let (h (cons a b))
                                                h)))
                                        g)))
                                (let (i (car f))
                                    i)))))))))

(define-term future-program-if
    (let (a 1)
        (let (b 2)
            (let (c 3)
                (let (d 4)
                    (let (e 5)
                        (let (NIL nil)
                            (let
                                (f (future
                                    (let
                                        (g (future
                                            (let (h (cons a b))
                                                h)))
                                        g)))
                                (let
                                    (i (if NIL
                                        (let (j (car f)) j)
                                        (let (j (cdr f)) j)))
                                    i)))))))))

; (define-term future-program-2
;     (let (a 1)
;         (let (b 2)
;             (let (c 3)
;                 (let (d 4)
;                     (let (e 5)
;                         (let (NIL nil)
;                             (let
;                                 (f (future
;                                     (let
;                                         (g (future
;                                             (let
;                                                 (h (future
;                                                     (let (i (cons a b)) i)))
;                                                 h)))
;                                             ; (let (h (cons a b))
;                                             ;     h)))
;                                         g)))
;                                 ; f))))))))
;                                 (let (i (car f))
;                                     i)))))))))

; (traces -> (load-PCEK (term future-program)))

(writeln
    (PCEK-eval (term future-program)))
                                
; (for
;     ([i (apply-reduction-relation* -> (load-PCEK (term future-program)))])
;     (writeln i))

#;(traces -> (load-PCEK (term future-program-if)))

(writeln
    (PCEK-eval (term future-program-if)))

; (writeln
;     (PCEK-eval (term future-program-2) ->))

; (traces -> (load-PCEK (term future-program-2)))

; (writeln (term (lookup f ((f (ph 0 ∘))))))

; (writeln
; (apply-reduction-relation ->
; (first
; (term
; (unload-state
; ,(first
; (apply-reduction-relation ->
; (first
; (apply-reduction-relation ->
; (first
; (apply-reduction-relation ->
; (first
; (apply-reduction-relation ->
; (first
; (apply-reduction-relation ->
; (first
; (apply-reduction-relation ->
; (first
; (apply-reduction-relation ->
; (first
; (apply-reduction-relation ->
; (first
;     (apply-reduction-relation
#;(traces
        ->
        (load-PCEK
            (term
                (let
                    ; (f
                    (f (future
                        (let
                            (g (future
                                (let
                                    (h (future
                                        (let (i 1) i)))
                                    h)))
                            g)))
                    f)))
)
; )))))))))))))))))
; )))
    ; #:cache-all? #true))
                    ; f))

(writeln
    (term
        (fresh-placeholder
            (extend
                x
                (ph
                    (fresh-placeholder () ε)
                    ∘)
                ())
            ε)
    ))