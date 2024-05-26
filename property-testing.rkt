#lang racket


(require redex
         "./input-language.rkt"
         "./C-machine.rkt"
         "./PCEK-machine.rkt")

(struct testp (in out))

(define-term a-parallel-program
  (let (func1
        (λ (x)
          (let (r (if x
                      (let (t 20) t)
                      (let (t 30) t))) r)))
    (let (func2
          (λ (x)
            (let (r (if x
                        (let (t 1000) t)
                        (let (t 5000) t))) r)))
      (let (input1 1)
        (let (input2 nil)
          (let (result1
                (future (let (r1 (apply func1 input1)) r1)))
            (let (result2
                  (future (let (r2 (apply func2 input2)) r2)))
              (let (final-result (cons result1 result2))
                final-result))))))))

(define-term future-program-simple
    (let (x 3) (let (y 4) (let (z (cons x y))
                (let (w (future (let (a (car z)) a)))
                    w)))))

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

(define-term future-program-2
    (let
        (f (future
            (let
                (g (future
                    (let
                        (h (future
                            (let (i 1) i)))
                        h)))
                g)))
        f))

(define testing-pairs
  (list
   (testp (term a-parallel-program) '(cons 20 5000))

   (testp (term (let (x 3) x))
               3)
        (testp (term (let (x 12)
                       (let (y (λ (z) z))
                         (let (z (apply y x))
                           z))))
               12)
        (testp (term (let
                         (f (future
                             (let
                                 (g (future
                                     (let
                                         (h (future
                                             (let (i 1) i)))
                                       h)))
                               g)))
                       f)) 1)
        (testp (term (let (x 12)
              (let (y 13)
                (let (z nil)
                  (let (q (cons y z))
                    (let (r (cons x q))
                      (let (other (if z x y)) other)))))))
               13)
        (testp (term (let (x 3)
              (let (y (future x)) y)))
               3)
        (testp (term (let (x (future
                        (let (a 3) a))) x))
               3)
        (testp (term (let (x 12)
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
                      (let (other (if z x y)) other)))))))
               13)
        (testp (term (let (x 12)
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
                      (let (other (if z x y)) other)))))))
               12)
        (testp
         (term (let (x (λ (y) z)) x))
         'procedure)
        (testp
         (term
   (let (x (λ (y) z))
     (let (y (cons x x))
        (let (z (cons y y)) z))))
         '(cons (cons procedure procedure) (cons procedure procedure)))
(testp
 (term
   (let (x 42)
     (let (y (cons x x))
        (let (z (cons y y)) z))))
 '(cons (cons 42 42) (cons 42 42)))
(testp
 (term future-program-simple) 3)
(testp
 (term future-program-long) '(cons 1 4))
(testp
 (term future-program) 1)
(testp
 (term future-program-if) 2)
(testp
 (term future-program-2) 1)))

(for ([pair testing-pairs])
  (define in (testp-in pair))
  (define out (testp-out pair))
  (test-equal (C-eval in) out)
  (test-equal (PCEK-eval in) out))