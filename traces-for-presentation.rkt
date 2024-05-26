#lang racket

(require redex
         "./input-language.rkt"
         "./C-machine.rkt"
         "./PCEK-machine.rkt")

(define-term future-program
  (let (a 1)
    (let (b 2)
      (let (NIL nil)
        (let
            (f (future
                (let
                    (g (future
                        (let (h (cons a b))
                          h)))
                  g)))
          (let (i (car f))
            i))))))

(traces -->c (term future-program))

(traces ->PCEK (load-PCEK (term future-program)))

(PCEK-eval (term future-program))

(C-eval (term future-program))