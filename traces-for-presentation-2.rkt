#lang racket

(require redex
         "./input-language.rkt"
         "./C-machine.rkt"
         "./PCEK-machine.rkt")

(define-term future-program
  (let (a 1)
    (let (b 2)
      (let
          (f (future
              (let
                  (g (future
                      (let
                          (h (future
                              (let (i (cons a b))
                                i)))
                        h)))
                g)))
        (let (i (car f))
          i)))))


(C-eval (term future-program))

; (traces -->c (term future-program))

; (PCEK-eval (term future-program))

; (traces ->PCEK (load-PCEK (term future-program)))
