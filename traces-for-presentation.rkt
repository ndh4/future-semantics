#lang racket

(require redex
         "./input-language.rkt"
         "./C-machine.rkt"
         "./PCEK-machine.rkt")

(define-term simple-parallel-program
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

#;(traces -->c (term simple-parallel-program))

#;(traces ->PCEK (l (term simple-parallel-program)))

#;(PCEK-eval (term simple-parallel-program))

#;(C-eval (term simple-parallel-program))

#;(C-eval (term simple-parallel-program))