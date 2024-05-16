#lang racket

(require redex)
(provide Λa-input)

(define-language Λa-input
  (M ::= x
         (let (x V) M)
         (let (x (future M)) M)
         (let (x (car y)) M)
         (let (x (cdr y)) M)
         (let (x (if y M M)) M)
         (let (x (apply y z)) M))
  (V ::= c x (λ (x) M) (cons x y))
  (x y z q r a b var ::=
     variable-not-otherwise-mentioned)
  (c ::= nil natural))