#lang racket

(provide (all-defined-out))

; count number of occurrences of an element in a list
(define (bagify lst)
  (foldl (lambda (key ht)
           (hash-update ht key add1 0))
         #hash() lst))

; more than 2 duplicate elements?
(define (more-than-2? lst)
  (ormap (lambda (x) (> x 2)) (hash-values (bagify lst))))