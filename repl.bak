#lang racket

(define ht (hash))

(set! ht (hash-set ht 1 2))

ht

(set! ht (hash-set ht 1 (cons 3 (hash-ref ht 1))))

ht

(set! ht (hash-set ht 1 (cons 4 (hash-ref ht 1))))

ht

(define (ht-add ht key val)
  (if (hash-has-key? ht key)
      (hash-set ht (append (list val) (hash-ref ht key)))
      (hash-set ht key val)))