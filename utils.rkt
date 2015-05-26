#lang racket

(provide real->decimal)

(define (real->decimal n digits)
    (let* ([e (expt 10 digits)]
           [num (round (abs (* e n)))]
           [left (quotient num e)]
           [right (/ (remainder num e) e)])
      (exact->inexact (+ left right))))

(define (make-whitespaces x)
      (cond ((= x 0) "")
            (else (string-append " " (make-whitespaces (- x 1))))))