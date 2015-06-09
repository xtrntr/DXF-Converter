#lang racket

(require "structs.rkt")

(provide match-struct)

(define-syntax match-struct
  (lambda (stx)
    (syntax-case stx ()
      [(_ (dot a) (line b) (arc c) (path d e))
       (with-syntax  ([tmp0 (syntax->datum #'a)]
                      [tmp1 (syntax->datum #'b)]
                      [tmp2 (syntax->datum #'c)]
                      [tmp3 (syntax->datum #'e)])
       #'(lambda (a-struct)
           (match a-struct
             [(dot highlighted selected visible layer p)                                tmp0]
             [(line highlighted selected visible layer p1 p2)                           tmp1]
             [(arc highlighted selected visible layer center radius start end p1 p2 p3) tmp2]
             [(path highlighted selected visible layer entities)                        (d tmp3)])))])))