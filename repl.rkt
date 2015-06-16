#lang racket

(: round-off (-> Real Real))
(define (round-off z)
  (let* ([power (expt 10 3)]
         [result (/ (round (* power z)) power)])
    (if (= result -0.0)
        0.0
        result)))

(: round-to-int (-> Number Integer))
(define (round-to-int x)
  (let* ((3dp (round-off x))
         (str (number->string 3dp))
         (len (string-length str))
         (str-int (substring str 0 (- len 4)))
         (result (string->number str-int)))
    result))

(round-to-int 19.993)