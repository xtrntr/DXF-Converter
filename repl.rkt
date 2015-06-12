#lang typed/racket

(struct point ([x : Real]
               [y : Real]))

(define lst (list (point 1 2)
                  (point 2 3)
                  (point 3 4)
                  (point 5 6)
                  (point 6 7)
                  (point 8 9)))

(: get-paths (Listof point)
(define (