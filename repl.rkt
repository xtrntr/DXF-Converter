#lang racket

(struct point (x y))

(define lst (list (point 1 2)
                  (point 2 3)
                  (point 3 4)
                  (point 5 6)
                  (point 6 7)
                  (point 8 9)))

(define (sort-into-groups lst)
  (define (is-connected? node lst)
    (cond ((empty? lst) #f)
          ((or (= (point-x node) (point-x (car lst))) (= (point-y node) (point-y (car lst)))) #t)
          (else (is-connected? node (cdr lst)))))
  (let loop
    ([current-path '()]
     [result '()])
    (cond ((empty? lst) result)
          (else
           (let ([current-node (car lst)])
             (cond ((foldl is-connected?
                   ((