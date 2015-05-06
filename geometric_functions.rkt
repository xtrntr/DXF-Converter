#lang racket

(require "structs.rkt")

(provide in-between?
         point-in-rect?
         biggest
         smallest
         reasonable-equal?
         get-arc-points
         filter-struct-list
         optimize-pattern)

;; geometric functions
(define (point-in-rect? x y xs ys xb yb)
  (and (> x xs) (< x xb) (> y ys) (< y yb)))         

;; auxilliary functions
(define (best fn lst)
  (unless (empty? lst)
    (let ((wins (car lst)))
      (for/list ([i (cdr lst)])
        (when (fn i wins)
          (set! wins i)))
      wins)))

(define (biggest lst)
  (best > lst))

(define (smallest lst)
  (best < lst))

;; this is used to check the equality of two numbers to a set decimal point (currently 5)
;; 0.009 -> accuracy up to 2 decimal point.
;; 0.09 -> accuracy up to 1 decimal point.
;; 0.9 -> integer test.
;; test up to 3 decimal points.
(define (reasonable-equal? x y)
  (<= (abs (- x y)) 0.000009))

;; accurate up to 14 decimal places
(define (in-between? test-num num-1 num-2)
  (or (and (> num-1 test-num) (< num-2 test-num))
      (and (> num-2 test-num) (< num-1 test-num))))

;; NOT YET TESTED PROPERLY.
;; from the center xy, start and end points, return x1 y1 x2 y2 x3 y3 which is the start, middle and end point of the arc respectively.
(define (get-arc-points center-x center-y radius start-angle end-angle)
  (define (determine-quadrant angle)
    (cond ((in-between? angle 0 90) 1)
          ((in-between? angle 90 180) 2)
          ((in-between? angle 180 270) 3)
          ((in-between? angle 270 360) 4)
          (else 0)))
  (define (get-narrow-angle angle quadrant)
    (cond ((= quadrant 2) (- angle 90))
          ((= quadrant 3) (- angle 180))
          ((= quadrant 4) (- angle 270))
          (else angle)))
  (define (get-x-value angle quadrant center-x radius)
    (cond ((= quadrant 1) (+ center-x (* radius (cos (degrees->radians angle)))))
          ((= quadrant 2) (- center-x (* radius (cos (degrees->radians angle)))))
          ((= quadrant 3) (- center-x (* radius (cos (degrees->radians angle)))))
          ((= quadrant 4) (+ center-x (* radius (cos (degrees->radians angle)))))
          ((reasonable-equal? angle 90) center-x)
          ((reasonable-equal? angle 180) (- center-x radius))
          ((reasonable-equal? angle 270) center-x)
          ((or (reasonable-equal? angle 360) (reasonable-equal? angle 0)) (+ center-x radius))))
  (define (get-y-value angle quadrant center-y radius)
    (cond ((= quadrant 1) (+ center-y (* radius (sin (degrees->radians angle)))))
          ((= quadrant 2) (+ center-y (* radius (sin (degrees->radians angle)))))
          ((= quadrant 3) (- center-y (* radius (sin (degrees->radians angle)))))
          ((= quadrant 4) (- center-y (* radius (sin (degrees->radians angle)))))
          ((reasonable-equal? angle 90) (+ center-y radius))
          ((reasonable-equal? angle 180) center-y)
          ((reasonable-equal? angle 270) (- center-y radius))
          ((or (reasonable-equal? angle 360) (reasonable-equal? angle 0)) center-y)))
  (let* ((mid-angle (/ (+ start-angle end-angle) 2))
         (first-point-quadrant (determine-quadrant start-angle))
         (second-point-quadrant (determine-quadrant mid-angle))
         (third-point-quadrant (determine-quadrant end-angle))
         (first-point-narrow-angle (get-narrow-angle start-angle first-point-quadrant))
         (second-point-narrow-angle (get-narrow-angle mid-angle second-point-quadrant))
         (third-point-narrow-angle (get-narrow-angle end-angle third-point-quadrant))
         (x1 (get-x-value first-point-narrow-angle first-point-quadrant center-x radius))
         (y1 (get-y-value first-point-narrow-angle first-point-quadrant center-y radius))
         (x2 (get-x-value second-point-narrow-angle second-point-quadrant center-x radius))
         (y2 (get-y-value second-point-narrow-angle second-point-quadrant center-y radius))
         (x3 (get-x-value third-point-narrow-angle third-point-quadrant center-x radius))
         (y3 (get-y-value third-point-narrow-angle third-point-quadrant center-y radius)))
    (list x1 y1 x2 y2 x3 y3)))

(define (filter-struct-list struct-list cond)
  (for/list ([i struct-list] 
             #:when (cond i))
    i)) 

;get all permutations of a set
(define (get-tours set origin)
  (map (lambda (x) (append (list origin) x (list origin))) ;append the origin to the start and end of each permutation
       (let loop ([temp-set set] [tail '()])
         (if (empty? temp-set) 
             (list tail) 
             (append-map (lambda (x) ;map a procedure onto each element of temp-set, then append them afterwards
                           (loop (remq x temp-set) (cons x tail))) temp-set)))))

(define (distance x1 y1 x2 y2)
  (sqrt (+ (sqr (abs (- x1 x2))) (sqr (abs (- y1 y2))))))

(define (get-best-tour list-of-tours)
  (unless (empty? list-of-tours)
    (let* ((best-tour (car list-of-tours))
           (best-distance (tour-distance best-tour)))
      (for/list ([a-tour (cdr list-of-tours)])
        (when (> (tour-distance a-tour) best-distance)
          (set! best-tour a-tour)
          (set! best-distance (tour-distance a-tour))))
      best-tour)))

;a-tour is a list of nodes
;show the distance for a given tour route
(define (tour-distance a-tour)
  (cond ((empty? a-tour) (error "should not happen"))
        ((= (length a-tour) 1) 0)
        (else (+ (tour-distance (cdr a-tour)) (node-distance (car a-tour) (cadr a-tour))))))

;get the start and end of a node and find their distance
(define (node-distance node-start node-end)
  (define (get-start)
    (match node-start
      [(line layer highlighted selected visible x1 y1 x2 y2)                           (list x1 y1)]
      [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (list x1 y1)]
      [(point layer highlighted selected visible x y)                                  (list x y)]))
  (define (get-end)
    (match node-end
      [(line layer highlighted selected visible x1 y1 x2 y2)                           (list x2 y2)]
      [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (list x3 y3)]
      [(point layer highlighted selected visible x y)                                  (list x y)]))
  (apply distance (append (get-start) (get-end))))
  
(define (optimize-pattern struct-list origin)
  (get-best-tour (get-tours struct-list origin)))