#lang typed/racket

#|
 t
This module must be stand alone i.e. purely numerical functions
This module contains all helper functions that can operate on numbers, strings, Booleans

|#

(provide (all-defined-out))

(: point-in-rect? (-> Real Real Real Real Real Real Boolean))
(define (point-in-rect? x y xs ys xb yb)
  (and (> x xs) (< x xb) (> y ys) (< y yb)))

(: capitalize (-> String String))
(define (capitalize str)
  (let* ((dissected (string->list str))
         (first-letter (char-upcase (car dissected)))
         (capitalized (list->string (append (list first-letter) (cdr dissected)))))
    capitalized))

(: to-display (-> Real String))
(define (to-display x)
  (format "~a" (number->string (round-3 x))))

(: string->real (-> String Real))
(define (string->real x)
  (cast (string->number x) Real))

;3 decimal place
(: round-3 (-> Real Real))
(define (round-3 z)
  (let* ([power (expt 10 3)]
         [result (/ (round (* power z)) power)])
    (if (= result -0.0)
        0.0
        result)))

;1 decimal place
(: round-1 (-> Real Real))
(define (round-1 z)
  (let* ([result (/ (round (* 1 z)) 1)])
    (if (= result -0.0)
        0.0
        result)))

(: round-to-int (-> Real Exact-Rational))
(define (round-to-int z)
  (inexact->exact (round-1 z)))

(: make-whitespaces (-> Real String))
(define (make-whitespaces x)
      (cond ((= x 0) "")
            (else (string-append " " (make-whitespaces (- x 1))))))

(: biggest (-> (Listof Real) Real))
(define (biggest lst)
  (let loop : Real
    ([wins : Real (car lst)]
     [lst : (Listof Real) lst])
    (cond ((empty? lst) wins)
          ((> (car lst) wins) (loop (car lst) (cdr lst)))
          (else (loop wins (cdr lst))))))

(: smallest (-> (Listof Real) Real))
(define (smallest lst)
  (let loop : Real
    ([wins : Real (car lst)]
     [lst : (Listof Real) lst])
    (cond ((empty? lst) wins)
          ((< (car lst) wins) (loop (car lst) (cdr lst)))
          (else (loop wins (cdr lst))))))

;; this is used to check the equality of two Reals to a set decimal point (currently 5)
;; 0.009 -> accuracy up to 2 decimal point.
;; 0.09 -> accuracy up to 1 decimal point.
;; 0.9 -> integer test.
;; test up to 3 decimal points.
(: reasonable-equal? (-> Real Real Boolean))
(define (reasonable-equal? x y)
  (<= (abs (- x y)) 0.0009))

(: =2dp? (-> Real Real Boolean))
(define (=2dp? x y)
  (<= (abs (- x y)) 0.009))

;; accurate up to 14 decimal places
(: in-between? (-> Real Real Real Boolean))
(define (in-between? test-num num-1 num-2)
  (or (and (> num-1 test-num) (< num-2 test-num))
      (and (> num-2 test-num) (< num-1 test-num))))

(: difference (-> Real Real Real))
(define (difference x y)
  (if (and (> 0 x) (> 0 y))
      (abs (- (- x) (- y)))
      (abs (- x y))))

(: break (All [T] (-> (T -> Any) (Listof T) (Values (Listof T) (Listof T)))))
(define (break pred lst)
  (splitf-at lst (negate pred)))

(: get-opposing-angle (-> Real Real))
(define (get-opposing-angle x)
  (cond ((reasonable-equal? x 360) 180)
        ((reasonable-equal? x 0) 180)
        ((reasonable-equal? x 90)  270)
        ((reasonable-equal? x 180) 0)
        ((reasonable-equal? x 270) 90)
        ((in-between? x 0 90) (+ x 180))
        ((in-between? x 90 180) (+ x 180))
        ((in-between? x 180 270) (- x 180))
        ((in-between? x 270 360) (- x 180))
        (error "Expected a number, given " x)))

(: get-y-mirror-angle (-> Real Real))
(define (get-y-mirror-angle x)
  (- 360 x))

(: get-x-mirror-angle (-> Real Real))
(define (get-x-mirror-angle x)
  (if (<= 180 x)
      (- 540 x)
      (- 180 x)))

(: get-rotated-angle (-> Real Real))
(define (get-rotated-angle x)
  (if (> (+ x 90) 360)
      (- (+ x 90) 360)
      (+ x 90)))

(: localize-degree (-> Real Real))
(define (localize-degree degree)
  (cond ((in-between? degree 0 90) degree)
        ((in-between? degree 90 180) (- degree 90))
        ((in-between? degree 180 270) (- degree 180))
        ((in-between? degree 270 360) (- degree 270))
        ((> degree 360) [error "localize-degree function in utils.rkt: " degree])
        (else degree)))

;;calculate the x and y coordinates for arc points
(: arc-point-x (-> Real Real Real Real))
(define (arc-point-x circle-x degree radius)
  (let ((adjusted (localize-degree degree)))
    (cond ((or (= degree 90) (= degree 270)) circle-x)
          ((= degree 180) (- circle-x radius))
          ((or (= degree 360) (= degree 0)) (+ circle-x radius)) 
          ((in-between? degree 0 90)    (+ circle-x (* radius (cos (degrees->radians adjusted)))))
          ((in-between? degree 90 180)  (- circle-x (* radius (sin (degrees->radians adjusted)))))
          ((in-between? degree 180 270) (- circle-x (* radius (cos (degrees->radians adjusted)))))
          ((in-between? degree 270 360) (+ circle-x (* radius (sin (degrees->radians adjusted)))))
          (else [error "arc-point-x function in utils.rkt"]))))

(: arc-point-y (-> Real Real Real Real))
(define (arc-point-y circle-y degree radius)
  (let ((adjusted (localize-degree degree)))
    (cond ((or (= degree 0) (= degree 360) (= degree 180)) circle-y)
          ((= degree 90) (+ circle-y radius))
          ((= degree 270) (- circle-y radius))
          ((in-between? degree 0 90)    (+ circle-y (* radius (sin (degrees->radians adjusted)))))
          ((in-between? degree 90 180)  (+ circle-y (* radius (cos (degrees->radians adjusted)))))
          ((in-between? degree 180 270) (- circle-y (* radius (sin (degrees->radians adjusted)))))
          ((in-between? degree 270 360) (- circle-y (* radius (cos (degrees->radians adjusted)))))
          (else [error "arc-point-y function in utils.rkt"]))))

;given 3 nodes a b c, find if a->b->c is in a clockwise or anticlockwise direction
(: cw-turn? (-> Real Real Real Real Real Real Boolean))
(define (cw-turn? x1 y1 x2 y2 x3 y3)
  (not (positive? (- (* (- x2 x1) (- y3 y1)) (* (- x3 x1) (- y2 y1))))))

;given 3 nodes a b c, find if a->b->c is in a clockwise or anticlockwise direction
(: ccw-turn? (-> Real Real Real Real Real Real Boolean))
(define (ccw-turn? x1 y1 x2 y2 x3 y3)
  (not (cw-turn? x1 y1 x2 y2 x3 y3)))
  
(: arc-centerpoint (-> Real Real Real Real Boolean Real Real Real Real (List Real Real)))
(define (arc-centerpoint center-x center-y radius angle ccw? x1 y1 x3 y3)
  (let* ([opposing-angle (get-opposing-angle angle)]
         [1st-x2 (arc-point-x center-x angle radius)]
         [1st-y2 (arc-point-y center-y angle radius)]
         [2nd-x2 (arc-point-x center-x opposing-angle radius)]
         [2nd-y2 (arc-point-y center-y opposing-angle radius)]
         [cw? (not ccw?)]
    [result (cond [(and ccw? (ccw-turn? x1 y1 1st-x2 1st-y2 x3 y3) (cw-turn? x1 y1 2nd-x2 2nd-y2 x3 y3))
                   (list 2nd-x2 2nd-y2)
                   ;(list 1st-x2 1st-y2)
                   ]
                  [(and ccw? (ccw-turn? x1 y1 2nd-x2 2nd-y2 x3 y3) (cw-turn? x1 y1 1st-x2 1st-y2 x3 y3))
                   (list 1st-x2 1st-y2)
                   ;(list 2nd-x2 2nd-y2)
                   ]
                  [(and cw?  (ccw-turn? x1 y1 2nd-x2 2nd-y2 x3 y3) (cw-turn? x1 y1 1st-x2 1st-y2 x3 y3))
                   (list 2nd-x2 2nd-y2)
                   ;(list 1st-x2 1st-y2)
                   ]
                  [(and cw?  (ccw-turn? x1 y1 1st-x2 1st-y2 x3 y3) (cw-turn? x1 y1 2nd-x2 2nd-y2 x3 y3))
                   (list 1st-x2 1st-y2)
                   ;(list 2nd-x2 2nd-y2)
                   ]
                  [else (list 1st-x2 1st-y2)])])
    result))
                    

;; NOT YET TESTED PROPERLY.
;; from the center xy, start and end points, return x1 y1 x2 y2 x3 y3 which is the start, middle and end point of the arc respectively.
(: get-arc-points (-> Real Real Real Real Real Boolean (Listof Real)))
(define (get-arc-points circle-x circle-y radius start end ccw?)
  (let* ([arc-x1 (arc-point-x circle-x start radius)]
         [arc-y1 (arc-point-y circle-y start radius)]
         [arc-x3 (arc-point-x circle-x end radius)]
         [arc-y3 (arc-point-y circle-y end radius)]
         [half-angle (let [(x (/ (+ start end) 2))]
                       (if (> x 360) (- x 360) x))]
         [arc-x2 (first (arc-centerpoint circle-x circle-y radius half-angle ccw? arc-x1 arc-y1 arc-x3 arc-y3))]
         [arc-y2 (second (arc-centerpoint circle-x circle-y radius half-angle ccw? arc-x1 arc-y1 arc-x3 arc-y3))])
    (list arc-x1 arc-y1 arc-x2 arc-y2 arc-x3 arc-y3)))

(: debug-display (-> Any Void))
(define (debug-display x)
  (display (string-append (symbol->string (quote x)) ": "))
  (newline)
  (newline)
  (if (list? x)
      (for ([element : Any (cast x (Listof Any))])
           (display element)
           (newline))
      (begin (display x) (newline)))
  (newline))

(: time-debug-display (-> Any Any))
(define (time-debug-display x)
  (display (string-append (symbol->string (quote x)) ": "))
  (newline)
  (time x))