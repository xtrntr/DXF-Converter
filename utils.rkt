#lang typed/racket

#|

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
  (let* ([power (expt 10 0)]
         [result (/ (round (* power z)) power)])
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

(: get-mirror-angle (-> Real Real))
(define (get-mirror-angle x)
  (- 360 x))

;; NOT YET TESTED PROPERLY.
;; from the center xy, start and end points, return x1 y1 x2 y2 x3 y3 which is the start, middle and end point of the arc respectively.
(: get-arc-points (-> Real Real Real Real Real Boolean (Listof Real)))
(define (get-arc-points center-x center-y radius start-angle end-angle ccw?)
  (: determine-quadrant (-> (Option Real) Real))
  (define (determine-quadrant angle)
    (assert angle real?)
    (cond ((or (reasonable-equal? angle 0) (reasonable-equal? angle 90) 
               (reasonable-equal? angle 270)(reasonable-equal? angle 180) 
               (reasonable-equal? angle 360)) 0)
          ((in-between? angle 0 90) 1)
          ((in-between? angle 90 180) 2)
          ((in-between? angle 180 270) 3)
          ((in-between? angle 270 360) 4)
          (error "Expected a number, given " angle)))
  (: get-narrow-angle (-> (Option Real) (Option Real) Real))
  (define (get-narrow-angle angle quadrant)
    (assert angle real?)
    (assert quadrant real?)
    (cond ((= quadrant 2) (- angle 90))
          ((= quadrant 3) (- angle 180))
          ((= quadrant 4) (- angle 270))
          (else angle)))
  (: get-x-value (-> (Option Real) (Option Real) Real Real Real))
  (define (get-x-value angle quadrant center-x radius)
    (assert angle real?)
    (assert quadrant real?)
    (cond ((reasonable-equal? angle 90) center-x)
          ((reasonable-equal? angle 180) (- center-x radius))
          ((reasonable-equal? angle 270) center-x)
          ((or (reasonable-equal? angle 360) (reasonable-equal? angle 0)) (+ center-x radius))
          ((= quadrant 1) (+ center-x (* radius (cos (degrees->radians angle)))))
          ((= quadrant 2) (- center-x (* radius (sin (degrees->radians angle)))))
          ((= quadrant 3) (- center-x (* radius (cos (degrees->radians angle)))))
          ((= quadrant 4) (+ center-x (* radius (sin (degrees->radians angle)))))
          (error "Expected a number, given " quadrant)))
  (: get-y-value (-> (Option Real) (Option Real) Real Real Real))
  (define (get-y-value angle quadrant center-y radius)
    (assert angle real?)
    (assert quadrant real?)
    (cond ((reasonable-equal? angle 90) (+ center-y radius))
          ((reasonable-equal? angle 180) center-y)
          ((reasonable-equal? angle 270) (- center-y radius))
          ((or (reasonable-equal? angle 360) (reasonable-equal? angle 0)) center-y)
          ((= quadrant 1) (+ center-y (* radius (sin (degrees->radians angle)))))
          ((= quadrant 2) (+ center-y (* radius (cos (degrees->radians angle)))))
          ((= quadrant 3) (- center-y (* radius (sin (degrees->radians angle)))))
          ((= quadrant 4) (- center-y (* radius (cos (degrees->radians angle)))))
          (error "Expected a number, given " quadrant)))
  (let* ((mid-angle (if (and ccw? (> start-angle end-angle))
                        (get-opposing-angle (/ (+ start-angle end-angle) 2))
                        (/ (+ start-angle end-angle) 2)))
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
;    (get-arc-points center-x           center-y           radius             start-angle    end-angle)
;for (get-arc-points 432.08641063489955 25.258493353064345 25.258493353064345 270.0000000014 360.0),
;should return (457.345 25.258) (432.086 0)
;instead it returns (457.3449039879639 25.258493352447164) (457.3449039879639 25.258493353064345)

;    (get-arc-points center-x           center-y           radius             start-angle    end-angle)
;for (get-arc-points  35.72747415069246 248.39734121147703 26.58788774003842 111.212723819 180.0)
;should return (9.139 ...) (10.941 
;instead it returns (10.941089388271768 258.0176792973305) 20.70863089581764 270.3370340480322 (9.139586410654056 248.39734121147703)