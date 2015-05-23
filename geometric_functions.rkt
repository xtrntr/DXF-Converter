#lang racket

(require "structs.rkt"
         rackunit)

(provide in-between?
         point-in-rect?
         biggest
         smallest
         reasonable-equal?
         get-arc-points
         filter-struct-list
         optimize-pattern
         cohen-sutherland
         arc-intersect?
         get-scales)

;scaling for display - only done once
(define (get-all-x struct-lst)
  (flatten (for/list ([i struct-lst])
             (match i
               [(line layer highlighted selected visible x1 y1 x2 y2)                           (list x1 x2)]
               [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (list (+ x radius) (- x radius))]
               [(point layer highlighted selected visible x y)                                  (list x)]
               [(path layer highlighted selected visible path-list)                             (get-all-x path-list)]))))
(define (get-all-y struct-lst)
  (flatten (for/list ([i struct-lst])
             (match i
               [(line layer highlighted selected visible x1 y1 x2 y2)                           (list y1 y2)]
               [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (list (+ y radius) (- y radius))]
               [(point layer highlighted selected visible x y)                                  (list y)]
               [(path layer highlighted selected visible path-list)                             (get-all-y path-list)]))))

(define (get-scales struct-lst frame-width frame-height)
  (let* ((top (biggest (get-all-y struct-lst)))
         (bottom (smallest (get-all-y struct-lst)))
         (left (smallest (get-all-x struct-lst)))
         (right (biggest (get-all-x struct-lst)))
         (height (abs (- top bottom)))
         (width (abs (- right left)))
         (x-scale (/ frame-width width))
         (y-scale (/ frame-height height))
         (drawing-scale (smallest (list x-scale y-scale))))
    (values drawing-scale left bottom)))

;; geometric functions
(define (point-in-rect? x y xs ys xb yb)
  (and (> x xs) (< x xb) (> y ys) (< y yb)))

;; divide the complete 2d space into 9 boxes
;; algorithm to detect line-rectangle intersection. separate 2d area into 9 rectangles where 0 represents the selected area
;; region numbers are bit->decimal
;; 9   1   5                1001   0001   0101
;; 8   0   4      --->      1000   0000   0100
;; 10  2   6                1010   0010   0110
(define (cohen-sutherland line-struct xs ys xb yb)
  (let ((lx1 (line-x1 line-struct))
        (ly1 (line-y1 line-struct))
        (lx2 (line-x2 line-struct))
        (ly2 (line-y2 line-struct)))
    (define (compute-outcode x y)
      (let ((inside 0))
        (cond ((< x xs) 
               (set! inside (bitwise-ior inside 1)))
              ((> x xb) 
               (set! inside (bitwise-ior inside 2))))
        (cond ((< y ys) 
               (set! inside (bitwise-ior inside 4)))
              ((> y yb) 
               (set! inside (bitwise-ior inside 8))))
        inside))
    ;return #t if intersect
    (define (trivial-accept? region1 region2)
      (or (not (bitwise-ior region1 region2)) 
          (= region1 0) 
          (= region2 0)
          (and (= region1 1) (= region2 2))
          (and (= region1 2) (= region2 1))
          (and (= region1 4) (= region2 8))
          (and (= region1 8) (= region2 4))))
    ;return #t if does not intersect
    (define (trivial-reject? region1 region2)  
      (not (= (bitwise-and region1 region2) 0)))
    ;clip until no more ambiguous cases
    (define (clip-until region1 region2 tries)
      (cond ((= tries 0) #f)
            ((trivial-reject? region1 region2) #f)
            ((trivial-accept? region1 region2) #t)
            (else (apply clip-until (append (do-clip region1 region2) (list (- tries 1)))))))
    (define (do-clip region1 region2)
      (define (not0 num)
        (if (= num 0) #f #t))
      (let* ((new-x 0)
             (new-y 0)
             (slope (/ (- ly2 ly1) (- lx2 lx1)))
             (y-intercept (- ly2 (* slope lx2))))
        ;apply the formula y = y1 + slope * (x - x1), x = x1 + (y - y1) / slope
        (cond ((not0 (bitwise-and 8 region2))
               (set! new-x (/ (- yb y-intercept) slope))
               (set! new-y yb))
              ((not0 (bitwise-and 4 region2))
               (set! new-x (/ (- ys y-intercept) slope))
               (set! new-y ys))
              ((not0 (bitwise-and 2 region2))
               (set! new-x xb)
               (set! new-y (+ (* slope xb) y-intercept)))
              ((not0 (bitwise-and 1 region2)) 
               (set! new-x xs)
               (set! new-y (+ (* slope xs) y-intercept))))
        (set! lx2 new-x)
        (set! ly2 new-y)
        (set! region2 (compute-outcode lx2 ly2)))
      (list region1 region2))
    (let* ((region1 (compute-outcode lx1 ly1))
           (region2 (compute-outcode lx2 ly2)))
      (clip-until region1 region2 4))))

;; these 3 functions calculate the x and y coordinates for arc points
(define (arc-point-x circle-x degree radius)
  (let ((adjusted (localize-degree degree)))
    (cond ((or (= degree 90) (= degree 270)) circle-x)
          ((= degree 180) (- circle-x radius))
          ((or (= degree 360) (= degree 0)) (+ circle-x radius)) 
          ((in-between? degree 0 90)    (+ circle-x (* radius (cos (degrees->radians adjusted)))))
          ((in-between? degree 90 180)  (- circle-x (* radius (sin (degrees->radians adjusted)))))
          ((in-between? degree 180 270) (- circle-x (* radius (cos (degrees->radians adjusted)))))
          ((in-between? degree 270 360) (+ circle-x (* radius (sin (degrees->radians adjusted)))))
          (else (display "error")))))
(define (arc-point-y circle-y degree radius)
  (let ((adjusted (localize-degree degree)))
    (cond ((or (= degree 0) (= degree 360) (= degree 180)) circle-y)
          ((= degree 90) (+ circle-y radius))
          ((= degree 270) (- circle-y radius))
          ((in-between? degree 0 90)    (+ circle-y (* radius (sin (degrees->radians adjusted)))))
          ((in-between? degree 90 180)  (+ circle-y (* radius (cos (degrees->radians adjusted)))))
          ((in-between? degree 180 270) (- circle-y (* radius (sin (degrees->radians adjusted)))))
          ((in-between? degree 270 360) (- circle-y (* radius (cos (degrees->radians adjusted)))))
          (else (display "error")))))
(define (localize-degree degree)
  (cond ((in-between? degree 0 90) degree)
        ((in-between? degree 90 180) (- degree 90))
        ((in-between? degree 180 270) (- degree 180))
        ((in-between? degree 270 360) (- degree 270))))

;; 1) check for the trivial case - the arc point is inside the select box
;; 2) imagine the 4 lines of the select box as an infinite line, do they intersect the circle of the arc?
;; 2.1) if they do, are they in the select box's actual length/width?
;; 2.2) if they are, does it intersect the arc?
;; 2.3.1) arc intersection is checked by comparing the points where the lines intersect the circle.
;; 2.3.2) imagine a line from arc point 1 to arc point 2 as a "dividing line", to differentiate points intersecting the circle on the "right" or "wrong" side of the arc
;; 2.3.3) calculate the mid-point of the arc to determine the "right" side of the arc.
;; 2.3.4) imagine a line that goes through the mid-point of the arc and is parallel to the "dividing line" (hence they have the same slope). 
;; 2.3.5) is the y-intercept bigger or smaller than the y-intercept of the dividing line? use that as a barometer for any point intersecting the circle.
;; 2.3.6) if the line formed with the intersecting point falls on the right side of the "dividing line" together with the line formed with the mid-point line, then there is an intersection.
(define (arc-intersect? arc-struct xs ys xb yb)
  (let* ((radius (arc-radius arc-struct))
         (circle-x (arc-center-x arc-struct))
         (circle-y (arc-center-y arc-struct))
         (start (arc-start arc-struct))
         (end (arc-end arc-struct))
         (angle-difference (if (> end start) (- end start) (+ (- 360 start) end)))
         (half-angle (if (> end start) (/ (+ start end) 2) (if (< 360 (+ 180 (/ (+ start end) 2))) (- (+ 180 (/ (+ start end) 2)) 360) (+ 180 (/ (+ start end) 2)))))
         (radius (arc-radius arc-struct))
         (arc-x1 (arc-point-x circle-x start radius))
         (arc-y1 (arc-point-y circle-y start radius))
         (arc-x2 (arc-point-x circle-x end radius))
         (arc-y2 (arc-point-y circle-y end radius))
         ;we calculate the middle arc-point to determine which is the right side
         (half-x (arc-point-x circle-x half-angle radius))
         (half-y (arc-point-y circle-y half-angle radius)))
    (define (right-side-y? x y)
      (let* ((dividing-line-slope       (/ (- arc-y2 arc-y1) (- arc-x2 arc-x1)))
             (dividing-line-yintercept  (- arc-y1 (* dividing-line-slope arc-x1)))
             (right-yintercept          (- half-y (* dividing-line-slope half-x)))
             (right-value-test          (> right-yintercept dividing-line-yintercept))
             (point-yintercept          (- y (* dividing-line-slope x))) 
             (point-test                (> point-yintercept dividing-line-yintercept)))
        (eq? right-value-test point-test)))
    (define (line-intersect-arc? x1 y1 x2 y2)
      ;return the point where line intersects arc. intersection of a y line with a circle, 2 possible x values
      (define (yline-intersect-circle? y)
        (let ((result1 (+ circle-x (sqrt (- (expt radius 2) (expt (- y circle-y) 2)))))
              (result2 (- circle-x (sqrt (- (expt radius 2) (expt (- y circle-y) 2))))))
          (if (real? result1)
              (cond ((and (in-between? result1 xs xb) (in-between? result2 xs xb)) 
                     (list (list result1 y) (list result2 y)))
                    ((in-between? result1 xs xb)
                     (list (list result1 y)))
                    ((in-between? result2 xs xb) 
                     (list (list result2 y)))
                    (else #f))
              #f)))
      ;return the point where line intersects arc. intersection of a x line with a circle, 2 possible y values
      (define (xline-intersect-circle? x)
        (let ((result1 (+ circle-y (sqrt (- (expt radius 2) (expt (- x circle-x) 2)))))
              (result2 (- circle-y (sqrt (- (expt radius 2) (expt (- x circle-x) 2))))))
          (if (real? result1)
              (cond ((and (in-between? result1 ys yb) (in-between? result2 ys yb)) 
                     (list (list x result1) (list x result2)))
                    ((in-between? result1 ys yb) 
                     (list (list x result1)))
                    ((in-between? result2 ys yb) 
                     (list (list x result2)))
                    (else #f))
              #f)))
      (if (= x1 x2)
          ((lambda (x) (if (eq? x #f) #f (ormap (lambda (a) (apply right-side-y? a)) x))) (xline-intersect-circle? x1))    ;is a x line, find y values
          ((lambda (x) (if (eq? x #f) #f (ormap (lambda (a) (apply right-side-y? a)) x))) (yline-intersect-circle? y1))))  ;is a y line, find x values
    (cond ((or (point-in-rect? arc-x1 arc-y1 xs ys xb yb) (point-in-rect? arc-x2 arc-y2 xs ys xb yb)) #t)
          ((or (line-intersect-arc? xs ys xs yb) 
               (line-intersect-arc? xs yb xb yb)
               (line-intersect-arc? xb yb xb ys)
               (line-intersect-arc? xb ys xs ys)) #t)
          (else #f))))

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
