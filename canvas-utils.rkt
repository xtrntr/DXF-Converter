#lang racket

(require "structs.rkt"
         "utils.rkt"
         "macros.rkt")

(provide point-in-rect?
         get-arc-points
         line-intersect?
         arc-intersect?
         get-display-scale)

;scaling for display - only done once
(define (get-display-scale struct-lst frame-width frame-height)
  (define (get-bounding-x struct-lst)
    (flatten (for/list ([i struct-lst])
               (match i
                 [(struct* line  ([p1 p1]
                                  [p2 p2]))              (list (point-x p1) (point-x p2))]
                 [(struct* arc   ([center center]
                                  [radius radius]))      (list (+ (point-x center) radius) (- (point-x center) radius))]
                 [(struct* dot   ([p p]))                (list (point-x p))]
                 [(struct* path  ([entities entities]))  (get-bounding-x entities)]))))
  (define (get-bounding-y struct-lst)
    (flatten (for/list ([i struct-lst])
               (match i
                 [(struct* line  ([p1 p1]
                                  [p2 p2]))              (list (point-y p1) (point-y p2))]
                 [(struct* arc   ([center center]
                                  [radius radius]))      (list (+ (point-y center) radius) (- (point-y center) radius))]
                 [(struct* dot   ([p p]))                (list (point-y p))]
                 [(struct* path  ([entities entities]))  (get-bounding-y entities)]))))
  (let* ((top (biggest (get-bounding-y struct-lst)))
         (bottom (smallest (get-bounding-y struct-lst)))
         (left (smallest (get-bounding-x struct-lst)))
         (right (biggest (get-bounding-x struct-lst)))
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
;; cohen-sutherland algorithm to detect line-rectangle intersection. separate 2d area into 9 rectangles where 0 represents the selected area
;; region numbers are bit->decimal
;; 9   1   5                1001   0001   0101
;; 8   0   4      --->      1000   0000   0100
;; 10  2   6                1010   0010   0110
(define (line-intersect? line-struct xs ys xb yb)
  (let ((lx1 (point-x (line-p1 line-struct)))
        (ly1 (point-y (line-p1 line-struct)))
        (lx2 (point-x (line-p2 line-struct)))
        (ly2 (point-y( line-p2 line-struct))))
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
         (circle-x (point-x (arc-center arc-struct)))
         (circle-y (point-y (arc-center arc-struct)))
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