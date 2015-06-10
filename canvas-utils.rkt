#lang typed/racket

(require "structs.rkt"
         "utils.rkt")

(provide point-in-rect?
         get-arc-points
         line-intersect?
         arc-intersect?
         get-display-scale)

;scaling for display - only done once
(: get-display-scale (-> (Listof Entities) Real Real (Values Real Real Real)))
(define (get-display-scale struct-lst frame-width frame-height)
  (: get-bounding-x (-> (Listof Entities) (Listof Real)))
  (define (get-bounding-x struct-lst)
    (let loop : (Listof Real)
      ([lst : (Listof Entities) struct-lst]
       [acc : (Listof Real) '()])
      (cond ((empty? lst) acc)
            (else (loop (cdr lst) (append ((match-struct (dot  (list (point-x p))) 
                                                         (line (list (point-x p1) (point-x p2)))
                                                         (arc  (list (+ (point-x center) radius) (- (point-x center) radius)))
                                                         (path (lambda ([x : (Listof Entities)]) (get-bounding-x x)))) (car lst)) acc))))))
  (: get-bounding-y (-> (Listof Entities) (Listof Real)))
  (define (get-bounding-y struct-lst)
    (let loop : (Listof Real)
      ([lst : (Listof Entities) struct-lst]
       [acc : (Listof Real) '()])
      (cond ((empty? lst) acc)
            (else (loop (cdr lst) (append ((match-struct (dot  (list (point-y p))) 
                                                         (line (list (point-y p1) (point-y p2)))
                                                         (arc  (list (+ (point-y center) radius) (- (point-y center) radius)))
                                                         (path (lambda ([x : (Listof Entities)]) (get-bounding-y x)))) (car lst)) acc))))))
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
(: point-in-rect? (-> Real Real Real Real Real Real Boolean))
(define (point-in-rect? x y xs ys xb yb)
  (and (> x xs) (< x xb) (> y ys) (< y yb)))

;; divide the complete 2d space into 9 boxes
;; cohen-sutherland algorithm to detect line-rectangle intersection. separate 2d area into 9 rectangles where 0 represents the selected area
;; region numbers are bit->decimal
;; 9   1   5                1001   0001   0101
;; 8   0   4      --->      1000   0000   0100
;; 10  2   6                1010   0010   0110
(: line-intersect? (-> line Real Real Real Real Boolean))
(define (line-intersect? line-struct xs ys xb yb)
  (let ((lx1 (point-x (line-p1 line-struct)))
        (ly1 (point-y (line-p1 line-struct)))
        (lx2 (point-x (line-p2 line-struct)))
        (ly2 (point-y( line-p2 line-struct))))
    (: compute-outcode (-> Real Real Real))
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
    (: trivial-accept? (-> Real Real Boolean))
    (define (trivial-accept? region1 region2)
      (or (not (bitwise-ior (cast region1 Integer) (cast region2 Integer))) 
          (= region1 0) 
          (= region2 0)
          (and (= region1 1) (= region2 2))
          (and (= region1 2) (= region2 1))
          (and (= region1 4) (= region2 8))
          (and (= region1 8) (= region2 4))))
    ;return #t if does not intersect
    (: trivial-reject? (-> Real Real Boolean))
    (define (trivial-reject? region1 region2)  
      (not (= (bitwise-and (cast region1 Integer) (cast region2 Integer)) 0)))
    ;clip until no more ambiguous cases
    (: clip-until (-> (List Real Real) Real Boolean))
    (define (clip-until regs tries)
      (cond ((= tries 0) #f)
            ((trivial-reject? (first regs) (second regs)) #f)
            ((trivial-accept? (first regs) (second regs)) #t)
            (else  (clip-until (do-clip (first regs) (second regs)) (- tries 1)))))
    (: do-clip (-> Real Real (List Real Real)))
    (define (do-clip region1 region2)
      (: not0 (-> Real Boolean))
      (define (not0 num)
        (if (= num 0) #f #t))
      (let* ([new-x : Real 0]
             [new-y : Real 0]
             (slope (/ (- ly2 ly1) (- lx2 lx1)))
             (y-intercept (- ly2 (* slope lx2))))
        ;apply the formula y = y1 + slope * (x - x1), x = x1 + (y - y1) / slope
        (cond ((not0 (bitwise-and 8 (cast region2 Integer)))
               (set! new-x (/ (- yb y-intercept) slope))
               (set! new-y yb))
              ((not0 (bitwise-and 4 (cast region2 Integer)))
               (set! new-x (/ (- ys y-intercept) slope))
               (set! new-y ys))
              ((not0 (bitwise-and 2 (cast region2 Integer)))
               (set! new-x xb)
               (set! new-y (+ (* slope xb) y-intercept)))
              ((not0 (bitwise-and 1 (cast region2 Integer))) 
               (set! new-x xs)
               (set! new-y (+ (* slope xs) y-intercept))))
        (set! lx2 new-x)
        (set! ly2 new-y)
        (set! region2 (compute-outcode lx2 ly2)))
      (list region1 region2))
    (let* ((region1 (compute-outcode lx1 ly1))
           (region2 (compute-outcode lx2 ly2)))
      (clip-until (list region1 region2) 4))))

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
(: arc-intersect? (-> arc Real Real Real Real Boolean))
(define (arc-intersect? arc-struct xs ys xb yb)
  (let* ((radius : Real (arc-radius arc-struct))
         (center-x : Real (point-x (arc-center arc-struct)))
         (center-y : Real (point-y (arc-center arc-struct)))
         (start : Real (arc-start arc-struct))
         (end : Real (arc-end arc-struct))
         (angle-difference : Real (if (> end start) (- end start) (+ (- 360 start) end)))
         [arc-pts : (Listof Real) (get-arc-points center-x center-y radius start end)]
         [x1 : Real (first arc-pts)]
         [y1 : Real (second arc-pts)]
         [x2 : Real (third arc-pts)]
         [y2 : Real (fourth arc-pts)]
         [x3 : Real (fifth arc-pts)]
         [y3 : Real (sixth arc-pts)])
  
    (: line-intersect-arc? (-> Real Real Real Real Boolean))
    (define (line-intersect-arc? x1 y1 x2 y2)
      ;return the point where line intersects arc. intersection of a y line with a circle, 2 possible x values
      (: yline-intersect-circle? (-> Real (U Boolean (Listof (List Real Real)))))
      (define (yline-intersect-circle? y)
        (let ((result1 : (U Number Real) (+ center-x (sqrt (- (sqr radius) (sqr (- y center-y))))))
              (result2 : (U Number Real) (- center-x (sqrt (- (sqr radius) (sqr (- y center-y)))))))
          (if (real? result1)
              (let ([result1 : Real (cast result1 Real)]
                    [result2 : Real (cast result2 Real)])
                (cond ((and (in-between? result1 xs xb) (in-between? result2 xs xb)) 
                       (list (list result1 y) (list result2 y)))
                      ((in-between? result1 xs xb)
                       (list (list result1 y)))
                      ((in-between? result2 xs xb) 
                       (list (list result2 y)))
                      (else #f)))
              #f)))
      
      ;return the point where line intersects arc. intersection of a x line with a circle, 2 possible y values
      (: xline-intersect-circle? (-> Real (U Boolean (Listof (List Real Real)))))
      (define (xline-intersect-circle? x)
        (let ([result1 : (U Number Real) (+ center-y (sqrt (- (sqr radius) (sqr (- x center-x)))))]
              [result2 : (U Number Real) (- center-y (sqrt (- (sqr radius) (sqr (- x center-x)))))])
          (if (real? result1)
              (let ([result1 : Real (cast result1 Real)]
                    [result2 : Real (cast result2 Real)])
                (cond ((and (in-between? result1 ys yb) (in-between? result2 ys yb))
                       (list (list x result1) (list x result2)))
                      ((in-between? result1 ys yb)
                       (list (list x result1)))
                      ((in-between? result2 ys yb)
                       (list (list x result2)))
                      (else #f)))
              #f)))
      
      (: right-side-y? (-> (List Real Real) Boolean))
      (define (right-side-y? lst)
        (let* ((x (first lst))
               (y (second lst))
               (dividing-line-slope       (/ (- y3 y1) (- x3 x1)))
               (dividing-line-yintercept  (- y1 (* dividing-line-slope x1)))
               (right-yintercept          (- y2 (* dividing-line-slope x2)))
               (right-value-test          (> right-yintercept dividing-line-yintercept))
               (point-yintercept          (- y (* dividing-line-slope x))) 
               (point-test                (> point-yintercept dividing-line-yintercept)))
          (eq? right-value-test point-test)))
      
      (if (= x1 x2)
          ((lambda ([x : (U Boolean (Listof (List Real Real)))]) 
             (if (not x) #f
                 (ormap right-side-y? (cast x (Listof (List Real Real))))))
           (xline-intersect-circle? x1)) ;is a x line, find y values
          ((lambda ([x : (U Boolean (Listof (List Real Real)))]) 
             (if (not x) #f
                 (ormap right-side-y? (cast x (Listof (List Real Real))))))
           (yline-intersect-circle? y1)))) ;is a y line, find x values
    
    (cond ((or (point-in-rect? x1 y1 xs ys xb yb) (point-in-rect? x3 y3 xs ys xb yb)) #t)
          ((or (line-intersect-arc? xs ys xs yb) 
               (line-intersect-arc? xs yb xb yb)
               (line-intersect-arc? xb yb xb ys)
               (line-intersect-arc? xb ys xs ys)) #t)
          (else #f))))