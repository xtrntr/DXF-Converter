#lang typed/racket

#| 

The difference between lst-utils and canvas-utils is that lst-utils can be used outside of the canvas. 
lst-utils is meant for containing list operations that do sorting and searching.
canvas-utils is meant for containing operations that affect the interactivity/display of canvas i.e. toggling visibility, intersection queries

|#

(require "structs.rkt"
         "utils.rkt")

(provide (all-defined-out))

(: get-selected-entities (-> Entities Entities)) 
(define (get-selected-entities lst)
  (filter (lambda ([i : Entity]) (and (entity-visible i) (entity-selected i))) lst))

(: get-visible-entities (-> Entities Entities)) 
(define (get-visible-entities lst)
  (filter (lambda ([i : Entity]) (entity-visible i)) lst))

;;HELPER functions for imperatively setting entity values for display purposes.
(: select-highlighted! (-> Entities Void))
(define (select-highlighted! lst)
  (: select! (-> Entity Void))
  (define (select! x)
    (set-entity-selected! x #t)
    (set-entity-highlighted! x #f))
  (for ([x : Entity (filter entity-highlighted lst)])
       (if (path? x)
           (begin (select! x)
                  (map select! (path-entities x)))
           (select! x))))

(: highlight-path! (-> path Void))
(define (highlight-path! p)
  (if (ormap entity-highlighted (path-entities p))
           (begin (set-entity-highlighted! p #t)
                  (for ([i : Entity (path-entities p)]) (set-entity-highlighted! i #t)))
           (begin (set-entity-highlighted! p #f)
                  (for ([i : Entity (path-entities p)]) (set-entity-highlighted! i #f)))))

(: highlight-paths! (-> Entities Void))
(define (highlight-paths! lst)
  (for ([p : path (filter path? lst)])
       (highlight-path! p)))

(: unselect-all! (-> Entities Void))
(define (unselect-all! lst)
  (: unselect! (-> Entity Void))
  (define (unselect! x)
    (set-entity-selected! x #f)
    (set-entity-highlighted! x #f))
  (for ([x : Entity lst])
       (if (path? x)
           (begin (unselect! x)
                  (map unselect! (path-entities x)))
           (unselect! x))))

(: delete-selected! (-> Entities Void))
(define (delete-selected! lst)
  (: delete! (-> Entity Void))
  (define (delete! x)
    (set-entity-selected! x #f)
    (set-entity-visible! x #f))
  (for ([x : Entity (filter entity-selected lst)])
       (if (path? x)
           (begin (delete! x)
                  (map delete! (path-entities x)))
           (delete! x))))

;scaling for display - done whenever the visible entities change
(: get-display-scale (-> Entities Real Real Real))
(define (get-display-scale entity-lst canvas-width canvas-height)
  (let* ([bottom (biggest (get-y-vals entity-lst))]
         [top (smallest (get-y-vals entity-lst))]
         [left (smallest (get-x-vals entity-lst))]
         [right (biggest (get-x-vals entity-lst))]
         [height (abs (- top bottom))]
         [width (abs (- right left))]
         [x-scale (/ canvas-width width)]
         [y-scale (/ canvas-height height)]
         ;0.864 -> 0.8, 1.113 -> 1.1
         [floor-0.1 (lambda ([x : Real])
                      (let* ([multiples (floor (/ x 0.1))]
                             [remainder (/ multiples 10)])
                        (if (= remainder 0) 0.1 remainder)))]
         [drawing-scale (floor-0.1 (smallest (list x-scale y-scale)))])
    drawing-scale))
  
;; divide the complete 2d space into 9 boxes
;; algorithm to detect line-rectangle intersection. separate 2d area into 9 rectangles where 0 represents the selected area
;; region numbers are bit->decimal
;; 9   1   5                1001   0001   0101
;; 8   0   4      --->      1000   0000   0100
;; 10  2   6                1010   0010   0110
(: line-intersect? (-> line Real Real Real Real Boolean))
(define (line-intersect? line-struct xs ys xb yb)
  (let ((lx1 (node-x (line-p1 line-struct)))
        (ly1 (node-y (line-p1 line-struct)))
        (lx2 (node-x (line-p2 line-struct)))
        (ly2 (node-y( line-p2 line-struct))))
    (: compute-outcode (-> Real Real Integer))
    (define (compute-outcode x y)
      (let ((inside 0))
        (cond [(< x xs) (set! inside (bitwise-ior inside 1))]
              [(> x xb) (set! inside (bitwise-ior inside 2))])
        (cond [(< y ys) (set! inside (bitwise-ior inside 4))]
              [(> y yb) (set! inside (bitwise-ior inside 8))])
        inside))
    ;return #t if intersect
    (: trivial-accept? (-> Integer Integer Boolean))
    (define (trivial-accept? region1 region2)
      (or (not (bitwise-ior region1 region2)) 
          (= region1 0) 
          (= region2 0)
          (and (= region1 1) (= region2 2))
          (and (= region1 2) (= region2 1))
          (and (= region1 4) (= region2 8))
          (and (= region1 8) (= region2 4))))
    ;return #t if does not intersect
    (: trivial-reject? (-> Integer Integer Boolean))
    (define (trivial-reject? region1 region2)  
      (not (= (bitwise-and region1 region2) 0)))
    ;clip until no more ambiguous cases
    (: clip-until (-> Integer Integer Integer Boolean))
    (define (clip-until region1 region2 tries)
      (cond [(= tries 0) #f]
            [(trivial-reject? region1 region2) #f]
            [(trivial-accept? region1 region2) #t]
            [else (apply clip-until (cast (append (do-clip region1 region2) (list (- tries 1)))
                                          (List Integer Integer Integer)))]))
    (: not0 (-> Real Boolean))
    (define (not0 num) (not (= num 0)))
    (: do-clip (-> Integer Integer (List Integer Integer)))
    (define (do-clip region1 region2)
      (let* ([new-x : Real 0]
             [new-y : Real 0]
             [slope : Real (/ (- ly2 ly1) (- lx2 lx1))]
             [y-intercept : Real (- ly2 (* slope lx2))])
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
(define (arc-intersect? an-arc xs ys xb yb)
  (let* ([radius (arc-radius an-arc)]
         [ccw? (arc-ccw an-arc)]
         [circle-x (node-x (arc-center an-arc))]
         [circle-y (node-y (arc-center an-arc))]
         [radius (arc-radius an-arc)]
         [arc-x1 (node-x (arc-p1 an-arc))]
         [arc-y1 (node-y (arc-p1 an-arc))]
         [arc-x2 (node-x (arc-p2 an-arc))]
         [arc-y2 (node-y (arc-p2 an-arc))]
         [arc-x3 (node-x (arc-p3 an-arc))]
         [arc-y3 (node-y (arc-p3 an-arc))])
    (: right-side-y? (-> Real Real Boolean))
    (define (right-side-y? x y)
      (let* ((dividing-line-slope       (/ (- arc-y3 arc-y1) (- arc-x3 arc-x1)))
             (dividing-line-yintercept  (- arc-y1 (* dividing-line-slope arc-x1)))
             (right-yintercept          (- arc-y2 (* dividing-line-slope arc-x2)))
             (right-value-test          (> right-yintercept dividing-line-yintercept))
             (point-yintercept          (- y (* dividing-line-slope x))) 
             (point-test                (> point-yintercept dividing-line-yintercept)))
        (eq? right-value-test point-test)))
      ;return the point where line intersects arc. intersection of a y line with a circle, 2 possible x values
      (: yline-intersect-circle? (-> Real (U Boolean (Listof (List Real Real)))))
      (define (yline-intersect-circle? y)
        (let ([result1 (+ circle-x (sqrt (- (sqr radius) (sqr (- y circle-y)))))]
              [result2 (- circle-x (sqrt (- (sqr radius) (sqr (- y circle-y)))))])
          (if (and (real? result1) (real? result2))
              (cond [(and (in-between? result1 xs xb) (in-between? result2 xs xb)) 
                     (list (list result1 y) (list result2 y))]
                    [(in-between? result1 xs xb)
                     (list (list result1 y))]
                    [(in-between? result2 xs xb) 
                     (list (list result2 y))]
                    [else #f])
              #f)))
      ;return the point where line intersects arc. intersection of a x line with a circle, 2 possible y values
      (: xline-intersect-circle? (-> Real (U Boolean (Listof (List Real Real)))))
      (define (xline-intersect-circle? x)
        (let ([result1 (+ circle-y (sqrt (- (sqr radius) (sqr (- x circle-x)))))]
              [result2 (- circle-y (sqrt (- (sqr radius) (sqr (- x circle-x)))))])
          (if (and (real? result1) (real? result2))
              (cond [(and (in-between? result1 ys yb) (in-between? result2 ys yb)) 
                     (list (list x result1) (list x result2))]
                    [(in-between? result1 ys yb) 
                     (list (list x result1))]
                    [(in-between? result2 ys yb) 
                     (list (list x result2))]
                    [else #f])
              #f)))
    (: line-intersect-arc? (-> Real Real Real Real Boolean))
    (define (line-intersect-arc? x1 y1 x2 y2)
      (let ([result (if (= x1 x2)
                        (xline-intersect-circle? x1) ;is a x line, find y values
                        (yline-intersect-circle? y1))]) ;is a y line, find x values)])
        (if (not result)
            #f
            (ormap (lambda (a)
                     (apply right-side-y? (cast a (List Real Real))))
                   (cast result (Listof (List Real Real)))))))
    (if (arc-is-circle? an-arc)
        (cond [(and (point-in-rect? circle-x circle-y xs ys xb yb)
                    (or (point-in-rect? (+ circle-x radius) (+ circle-y radius) xs ys xb yb)
                        (point-in-rect? (+ circle-x radius) (- circle-y radius) xs ys xb yb)
                        (point-in-rect? (- circle-x radius) (+ circle-y radius) xs ys xb yb)
                        (point-in-rect? (- circle-x radius) (- circle-y radius) xs ys xb yb))) #t]
              [(or (xline-intersect-circle? xs)
                   (xline-intersect-circle? xb)
                   (yline-intersect-circle? ys)
                   (yline-intersect-circle? yb)) #t]
              [else #f])
        (cond [(or (point-in-rect? arc-x1 arc-y1 xs ys xb yb) (point-in-rect? arc-x3 arc-y3 xs ys xb yb)) #t]
              [(or (line-intersect-arc? xs ys xs yb) 
                   (line-intersect-arc? xs yb xb yb)
                   (line-intersect-arc? xb yb xb ys)
                   (line-intersect-arc? xb ys xs ys)) #t]
              [else #f]))))

(: circ2dot (-> arc dot))
(define (circ2dot a)
  (cast (make-selected (make-dot (entity-layer a)
                           (node-x (arc-center a))
                           (node-y (arc-center a)))) dot))

(: circ2dots (-> Entities Entities))
(define (circ2dots entity-lst)
  (for/list ([entity entity-lst])
            ;circles shouldn't be inside a path. so, don't look inside path-entities
            (if (and (entity-selected entity)
                     (entity-visible entity)
                     (arc? entity)
                     (arc-is-circle? entity))
                (circ2dot entity)
                entity)))