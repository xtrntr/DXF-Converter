#lang racket

(require "structs.rkt"
         "read_dxf.rkt"
         "geometric_functions.rkt"
         "ils-pattern-generator.rkt"
         "ids-pattern-generator.rkt"
         "constants.rkt"
         racket/gui/base pict
         racket/draw
         mrlib/path-dialog
         racket/math
         math/matrix
         framework)

;; global variables
(define struct-list '())
(define search-list '())
(define list-of-layers '())
(define select-box '())
(define display-select-box #f)
(define drawing-scale '())
(define stripped '())
(define set-park-position #f)

;; gui definitions
(define editor-frame-width 800)
(define editor-frame-height 600)
(define global-x-offset 0)
(define base-offset (- editor-frame-height 150))
(define global-y-offset base-offset)
(define global-x-scale 1)
(define global-y-scale -1)
(define dimension-scale 0) ;scale the dimensions of the shape in DXF file to fit the current frame dimensions
(define transformation-matrix (vector 1 0 0 1 0 0))
(define rotation 0)
(define init-x 0)
(define init-y 0)
(define left 0)
(define bottom 0)

;create entity and struct list when opening new file
(define (open-file input-port)
  (set! struct-list (file->struct-list input-port))
  (update-layer-list struct-list)
  (set! drawing-scale (car (get-scales struct-list)))
  (set! left (cadr (get-scales struct-list)))
  (set! bottom (caddr (get-scales struct-list)))
  (set! search-list (rescale struct-list drawing-scale))
  (display-layers))

(define (get-relevant-list)
  (filter-struct-list search-list (lambda (i) (and (entity-visible i) (entity-selected i)))))

(define (select-highlighted)
  (lambda (x) 
    (when (path? x) 
      (when (filter-struct-list (path-entities x) (select-highlighted))
        (set-entity-selected! x #t)))
    (when (entity-highlighted x)
      (set-entity-selected! x #t)
      (set-entity-highlighted! x #f))))
(define (highlight-path)
  (define (any-entity-highlighted? lst)
    (cond ((empty? lst) #f)
          ((entity-highlighted (car lst)) #t)
          (else (any-entity-highlighted? (cdr lst)))))
  (map (lambda (x) (when (any-entity-highlighted? (path-entities x))
                     (foldl set-entity-highlighted! #t (path-entities x))))
       (filter-struct-list search-list path?)))
(define (unselect-all)
  (lambda (x) 
    (when (path? x) (filter-struct-list (path-entities x) (unselect-all)))
    (set-entity-selected! x #f)))
(define (delete-selected)
  (lambda (x) 
    (when (path? x) (filter-struct-list (path-entities x) (delete-selected)))
    (when (entity-selected x) (set-entity-visible! x #f) (set-entity-selected! x #f))))
                       
;; pass intersect? the start and end point of select box and the struct-list
;; it will traverse the struct-list to see if any elements
(define (intersect? x1 y1 x2 y2 struct-lst)
  (let ((big-x (biggest (list x1 x2)))
        (big-y (biggest (list y1 y2)))
        (small-x (smallest (list x1 x2)))
        (small-y (smallest (list y1 y2))))
    (for/list ([i struct-lst])
      ;only calculate intersections for visible and not yet selected items
      (when (and (entity-visible i) (not (entity-selected i)))
        (cond ((line? i) 
               (if (cohen-sutherland i small-x small-y big-x big-y) 
                   (set-entity-highlighted! i #t)
                   (set-entity-highlighted! i #f)))
              ((arc? i)
               (if (arc-intersect? i small-x small-y big-x big-y) 
                   (set-entity-highlighted! i #t)
                   (set-entity-highlighted! i #f)))
              ((point? i)
               (if (point-in-rect? (point-x i) (point-y i) small-x small-y big-x big-y) 
                   (set-entity-highlighted! i #t)
                   (set-entity-highlighted! i #f)))
              ((path? i)
               (intersect? x1 y1 x2 y2 (path-entities i))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; scaling for display - only done once ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; return the scales
(define (get-scales struct-lst)
  (let* ((top (biggest (get-all-y struct-lst)))
         (bottom (smallest (get-all-y struct-lst)))
         (left (smallest (get-all-x struct-lst)))
         (right (biggest (get-all-x struct-lst)))
         (height (abs (- top bottom)))
         (width (abs (- right left)))
         (x-scale (/ editor-frame-width width))
         (y-scale (/ editor-frame-height height))
         (drawing-scale (smallest (list x-scale y-scale))))
    (list drawing-scale left bottom)))

(define (rescale struct-lst scale)
  (flatten (for/list ([i struct-lst])
             (match i
               [(line layer highlighted selected visible x1 y1 x2 y2)                           (line layer (scale-x-coord x1) (scale-y-coord y1) (scale-x-coord x2) (scale-y-coord y2))]
               [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (arc layer (scale-x-coord x) (scale-y-coord y) (* scale radius) start end (scale-x-coord x1) (scale-y-coord y1) (scale-x-coord x2) (scale-y-coord y2) (scale-x-coord x3) (scale-y-coord y3))]
               [(point layer highlighted selected visible x y)                                  (point layer (scale-x-coord x) (scale-y-coord y))]
               [(path layer highlighted selected visible path-list)                             (path layer (rescale path-list scale))]))))

(define (downscale struct-lst scale)
  (flatten (for/list ([i struct-lst])
             (match i
               [(line layer highlighted selected visible x1 y1 x2 y2)                           (line layer (unscale-x-coord x1) (unscale-y-coord y1) (unscale-x-coord x2) (unscale-y-coord y2))]
               [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (arc layer (unscale-x-coord x) (unscale-y-coord y) (/ radius scale) start end (unscale-x-coord x1) (unscale-y-coord y1) (unscale-x-coord x2) (unscale-y-coord y2) (unscale-x-coord x3) (unscale-y-coord y3))]
               [(point layer highlighted selected visible x y)                                  (point layer (unscale-x-coord x) (unscale-y-coord y))]
               [(path layer highlighted selected visible path-list)                             (path layer (downscale path-list scale))]))))

(define (scale-x-coord coord)
  (* drawing-scale (- coord left)))
(define (scale-y-coord coord)
  (* drawing-scale (- coord bottom)))

(define (unscale-x-coord coord)
  (+ left (/ coord drawing-scale)))
(define (unscale-y-coord coord)
  (+ bottom (/ coord drawing-scale)))

;; drawing functions
(define (draw-point x y selected highlighted)
  (if (or selected highlighted)
      (send drawer set-pen red-pen)
      (send drawer set-pen normal-pen))
  (send drawer draw-point x y))

(define (draw-line x1 y1 x2 y2 selected highlighted)
  (if (or selected highlighted)
      (send drawer set-pen red-pen)
      (send drawer set-pen normal-pen))
  (send drawer draw-line x1 y1 x2 y2))

;; racket's draw-arc function's x,y starts at bottom left corner (docs say top left but inverted because of -ve y-scale)
;; DXF provided arc x,y coordinates are at the center of the arc/circle
(define (draw-arc x y radius start end selected highlighted)
  (if (or selected highlighted)
      (send drawer set-pen red-pen)
      (send drawer set-pen normal-pen))
  (let ((convert-angle1 (degrees->radians (- 360 start))) ;; DXF angles are CW, Racket angles are CCW (because of inverting y scale)
        (convert-angle2 (degrees->radians (- 360 end)))
        (start-x (- x radius))
        (start-y (- y radius)))
    (send drawer draw-arc start-x start-y (* 2 radius) (* 2 radius) convert-angle2 convert-angle1)))

(define (draw-objects lst) ;get a struct-list.
  (define (apply-procedure x)
    (when (entity-visible x)
      (match x
        [(line layer highlighted selected visible x1 y1 x2 y2)                           (draw-line x1 y1 x2 y2 selected highlighted)]
        [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (draw-arc x y radius start end selected highlighted)]
        [(point layer highlighted selected visible x y)                                  (draw-point x y selected highlighted)]
        [(path layer highlighted selected visible path-list)                             (draw-objects path-list)])))
  (map apply-procedure lst))

;; gui control/frame definitions
(define top-frame (new frame%
                       [label "KR"]
                       [width editor-frame-width]
                       [height editor-frame-height]
                       [alignment (list 'left 'top)]))

(define menu-bar (new menu-bar%
                      (parent top-frame)))

(define file (new menu%
                  (label "&File")
                  (parent menu-bar)))

(new menu-item%
     (label "&Open DXF File ")
     (parent file)
     (callback (lambda (b e)
                 (define input-port-or-not (send open run))
                 (when input-port-or-not
                   (open-file input-port-or-not)))))
                     

(define open (new path-dialog%
                  [existing? #t]
                  [filters (list (list "DXF Files" "*.dxf") (list "Text Files" "*.txt"))]))

#|
global-x-scale
global-y-scale
transformation-matrix
global-x-offset
global-y-offset
rotation
make-object for hand,arrow,cross
search-list
|#
(define my-canvas%
  (class canvas%
    
    ;; KEYBOARD events
    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (special-control-key #t)
        (case key
          ['wheel-up    (set! global-x-scale (+ global-x-scale 0.1)) 
                        (set! global-y-scale (- global-y-scale 0.1))
                        (send drawer set-transformation (vector transformation-matrix global-x-offset global-y-offset global-x-scale global-y-scale rotation))]
          ['escape      (filter-struct-list search-list (unselect-all))]
          ['wheel-down  (when (> (- global-x-scale 0.1) 0) 
                          (set! global-x-scale (- global-x-scale 0.1))
                          (set! global-y-scale (+ global-y-scale 0.1))
                          (send drawer set-transformation (vector transformation-matrix global-x-offset global-y-offset global-x-scale global-y-scale rotation)))]
          ['#\backspace (filter-struct-list search-list (delete-selected))]))
      (send canvas refresh))
    
    ;; MOUSE events
    (define/override (on-event event)
      (define x (send event get-x))
      (define y (send event get-y))
      (define-syntax-rule (is-key-event? query)
        (send event query))
      (define (is-mouse-event? query)
        (equal? query (send event get-event-type)))
      
      ;; scale the x and y values.
      (define (scalex-to-display x)
        (/ (- x global-x-offset) global-x-scale))
      (define (scaley-to-display y) 
        (/ (- y global-y-offset) global-y-scale))
      (define scaled-x (scalex-to-display (send event get-x)))
      (define scaled-y (scaley-to-display (send event get-y)))
      
      ;key and mouse combinations
      (define start-panning? (is-mouse-event? 'left-down))
      (define is-panning? (send event dragging?))
      (define end-panning? (is-mouse-event? 'left-up))
      (define start-selecting? (and (is-mouse-event? 'left-down) (is-key-event? get-caps-down)))
      (define is-selecting? (and (send event dragging?) (is-key-event? get-caps-down)))
      (define end-selecting? (and (is-mouse-event? 'left-up) (is-key-event? get-caps-down)))
      (define set-park-position? (and set-park-position (is-mouse-event? 'left-down)))
      
      (cond
        (set-park-position?
         (display (list (unscale-x-coord scaled-x) (unscale-y-coord scaled-y)))
         (send drawer draw-point scaled-x scaled-y)
         (display (optimize-pattern (get-relevant-list) (point "origin" (unscale-x-coord scaled-x) (unscale-y-coord scaled-y))))
         (set! set-park-position #f))
        (start-selecting?
         (set! init-x scaled-x)
         (set! init-y scaled-y)
         (set! display-select-box #t))
        (end-selecting?
         (send canvas set-cursor (make-object cursor% 'arrow))
         (set! display-select-box #f)
         (filter-struct-list search-list (select-highlighted))
         (send canvas refresh))
        (is-selecting?
         (send canvas set-cursor (make-object cursor% 'cross))
         (intersect? init-x init-y scaled-x scaled-y search-list)
         (highlight-path)
         (set! select-box (list (list init-x init-y scaled-x init-y #t #f)
                                (list scaled-x init-y scaled-x scaled-y #t #f)
                                (list scaled-x scaled-y init-x scaled-y #t #f) 
                                (list init-x scaled-y init-x init-y #t #f)))
         (send canvas refresh))
        (start-panning?
         (set! init-x x)
         (set! init-y y)
         (send drawer draw-point scaled-x scaled-y))
        (end-panning?
         (send canvas set-cursor (make-object cursor% 'arrow))
         (set! global-x-offset (vector-ref (send drawer get-transformation) 1)) 
         (set! global-y-offset (vector-ref (send drawer get-transformation) 2)))
        (is-panning?
         (let* ((current-x (- x init-x))
                (current-y (- y init-y)))
           (send canvas set-cursor (make-object cursor% 'hand))
           (send drawer set-transformation (vector transformation-matrix (+ current-x global-x-offset) (+ current-y global-y-offset) global-x-scale global-y-scale rotation))
           (send canvas refresh)))))
    
    (define/override (on-paint)
       (send drawer set-brush no-brush)
      (when display-select-box (draw-select-box select-box))
      (draw-objects search-list)
      (send drawer set-pen normal-pen))
    
    (super-instantiate ())))

(define canvas (new my-canvas%
                    [parent top-frame]))

(define refocus-button (new button%
                    [label "Refocus"]
                    [parent top-frame]
                    [callback (lambda (b e)
                                (set! global-x-offset 0)
                                (set! global-y-offset base-offset)
                                (set! global-x-scale 1)
                                (set! global-y-scale -1)
                                (send drawer set-transformation (vector transformation-matrix global-x-offset global-y-offset global-x-scale global-y-scale rotation))
                                (send canvas on-paint)
                                (send canvas refresh))]))

(define (draw-select-box lst)
  (for/list ([i lst])
    (apply draw-line i)))

(define (update-layer-list struct-list)
  (set! list-of-layers (map (lambda (x) (if (string? x) x (number->string x)))
                            (remove-duplicates (map entity-layer struct-list))))) ;layers as numbers changed to string

(define (delete-existing-children frame children-type)
  (map (lambda (x) (when (is-a? x children-type) (send frame delete-child x)))
       (send frame get-children)))

(define (display-layers)
  (delete-existing-children top-frame check-box%)
  (for/list ([i list-of-layers])
    (new check-box%
         (label i)
         (parent top-frame)
         (callback (lambda (checked e)
                     (define make-visible? (send checked get-value))
                     (define (entity-same-layer? struct) (equal? (entity-layer struct) i))
                     (define (toggle-visibility struct)
                       (if make-visible? 
                           (begin (set-entity-visible! struct #t) (set-entity-selected! struct #f))
                           (set-entity-visible! struct #f)))
                     (define (map-toggle-visibility struct-list)
                       (cond ((empty? struct-list) '())
                             ((path? (car struct-list)) (begin 
                                                          (toggle-visibility (car struct-list))
                                                          (map-toggle-visibility (path-entities (car struct-list)))
                                                          (map-toggle-visibility (cdr struct-list))))
                             (else (begin (toggle-visibility (car struct-list))
                                          (map-toggle-visibility (cdr struct-list))))))
                     (map-toggle-visibility (filter-struct-list search-list entity-same-layer?))
                     (draw-objects search-list)
                     (send canvas on-paint)
                     (send canvas refresh-now))))))

;(define (struct-list->hash-table))

(define cell-contents-hash (make-hash))

(define (get-cell-contents row column)
  (hash-ref cell-contents-hash (list row column) ""))

(define (set-cell-contents! row column contents)
  (hash-set! cell-contents-hash (list row column) contents))

(define (get-column-label i)
  (list-ref column-labels i))

(define (get-row-label i)
  (format "Row ~a" i))

(define (get-column-alignment i)
  'left)

(define (delete-row i)
  (let ((old-hash cell-contents-hash))
    (set! cell-contents-hash (make-hash))
    (hash-for-each
     old-hash
     (lambda (k v)
       (match-define (list row col) k)
       (cond
         ((< row i) (set-cell-contents! row col v))
         ((> row i) (set-cell-contents! (- row 1) col v)))))))

(define (add-row-before i)
  (let ((old-hash cell-contents-hash))
    (set! cell-contents-hash (make-hash))
    (hash-for-each
     old-hash
     (lambda (k v)
       (match-define (list row col) k)
       (cond
         ((< row i) (set-cell-contents! row col v))
         (else (set-cell-contents! (+ row 1) col v)))))))
  

(define create (new path-dialog%
                    [put? #t]
                    [filters (list (list "Text Files" "*.txt"))]))

(define generate-button
    (new button%
       [label "Generate IDS pattern file"]
       [parent top-frame]
       [callback (lambda (b e)
                   (set! stripped (get-relevant-list))
                   (generate-pattern-file (downscale stripped drawing-scale) (open-output-file (send create run) #:mode 'binary #:exists 'truncate/replace)))]))

(define optimize-button
  (new button%
       [label "Optimize pattern"]
       [parent top-frame]
       [callback (lambda (b e)
                   (set! set-park-position #t)
                   (send canvas set-cursor (make-object cursor% 'cross)))]))

; Create a dialog
(define dialog 
  (new dialog% 
    [label "ILS pattern generation settings"]
    [width 500]
    [height 70]
    [min-height 70]))
 
; Add a text field to the dialog
(new list-box% 
     [parent dialog]
     [choices '("Jetting Valve" "Slider Valve")]
     [label "Dispenser Type"])
(new list-box% 
     [parent dialog]
     [choices '("Left Head" "Right Head")]
     [label "Dispenser Head"])
 
; Add a horizontal panel to the dialog, with centering for buttons
(define panel (new horizontal-panel% [parent dialog]
                                     [alignment '(center center)]))
 
; Add Cancel and Ok buttons to the horizontal panel
(new button% 
     [parent panel] 
     [label "Cancel"]
     [callback (lambda (b e)
                       (send dialog show #f))])
(new button% 
     [parent panel] 
     [label "Ok"]
     [callback (lambda (b e)
                 (set! stripped (get-relevant-list))
                 (generate-ils-pattern (downscale stripped drawing-scale) (open-output-file (send create run) #:mode 'binary #:exists 'truncate/replace))
                 (send dialog show #f))])

(when (system-position-ok-before-cancel?)
  (send panel change-children reverse))

(new menu-item%
     (label "&Generate ILS pattern file")
     (parent file)
     (callback (lambda (b e)
                 (send dialog show #t))))


(define no-brush (new brush% [style 'transparent]))
(define red-pen (new pen% [color "red"] [width 2]))
(define normal-pen (new pen% [color "black"] [width 1]))
(define drawer (send canvas get-dc))

(foldl set-entity-visible! #t struct-list)
(send top-frame show #t)
(send drawer set-transformation (vector transformation-matrix global-x-offset global-y-offset global-x-scale global-y-scale rotation))
(sleep/yield 0.1)