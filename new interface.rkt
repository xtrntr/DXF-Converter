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

(define my-canvas%
  (class canvas%
    ;shorten
    (inherit get-dc)
    (define dc (get-dc))
    
    (init-field search-list
                x-offset
                y-offset
                drawing-scale
                [x-scale 1]
                [y-scale -1]
                [display-select-box #f]
                [select-box '()])
    
    (field [set-park-position #f]
           [rotation 0]
           [init-x 0]
           [init-y 0]
           [left 0]
           [bottom 0]
           [transformation-matrix (vector 1 0 0 1 0 0)])
    
    (define no-brush (new brush% [style 'transparent]))
    (define red-pen (new pen% [color "red"] [width 2]))
    (define normal-pen (new pen% [color "black"] [width 1]))
    
    ;; DRAWING functions
    (define (draw-point x y selected highlighted)
      (if (or selected highlighted)
          (send dc set-pen red-pen)
          (send dc set-pen normal-pen))
      (send dc draw-point x y))
    
    (define (draw-line x1 y1 x2 y2 selected highlighted)
      (if (or selected highlighted)
          (send dc set-pen red-pen)
          (send dc set-pen normal-pen))
      (send dc draw-line x1 y1 x2 y2))
    
    ;; racket's draw-arc function's x,y starts at bottom left corner (docs say top left but inverted because of -ve y-scale)
    ;; DXF provided arc x,y coordinates are at the center of the arc/circle
    (define (draw-arc x y radius start end selected highlighted)
      (if (or selected highlighted)
          (send dc set-pen red-pen)
          (send dc set-pen normal-pen))
      (let ((convert-angle1 (degrees->radians (- 360 start))) ;; DXF angles are CW, Racket angles are CCW (because of inverting y scale)
            (convert-angle2 (degrees->radians (- 360 end)))
            (start-x (- x radius))
            (start-y (- y radius)))
        (send dc draw-arc start-x start-y (* 2 radius) (* 2 radius) convert-angle2 convert-angle1)))
    (define (draw-objects lst) ;get a struct-list.
      (define (apply-procedure x)
        (when (entity-visible x)
          (match x
            [(line layer highlighted selected visible x1 y1 x2 y2)                           (draw-line x1 y1 x2 y2 selected highlighted)]
            [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (draw-arc x y radius start end selected highlighted)]
            [(point layer highlighted selected visible x y)                                  (draw-point x y selected highlighted)]
            [(path layer highlighted selected visible path-list)                             (draw-objects path-list)])))
      (map apply-procedure lst))
    
    (define (draw-select-box lst)
      (for/list ([i lst])
        (apply draw-line i)))
    
    ;; UTILITY functions
    ;pass intersect? the start and end point of select box and the struct-list
    ;it will traverse the struct-list to see if any elements
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
    
    (define (rescale struct-lst scale)
      (flatten (for/list ([i struct-lst])
                 (match i
                   [(line layer highlighted selected visible x1 y1 x2 y2)                           (line layer (scale-x x1) (scale-y y1) (scale-x x2) (scale-y y2))]
                   [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (arc layer (scale-x x) (scale-y y) (* scale radius) start end (scale-x x1) (scale-y y1) (scale-x x2) (scale-y y2) (scale-x x3) (scale-y y3))]
                   [(point layer highlighted selected visible x y)                                  (point layer (scale-x x) (scale-y y))]
                   [(path layer highlighted selected visible path-list)                             (path layer (rescale path-list scale))]))))
    
    (define (downscale struct-lst scale)
      (flatten (for/list ([i struct-lst])
                 (match i
                   [(line layer highlighted selected visible x1 y1 x2 y2)                           (line layer (unscale-x x1) (unscale-y y1) (unscale-x x2) (unscale-y y2))]
                   [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3) (arc layer (unscale-x x) (unscale-y y) (/ radius scale) start end (unscale-x x1) (unscale-y y1) (unscale-x x2) (unscale-y y2) (unscale-x x3) (unscale-y y3))]
                   [(point layer highlighted selected visible x y)                                  (point layer (unscale-x x) (unscale-y y))]
                   [(path layer highlighted selected visible path-list)                             (path layer (downscale path-list scale))]))))
    
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
    
    (define (scale-x coord)
      (* drawing-scale (- coord left)))
    
    (define (scale-y coord)
      (* drawing-scale (- coord bottom)))
    
    (define (unscale-x coord)
      (+ left (/ coord drawing-scale)))
    
    (define (unscale-y coord)
      (+ bottom (/ coord drawing-scale)))
    
    ;; KEYBOARD events
    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (special-control-key #t)
        (case key
          ['wheel-up    (set! x-scale (+ x-scale 0.1)) 
                        (set! y-scale (- y-scale 0.1))
                        (send this set-transformation (vector transformation-matrix x-offset y-offset x-scale y-scale rotation))]
          ['escape      (filter-struct-list search-list (unselect-all))]
          ['wheel-down  (when (> (- x-scale 0.1) 0) 
                          (set! x-scale (- x-scale 0.1))
                          (set! y-scale (+ y-scale 0.1))
                          (send dc set-transformation (vector transformation-matrix x-offset y-offset x-scale y-scale rotation)))]
          ['#\backspace (filter-struct-list search-list (delete-selected))]))
      (send this refresh))
    
    ;; MOUSE events
    (define/override (on-event event)
      (define x (send event get-x))
      (define y (send event get-y))
      (define-syntax-rule (is-key-event? query)
        (send event query))
      (define (is-mouse-event? query)
        (equal? query (send event get-event-type)))
      
      ;scale the x and y values.
      (define (scalex-to-display x)
        (/ (- x x-offset) x-scale))
      (define (scaley-to-display y) 
        (/ (- y y-offset) y-scale))
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
         (display (list (unscale-x scaled-x) (unscale-y scaled-y)))
         (send dc draw-point scaled-x scaled-y)
         (display (optimize-pattern (get-relevant-list) (point "origin" (unscale-x scaled-x) (unscale-y scaled-y))))
         (set! set-park-position #f))
        (start-selecting?
         (set! init-x scaled-x)
         (set! init-y scaled-y)
         (set! display-select-box #t))
        (end-selecting?
         (send this set-cursor (make-object cursor% 'arrow))
         (set! display-select-box #f)
         (filter-struct-list search-list (select-highlighted))
         (send this refresh))
        (is-selecting?
         (send this set-cursor (make-object cursor% 'cross))
         (intersect? init-x init-y scaled-x scaled-y search-list)
         (highlight-path)
         (set! select-box (list (list init-x init-y scaled-x init-y #t #f)
                                (list scaled-x init-y scaled-x scaled-y #t #f)
                                (list scaled-x scaled-y init-x scaled-y #t #f) 
                                (list init-x scaled-y init-x init-y #t #f)))
         (send this refresh))
        (start-panning?
         (set! init-x x)
         (set! init-y y)
         (send dc draw-point scaled-x scaled-y))
        (end-panning?
         (send this set-cursor (make-object cursor% 'arrow))
         (set! x-offset (vector-ref (send dc get-transformation) 1)) 
         (set! y-offset (vector-ref (send dc get-transformation) 2)))
        (is-panning?
         (let* ((current-x (- x init-x))
                (current-y (- y init-y)))
           (send this set-cursor (make-object cursor% 'hand))
           (send dc set-transformation (vector transformation-matrix (+ current-x x-offset) (+ current-y y-offset) x-scale y-scale rotation))
           (send this refresh)))))
    
    (define/override (on-paint)
       (send dc set-brush no-brush)
      (when display-select-box (draw-select-box select-box))
      (draw-objects search-list)
      (send dc set-pen normal-pen))
    
    (super-instantiate ())))