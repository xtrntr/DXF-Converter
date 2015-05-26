#lang racket/gui

(require "structs.rkt"
         "geometric-functions.rkt"
         "struct-list-utils.rkt"
         racket/draw)

(provide dxf-canvas%)
        
(define dxf-canvas%
  (class canvas%
    ;shorten
    (inherit get-dc)
    
    (init-field search-list
                x-offset
                y-offset
                drawing-scale
                unscale-x
                scale-x
                unscale-y
                scale-y
                update-spreadsheet
                [x-scale 1]
                [y-scale -1]
                [display-select-box #f]
                [select-box '()])
    
    (field [set-park-position #f]
           [rotation 0]
           [init-x 0]
           [init-y 0]
           [transformation-matrix (vector 1 0 0 1 0 0)])
    
    (define no-brush (new brush% [style 'transparent]))
    (define red-pen (new pen% [color "red"] [width 2]))
    (define normal-pen (new pen% [color "black"] [width 1]))
    
    ;; DRAWING functions
    (define (draw-point x y selected highlighted)
      (define drawer (get-dc))
      (if (or selected highlighted)
          (send drawer set-pen red-pen)
          (send drawer set-pen normal-pen))
      (send drawer draw-point x y))
    
    (define (draw-line x1 y1 x2 y2 selected highlighted)
      (define drawer (get-dc))
      (if (or selected highlighted)
          (send drawer set-pen red-pen)
          (send drawer set-pen normal-pen))
      (send drawer draw-line x1 y1 x2 y2))
    
    ;racket's draw-arc function's x,y starts at bottom left corner (docs say top left but inverted because of -ve y-scale)
    ;DXF provided arc x,y coordinates are at the center of the arc/circle
    (define (draw-arc x y radius start end selected highlighted)
      (define drawer (get-dc))
      (if (or selected highlighted)
          (send drawer set-pen red-pen)
          (send drawer set-pen normal-pen))
      (let ((convert-angle1 (degrees->radians (- 360 start))) ;; DXF angles are CW, Racket angles are CCW (because of inverting y scale)
            (convert-angle2 (degrees->radians (- 360 end)))
            (start-x (- x radius))
            (start-y (- y radius)))
        (send drawer draw-arc start-x start-y (* 2 radius) (* 2 radius) convert-angle2 convert-angle1)))
    
    ;expose to allow checking/unchecking of layers
    (define/public (draw-objects lst) ;get a struct-list.
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
                   (if (line-intersect? i small-x small-y big-x big-y) 
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
    
    (define/public (update-canvas)
      (define drawer (get-dc))
      (send drawer set-transformation (vector transformation-matrix x-offset y-offset x-scale y-scale rotation))
      (send this refresh))
      
    
    ;; KEYBOARD events
    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (special-control-key #t)
        (case key
          ['wheel-up    (set! x-scale (+ x-scale 0.1)) 
                        (set! y-scale (- y-scale 0.1))]
          ['escape      (unselect-all search-list)
                        (update-spreadsheet search-list)]
          ['wheel-down  (when (> (- x-scale 0.1) 0) 
                          (set! x-scale (- x-scale 0.1))
                          (set! y-scale (+ y-scale 0.1)))]
          ['#\backspace (delete-selected search-list)
                        (update-spreadsheet search-list)]))
      (update-canvas))
    
    ;; MOUSE events
    (define/override (on-event event)
      (define drawer (get-dc))
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
      (define start-selecting? (and (is-mouse-event? 'left-down) (is-key-event? get-control-down)))
      (define is-selecting? (and (send event dragging?) (is-key-event? get-control-down)))
      (define end-selecting? (and (is-mouse-event? 'left-up) (is-key-event? get-control-down)))
      (define set-park-position? (and set-park-position (is-mouse-event? 'left-down)))
      
      (cond
        (set-park-position?
         (display (list (unscale-x scaled-x) (unscale-y scaled-y)))
         (send drawer draw-point scaled-x scaled-y)
         ;(display (optimize-pattern (get-relevant-list) (point "origin" (unscale-x scaled-x) (unscale-y scaled-y))))
         (set! set-park-position #f))
        (start-selecting?
         (set! init-x scaled-x)
         (set! init-y scaled-y)
         (set! display-select-box #t))
        (end-selecting?
         (send this set-cursor (make-object cursor% 'arrow))
         (set! display-select-box #f)
         (select-highlighted search-list)
         (update-spreadsheet search-list)
         (update-canvas))
        (is-selecting?
         (send this set-cursor (make-object cursor% 'cross))
         (intersect? init-x init-y scaled-x scaled-y search-list)
         (highlight-path search-list)
         (set! select-box (list (list init-x init-y scaled-x init-y #t #f)
                                (list scaled-x init-y scaled-x scaled-y #t #f)
                                (list scaled-x scaled-y init-x scaled-y #t #f) 
                                (list init-x scaled-y init-x init-y #t #f)))
         (update-canvas))
        (start-panning?
         (set! init-x x)
         (set! init-y y))
        (end-panning?
         (send this set-cursor (make-object cursor% 'arrow))
         (set! x-offset (vector-ref (send drawer get-transformation) 1)) 
         (set! y-offset (vector-ref (send drawer get-transformation) 2)))
        (is-panning?
         (let* ((current-x (- x init-x))
                (current-y (- y init-y)))
           (send this set-cursor (make-object cursor% 'hand))
           (send drawer set-transformation (vector transformation-matrix (+ current-x x-offset) (+ current-y y-offset) x-scale y-scale rotation))
           (send this refresh)))))
    
    (define/override (on-paint)
      (define drawer (get-dc))
      (send drawer set-brush no-brush)
      (when display-select-box (draw-select-box select-box))
      (draw-objects search-list)
      (send drawer set-pen normal-pen))
    
    (super-instantiate ())))