#lang racket/gui

(require "structs.rkt"
         "canvas-utils.rkt"
         "lst-utils.rkt"
         "utils.rkt"
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
                [display-start-end-points #f]
                [select-box '()])
    
    (field [set-park-position #f]
           [rotation 0]
           [init-x 0]
           [init-y 0]
           [transformation-matrix (vector 1 0 0 1 0 0)])
    
    (define no-brush (new brush% [style 'transparent]))
    (define red-pen (new pen% [color "red"] [width 2]))
    (define normal-pen (new pen% [color "black"] [width 1]))
    (define highlight-pen (new pen% [color "RoyalBlue"] [width 2]))
    
    ;; DRAWING functions
    (define (draw-dot x y highlight?)
      (define drawer (get-dc))
      (if highlight?
          (send drawer set-pen red-pen)
          (send drawer set-pen normal-pen))
      (send drawer draw-point x y))
    
    (define (draw-transition-point x y)
      (define drawer (get-dc))
      (send drawer set-pen highlight-pen)
      (send drawer draw-point x y))
    
    (define (draw-line x1 y1 x2 y2 highlight?)
      (define drawer (get-dc))
      (if highlight?
          (send drawer set-pen red-pen)
          (send drawer set-pen normal-pen))
      (send drawer draw-line x1 y1 x2 y2))
    
    ;racket's draw-arc function's x,y starts at bottom left corner (docs say top left but inverted because of -ve y-scale)
    ;DXF provided arc x,y coordinates are at the center of the arc/circle
    (define (draw-arc x y radius start end highlight?)
      (define drawer (get-dc))
      (if highlight?
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
            [(line highlighted selected visible layer p1 p2)                           (draw-line (point-x p1) (point-y p1) (point-x p2) (point-y p2) (or selected highlighted))]
            [(arc highlighted selected visible layer center radius start end p1 p2 p3) (draw-arc (point-x center) (point-y center) radius start end (or selected highlighted))]
            [(dot highlighted selected visible layer p)                                (draw-dot (point-x p) (point-y p) (or selected highlighted))]
            [(path highlighted selected visible layer path-list)                       (draw-objects path-list)])))
      (map apply-procedure lst))
    
    (define (draw-select-box lst)
      (for/list ([i lst])
        (apply draw-line i)))
    
    (define (draw-start-end-points lst)
      (for/list ([i lst])
        (apply draw-transition-point i)))
    
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
    
    ;this only highlights the end and start of detected paths.
    ;lst should be (Listof (Listof Entities)) - a list of path-lists.
    ;(define (highlight-points lst)
    ;  )
    
    ;; POPUP MENU
    (define popup
      (new popup-menu%
           [title "ugh"]))
    
    ;; POPUP MENU items
    (define select-append-option
      (new menu-item%
           [label "Append selected items into a single path"]
           [parent popup]
           [callback (lambda (b e)
                       (define selected-entities (get-selected search-list))
                       (define paths (sort selected-entities))
                       (display paths))]))
    
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
      (define is-panning? (and (send event dragging?) (not (is-mouse-event? 'right-down))))
      (define end-panning? (is-mouse-event? 'left-up))
      (define start-selecting? (and (is-mouse-event? 'left-down) (is-key-event? get-control-down)))
      (define is-selecting? (and (send event dragging?) (is-key-event? get-control-down)))
      (define end-selecting? (and (is-mouse-event? 'left-up) (is-key-event? get-control-down)))
      (define set-park-position? (and set-park-position (is-mouse-event? 'left-down)))
      (define show-popup? (is-mouse-event? 'right-down))
      
      (cond
        (show-popup?
         (send this popup-menu popup x y))
        (set-park-position?
         (display (list (unscale-x scaled-x) (unscale-y scaled-y)))
         (send drawer draw-point scaled-x scaled-y)
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
         (highlight-lst search-list)
         (set! select-box (list (list init-x init-y scaled-x init-y #t)
                                (list scaled-x init-y scaled-x scaled-y #t)
                                (list scaled-x scaled-y init-x scaled-y #t) 
                                (list init-x scaled-y init-x init-y #t)))
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
      ;(when display-start-end-points (draw-start-end-points lst))
      (draw-objects search-list)
      (send drawer set-pen normal-pen))
    
    (super-instantiate ())))