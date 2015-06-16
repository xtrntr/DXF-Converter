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
                
                ;methods
                unscale-x
                unscale-y
                scale-x
                scale-y
                update-spreadsheet)
    
    (field [rotation 0]
           [init-x 0]
           [init-y 0]
           [transformation-matrix (vector 1 0 0 1 0 0)]
           [x-scale 1]
           [y-scale -1]
           [display-select-box #f]
           [reorder? #f]
           [cursor-x 0]
           [cursor-y 0]
           [highlighted-point '()]
           [select-box '()]
           [node-lst '()])
    
    (define no-brush (new brush% [style 'transparent]))
    (define red-pen (new pen% [color "RoyalBlue"] [width 1]))
    (define normal-pen (new pen% [color "black"] [width 1]))
    (define big-red-pen (new pen% [color "RoyalBlue"] [width 5]))
    (define orange-pen (new pen% [color "Orange"] [width 5]))
    
    ;; MOUSE SCALING
    ;scale mouse coordinates to display coordinates
    (define (mouse2display-x x)
      (/ (- x x-offset) x-scale))
    (define (mouse2display-y y) 
      (/ (- y y-offset) y-scale))
    ;scale display coordinates to mouse
    (define (display2mouse-x x)
      (+ (* x x-scale) x-offset))
    (define (display2mouse-y y)
      (+ (* y y-scale) y-offset))
    
    ;; DRAWING functions
    (define (draw-dot x y highlight?)
      (define drawer (get-dc))
      (if highlight?
          (send drawer set-pen red-pen)
          (send drawer set-pen normal-pen))
      (send drawer draw-point x y))
    
    (define (draw-start/end-nodes a-point highlight?)
      (define drawer (get-dc))
      (if highlight?
          (send drawer set-pen orange-pen)
          (send drawer set-pen big-red-pen))
      (send drawer draw-point (point-x a-point) (point-y a-point)))
    
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
      
    (define (cursor-nearby? x1 y1 x2 y2 lst)
      (let ((big-x (biggest (list x1 x2)))
            (big-y (biggest (list y1 y2)))
            (small-x (smallest (list x1 x2)))
            (small-y (smallest (list y1 y2))))
        (when reorder? 
          (for/list ([i lst])
            (cond ((point-in-rect? (point-x i) (point-y i) small-x small-y big-x big-y) 
                   (draw-start/end-nodes i #t)
                   (set! highlighted-point i))
                  (else (draw-start/end-nodes i #f)))))))
    
    ;pass intersect? the start and end point of select box and the struct-list
    ;it will traverse the struct-list to see if any elements
      (define (intersect? x1 y1 x2 y2 lst)
        (let ((big-x (biggest (list x1 x2)))
              (big-y (biggest (list y1 y2)))
              (small-x (smallest (list x1 x2)))
              (small-y (smallest (list y1 y2))))
          (for/list ([i lst])
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
    
    (define (update-node-lst)
      (define path-lst (sort (get-nodes (get-selected search-list))))
      (set! node-lst (flatten (map get-start/end-nodes path-lst))))
    
    ;; POPUP MENU
    (define popup
      (new popup-menu%
           [title "ugh"]))
    
    ;; POPUP MENU items
    (define clockwise-item
      (new menu-item%
           [label "Form a path that moves clockwise from this point."]
           [parent popup]
           [callback (lambda (b e)
                       (set highlighted-point (void)))]))
    
    (define anticlockwise-item
      (new menu-item%
           [label "Form a path that moves anti-clockwise from this point."]
           [parent popup]
           [callback (lambda (b e)
                       (set highlighted-point (void)))]))
    
    
    ;; KEYBOARD events
    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (special-control-key #t)
        (case key
          ['wheel-up    (set! x-scale (+ x-scale 0.1)) 
                        (set! y-scale (- y-scale 0.1))]
          ['escape      (unselect-all search-list)
                        (update-node-lst)
                        (update-spreadsheet search-list)]
          ['wheel-down  (when (> (- x-scale 0.1) 0) 
                          (set! x-scale (- x-scale 0.1))
                          (set! y-scale (+ y-scale 0.1)))]
          ['#\backspace (delete-selected search-list)
                        (update-node-lst)
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
     
      (define scaled-x (mouse2display-x x))
      (define scaled-y (mouse2display-y y))
      
      ;key and mouse combinations
      (define click-right (is-mouse-event? 'right-down))
      (define click-left (is-mouse-event? 'left-down))
      (define release-left (is-mouse-event? 'left-up))
      (define hold-ctrl (is-key-event? get-control-down))
      (define caps-on (is-key-event? get-caps-down))
      (define dragging (send event dragging?)) ;click and hold
      
      (define start-panning? click-left)
      (define is-panning? (and dragging (number? init-x) (number? init-y)))
      (define end-panning? release-left)
      (define start-selecting? (and click-left hold-ctrl)) 
      (define is-selecting? (and dragging hold-ctrl))
      (define end-selecting? (and release-left hold-ctrl))
      
      (set! cursor-x scaled-x)
      (set! cursor-y scaled-y)
      (send this refresh-now)
      
      (cond
        ((and (point? highlighted-point) click-right)
         (set highlighted-point (void))
         (send this popup-menu popup x y))
        (start-selecting?
         (set! init-x scaled-x)
         (set! init-y scaled-y)
         (set! display-select-box #t))
        (end-selecting?
         (send this set-cursor (make-object cursor% 'arrow))
         (set! display-select-box #f)
         (select-highlighted search-list)
         (update-spreadsheet search-list)
         (update-node-lst)
         (update-canvas))
        (is-selecting?
         (send this set-cursor (make-object cursor% 'cross))
         (intersect? init-x init-y scaled-x scaled-y search-list)
         (update-node-lst)
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
         ;see conditions for is-panning? to understand setting init-x and init-y values
         (set! init-x #f)
         (set! init-y #f)
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
      (cursor-nearby? (- cursor-x 3) (- cursor-y 3) (+ cursor-x 3) (+ cursor-y 3) node-lst)
      (draw-objects search-list)
      (send drawer set-pen normal-pen))
    
    (super-instantiate ())))