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
    
    (init-field 
     ;variables
     search-list
     entity-ht
     x-offset
     y-offset
     drawing-scale
     
     ;methods
     unscale-x
     unscale-y
     scale-x
     scale-y
     update-spreadsheet)
    
    (field 
     ;canvas display variables
     [rotation 0]
     [transformation-matrix (vector 1 0 0 1 0 0)]
     [x-scale 1]
     [y-scale -1]
     
     ;interaction variables
     [init-cursor-x 0]
     [init-cursor-y 0]
     [cursor-x 0]
     [cursor-y 0]
     [scaled-cursor-x 0]
     [scaled-cursor-y 0]
     
     ;display selection box
     [display-select-box #f] 
     [select-box '()]
     
     [reorder? #f] ;display clickpoints of selected entities
     [highlighted-node '()] ;clickpoint near cursor
     [path-lst '()]  ;list of connections
     [node-lst '()]) ;list of clickable nodes
    
    ;; DRAWING COLORS
    (define no-brush (new brush% [style 'transparent]))
    (define red-pen (new pen% [color "RoyalBlue"] [width 1]))
    (define black-pen (new pen% [color "black"] [width 1]))
    (define big-blue-pen (new pen% [color "RoyalBlue"] [width 5]))
    (define big-orange-pen (new pen% [color "Orange"] [width 5]))
    
    ;; CURSOR TYPES
    (define normal (make-object cursor% 'arrow))
    (define panning (make-object cursor% 'hand))
    (define selecting (make-object cursor% 'cross))
    
    ;; MOUSE SCALING
    ;scale mouse coordinates to display coordinates
    (define (mouse2display-x x)
      (/ (- x x-offset) x-scale))
    (define (mouse2display-y y) 
      (/ (- y y-offset) y-scale))
    
    (define-syntax change-pen
      (lambda (stx)
        (syntax-case stx ()
          [(_ pen-type)
           #'(begin ;(define drawer (get-dc))
                    (send (get-dc) set-pen pen-type))])))
    
    (define-syntax change-cursor
      (lambda (stx)
        (syntax-case stx ()
          [(_ cursor-type)
           #'(send this set-cursor cursor-type)])))
    
    (define-syntax my-draw
      (lambda (stx)
        (syntax-case stx ()
          [(_ (entity) (args ...) highlight? (pen1 pen2))
           (let* ([method-name/str (symbol->string (syntax->datum #'entity))]
                  [method-name/syn (string->symbol (string-append "draw-" method-name/str))])
             (with-syntax ([name (datum->syntax stx method-name/syn)])
               #'(begin (define drawer (get-dc))
                        (if highlight?
                            (change-pen pen1)
                            (change-pen pen2))
                        (send drawer name args ...))))])))
    
    ;; DRAWING functions
    (define (draw-dot x y highlight?)
      (my-draw (point) (x y) highlight? (red-pen black-pen)))
    
    (define (draw-line x1 y1 x2 y2 highlight?)
      (my-draw (line) (x1 y1 x2 y2) highlight? (red-pen black-pen)))
    
    (define (draw-start/end-nodes x y highlight?)
      (my-draw (point) (x y) highlight? (big-orange-pen big-blue-pen)))
    
    (define (draw-arc x y radius start end highlight?)
      ;racket's draw-arc function's x,y starts at bottom left corner (docs say top left but inverted because of -ve y-scale)
      ;DXF provided arc x,y coordinates are at the center of the arc/circle
      (let ((start (degrees->radians (- 360 start))) ;; DXF angles are CW, Racket angles are CCW (because of inverting y scale)
            (end (degrees->radians (- 360 end)))
            (center-x (- x radius))
            (center-y (- y radius)))
        (my-draw (arc) (center-x center-y (* 2 radius) (* 2 radius) end start) highlight? (red-pen black-pen))))
    
    ;make public to allow checking/unchecking of layers outside of the canvas
    (define/public (draw-objects lst)
      (define (apply-procedure x)
        (when (entity-visible x)
          (match x
            [(line highlighted selected visible layer p1 p2)                           (draw-line (node-x p1) (node-y p1) (node-x p2) (node-y p2) (or selected highlighted))]
            [(arc highlighted selected visible layer center radius start end p1 p2 p3) (draw-arc (node-x center) (node-y center) radius start end (or selected highlighted))]
            [(dot highlighted selected visible layer p)                                (draw-dot (node-x p) (node-y p) (or selected highlighted))]
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
            (cond ((point-in-rect? (node-x i) (node-y i) small-x small-y big-x big-y) 
                   (draw-start/end-nodes (node-x i) (node-y i) #t)
                   (set! highlighted-node i))
                  (else (draw-start/end-nodes (node-x i) (node-y i) #f)))))))
    
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
                    ((dot? i)
                     (if (point-in-rect? (node-x i) (node-y i) small-x small-y big-x big-y) 
                         (set-entity-highlighted! i #t)
                         (set-entity-highlighted! i #f)))
                    ((path? i)
                     (intersect? x1 y1 x2 y2 (path-entities i))))))))
    
    (define/public (update-canvas)
      (define drawer (get-dc))
      (send drawer set-transformation (vector transformation-matrix x-offset y-offset x-scale y-scale rotation))
      (send this refresh-now))
    
    (define (update-node-lst)
      (set! path-lst (sort (get-connection-lst (get-selected search-list))))
      (set! node-lst (flatten (map get-start/end-nodes path-lst))))
    
    ;; POPUP MENU
    (define popup
      (new popup-menu%
           [title "ugh"]))
    
    ;; POPUP MENU items
    (define open-nodir
      (new menu-item%
           [label "Form an open path."]
           [parent popup]
           [callback (lambda (b e)
                       (define connection-lst (find-connection highlighted-node path-lst))
                       ;create a new path..
                       (define new-path (form-open-path highlighted-node connection-lst search-list))
                       ;remove the old entities that have been appended into a path
                       (set! search-list (remove* (path-entities new-path) search-list))
                       ;then add the new patch
                       (set! search-list (append (list new-path) search-list))
                       (update-spreadsheet search-list))]))
      
    (define closed-clockwise
      (new menu-item%
           [label "Form a path that moves clockwise from this point."]
           [parent popup]
           [callback (lambda (b e)
                       (define connection-lst (find-connection highlighted-node path-lst))
                       ;create a new path..
                       (define new-path (form-closed-path highlighted-node connection-lst search-list #t))
                       ;remove the old entities that have been appended into a path
                       (set! search-list (remove* (path-entities new-path) search-list))
                       ;then add the new patch
                       (set! search-list (append (list new-path) search-list))
                       (update-spreadsheet search-list))]))
    
    (define closed-anticlockwise
      (new menu-item%
           [label "Form a path that moves anti-clockwise from this point."]
           [parent popup]
           [callback (lambda (b e)
                       (define connection-lst (find-connection highlighted-node path-lst))
                       ;create a new path..
                       (define new-path (form-closed-path highlighted-node connection-lst search-list #f))
                       ;remove the old entities that have been appended into a path
                       (set! search-list (remove* (path-entities new-path) search-list))
                       ;then add the new patch
                       (set! search-list (append (list new-path) search-list))
                       (update-spreadsheet search-list))]))
    
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
      
      (set! cursor-x (send event get-x))
      (set! cursor-y (send event get-y))
      (set! scaled-cursor-x (mouse2display-x cursor-x))
      (set! scaled-cursor-y (mouse2display-y cursor-y))
      
      (define-syntax-rule (is-key-event? query)
        (send event query))
      (define (is-mouse-event? query)
        (equal? query (send event get-event-type)))
     
      
      ;key and mouse combinations
      (define click-right (is-mouse-event? 'right-down))
      (define click-left (is-mouse-event? 'left-down))
      (define release-left (is-mouse-event? 'left-up))
      (define hold-ctrl (is-key-event? get-control-down))
      (define caps-on (is-key-event? get-caps-down))
      (define dragging (send event dragging?)) ;click and hold
      
      (define start-panning? click-left)
      (define is-panning? (and dragging (number? init-cursor-x) (number? init-cursor-y)))
      (define end-panning? release-left)
      (define start-selecting? (and click-left hold-ctrl))
      (define is-selecting? (and dragging hold-ctrl))
      (define end-selecting? (and release-left hold-ctrl))
      (define show-popup? (and (node? highlighted-node) click-right))
      
      (send this refresh-now)
      
      (cond
        (end-selecting?
         (change-cursor normal)
         (set! display-select-box #f)
         (select-highlighted search-list)
         (update-spreadsheet search-list)
         (update-node-lst)
         (update-canvas))
        (end-panning?
         ;see conditions for is-panning? to understand setting init-cursor-x and init-cursor-y values
         (set! init-cursor-x #f)
         (set! init-cursor-y #f)
         ;fix the offset so that the screen stays where it is after panning is finished
         (set! x-offset (vector-ref (send drawer get-transformation) 1)) 
         (set! y-offset (vector-ref (send drawer get-transformation) 2))
         (change-cursor normal))
        (start-selecting?
         (set! init-cursor-x scaled-cursor-x)
         (set! init-cursor-y scaled-cursor-y)
         (set! display-select-box #t))
        (start-panning?
         (set! init-cursor-x cursor-x)
         (set! init-cursor-y cursor-y))
        (show-popup?
         (send this popup-menu popup cursor-x cursor-y))
        (is-selecting?
         (change-cursor selecting)
         (intersect? init-cursor-x init-cursor-y scaled-cursor-x scaled-cursor-y search-list)
         (update-node-lst)
         (highlight-lst search-list)
         (set! select-box (list (list init-cursor-x init-cursor-y scaled-cursor-x init-cursor-y #t)
                                (list scaled-cursor-x init-cursor-y scaled-cursor-x scaled-cursor-y #t)
                                (list scaled-cursor-x scaled-cursor-y init-cursor-x scaled-cursor-y #t) 
                                (list init-cursor-x scaled-cursor-y init-cursor-x init-cursor-y #t)))
         (update-canvas))
        (is-panning?
         (let* ((current-x (- cursor-x init-cursor-x))
                (current-y (- cursor-y init-cursor-y)))
           (change-cursor panning)
           (send drawer set-transformation (vector transformation-matrix (+ current-x x-offset) (+ current-y y-offset) x-scale y-scale rotation))
           (send this refresh)))))
    
    (define/override (on-paint)
      (define drawer (get-dc))
      (send drawer set-brush no-brush)
      (when display-select-box (draw-select-box select-box))
      
      (cursor-nearby? (- scaled-cursor-x 3) (- scaled-cursor-y 3) (+ scaled-cursor-x 3) (+ scaled-cursor-y 3) node-lst)
      
      (draw-objects search-list)
      (change-pen black-pen))
    
    (super-instantiate ())))