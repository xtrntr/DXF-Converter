#lang racket/gui

#|

This module is where i store the custom canvas class for displaying DXF elements : ARC, LINE, DOT, CIRCLE, LWPOLYLINE.
Functions here include:
custom popup menus
custom event handling behavior with respect to mouse and key events (see on-event and on-char respectively
helper functions (scaling, sorting connected entities into a single path, intersection query functions, updating entity list that is displayed from an outside spreadsheet)
drawing functions

TODO:
add ctrl-a for select all function
add undo function
limit panning and zooming with respect to a specified workspace limit

|#

(require "structs.rkt"
         "canvas-utils.rkt"
         "lst-utils.rkt"
         "utils.rkt"
         "graph.rkt"
         racket/draw)

(provide dxf-canvas%)

(define dxf-canvas%
  (class canvas%
    
    ;shorten
    (inherit get-dc refresh refresh-now get-width get-height suspend-flush resume-flush)
    
    (init-field
     ;;vars
     search-list
     original-list
     x-offset
     y-offset
     
     ;;fns
     ;coordinate scaling
     scale-x
     unscale-x
     scale-y
     unscale-y
     
     update-spreadsheet)
    
    (field 
     ;canvas display variables
     [rotation 0]
     [transformation-matrix (vector 1 0 0 1 0 0)]
     [x-scale 1]
     [y-scale 1]
     
     ;interaction variables
     [init-cursor-x 0]
     [init-cursor-y 0]
     [cursor-x 0]
     [cursor-y 0]
     [scaled-cursor-x 0]
     [scaled-cursor-y 0]
     
     ;display selection box
     [select-box '()]
     
     [reorder? #f] ;display clickpoints of selected entities
     [highlighted-node #f] ;clickpoint near cursor
     [path-lst '()]  ;list of connections
     [node-lst '()]) ;list of clickable nodes
    
    ;; DRAWING COLORS
    (define no-brush (new brush% [style 'transparent]))
    (define blue-pen (new pen% [color "RoyalBlue"] [width 1]))
    (define black-pen (new pen% [color "black"] [width 1]))
    (define big-blue-pen (new pen% [color "RoyalBlue"] [width 5]))
    (define big-orange-pen (new pen% [color "Orange"] [width 5]))
    ;debugging pen
    (define orange-pen (new pen% [color "Orange"] [width 1]))
    
    ;; CURSOR TYPES
    (define normal (make-object cursor% 'arrow))
    (define panning (make-object cursor% 'hand))
    (define selecting (make-object cursor% 'cross))
    
    ;; MOUSE SCALING
    ;scale mouse coordinates to pixel coordinates
    (define (mouse2display-x x)
      (/ (- x x-offset) x-scale))
    (define (mouse2display-y y)
      (/ (- y y-offset) y-scale))
    
    (define (change-pen pen-type)
      (send (get-dc) set-pen pen-type))
    
    (define (change-cursor cursor-type)
      (send this set-cursor cursor-type))
    
    ;; DRAWING FUNCTIONS
    (define (draw-dot x y)
      (send (get-dc) draw-point x y))

    (define (draw-line x1 y1 x2 y2 dc-path)
      (send dc-path move-to x1 y1)
      (send dc-path line-to x2 y2)
      (send dc-path close))
    
    (define (draw-start/end-nodes x y highlight?)
      (if highlight?
          (change-pen big-orange-pen)
          (change-pen big-blue-pen))
      (send (get-dc) draw-point x y))

    ;p1 p2 p3 and ccw are for debugging. they're so useful so leave them in for now.
    (define (draw-arc x y radius start end p1 p2 p3 ccw dc)
      ;racket's draw-arc function's x,y starts at bottom left corner (docs say top left but inverted because of -ve y-scale)
      ;DXF provided arc x,y coordinates are at the center of the arc/circle
      (let ((start (degrees->radians (- 360 start))) ;; DXF angles are CW, Racket angles are CCW (because of inverting y scale)
            (end (degrees->radians (- 360 end)))
            (center-x (- x radius))
            (center-y (- y radius))
            [x1 (node-x p1)]
            [y1 (node-y p1)]
            [x2 (node-x p2)]
            [y2 (node-y p2)]
            [x3 (node-x p3)]
            [y3 (node-y p3)])
        ;(draw-line x1 y1 x2 y2 highlight?)
        ;(draw-line x2 y2 x3 y3 highlight?)
        (if ccw
            (send (get-dc) draw-arc center-x center-y (* 2 radius) (* 2 radius) start end)
            (send (get-dc) draw-arc center-x center-y (* 2 radius) (* 2 radius) end start))))
    
    ;make public to allow checking/unchecking of layers outside of the canvas
    (define/public (draw-objects lst)
      (let ([dc (get-dc)])
        (define (apply-procedure x dc-path)
          (when (entity-visible x)
            (match x
              [(line _ _ _ _ p1 p2 _)                                 (draw-line (node-x p1) (node-y p1) (node-x p2) (node-y p2) dc-path)]
              [(arc _ _ _ _ center radius start end p1 p2 p3 ccw _)   (draw-arc (node-x center) (node-y center) radius start end p1 p2 p3 ccw dc)]
              [(dot _ _ _ _ p)                                        (draw-dot (node-x p) (node-y p))]
              [(path _ _ _ _ path-list)                               (draw-objects path-list)])))
        (let ([blue-path (new dc-path%)]
              [orange-path (new dc-path%)]
              [black-path (new dc-path%)])
          (for/list ([x (filter (lambda (x) (and (not (entity-highlighted x)) (not (entity-selected x)))) lst)])
                    (change-pen black-pen)
                    (apply-procedure x black-path))
          (send (get-dc) draw-path black-path)
          (send black-path reset)
          
          (for/list ([x (filter (lambda (x) (or (entity-highlighted x) (entity-selected x))) lst)])
          (change-pen blue-pen)
                    (apply-procedure x blue-path))
          (send (get-dc) draw-path blue-path)
          (draw-select-box select-box blue-path)
          (send blue-path reset)
          
          (for/list ([x (filter (lambda (x) (and (entity-highlighted x) (entity-selected x))) lst)])
                    (change-pen orange-pen)
                    (apply-procedure x orange-path))
          (send (get-dc) draw-path orange-path)
          (send orange-path reset))))
    
    (define (draw-select-box lst dc-path)
        (unless (empty? select-box)
            (let* ([big-x (biggest (take lst 2))]
                   [big-y (biggest (drop lst 2))]
                   [small-x (smallest (take lst 2))]
                   [small-y (smallest (drop lst 2))]
                   [width (abs (- big-x small-x))]
                   [height (abs (- big-y small-y))])
              (begin (send dc-path rectangle small-x small-y width height)
                     (send (get-dc) draw-path dc-path)))))
            
    
    (define (cursor-nearby? x1 y1 x2 y2)
      (let ([big-x (biggest (list x1 x2))]
            [big-y (biggest (list y1 y2))]
            [small-x (smallest (list x1 x2))]
            [small-y (smallest (list y1 y2))]
            [selected-entities (get-selected-entities search-list)])
        (when reorder? 
          (for/list ([i node-lst])
                    (draw-start/end-nodes (node-x i) (node-y i) #f))
          (for/list ([j selected-entities])
                    (when (path? j) (map (lambda (x) (set-entity-highlighted! x #f)) (path-entities j)))
                    (set-entity-highlighted! j #f))
          (for/list ([i node-lst])
                    (when (point-in-rect? (node-x i) (node-y i) small-x small-y big-x big-y) 
                      (let* ([groups (group-entities selected-entities)]
                             [selected-list (get-belonging-list i groups)]
                             [the-rest (flatten (remove selected-list groups))]
                             [highlighted-node-lst (get-start/end-nodes selected-list)])
                        (set! highlighted-node i)
                        (for/list ([i selected-list])
                                  (when (path? i) (map (lambda (x) (set-entity-highlighted! x #t)) (path-entities i)))
                                  (set-entity-highlighted! i #t))
                        (for/list ([i highlighted-node-lst])
                                  (draw-start/end-nodes (node-x i) (node-y i) #t))))))))

    
    ;pass intersect? the start and end point of select box and the struct-list
    ;it will traverse the struct-list to see if any elements
    (define (intersect? x1 y1 x2 y2 lst)
      (let* ([big-x (biggest (list x1 x2))]
             [big-y (biggest (list y1 y2))]
             [small-x (smallest (list x1 x2))]
             [small-y (smallest (list y1 y2))]
             [query-mbr (rect small-x small-y big-x big-y)]
             )
        (for/list ([i (filter (lambda (x) (and (entity-visible x) (not (entity-selected x)))) lst)])
                  ;only calculate intersections for visible and not yet selected items
                  (cond [(line? i)
                         (if (and (line-intersect? i small-x small-y big-x big-y)
                                  (rect-intersect? (line-mbr i) query-mbr)
                                  )
                             (set-entity-highlighted! i #t)
                             (set-entity-highlighted! i #f))]
                        [(arc? i)
                         (if (and (arc-intersect? i small-x small-y big-x big-y)
                                  (rect-intersect? (arc-mbr i) query-mbr)
                                  )
                             (set-entity-highlighted! i #t)
                             (set-entity-highlighted! i #f))]
                        [(dot? i)
                         (if (point-in-rect? (node-x (dot-p i)) (node-y (dot-p i)) small-x small-y big-x big-y)
                             (set-entity-highlighted! i #t)
                             (set-entity-highlighted! i #f))]
                        [(path? i)
                         (intersect? x1 y1 x2 y2 (path-entities i))]))))
    
    ;don't include paths
    (define/public (update-node-lst)
      (if (empty? (get-selected-entities search-list))
          (set! node-lst '())
          (let* ([to-display-for (get-selected-entities (filter-not dot? search-list))]
                 [groups (group-entities to-display-for)] 
                 [islands-removed (filter-not (lambda (group) (= 1 (length group))) groups)]) ;i.e. a path, dont need to show nodes for it
            (set! node-lst (flatten (map get-start/end-nodes islands-removed))))))
    
    (define/public (update-canvas)
      (send (get-dc) set-transformation (vector transformation-matrix x-offset y-offset x-scale y-scale rotation))
      (refresh))
    
    (define/public (refresh-spreadsheet)
      (update-spreadsheet search-list))
    
    (define/public (refocus)
      (define left (+ 0 (smallest (get-x-vals (get-visible-entities search-list)))))
      (define bottom (+ 0 (* -1 (smallest (get-y-vals (get-visible-entities search-list))))))
      (define drawing-scale (get-display-scale search-list (get-width) (get-height)))
      (set! x-offset left)
      (set! y-offset bottom)
      (set! x-scale  drawing-scale)
      (set! y-scale  drawing-scale)
      (update-node-lst)
      (update-canvas))
    
    ;; POPUP MENU
    (define popup-opened
      (new popup-menu%
           [popdown-callback (lambda (p e)
                               (when (equal? (send e get-event-type) 'menu-popdown-none) (set! highlighted-node #f)))]))
    
    (define popup-error
      (new popup-menu%
           [popdown-callback (lambda (p e)
                               (when (equal? (send e get-event-type) 'menu-popdown-none) (set! highlighted-node #f)))]))
    
    (define popup-closed
      (new popup-menu%
           [popdown-callback (lambda (p e)
                               (when (equal? (send e get-event-type) 'menu-popdown-none) (set! highlighted-node #f)))]))
    
    ;; POPUP MENU items
    (define display-err
      (new menu-item%
           [label "This node is connected to a tree pattern."]
           [parent popup-error]
           [callback (lambda (b e)
                       (let* ([groups (group-entities (get-selected-entities search-list))]
                              [selected-list (get-belonging-list highlighted-node groups)])
                         (+ 1 1)))]))
    
    (define open-nodir
      (new menu-item%
           [label "Form an open path."]
           [parent popup-opened]
           [callback (lambda (b e)
                       (let* ([groups-of-connected-entities (group-entities (get-selected-entities search-list))]
                              [list-of-entities-to-reorder (get-belonging-list highlighted-node groups-of-connected-entities)]
                              [base-elements (get-base-elements list-of-entities-to-reorder)]
                              [new-path (make-selected (make-path (reorder-open-path highlighted-node base-elements)))])
                         (set! search-list (append (list new-path) (remove* list-of-entities-to-reorder search-list)))
                         (update-node-lst)
                         (update-canvas)
                         (update-spreadsheet search-list)))]))
    
    (define closed-clockwise
      (new menu-item%
           [label "Form a path that moves clockwise from this point."]
           [parent popup-closed]
           [callback (lambda (b e)
                       (let* ([groups-of-connected-entities (group-entities (get-selected-entities search-list))]
                              [list-of-entities-to-reorder (get-belonging-list highlighted-node groups-of-connected-entities)]
                              [base-elements (get-base-elements list-of-entities-to-reorder)]
                              [new-path (make-selected (make-path (reorder-closed-path highlighted-node base-elements #f)))])
                         (set! search-list (append (list new-path) (remove* list-of-entities-to-reorder search-list)))
                         (update-node-lst)
                         (update-canvas)
                         (update-spreadsheet search-list)))]))
    
    (define closed-anticlockwise
      (new menu-item%
           [label "Form a path that moves anti-clockwise from this point."]
           [parent popup-closed]
           [callback (lambda (b e)
                       (let* ([groups-of-connected-entities (group-entities (get-selected-entities search-list))]
                              [list-of-entities-to-reorder (get-belonging-list highlighted-node groups-of-connected-entities)]
                              [base-elements (get-base-elements list-of-entities-to-reorder)]
                              [new-path (make-selected (make-path (reorder-closed-path highlighted-node base-elements #t)))])
                         (set! search-list (append (list new-path) (remove* list-of-entities-to-reorder search-list)))
                         (update-node-lst)
                         (update-canvas)
                         (update-spreadsheet search-list)))]))
    
    ;; KEYBOARD events
    (define/override (on-char event)
      (let ((key (send event get-key-code)))
        (special-control-key #t)
        (cond [(equal? key 'wheel-up)
               (set! x-scale (+ x-scale 0.1)) 
               (set! y-scale (+ y-scale 0.1))]
              [(equal? key 'escape) 
               (unselect-all search-list)
               (update-node-lst)
               (update-spreadsheet search-list)]
              [(equal? key 'wheel-down) 
               (when (> (- x-scale 0.1) 0) 
                 (set! x-scale (- x-scale 0.1))
                 (set! y-scale (- y-scale 0.1)))]
              [(equal? key '#\backspace)
               (delete-selected search-list)
               (update-node-lst)
               (update-spreadsheet search-list)])
        (update-canvas)))
    
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
      (define start-selecting? (and click-left caps-on))
      (define is-selecting? (and dragging caps-on))
      ;use select-box as a flag to check whether we were selecting previously.
      (define end-selecting? (and release-left (not (empty? select-box))))
      (define show-popup? (and (node? highlighted-node) click-right))
      (define click-detected? (and release-left
                                   (> 0.5 (abs (- scaled-cursor-x init-cursor-x)))
                                   (> 0.5 (abs (- scaled-cursor-y init-cursor-y)))))

      (refresh)

      (cond
        (end-selecting?
         (change-cursor normal)
         (select-highlighted search-list)
         (update-spreadsheet search-list)
         (set! select-box '())
         (time (update-node-lst))
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
         (change-cursor selecting)
         (set! init-cursor-x scaled-cursor-x)
         (set! init-cursor-y scaled-cursor-y)
         (set! select-box (list init-cursor-x scaled-cursor-x init-cursor-y scaled-cursor-y)))
        (start-panning?
         (set! init-cursor-x cursor-x)
         (set! init-cursor-y cursor-y)
         (set! highlighted-node #f))
        (show-popup?
         (let* ([groups (group-entities (get-selected-entities search-list))]
                [selected-list (get-belonging-list highlighted-node groups)]
                [node-lst (entities->nodes selected-list)]
                [closed-pattern? (closed-path? node-lst)]
                [open-pattern? (open-path? node-lst)]
                [tree-pattern? (tree-path? node-lst)])
           (cond [open-pattern? (send this popup-menu popup-opened cursor-x cursor-y)]
                 [closed-pattern? (send this popup-menu popup-closed cursor-x cursor-y)]
                 [tree-pattern? (send this popup-menu popup-error cursor-x cursor-y)])))
        (is-selecting?
         ;(time
         (intersect? init-cursor-x init-cursor-y scaled-cursor-x scaled-cursor-y search-list)
         (highlight-paths search-list)
         (set! select-box (list init-cursor-x scaled-cursor-x init-cursor-y scaled-cursor-y)))
        (is-panning?
         (let* ((current-x (- cursor-x init-cursor-x))
                (current-y (- cursor-y init-cursor-y)))
           (change-cursor panning)
           (send drawer set-transformation (vector transformation-matrix (+ current-x x-offset) (+ current-y y-offset) x-scale y-scale rotation))
           (refresh)))))
    
    (define/override (on-paint)
      (define drawer (get-dc))
      (send this suspend-flush)
      (send drawer set-brush no-brush)
      (cursor-nearby? (- scaled-cursor-x 3) (- scaled-cursor-y 3) (+ scaled-cursor-x 3) (+ scaled-cursor-y 3))
      (draw-objects search-list)
      (send this resume-flush))
      
    
    (super-instantiate ())))