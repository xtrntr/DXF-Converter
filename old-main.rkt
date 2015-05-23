#lang racket

(require "structs.rkt"
         "read-dxf.rkt"
         "geometric-functions.rkt"
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
  (set!-values (drawing-scale left bottom) (get-scales struct-list editor-frame-width editor-frame-height))
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