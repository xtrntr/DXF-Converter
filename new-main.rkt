#lang racket

(require "structs.rkt"
         "read_dxf.rkt"
         "geometric_functions.rkt"
         "ils-pattern-generator.rkt"
         "ids-pattern-generator.rkt"
         "constants.rkt"
         "dxf-canvas.rkt"
         mrlib/path-dialog
         racket/gui/base
         framework)

(define top-frame (new frame%
                       [label "Main"]
                       [width 800]
                       [height 600]
                       [alignment (list 'left 'top)]))

(send top-frame show #t)

(define menu-bar (new menu-bar%
                      (parent top-frame)))

(define file (new menu%
                  (label "&File")
                  (parent menu-bar)))

(define (open-file input-port)
  
  (define editor-width  800)
  (define editor-height 600)
  
  (define a-frame 
    (new frame%
         [label (path->string (last (explode-path input-port)))]
         [width  editor-width]
         [height editor-height]
         [alignment (list 'left 'top)]))
  
  (define (scale-x coord)
    (* drawing-scale (- coord left)))
  
  (define (scale-y coord)
    (* drawing-scale (- coord bottom)))
  
  (define (unscale-x coord)
    (+ left (/ coord drawing-scale)))
  
  (define (unscale-y coord)
    (+ bottom (/ coord drawing-scale)))
  
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
  
  (define struct-list (file->struct-list input-port))
  (define-values (drawing-scale left bottom) (get-scales struct-list editor-width editor-height))
  (define search-list (rescale struct-list drawing-scale))
  (define layer-list (map (lambda (x) (if (string? x) x (number->string x)))
                            (remove-duplicates (map entity-layer struct-list))))
  
  (define a-canvas
    (new dxf-canvas%
       [parent a-frame]
       [search-list search-list]
       [x-offset 0]
       [y-offset (- editor-height 150)]
       [drawing-scale drawing-scale]
       [x-scale 1]
       [y-scale -1]
       
       ;scaling and unscaling methods. placed outside because they depend on left/bottom that are calculated when opening a struct-list for the first time.
       [scale-x scale-x]
       [unscale-x unscale-x]
       [scale-y scale-y]
       [unscale-y unscale-y]
       
       [display-select-box #f]
       [select-box '()]))
  
  (for/list ([i layer-list])
    (new check-box%
         (label i)
         (parent a-frame)
         (callback (lambda (checked e)
                     (define make-visible? (send checked get-value))
                     (define (entity-same-layer? struct) (equal? (entity-layer struct) i))
                     (define (toggle-visibility struct)
                       (if make-visible? 
                           (begin (set-entity-visible! struct #t) (set-entity-selected! struct #f))
                           (set-entity-visible! struct #f)))
                     (define (map-toggle-visibility lst)
                       (cond ((empty? lst) '())
                             ((path? (car lst)) (begin 
                                                  (display "1")
                                                  (toggle-visibility (car lst))
                                                  (map-toggle-visibility (path-entities (car lst)))
                                                  (map-toggle-visibility (cdr lst))))
                             (else (begin (toggle-visibility (car lst))
                                          (map-toggle-visibility (cdr lst))))))
                     
                     (display (map entity-same-layer? (filter-struct-list search-list path?)))
                     (display (entity-layer (car (filter-struct-list search-list path?))))
                     (display (entity-layer (car (filter-struct-list search-list arc?))))
                     (send a-canvas update-canvas)
                     (map-toggle-visibility (filter-struct-list search-list entity-same-layer?))
                     (send a-canvas draw-objects search-list)
                     (send a-canvas on-paint)
                     (send a-canvas refresh-now)))))
  
  (new button%
       [label "Refocus"]
       [parent a-frame]
       [callback (lambda (b e)
                   (set-field! x-offset a-canvas 0)
                   (set-field! y-offset a-canvas (- editor-height 150))
                   (set-field! x-scale  a-canvas 1)
                   (set-field! y-scale  a-canvas -1)
                   (send a-canvas update-canvas))])
  
  (send a-frame show #t))

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