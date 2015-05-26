#lang racket

(require "structs.rkt"
         "read-dxf.rkt"
         "geometric-functions.rkt"
         "ils-pattern-generator.rkt"
         "ids-pattern-generator.rkt"
         "constants.rkt"
         "dxf-canvas.rkt"
         mrlib/path-dialog
         racket/gui/base
         framework)

(application:current-app-name "DXF converter") 

(define menu-super-frame%
  (frame:standard-menus-mixin
   frame:basic%))
      
(define menu-frame%
  (class menu-super-frame%
    (inherit get-menu-bar set-icon)

    ;file menu
    (define/override (file-menu:create-new?) #f)
    (define/override (file-menu:create-open?) #f)
    (define/override (file-menu:create-open-recent?) #f)
    (define/override (file-menu:create-close?) #f)
    
    ;edit menu
    (define/override (edit-menu:create-undo?) #f)
    (define/override (edit-menu:create-redo?) #f)
    (define/override (edit-menu:create-cut?) #f)
    (define/override (edit-menu:create-copy?) #f)
    (define/override (edit-menu:create-paste?) #f)
    (define/override (edit-menu:create-clear?) #f)
    (define/override (edit-menu:create-select-all?) #f)
    
    ;help menu
    (define/override (help-menu:create-about?) #t)
    
    ;drag n drop
    (define/override (on-drop-file pathname)
      (open-file pathname))
    
    (define/override (file-menu:between-save-as-and-print file-menu)
      (new menu-item%
           [label "&Open DXF File "]
           [parent file-menu]
           [callback (lambda (b e)
                       (define input-port-or-not (send open run))
                       (when input-port-or-not
                         (open-file input-port-or-not)))]))
    
    (super-new)))

(define top-frame 
  (new menu-frame%
       [label "Main"]
       [width 800]
       [height 600]
       [alignment (list 'left 'top)]))

(send top-frame show #t)

(define open (new path-dialog%
                  [existing? #t]
                  [filters (list (list "DXF Files" "*.dxf") (list "Text Files" "*.txt"))]))

(define (open-file input-port)
  
  (define editor-width  1000)
  (define editor-height 800)
  (define canvas-height 600)
  (define element-display-height 100)
  
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
  
  (define a-list-box
    (new list-box%
         [label "Element Display"]
         [parent a-frame]
         [min-height element-display-height]
         [choices '("1" "2")]))
  
  (define a-canvas
    (new dxf-canvas%
       [parent a-frame]
       [min-height 600]
       
       [search-list search-list]
       [x-offset 0]
       [y-offset (- canvas-height 150)]
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
  
  (define layer-panel
    (new horizontal-panel%
         [parent a-frame]
         [style '(border)]
         [min-height 30]
         [alignment '(center top)]
         [horiz-margin 0]
         [vert-margin 0]))
  
  (for/list ([i layer-list])
    (new check-box%
         (label i)
         (parent layer-panel)
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
                                                  (toggle-visibility (car lst))
                                                  (map-toggle-visibility (path-entities (car lst)))
                                                  (map-toggle-visibility (cdr lst))))
                             (else (begin (toggle-visibility (car lst))
                                          (map-toggle-visibility (cdr lst))))))
                     
                     (send a-canvas update-canvas)
                     (map-toggle-visibility (filter-struct-list search-list entity-same-layer?))
                     (send a-canvas draw-objects search-list)
                     (send a-canvas on-paint)
                     (send a-canvas refresh-now)))))
  
  (new button%
       [label "Refocus"]
       [parent a-frame]
       [callback (lambda (b e) 
                   (display (format "For the canvas, actual size is ~a, versus ~a" (call-with-values (thunk (send a-canvas get-client-size)) list)
                                    (list canvas-height)))
                   (set-field! x-offset a-canvas 0)
                   (set-field! y-offset a-canvas (- editor-height 150))
                   (set-field! x-scale  a-canvas 1)
                   (set-field! y-scale  a-canvas -1)
                   (send a-canvas update-canvas))])
  
  (send a-frame show #t))