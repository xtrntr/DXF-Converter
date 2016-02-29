#lang racket

#|

TODO:
more sophisticated way of refocusing canvas.
when you select/unselect item(s) in the spreadsheet display, highlight/unhighlight relevant entities
in canvas but it should not work the other way round.
be able to "drag" 

|#

(require "structs.rkt"
         "read-dxf.rkt"
         "canvas-utils.rkt"
         "constants.rkt"
         "dxf-canvas.rkt"
         "utils.rkt"
         "gr-pattern-generator.rkt"
         mrlib/path-dialog
         racket/gui/base

         ;test
         "graph.rkt")

(provide (all-defined-out))

(define canvas-height 600)
(define canvas-width 700)
(define spreadsheet-height 600)
(define spreadsheet-width 300)
(define button-height 30)
(define tolerance 1) ;in mm

(define (open-file input-port a-frame)
  
  (define area-container (send a-frame get-area-container))
  (for/list ([i (send area-container get-children)])
    (send area-container delete-child i))
  (define-values (editor-width editor-height) (send a-frame get-size))
  
  ;;drawing scaling/unscaling
  (define (upscale-x coord)
    (* drawing-scale (- coord left)))
  (define (upscale-y coord)
    (* drawing-scale (- coord bottom)))
  (define (downscale-x coord)
    (+ left (/ coord drawing-scale)))
  (define (downscale-y coord)
    (+ bottom (/ coord drawing-scale)))
  ;; upscale really means scale for drawing.
  (define (upscale entity scale)
    (match entity
      [(line highlighted selected visible layer p1 p2 mbr)
       (make-line layer (upscale-x (node-x p1)) (upscale-y (node-y p1)) (upscale-x (node-x p2)) (upscale-y (node-y p2)))]
      [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
       (make-arc layer (upscale-x (node-x center)) (upscale-y (node-y center)) (* scale radius) start end ccw)]
      [(dot highlighted selected visible layer p)
       (make-dot layer (upscale-x (node-x p)) (upscale-y (node-y p)))]
      [(path highlighted selected visible layer path-list)
       (path #f #f #f layer (for/list ([entity path-list]) (upscale entity scale)))]))
  ;; downscale 
  (define (downscale entity scale)
    (match entity
      [(line highlighted selected visible layer p1 p2 mbr)
       (make-line layer (downscale-x (node-x p1)) (downscale-y (node-y p1)) (downscale-x (node-x p2)) (downscale-y (node-y p2)))]
      [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
       (make-arc layer (downscale-x (node-x center)) (downscale-y (node-y center)) (/ radius scale) start end ccw)]
      [(dot highlighted selected visible layer p)
       (make-dot layer (downscale-x (node-x p)) (downscale-y (node-y p)))]
      [(path highlighted selected visible layer path-list)
       (path #f #f #f layer (for/list ([entity path-list]) (downscale entity scale)))]))

  ;dxf-scale is 25.4(inches) or 0.0394(mm)
  (define original-list (file->struct-list input-port))
  (define left (smallest (get-x-vals original-list)))
  (define bottom (smallest (get-y-vals original-list)))
  (define drawing-scale (get-display-scale original-list editor-width editor-height))
  (define search-list (for/list ([entity original-list]) (upscale entity drawing-scale)))
  (define layer-list (map (lambda (x) (if (string? x) x (number->string x)))
                          (remove-duplicates (map entity-layer original-list))))
  
  (define main-panel
    (new horizontal-panel%
         [parent area-container]))
  
  (define drawing-panel
    (new vertical-panel%
         [parent main-panel]
         [style '(border)]))
  
  (define spreadsheet-panel
    (new vertical-panel%
         [parent main-panel]
         [style '(border)]))
  
  (define a-list-box
    (new list-box%
         [label #f]
         [parent spreadsheet-panel]
         ;[min-height spreadsheet-height]
         ;[min-width spreadsheet-width]
         [choices '()]
         [style '(extended column-headers)]
         [columns spreadsheet-headers]))
  
  (define (update-spreadsheet a-lst)
    (define displayed-list (filter entity-selected a-lst))
    (if (empty? displayed-list)
        (send a-list-box clear)
        (send a-list-box set 
              ;map unscale-x/unscale-y after node-x/node-y after debugging finished to display real DXF values
              (entities-to-strings displayed-list)
              (map (lambda (x) (to-display (downscale-x (node-x (get-entity-start x))))) displayed-list)
              (map (lambda (x) (to-display (downscale-y (node-y (get-entity-start x))))) displayed-list)
              (map (lambda (x) (to-display (downscale-x (node-x (get-entity-end x))))) displayed-list)
              (map (lambda (x) (to-display (downscale-y (node-y (get-entity-end x))))) displayed-list))))
              ;debugging mode
              ;(map number->string (range (length (filter (lambda (x) (arc? x)) displayed-list))))
              ;(map (lambda (x) (to-display (*dxf-scale (/ (arc-radius x) drawing-scale)))) (filter (lambda (x) (arc? x)) displayed-list))
              ;(map (lambda (x) (to-display (*dxf-scale (/ (arc-radius x) drawing-scale)))) (filter (lambda (x) (arc? x)) displayed-list))
              ;(map (lambda (x) (to-display (*dxf-scale (/ (arc-radius x) drawing-scale)))) (filter (lambda (x) (arc? x)) displayed-list))
              ;(map (lambda (x) (to-display (*dxf-scale (/ (arc-radius x) drawing-scale)))) (filter (lambda (x) (arc? x)) displayed-list)))))
              ;(map to-display (map node-x (map get-entity-start displayed-list)))
              ;(map to-display (map node-y (map get-entity-start displayed-list)))
              ;(map to-display (map node-x (map get-entity-end displayed-list)))
              ;(map to-display (map node-y (map get-entity-end displayed-list))))))
  
  (define a-canvas
    (new dxf-canvas%
         [parent drawing-panel]
         [min-height canvas-height]
         [min-width canvas-width]
         [tolerance tolerance]
         
         [search-list search-list]
         [original-list original-list]
         
         [x-offset 0]
         [y-offset canvas-height]
         
         ;scaling and unscaling methods. placed outside because they depend on left/bottom that are calculated when opening a struct-list for the first time.
         [upscale-x upscale-x]
         [upscale-y upscale-y]
         [downscale-x downscale-x]
         [downscale-y downscale-y]
         
         [update-spreadsheet update-spreadsheet]))
  
  (define layer-panel
    (new horizontal-panel%
         [parent drawing-panel]
         [style '(border)]	 
         [stretchable-height #f]
         ;[min-height button-height]
         [alignment '(center top)]))
  
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
                       (cond [(empty? lst) '()]
                             [(path? (car lst)) (begin 
                                                  (toggle-visibility (car lst))
                                                  (map-toggle-visibility (path-entities (car lst)))
                                                  (map-toggle-visibility (cdr lst)))]
                             [else (begin (toggle-visibility (car lst))
                                          (map-toggle-visibility (cdr lst)))]))
                     (send a-canvas update-canvas)
                     (map-toggle-visibility (filter entity-same-layer? (get-field search-list a-canvas)))
                     (send a-canvas draw-objects search-list)
                     (send a-canvas update-node-lst)
                     (send a-canvas on-paint)
                     (send a-canvas refresh-now)
                     (when make-visible? (send a-canvas refocus))
                     ))))
  
  (define button-panel-1
    (new horizontal-panel%
         [parent spreadsheet-panel]
         [style '(border)]	 
         [stretchable-height #f]
         [alignment '(center top)]))
  
  (define button-panel-2
    (new horizontal-panel%
         [parent spreadsheet-panel]
         [style '(border)]	 	 
         [stretchable-height #f]
         [alignment '(center top)]))
  
  (new button%
       [label "Reorder paths"]
       [parent button-panel-1]
       [callback (lambda (b e)
                   (set-field! reorder? a-canvas #t)
                   (send a-canvas update-canvas)
                   )])
  
  (new button%
       [label "Make mirror image"]
       [parent button-panel-1]
       [callback (lambda (b e)
                   (set-field! search-list a-canvas (make-mirror (get-field search-list a-canvas)))
                   (send a-canvas update-node-lst)
                   (send a-canvas update-canvas)
                   (send a-canvas refresh-spreadsheet)
                   (send a-canvas refocus)
                   )])
  
  (new button%
       [label "Refocus"]
       [parent button-panel-1]
       [callback (lambda (b e)
                   (send a-canvas refocus)
                   )])
  
  (define create (new path-dialog%
                      [put? #t]
                      [filters (list (list "Text Files" "*.txt"))]))

  #|
  (new button%
       [label "Generate for IDS"]
       [parent button-panel-2]
       [callback (lambda (b e) 
                   (define stripped (filter (lambda (x) (tree-path? (entities->nodes x))) (group-entities (get-selected-entities (get-field search-list a-canvas)))))
                   (for/list ([tree stripped])
                             (display tree)
                             (newline)
                             (newline)))
                 ])
  |#
  ;binary for osx, text for windows
  ;(generate-ids-pattern (downscale stripped drawing-scale) (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace)))])
  
  
  (new button%
       [label "display selected entities"]
       [parent button-panel-2]
       [callback (lambda (b e)
                   (define stripped (get-selected-entities (get-field search-list a-canvas)))

                   (display "get-start/end-nodes : ")
                   (display (get-start/end-nodes stripped))
                   (newline)
                   
                   (display "stripped : ")
                   (display stripped)
                   (newline)
                   
                   #|
                   (let loop ([lst stripped])
                     (for/list ([entity lst])
                             (let ([x-off 0] ;1877.4885921766004]
                                   [y-off 0]);-311.00031196211586])
                               (when (line? entity)
                                 (display (to-display (+ (node-x (line-p1 entity)) x-off)))
                                 (display ", ")
                                 (display (to-display (+ (node-y (line-p1 entity)) y-off)))
                                 (newline)
                                 (display (to-display (+ (node-x (line-p2 entity)) x-off)))
                                 (display " , ")
                                 (display (to-display (+ (node-y (line-p2 entity)) y-off)))
                                 (newline)
                                 (newline))
                               (when (arc? entity)
                                 (display (to-display (+ (node-x (arc-p1 entity)) x-off)))
                                 (display " , ")
                                 (display (to-display (+ (node-y (arc-p1 entity)) y-off)))
                                 (newline)
                                 ;(display (to-display (+ (node-x (arc-p2 entity)) x-off)))
                                 ;(display " , ")
                                 ;(display (to-display (+ (node-y (arc-p2 entity)) y-off)))
                                 ;(newline)
                                 (display (to-display (+ (node-x (arc-p3 entity)) x-off)))
                                 (display " , ")
                                 (display (to-display (+ (node-y (arc-p3 entity)) y-off)))
                                 (newline)
                                 (newline))
                               (when (path? entity)
                                 (loop (path-entities entity))))))
|#
                   )])

  (new button%
       [label "get start/end nodes of entities"]
       [parent button-panel-2]
       [callback (lambda (b e)
                   (define stripped (get-selected-entities (get-field search-list a-canvas)))
                   (display "start/end-nodes: ")
                   (newline)
                   (for/list ([node (get-start/end-nodes stripped)])
                             (display node)
                             (newline))
                   (newline))])

  (new button%
       [label "do optimization"]
       [parent button-panel-2]
       [callback (lambda (b e)
                   (define stripped (get-selected-entities (get-field search-list a-canvas)))
                   ;(debug-display (length (get-field search-list a-canvas)))
                   ;(set-field! search-list a-canvas (do-optimization stripped))
                   (send a-canvas update-node-lst)
                   (send a-canvas update-canvas)
                   (send a-canvas refresh-spreadsheet))])
  
  (new button%
       [label "Generate for GR/ILS"]
       [parent button-panel-2]
       [callback (lambda (b e)
                   (define stripped (get-selected-entities (make-mirror (get-field search-list a-canvas))))
                   ;binary for osx, text for windows
                   (generate-gr-pattern
                    (for/list ([entity stripped]) (downscale entity drawing-scale))
                    (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace))
                   )]))