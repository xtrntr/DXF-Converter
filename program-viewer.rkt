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
         "lst-utils.rkt"
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

(define (open-file input-port a-frame)
  
  (define area-container (send a-frame get-area-container))
  (for/list ([i (send area-container get-children)])
    (send area-container delete-child i))
  (define-values (editor-width editor-height) (send a-frame get-size))
  
  ;;drawing scaling/unscaling
  (define *dxf-scale (lambda (x) (* x dxf-scale)))
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
                 [(line highlighted selected visible layer p1 p2 mbr)
                  (make-line layer (scale-x (node-x p1)) (scale-y (node-y p1)) (scale-x (node-x p2)) (scale-y (node-y p2)))]
                 [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
                  (make-arc layer (scale-x (node-x center)) (scale-y (node-y center)) (* scale radius) start end ccw)]
                 [(dot highlighted selected visible layer p)
                  (make-dot layer (scale-x (node-x p)) (scale-y (node-y p)))]
                 [(path highlighted selected visible layer path-list)
                  (path #f #f #f layer (rescale path-list scale))]))))
  (define (downscale struct-lst scale)
    (flatten (for/list ([i struct-lst])
               (match i
                 [(line highlighted selected visible layer p1 p2 mbr)
                  (make-line layer (unscale-x (node-x p1)) (unscale-y (node-y p1)) (unscale-x (node-x p2)) (unscale-y (node-y p2)))]
                 [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
                  (make-arc layer (unscale-x (node-x center)) (unscale-y (node-y center)) (/ radius scale) start end ccw)]
                 [(dot highlighted selected visible layer p)
                  (make-dot layer (unscale-x (node-x p)) (unscale-y (node-y p)))]
                 [(path highlighted selected visible layer path-list)
                  (path #f #f #f layer (downscale path-list scale))]))))
  (define (unit-scale struct-lst)
    (flatten (for/list ([i struct-lst])
               (match i
                 [(line highlighted selected visible layer p1 p2 mbr)
                  (make-line layer (*dxf-scale (node-x p1)) (*dxf-scale (node-y p1)) (*dxf-scale (node-x p2)) (*dxf-scale (node-y p2)))]
                 [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
                  (make-arc layer (*dxf-scale (node-x center)) (*dxf-scale (node-y center)) (*dxf-scale radius) start end ccw)]
                 [(dot highlighted selected visible layer p)
                  (make-dot layer (*dxf-scale (node-x p)) (*dxf-scale (node-y p)))]
                 [(path highlighted selected visible layer path-list)
                  (path #f #f #f layer (unit-scale path-list))]))))

  ;dxf-scale is 25.4(inches) or 0.0394(mm)
  (define-values (original-list dxf-scale) (file->struct-list input-port))
  (define left (smallest (get-x-vals original-list)))
  (define bottom (smallest (get-y-vals original-list)))
  (define drawing-scale (get-display-scale original-list editor-width editor-height))
  (define search-list (rescale original-list drawing-scale))
  (define layer-list (map (lambda (x) (if (string? x) x (number->string x)))
                          (remove-duplicates (map entity-layer original-list))))

  (display "length of entities list")
  (newline)
  (display (length search-list))
  (newline)
  
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
              (map (lambda (x) (to-display (unscale-x (node-x (get-entity-start x))))) displayed-list)
              (map (lambda (x) (to-display (unscale-y (node-y (get-entity-start x))))) displayed-list)
              (map (lambda (x) (to-display (unscale-x (node-x (get-entity-end x))))) displayed-list)
              (map (lambda (x) (to-display (unscale-y (node-y (get-entity-end x))))) displayed-list))))
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
         
         [search-list search-list]
         [original-list original-list]
         
         [x-offset 0]
         [y-offset canvas-height]
         
         ;scaling and unscaling methods. placed outside because they depend on left/bottom that are calculated when opening a struct-list for the first time.
         [scale-x scale-x]
         [unscale-x unscale-x]
         [scale-y scale-y]
         [unscale-y unscale-y]
         
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
  
  (define (do-optimization lst)
    (let loop ([start (node 0 0)]
               [entity-lst lst]
               [acc '()]
               [individuals '()])
      (if (empty? entity-lst)
          (append (reverse acc) individuals) ;cuz order of the list matters here
          (let* ([groups (group-entities entity-lst)]
                 [start-n (nn start (flatten (map get-start/end-nodes groups)))]
                 [lst-to-reorder (get-belonging-list start-n groups)]
                 [base-elements (get-base-elements lst-to-reorder)]
                 ;we actually want to remove one instance of each element in base elements, not all instances
                 [rest-of-lst (remove* base-elements entity-lst)]
                 [nodes (entities->nodes base-elements)]
                 [single-entity? (= (length lst-to-reorder) 1)]
                 [closed-pattern? (closed-path? nodes)]
                 [open-pattern? (open-path? nodes)]
                 [tree-pattern? (tree-path? nodes)])
            ;possible bug where entity start and end node are equal to each other because they are too close
            ;need to fix node-equal?
            (cond [single-entity? (let ([x (first lst-to-reorder)])
                                    (loop (if (node-equal? start-n (get-entity-start x)) (get-entity-end x) (get-entity-start x))
                                          rest-of-lst
                                          (cons (reorder-entity start-n x) acc)
                                          individuals))]
                  [open-pattern? (let ([new-path (make-selected (make-path (reorder-open-path start-n base-elements)))])
                                   (loop (get-entity-end new-path)
                                         rest-of-lst
                                         (cons new-path acc)
                                         individuals))]
                  [closed-pattern? (let ([new-path (make-selected (make-path (reorder-closed-path start-n base-elements #f)))])
                                     (loop (get-entity-end new-path)
                                           rest-of-lst
                                           (cons new-path acc)
                                           individuals))]
                  [tree-pattern? (loop start-n
                                       rest-of-lst
                                       acc
                                       (append lst-to-reorder individuals))])))))
  
  (new button%
       [label "display selected entities"]
       [parent button-panel-2]
       [callback (lambda (b e)
                   (define stripped (get-selected-entities (get-field search-list a-canvas)))
                   (for/list ([entity stripped])
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
                                 (newline)))))])

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
                   (set-field! search-list a-canvas (do-optimization stripped))
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
                    (unit-scale (downscale stripped drawing-scale))
                    (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace))
                   )]))