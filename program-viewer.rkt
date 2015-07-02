#lang racket

(require "structs.rkt"
         "read-dxf.rkt"
         "canvas-utils.rkt"
         "constants.rkt"
         "dxf-canvas.rkt"
         "lst-utils.rkt"
         "utils.rkt"
         "ils-pattern-generator.rkt"
         mrlib/path-dialog
         racket/gui/base)

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
                 [(line highlighted selected visible layer p1 p2)                               (make-line layer (scale-x (node-x p1)) (scale-y (node-y p1)) (scale-x (node-x p2)) (scale-y (node-y p2)))]
                 [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw) (make-arc layer (scale-x (node-x center)) (scale-y (node-y center)) (* scale radius) start end ccw)]
                 [(dot highlighted selected visible layer p)                                    (make-dot layer (scale-x (node-x p)) (scale-y (node-y p)))]
                 [(path highlighted selected visible layer path-list)                           (make-path layer (rescale path-list scale))]))))
  
  (define (downscale struct-lst scale)
    (flatten (for/list ([i struct-lst])
               (match i
                 [(line highlighted selected visible layer p1 p2)                               (make-line layer (unscale-x (node-x p1)) (unscale-y (node-y p1)) (unscale-x (node-x p2)) (unscale-y (node-y p2)))]
                 [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw) (make-arc layer (unscale-x (node-x center)) (unscale-y (node-y center)) (/ radius scale) start end ccw)]
                 [(dot highlighted selected visible layer p)                                    (make-dot layer (unscale-x (node-x p)) (unscale-y (node-y p)))]
                 [(path highlighted selected visible layer path-list)                           (make-path layer (downscale path-list scale))]))))
  
  (define original-list (file->struct-list input-port))
  (define-values (drawing-scale left bottom) (get-display-scale original-list editor-width editor-height))
  (define search-list (rescale original-list drawing-scale))
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
         [min-height spreadsheet-height]
         [min-width spreadsheet-width]
         [choices '()]
         [style '(extended column-headers)]
         [columns spreadsheet-headers]))
  
  (define (update-spreadsheet a-lst)
    (define displayed-list (filter entity-selected a-lst))
    (if (empty? displayed-list)
        (send a-list-box clear)
        (send a-list-box set 
              (entities-to-strings displayed-list)
              ;map unscale-x/unscale-y after node-x/node-y after debugging finished to display real DXF values
              (map to-display (map unscale-x (map node-x (map get-entity-start displayed-list))))
              (map to-display (map unscale-y (map node-y (map get-entity-start displayed-list))))
              (map to-display (map unscale-x (map node-x (map get-entity-end displayed-list))))
              (map to-display (map unscale-y (map node-y (map get-entity-end displayed-list)))))))
  
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
         [min-height button-height]
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
                       (cond ((empty? lst) '())
                             ((path? (car lst)) (begin 
                                                  (toggle-visibility (car lst))
                                                  (map-toggle-visibility (path-entities (car lst)))
                                                  (map-toggle-visibility (cdr lst))))
                             (else (begin (toggle-visibility (car lst))
                                          (map-toggle-visibility (cdr lst))))))
                     (send a-canvas update-canvas)
                     (map-toggle-visibility (filter entity-same-layer? search-list))
                     (send a-canvas draw-objects search-list)
                     (send a-canvas on-paint)
                     (send a-canvas refresh-now)))))
  
  (define button-panel-1
    (new horizontal-panel%
         [parent spreadsheet-panel]
         [style '(border)]
         [min-height button-height]
         [alignment '(center top)]))
  
  (define button-panel-2
    (new horizontal-panel%
         [parent spreadsheet-panel]
         [style '(border)]
         [min-height button-height]
         [alignment '(center top)]))
  
  (new button%
       [label "Reorder paths"]
       [parent button-panel-1]
       [callback (lambda (b e)
                   (set-field! reorder? a-canvas #t)
                   (send a-canvas update-canvas)
                   )])
  
  (new button%
       [label "Refocus"]
       [parent button-panel-1]
       [callback (lambda (b e) 
                   (set-field! x-offset a-canvas 0)
                   (set-field! y-offset a-canvas (- editor-height 150))
                   (set-field! x-scale  a-canvas 1)
                   (set-field! y-scale  a-canvas -1)
                   (send a-canvas update-canvas)
                   )])
  
  (define create (new path-dialog%
                      [put? #t]
                      [filters (list (list "Text Files" "*.txt"))]))
  
  (new button%
       [label "Generate for ILS"]
       [parent button-panel-2]
       [callback (lambda (b e) 
                   (define stripped (get-selected (get-field search-list a-canvas)))
                   (display "1"))])
  ;binary for osx, text for windows
  ;(generate-ids-pattern (downscale stripped drawing-scale) (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace)))])
  
  (new button%
       [label "Generate for GR"]
       [parent button-panel-2]
       [callback (lambda (b e) 
                   (define stripped (get-selected (get-field search-list a-canvas)))
                   ;binary for osx, text for windows
                   (generate-gr-pattern (downscale stripped drawing-scale) (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace)))]))