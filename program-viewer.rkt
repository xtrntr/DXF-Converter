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
         "lst-utils.rkt"
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
(define spreadsheet-width 100)
(define button-height 30)
(define tolerance 0.001) ;in mm

(define (open-file input-port a-frame)
  
  (define area-container (send a-frame get-area-container))
  (for/list ([i (send area-container get-children)])
    (send area-container delete-child i))
  (define-values (editor-width editor-height) (send a-frame get-size))
  
  ;;drawing scaling/unscaling
  (define (upscale-x coord)
    (* display-scale (- coord left)))
  (define (upscale-y coord)
    (* display-scale (- coord bottom)))
  (define (downscale-x coord)
    (+ left (/ coord display-scale)))
  (define (downscale-y coord)
    (+ bottom (/ coord display-scale)))
  ;; upscale really means scale for drawing.
  (define (upscale entity scale)
    (match entity
      [(line highlighted selected visible layer p1 p2 mbr)
       (make-line layer (upscale-x (node-x p1)) (upscale-y (node-y p1)) (upscale-x (node-x p2)) (upscale-y (node-y p2)))]
      [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
       ;(if (> radius 0.5)
           (make-arc layer (upscale-x (node-x center)) (upscale-y (node-y center)) (* scale radius) start end ccw)
           ;(make-dot layer (upscale-x (node-x center)) (upscale-y (node-y center))))
       ]
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
  (define display-scale (get-display-scale original-list editor-width editor-height))
  (define search-list (for/list ([entity original-list])
                                (upscale entity display-scale)))
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

  
  (define (highlight-clicked index-lst entity-lst)
    (for/list ([entity entity-lst]
               [idx (range (length entity-lst))])
              (if (and (entity-visible entity)
                       (entity-selected entity)
                       (ormap (lambda (index)
                                (= (string->number (first index)) idx)) index-lst))
                  (make-highlighted entity)
                  (begin (set-entity-highlighted! entity #f)
                         entity))))
  
  (define a-list-box
    (new list-box%
         [label #f]
         [parent spreadsheet-panel]
         [choices '()]
         [style '(extended column-headers)]
         [columns spreadsheet-headers]
         [callback (lambda (c e)
                     (define selected-on-listbox (send a-list-box get-selections))
                     ;(regexp-match #rx".+?(?=:)" "123:2")
                     (define indexes (for/list ([index (send a-list-box get-selections)])
                                               (send a-list-box get-data index)))
                     (set-field! search-list a-canvas (highlight-clicked indexes (get-field search-list a-canvas)))
                     (send a-canvas refresh-now)
                     )]))
  
  (define (update-spreadsheet a-lst)
    (define to-show (filter entity-selected a-lst))
    (if (empty? to-show)
        (send a-list-box clear)
        (let ([index-labels (remv* (list (void))
                                (for/list ([x a-lst]
                                           [index (range (length a-lst))])
                                          (when (entity-selected x)
                                            (string-append (number->string index) ": " (entity-to-string x)))))])
          (send a-list-box set
                index-labels
                (for/list ([x to-show])
                          (to-display
                           (node-x (get-entity-start x))
                           ;(downscale-x (node-x (get-entity-start x)))
                           ))
                (for/list ([x to-show])
                          (to-display
                           (node-y (get-entity-start x))
                           ;(downscale-y (node-y (get-entity-start x)))
                           ))
                (for/list ([x to-show])
                          (to-display
                           (node-x (get-entity-end x))
                           ;(downscale-x (node-x (get-entity-end x)))
                           ))
                (for/list ([x to-show])
                          (to-display
                           (node-y (get-entity-end x))
                           ;(downscale-y (node-y (get-entity-end x)))
                           )))
          (for ([idx-label index-labels]
                [list-box-idx (range (length index-labels))])
               (send a-list-box set-data list-box-idx (regexp-match #rx".+?(?=:)" idx-label))))))
  
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
                     (send a-canvas refocus)
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
       [label "Display scale/offset"
        ;"Display start/end nodes"
              ]
       [parent button-panel-1]
       [callback (lambda (b e)
                   (display "xscale : ")
                   (display (get-field x-scale a-canvas))
                   (newline)
                   (display "yscale : ")
                   (display (get-field y-scale a-canvas))
                   (newline)
                   (display "xoff : ")
                   (display (get-field x-offset a-canvas))
                   (newline)
                   (display "yoff : ")
                   (display (get-field y-offset a-canvas))
                   (newline)
                   ;(set-field! reorder? a-canvas (not (get-field reorder? a-canvas)))
                   ;(send a-canvas update-canvas)
                   )])

  (new button%
       [label "Change circle into dots"]
       [parent button-panel-2]
       [callback (lambda (b e)
                   (set-field! search-list a-canvas (circ2dots (get-field search-list a-canvas)))
                   (send a-canvas update-canvas)
                   (send a-canvas refresh-spreadsheet)
                   (send a-canvas refresh)
                   )])
  
  (new button%
       [label "Mirror images"]
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

  (define dialog
    (new dialog% [label "Tolerance"]))
  
  (define tolerance-input
    (new text-field% [parent dialog] [label "Tolerance :"]))
  
  ; Add a horizontal panel to the dialog, with centering for buttons
  (define dialog-panel (new horizontal-panel%
                            [parent dialog]
                            [alignment '(center center)]))
  
  ; Add Cancel and Ok buttons to the horizontal panel
  (new button%
       [parent dialog-panel]
       [label "Cancel"]
       [callback (lambda (b e) (send dialog show #f))])
  (new button%
       [parent dialog-panel]
       [label "Ok"]
       [callback (lambda (b e)
                   (let* ([input-value (string->number (send tolerance-input get-value))]
                          [too-high (and input-value (> input-value 100))]
                          [too-low (and input-value (negative? input-value))]
                          [not-num (not input-value)]
                          [valid (and input-value (not too-high) (not too-low))])
                     (cond [valid (set! tolerance input-value) (send dialog show #f)]
                           [too-high (error "tolerance cannot be too high, please put within 0 ~ 100")]
                           [too-low (error "tolerance cannot be negative, please put within 0 ~ 100")]
                           [not-num (error "tolerance should be a number value")])))])
  
  (new button%
       [label "Optimize"]
       [parent button-panel-2]
       [callback (lambda (b e)
                   (define selected (get-selected-entities (get-field search-list a-canvas)))
                   (define not-selected (filter (lambda (x) (not (entity-selected x))) (get-field search-list a-canvas)))
                   (let* ([smallest-y (smallest (get-y-vals selected))]
                          [smallest-x (smallest (get-x-vals selected))]
                          [x-offset (add1 (* -1 smallest-x))]
                          [y-offset (add1 (* -1 smallest-y))]
                          [start-n (node (add1 smallest-x) (add1 smallest-y))])
                     (set-field! search-list a-canvas (append (do-optimization selected start-n) not-selected))
                     (display start-n)
                     (newline)
                     (send a-canvas update-node-lst)
                     (send a-canvas update-canvas)
                     (send a-canvas refresh-spreadsheet)
                     (send a-canvas refocus)))])
  
  (new button%
       [label "Generate for GR/ILS"]
       [parent button-panel-2]
       [callback (lambda (b e)
                   (define selected (for/list ([entity (get-selected-entities (get-field search-list a-canvas))])
                                              (downscale entity display-scale)))
                   ;mirroring works because the mirrored entities have negative y values
                   ;negative y values cause the furthest entities from the 0,0 point to be the nearest entities instead.
                   ;however, this may not work if all y values of the unmirrored version is negative, and mirroring makes it positive
                   (let* ([smallest-y (smallest (get-y-vals selected))]
                          [smallest-x (smallest (get-x-vals selected))]
                          [x-offset (+ 1 (* -1 smallest-x))]
                          [y-offset (+ 1 (* -1 smallest-y))])
                   ;binary for osx, text for windows
                     (generate-gr-pattern
                      selected
                      (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace)
                      x-offset y-offset)))])

  )

  #|
  (new button%
       [label "Generate for IDS"]
       [parent button-panel-2]
       [callback (lambda (b e) 
                   (define stripped (filter (lambda (x) (tree-path? (entities->nodes x))) (group-entities (get-selected-entities (get-field search-list a-canvas)))))
                  ;(generate-ids-pattern (downscale stripped display-scale) (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace)))])
                 ])
  |#