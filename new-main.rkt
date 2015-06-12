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
         mrlib/hierlist
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

(define editor-width  1000)
(define editor-height 630)
(define canvas-height 600)
(define canvas-width 700)
(define spreadsheet-height 600)
(define spreadsheet-width 300)
(define button-height 30)

(define top-frame 
  (new menu-frame%
       [label "Main"]
       [width editor-width]
       [height editor-height]
       [alignment (list 'left 'top)]))

(send top-frame show #t)

(define open (new path-dialog%
                  [existing? #t]
                  [filters (list (list "DXF Files" "*.dxf") (list "Text Files" "*.txt"))]))

(define (open-file input-port)
  
  (define a-frame 
    (new frame%
         [label (path->string (last (explode-path input-port)))]
         [width  editor-width]
         [height editor-height]
         [alignment (list 'left 'top)]))
  
  (define (scale-x coord)
    (* drawing-scale (- (point-x coord) left)))
  
  (define (scale-y coord)
    (* drawing-scale (- (point-y coord) bottom)))
  
  (define (unscale-x coord)
    (+ left (/ (point-x coord) drawing-scale)))
  
  (define (unscale-y coord)
    (+ bottom (/ (point-y coord) drawing-scale)))
  
  (define (rescale struct-lst scale)
    (flatten (for/list ([i struct-lst])
               (match i
                 [(line highlighted selected visible layer p1 p2)                           (make-line layer (scale-x p1) (scale-y p1) (scale-x p2) (scale-y p2))]
                 [(arc highlighted selected visible layer center radius start end p1 p2 p3) (make-arc layer (scale-x center) (scale-y center) (* scale radius) start end)]
                 [(dot highlighted selected visible layer p)                                (make-dot layer (scale-x p) (scale-y p))]
                 [(path highlighted selected visible layer path-list)                       (make-path layer (rescale path-list scale))]))))
  
  (define (downscale struct-lst scale)
    (flatten (for/list ([i struct-lst])
               (match i
                 [(line highlighted selected visible layer p1 p2)                           (make-line layer (unscale-x p1) (unscale-y p1) (unscale-x p2) (unscale-y p2))]
                 [(arc highlighted selected visible layer center radius start end p1 p2 p3) (make-arc layer (unscale-x center) (unscale-y center) (/ radius scale) start end)]
                 [(dot highlighted selected visible layer p)                                (make-dot layer (unscale-x p) (unscale-y p))]
                 [(path highlighted selected visible layer path-list)                       (make-path layer (downscale path-list scale))]))))
  
  (define struct-list (file->struct-list input-port))
  (define-values (drawing-scale left bottom) (get-display-scale struct-list editor-width editor-height))
  (define search-list (rescale struct-list drawing-scale))
  (define layer-list (map (lambda (x) (if (string? x) x (number->string x)))
                            (remove-duplicates (map entity-layer struct-list))))
 
  (define main-panel
    (new horizontal-panel%
         [parent a-frame]))
  
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
  
  (define (update-spreadsheet lst)
    (define displayed-list (filter entity-selected lst))
    (if (empty? displayed-list)
        (send a-list-box clear)
        (send a-list-box set 
              (structs-to-strings displayed-list)
              (map to-display (map point-x (map get-start displayed-list)))
              (map to-display (map point-y (map get-start displayed-list)))
              (map to-display (map point-x (map get-end displayed-list)))
              (map to-display (map point-y (map get-end displayed-list))))))
  
  (define a-canvas
    (new dxf-canvas%
       [parent drawing-panel]
       [min-height canvas-height]
       [min-width canvas-width]
       
       [search-list search-list]
       [x-offset 0]
       [y-offset canvas-height]
       [drawing-scale drawing-scale]
       [x-scale 1]
       [y-scale -1]
       
       ;scaling and unscaling methods. placed outside because they depend on left/bottom that are calculated when opening a struct-list for the first time.
       [scale-x scale-x]
       [unscale-x unscale-x]
       [scale-y scale-y]
       [unscale-y unscale-y]
       
       [update-spreadsheet update-spreadsheet]
       [display-select-box #f]
       [select-box '()]))
  
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
       [label "For testing"]
       [parent button-panel-1]
       [callback (lambda (b e)
                   (display (remove-singles (get-nodes (get-selected search-list)))))])
  
  (new button%
       [label "Refocus"]
       [parent button-panel-1]
       [callback (lambda (b e) 
                   (set-field! x-offset a-canvas 0)
                   (set-field! y-offset a-canvas (- editor-height 150))
                   (set-field! x-scale  a-canvas 1)
                   (set-field! y-scale  a-canvas -1)
                   (send a-canvas update-canvas))])
  
  (define create (new path-dialog%
                    [put? #t]
                    [filters (list (list "Text Files" "*.txt"))]))
  
  (new button%
       [label "Generate for IDS"]
       [parent button-panel-2]
       [callback (lambda (b e) 
                   (define stripped (get-selected search-list))
                   (display "1"))])
                   ;binary for osx, text for windows
                   ;(generate-ids-pattern (downscale stripped drawing-scale) (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace)))])
  
  (new button%
       [label "Generate for ILS"]
       [parent button-panel-2]
       [callback (lambda (b e) 
                   (define stripped (get-selected search-list))
                   ;binary for osx, text for windows
                   (generate-ils-pattern (downscale stripped drawing-scale) (open-output-file (send create run) #:mode 'text #:exists 'truncate/replace)))])
  
  (send a-frame show #t))