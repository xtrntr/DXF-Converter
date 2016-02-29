#lang racket

#|

This is where initialization and everything comes together. This is mainly for holding custom one-time GUI elements, such as the buttons that are initialized in a DXF frame.

TODO:
make spreadsheet display look nice. space out the coordinates
refocus button should work better
resizing panels when resizing the window

TYPED MODULES:
structs
utils
canvas-utils
lst-utils
generate-gr
read-dxf

UNTYPED MODULES:
program-viewer
dxf-canvas

|#

(require "program-viewer.rkt"
         racket/gui/base
         mrlib/path-dialog
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
    
    ;drag and drop
    (define/override (on-drop-file pathname)
      (open-file pathname this))
    
    (define/override (file-menu:between-save-as-and-print file-menu)
      (new menu-item%
           [label "&Open DXF File "]
           [parent file-menu]
           [callback (lambda (b e)
                       (define input-port-or-not (send open run))
                       (when input-port-or-not
                         (open-file input-port-or-not this)))]))
    (super-new)))

(define frame-width  1000)
(define frame-height 800)

(define top-frame 
  (new menu-frame%
       [label "Main"]
       [width frame-width]
       [height frame-height]
       [alignment (list 'left 'top)]))

(send top-frame show #t)

(define open (new path-dialog%
                  [existing? #t]
                  [filters (list (list "DXF Files" "*.dxf") (list "Text Files" "*.txt"))]))