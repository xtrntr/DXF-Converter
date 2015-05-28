#lang typed/racket

(provide (struct-out entity)
         (struct-out line)
         (struct-out arc)
         (struct-out point)
         (struct-out path))

(struct: entity
  ([layer : String]
   [highlighted : Boolean]
   [selected : Boolean]
   [visible : Boolean])
  #:mutable #:transparent)

(struct line entity 
  ([x1 : Real]
   [y1 : Real]
   [x2 : Real]
   [y2 : Real])
  #:transparent)

(struct point entity 
  ([x : Real]
   [y : Real])
  #:transparent)

(struct arc entity 
  ([center-x : Real]
   [center-y : Real]
   [radius : Real]
   [start : Real]
   [end : Real]
   [x1 : Real]
   [y1 : Real]
   [x2 : Real]
   [y2 : Real]
   [x3 : Real]
   [y3 : Real]) 
  #:transparent)

(struct path entity
  ([entities : (List point line arc)])
  #:transparent)

#|
#lang racket
;; This module contains methods for geometric manipulation on structs.
;; Also includes procedures for converting lists of DXF entities to said structs.

(provide (struct-out entity)
         (struct-out line)
         (struct-out arc)
         (struct-out point)
         (struct-out path))

;; struct definitions
(struct entity 
  (layer
   [highlighted #:auto #:mutable] 
   [selected #:auto #:mutable] 
   [visible #:auto #:mutable]) 
  #:auto-value #f #:transparent)

(struct path entity
  (entities)
  #:transparent)

(struct line entity 
  (x1 y1 x2 y2) 
  #:transparent)

(struct point entity 
  (x y)
  #:transparent)

(struct arc entity 
  (center-x center-y radius start end x1 y1 x2 y2 x3 y3) 
  #:transparent)

(define (reverse-arc a-struct)
  (let ((start-x (arc-x1 a-struct))
        (start-y (arc-y1 a-struct))
        (end-x (arc-x3 a-struct))
        (end-y (arc-y3 a-struct)))
    (struct-copy arc a-struct
                 [x1 end-x]
                 [y1 end-y]
                 [x3 start-x]
                 [y3 start-y])))

;(reverse-arc (arc "0" 1 2 3 90 180 1 1 2 2 3 3))
|#