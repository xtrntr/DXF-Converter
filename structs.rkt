#lang typed/racket

(require "utils.rkt")

(provide (struct-out entity)
         (struct-out line)
         (struct-out arc)
         (struct-out point)
         (struct-out path)
         make-point
         make-arc
         make-line
         make-path)

(struct: entity
  ([highlighted : Boolean]
   [selected : Boolean]
   [visible : Boolean]
   [layer : String])
  #:mutable #:transparent)

(struct line entity 
  ([x1 : Flonum]
   [y1 : Flonum]
   [x2 : Flonum]
   [y2 : Flonum])
  #:transparent)

(struct point entity 
  ([x : Flonum]
   [y : Flonum])
  #:transparent)

(struct arc entity 
  ([center-x : Flonum]
   [center-y : Flonum]
   [radius : Flonum]
   [start : Flonum]
   [end : Flonum]
   [x1 : Flonum]
   [y1 : Flonum]
   [x2 : Flonum]
   [y2 : Flonum]
   [x3 : Flonum]
   [y3 : Flonum]) 
  #:transparent)

(struct path entity
  ([entities : (U line arc)])
  #:transparent)

(: make-point (-> String Flonum Flonum point))
(define (make-point layer x y)
  (point #f #f #f layer x y))

(: make-line (-> String Flonum Flonum Flonum Flonum line))
(define (make-line layer x1 y1 x2 y2)
  (line #f #f #f layer x1 y1 x2 y2))

(: make-arc (-> String Flonum Flonum Flonum Flonum Flonum arc))
(define (make-arc layer center-x center-y radius start end)
  (let* ([arc-pts : (Listof Flonum) (get-arc-points center-x center-y radius start end)]
        [x1 : Flonum                (first arc-pts)]
        [y1 : Flonum                (second arc-pts)]
        [x2 : Flonum                (third arc-pts)]
        [y2 : Flonum                (fourth arc-pts)]
        [x3 : Flonum                (fifth arc-pts)]
        [y3 : Flonum                (sixth arc-pts)])
    (arc #f #f #f layer center-x center-y radius start end x1 y1 x2 y2 x3 y3)))

(: make-path (-> String (U line arc) path))
(define (make-path layer lst)
  (path #f #f #f layer lst))

#|lang racket
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

