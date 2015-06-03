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
         make-path
         highlight-lst
         select-lst)

(struct: entity
  ([highlighted : Boolean]
   [selected : Boolean]
   [visible : Boolean]
   [layer : String])
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
  ([entities : (Listof (U line arc))])
  #:transparent)

(: highlight-lst (-> (Listof (U line arc path point)) Void))
(define (highlight-lst x)
  (let loop : Void
    ([lst : (Listof (U line arc path point)) x])
    (cond ((empty? lst) (void))
          (else (set-entity-highlighted! (car lst) #t)
                (loop (cdr lst))))))

(: select-lst (-> (Listof (U line arc path point)) Void))
(define (select-lst x)
  (let loop : Void
    ([lst : (Listof (U line arc path point)) x])
    (cond ((empty? lst) (void))
          (else (set-entity-selected! (car lst) #t)
                (loop (cdr lst))))))

(: make-point (-> String Real Real point))
(define (make-point layer x y)
  (point #f #f #f layer x y))

(: make-line (-> String Real Real Real Real line))
(define (make-line layer x1 y1 x2 y2)
  (line #f #f #f layer x1 y1 x2 y2))

(: make-arc (-> String Real Real Real Real Real arc))
(define (make-arc layer center-x center-y radius start end)
  (let* ([arc-pts : (Listof Real) (get-arc-points center-x center-y radius start end)]
        [x1 : Real                (first arc-pts)]
        [y1 : Real                (second arc-pts)]
        [x2 : Real                (third arc-pts)]
        [y2 : Real                (fourth arc-pts)]
        [x3 : Real                (fifth arc-pts)]
        [y3 : Real                (sixth arc-pts)])
    (arc #f #f #f layer center-x center-y radius start end x1 y1 x2 y2 x3 y3)))

(: make-path (-> String (Listof (U line arc)) path))
(define (make-path layer lst)
  (path #f #f #f layer lst))
                     
(define k (make-path "0" (list (make-line "0" 1 2 3 4) (make-line "0" 1 2 3 4))))