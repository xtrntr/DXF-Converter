#lang typed/racket

(require "utils.rkt")

(provide (struct-out entity)
         (struct-out line)
         (struct-out arc)
         (struct-out dot)
         (struct-out point)
         (struct-out path)
         Entities
         Header-Value
         Connection
         make-dot
         make-arc
         make-line
         make-path
         reverse-path
         get-end
         get-start
         match-struct)

(define-type Entities (U line arc path dot))
(define-type Connection (List point point))
(define-type Header-Value (HashTable point Entities)) ;DXF is of the format "header_0" "value_0" ... "header_n" "value_n"

(struct point 
  ([x : Real]
   [y : Real])
  #:transparent)

(struct entity
  ([highlighted : Boolean]
   [selected : Boolean]
   [visible : Boolean]
   [layer : String])
  #:mutable #:transparent)

(struct dot entity
  ([p : point])
  #:transparent)

(struct line entity 
  ([p1 : point]
   [p2 : point])
  #:transparent)

(struct arc entity 
  ([center : point]
   [radius : Real]
   [start : Real]
   [end : Real]
   [p1 : point]
   [p2 : point]
   [p3 : point]) 
  #:transparent)
     
(struct path entity
  ([entities : (Listof (U line arc))])
  #:transparent)

(: make-dot (-> String Real Real dot))
(define (make-dot layer x y)
  (dot #f #f #f layer (point x y)))

(: make-line (-> String Real Real Real Real line))
(define (make-line layer x1 y1 x2 y2)
  (line #f #f #f layer (point x1 y1) (point x2 y2)))

(: make-arc (-> String Real Real Real Real Real arc))
(define (make-arc layer center-x center-y radius start end)
  (let* ([arc-pts : (Listof Real) (get-arc-points center-x center-y radius start end)]
        [x1 : Real                (first arc-pts)]
        [y1 : Real                (second arc-pts)]
        [x2 : Real                (third arc-pts)]
        [y2 : Real                (fourth arc-pts)]
        [x3 : Real                (fifth arc-pts)]
        [y3 : Real                (sixth arc-pts)])
    (arc #f #f #f layer (point center-x center-y) radius start end (point x1 y1) (point x2 y2) (point x3 y3))))

(: make-path (-> String (Listof (U line arc)) path))
(define (make-path layer lst)
  (path #f #f #f layer lst))

(: reverse-path (-> Entities Entities))
(define (reverse-path a-struct)
  (let ([layer : String (entity-layer a-struct)])
    ((match-struct (dot (make-dot layer (point-x p) (point-y p)))
                   (line (make-line layer (point-x p2) (point-y p2) (point-x p1) (point-y p1)))
                   (arc (make-arc layer (point-x center) (point-y center) radius end start))
                   (path (lambda (x) (make-path layer (reverse x)))))
     a-struct)))

(: round-off-point (-> point point))
(define (round-off-point p)
  (point (round-off (point-x p)) (round-off (point-y p))))

(: get-start (-> Entities point))
(define (get-start a-struct)
  (round-off-point ((match-struct (dot p)
                                  (line p1)
                                  (arc p1)
                                  (path (lambda (x) (get-start (first x)))))
                    a-struct)))

(: get-end (-> Entities point))
(define (get-end a-struct)
  (round-off-point ((match-struct (dot p)
                                  (line p2)
                                  (arc p3)
                                  (path (lambda (x) (get-end (last x)))))
                    a-struct)))

(define-syntax match-struct
  (lambda (stx)
    (syntax-case stx ()
      [(_ (dot a) (line b) (arc c) (path d))
       (with-syntax  ([tmp0 (syntax->datum #'a)]
                      [tmp1 (syntax->datum #'b)]
                      [tmp2 (syntax->datum #'c)])
       #'(lambda (a-struct)
           (match a-struct
             [(dot highlighted selected visible layer p)                                tmp0]
             [(line highlighted selected visible layer p1 p2)                           tmp1]
             [(arc highlighted selected visible layer center radius start end p1 p2 p3) tmp2]
             [(path highlighted selected visible layer entities)                        (d entities)])))])))