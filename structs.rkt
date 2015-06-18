#lang typed/racket

(require "utils.rkt")

(provide (struct-out entity)
         (struct-out line)
         (struct-out arc)
         (struct-out dot)
         (struct-out node)
         (struct-out path)
         Entities
         Node-Structs
         Connection
         make-dot
         make-arc
         make-line
         make-path
         reverse-direction
         get-end
         get-start
         match-struct
         connection-linked?)

(define-type Entities (U line arc path dot))
(define-type Connection (List node node))
(define-type Node-Structs (HashTable node (Listof Entities))) ;DXF is of the format "header_0" "value_0" ... "header_n" "value_n"

(struct node 
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
  ([p : node])
  #:transparent)

(struct line entity 
  ([p1 : node]
   [p2 : node])
  #:transparent)

(struct arc entity 
  ([center : node]
   [radius : Real]
   [start : Real]
   [end : Real]
   [p1 : node]
   [p2 : node]
   [p3 : node]) 
  #:transparent)
     
(struct path entity
  ([entities : (Listof (U line arc))])
  #:transparent)

(: make-dot (-> String Real Real dot))
(define (make-dot layer x y)
  (dot #f #f #f layer (node x y)))

(: make-line (-> String Real Real Real Real line))
(define (make-line layer x1 y1 x2 y2)
  (line #f #f #f layer (node x1 y1) (node x2 y2)))

(: make-arc (-> String Real Real Real Real Real arc))
(define (make-arc layer center-x center-y radius start end)
  (let* ([arc-pts : (Listof Real) (get-arc-points center-x center-y radius start end)]
        [x1 : Real                (first arc-pts)]
        [y1 : Real                (second arc-pts)]
        [x2 : Real                (third arc-pts)]
        [y2 : Real                (fourth arc-pts)]
        [x3 : Real                (fifth arc-pts)]
        [y3 : Real                (sixth arc-pts)])
    (arc #f #f #f layer (node center-x center-y) radius start end (node x1 y1) (node x2 y2) (node x3 y3))))

(: make-path (-> String (Listof (U line arc)) path))
(define (make-path layer lst)
  (path #f #f #f layer lst))

(: reverse-direction (-> Entities Entities))
(define (reverse-direction a-struct)
  (let ([layer : String (entity-layer a-struct)])
    ((match-struct (dot (make-dot layer (node-x p) (node-y p)))
                   (line (make-line layer (node-x p2) (node-y p2) (node-x p1) (node-y p1)))
                   (arc (make-arc layer (node-x center) (node-y center) radius end start))
                   (path (lambda (x) (make-path layer (reverse x)))))
     a-struct)))

(: round-off-point (-> node node))
(define (round-off-point p)
  (node (round-3 (node-x p)) (round-3 (node-y p))))

(: get-start (-> Entities node))
(define (get-start a-struct)
  (round-off-point ((match-struct (dot p)
                                  (line p1)
                                  (arc p1)
                                  (path (lambda (x) (get-start (first x)))))
                    a-struct)))

(: get-end (-> Entities node))
(define (get-end a-struct)
  (round-off-point ((match-struct (dot p)
                                  (line p2)
                                  (arc p3)
                                  (path (lambda (x) (get-end (last x)))))
                    a-struct)))
      
(: connection-linked? (-> Connection Connection Boolean))
(define (connection-linked? c1 c2)
  (: point-equal? (-> node node Boolean))
  (define (point-equal? n1 n2)
    (and (= (node-x n1) (node-x n2))
         (= (node-y n1) (node-y n2))))
  (let ([start1 (car c1)]
        [end1 (cadr c1)]
        [start2 (car c2)]
        [end2 (cadr c2)])
    (or (point-equal? start1 end2)
        (point-equal? start1 start2)
        (point-equal? end1 end2)
        (point-equal? end1 start2))))

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