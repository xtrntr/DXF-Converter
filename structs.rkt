#lang typed/racket

(require "utils.rkt")

(provide (struct-out entity)
         (struct-out line)
         (struct-out arc)
         (struct-out point)
         (struct-out path)
         Entities
         make-dot
         make-arc
         make-line
         make-path
         highlight-lst
         select-lst
         reverse-path
         get-nodes)

(define-type Entities (U line arc path dot))

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
  ([p : p])
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

(: highlight-lst (-> (Listof Entities) Void))
(define (highlight-lst x)
  (let loop : Void
    ([lst : (Listof Entities) x])
    (cond ((empty? lst) (void))
          (else (set-entity-highlighted! (car lst) #t)
                (loop (cdr lst))))))

(: select-lst (-> (Listof Entities) Void))
(define (select-lst x)
  (let loop : Void
    ([lst : (Listof Entities) x])
    (cond ((empty? lst) (void))
          (else (set-entity-selected! (car lst) #t)
                (loop (cdr lst))))))

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
    (match a-struct
      [(struct* line  ([p1 p1]
                       [p2 p2]))            (make-line layer p2 p1)]
      [(struct* arc   ([center center]
                       [radius radius]
                       [start start]
                       [end end]))          (make-arc layer (point-x center) (point-y center) radius end start)])))

(: get-start (-> Entities Point))
(define (get-start a-struct)
  (round-off (match a-struct
    [(struct* line  ([p1 p1]))               p1]
    [(struct* arc   ([p1 p1]))               p1]
    [(struct* path  ([entities entities]))  (get-start (first entities))])))

(: get-end (-> Entities Point))
(define (get-end a-struct)
  (round-off (match a-struct
    [(struct* line  ([p2 p2]))               p2]
    [(struct* arc   ([p3 p3]))               p3]
    [(struct* path  ([entities entities]))  (get-end (last entities))])))

(: get-node (-> Entities (List Point Point)))
(define (get-node a-struct)
  (list (get-end a-struct) (get-start a-struct)))

(: get-nodes (-> (Listof Entities) (Listof Point)))
(define (get-nodes a-list)
  (cond ((empty? a-list) '())
        (else (let ((current (car a-list)))
                (append (get-node current)
                        (get-nodes (cdr a-list)))))))

