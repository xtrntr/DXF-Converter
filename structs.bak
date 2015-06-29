#lang typed/racket

(require "utils.rkt")

(provide (all-defined-out))

(define-type Entity (U line arc path dot))
(define-type Entities (Listof Entity))

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
   [p3 : node]
   [ccw : Boolean]) 
  #:transparent)

(struct path entity
  ([entities : (Listof (U line arc))])
  #:transparent)

;; STRUCT OPERATIONS
(: make-dot (-> String Real Real dot))
(define (make-dot layer x y)
  (dot #f #f #f layer (node x y)))

(: make-line (-> String Real Real Real Real line))
(define (make-line layer x1 y1 x2 y2)
  (line #f #f #f layer (node x1 y1) (node x2 y2)))

(: make-arc (-> String Real Real Real Real Real Boolean arc))
(define (make-arc layer center-x center-y radius start end ccw?)
  (let* ([arc-pts : (Listof Real) (get-arc-points center-x center-y radius start end)]
         [x1 : Real                (first arc-pts)]
         [y1 : Real                (second arc-pts)]
         [x2 : Real                (third arc-pts)]
         [y2 : Real                (fourth arc-pts)]
         [x3 : Real                (fifth arc-pts)]
         [y3 : Real                (sixth arc-pts)])
    (if ccw?
        (arc #f #f #f layer (node center-x center-y) radius start end (node x3 y3) (node x2 y2) (node x1 y1) #t)
        (arc #f #f #f layer (node center-x center-y) radius start end (node x1 y1) (node x2 y2) (node x3 y3) #f))))

(: make-path (-> String (Listof (U line arc)) path))
(define (make-path layer lst)
  (path #f #f #f layer lst))

(: make-visible-path (-> String (Listof (U line arc)) path))
(define (make-visible-path layer lst)
  (path #f #t #t layer lst))

(: reverse-direction (-> Entity Entity))
(define (reverse-direction a-struct)
  (let* ([layer : String (entity-layer a-struct)]
         [highlighted? : Boolean (entity-highlighted a-struct)]
         [selected? : Boolean (entity-selected a-struct)]
         [visible? : Boolean (entity-visible a-struct)]
         [reversed-struct : Entity ((match-struct (dot (make-dot layer (node-x p) (node-y p)))
                                                  (line (make-line layer (node-x p2) (node-y p2) (node-x p1) (node-y p1)))
                                                  (arc (make-arc layer (node-x center) (node-y center) radius start end #t))
                                                  (path (lambda (x) (make-path layer (reverse x)))))
                                    a-struct)])
    (when highlighted? (set-entity-highlighted! reversed-struct #t))
    (when selected? (set-entity-selected! reversed-struct #t))
    (when visible? (set-entity-visible! reversed-struct #t))
    reversed-struct))

;; ENTITY OPERATIONS
(: are-entities-connected? (-> Entity Entity Boolean))
(define (are-entities-connected? x y)
  (or (equal? (get-entity-end x) (get-entity-end y))
      (equal? (get-entity-end x) (get-entity-start y))
      (equal? (get-entity-start x) (get-entity-end y))
      (equal? (get-entity-start x) (get-entity-start y))))

;sort a group of entities according to whether they are connected or not
(: sort-entities (-> Entities (Listof Entities)))
(define (sort-entities entity-lst)
  ;iterate through each value of entity-ls
  (let master : (Listof Entities)
    ([acc : (Listof Entities) '()]
     [remainder : Entities entity-lst])
    (cond ((empty? remainder) acc)
          (else (master (let sort :  (Listof Entities)
                          ([current : Entity (first remainder)]
                           [checked : (Listof Entities) '()]
                           [unchecked : (Listof Entities) acc])
                          (cond ((empty? unchecked) 
                                 (cons (list current)
                                       checked))
                                ((ormap (lambda ([check-against : Entity]) 
                                          (are-entities-connected? current check-against)) (first unchecked))
                                 (append (rest unchecked) 
                                         (list (cons current (first unchecked)))
                                         checked))
                                (else (sort current (cons (first unchecked) checked) (rest unchecked)))))
                        (rest remainder))))))
 
;; NODE OPERATIONS
(: node-equal? (-> node node Boolean))
(define (node-equal? n1 n2)
  (and (= (node-x n1) (node-x n2))
       (= (node-y n1) (node-y n2))))

(: round-off-node (-> node node))
(define (round-off-node p)
  (node (round-3 (node-x p)) (round-3 (node-y p))))

(: get-entity-start (-> Entity node))
(define (get-entity-start a-struct)
  (round-off-node ((match-struct (dot p)
                                 (line p1)
                                 (arc p1)
                                 (path (lambda (x) (get-entity-start (first x)))))
                   a-struct)))

(: get-entity-end (-> Entity node))
(define (get-entity-end a-struct)
  (round-off-node ((match-struct (dot p)
                                 (line p2)
                                 (arc p3)
                                 (path (lambda (x) (get-entity-end (last x)))))
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
               [(dot highlighted selected visible layer p)                                    tmp0]
               [(line highlighted selected visible layer p1 p2)                               tmp1]
               [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw) tmp2]
               [(path highlighted selected visible layer entities)                            (d entities)])))])))