#lang typed/racket

#| 

This module is where we define the structs, their constructors, custom types that build on combined structs and simple operations on entities/nodes.
Try to keep the more complex and specific functions in lst-utils.

|#

(require "utils.rkt")
(provide (all-defined-out))

(define-type Entity (U line arc path dot))
(define-type Entities (Listof Entity))
(define-type Path-Entity (U line arc))
(define-type Path-Entities (Listof Path-Entity))

(define-type Connection (List node node))

(struct node 
  ([x : Real]
   [y : Real])
  #:mutable #:transparent)

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
  #:transparent #:mutable)

(struct path entity
  ([entities : (Listof Path-Entity)])
  #:transparent)

;; CONSTRUCTORS
(: make-dot (-> String Real Real dot))
(define (make-dot layer x y)
  (dot #f #f #f layer (node x y)))

(: make-line (-> String Real Real Real Real line))
(define (make-line layer x1 y1 x2 y2)
  (line #f #f #f layer (node x1 y1) (node x2 y2)))

(: make-arc (-> String Real Real Real Real Real Boolean arc))
(define (make-arc layer center-x center-y radius start end ccw?)
  (match (get-arc-points center-x center-y radius start end ccw?)
    [(list x1 y1 x2 y2 x3 y3)
     (if ccw?
         (arc #f #f #f layer (node center-x center-y) radius start end (node x1 y1) (node x2 y2) (node x3 y3) #t)
         (arc #f #f #f layer (node center-x center-y) radius start end (node x1 y1) (node x2 y2) (node x3 y3) #f))]))

(: make-selected (-> Entity Entity))
(define (make-selected an-entity)
  (set-entity-selected! an-entity #t)
  (set-entity-visible! an-entity #t)
  an-entity)

;; ENTITY OPERATIONS
(: are-entities-connected? (-> Entity Entity Boolean))
(define (are-entities-connected? x y)
  (or (node-equal? (get-entity-end x) (get-entity-end y))
      (node-equal? (get-entity-end x) (get-entity-start y))
      (node-equal? (get-entity-start x) (get-entity-end y))
      (node-equal? (get-entity-start x) (get-entity-start y))))

(: get-x-vals (-> Entities (Listof Real)))
(define (get-x-vals struct-lst)
  (let loop : (Listof Real)
    [(acc : (Listof Real) '())
     (lst : Entities struct-lst)]
    (cond ((empty? lst) acc)
          (else
           (match (car lst)
             [(struct* line  ([p1 p1]
                              [p2 p2]))
              (loop (cons (node-x p1) (cons (node-x p2) acc)) (cdr lst))]
             [(struct* arc   ([center center]
                              [radius radius]))
              (loop (cons (+ (node-x center) radius) (cons (- (node-x center) radius) acc)) (cdr lst))]
             [(struct* dot   ([p p]))
              (loop (cons (node-x p) acc) (cdr lst))]
             [(struct* path  ([entities entities]))
              (loop acc (append entities (cdr lst)))])))))

(: get-y-vals (-> Entities (Listof Real)))
(define (get-y-vals struct-lst)
  (let loop : (Listof Real)
    [(acc : (Listof Real) '())
     (lst : Entities struct-lst)]
    (cond ((empty? lst) acc)
          (else
           (match (car lst)
             [(struct* line  ([p1 p1]
                              [p2 p2]))
              (loop (cons (node-y p1) (cons (node-y p2) acc)) (cdr lst))]
             [(struct* arc   ([center center]
                              [radius radius]))
              (loop (cons (+ (node-y center) radius) (cons (- (node-y center) radius) acc)) (cdr lst))]
             [(struct* dot   ([p p]))
              (loop (cons (node-y p) acc) (cdr lst))]
             [(struct* path  ([entities entities]))
              (loop acc (append entities (cdr lst)))])))))

;'((1 2) (3 4) (1 2)) -> '((1 2))
;self explanatory
(: get-duplicate-nodes (-> (Listof node) (Listof node)))
(define (get-duplicate-nodes lst)
  (let* ([elements : (Listof node) (set->list (list->set lst))]
         [duplicates : (Listof node) (for/fold ([x lst]) ([y elements])
                                       ((inst remove node) y x))])
    duplicates))

;'((1 2) (3 4) (1 2)) -> '((3 4))
;self explanatory
(: get-unique-nodes (-> (Listof node) (Listof node)))
(define (get-unique-nodes lst)
  (let* ([elements : (Listof node) (set->list (list->set lst))]
         [duplicates : (Listof node) (for/fold ([x lst]) ([y elements])
                                       ((inst remove node) y x))]
         [uniques : (Listof node) (remove* duplicates elements)])
    uniques))

(: no-of-unique-nodes (-> (Listof node) Integer))
(define (no-of-unique-nodes node-lst)
  (length (get-unique-nodes node-lst)))

;a list of nodes that form a circle contains no unique nodes.
(: closed-path? (-> (Listof node) Boolean))
(define (closed-path? node-lst)
  (= (no-of-unique-nodes node-lst) 0))

;a list of nodes that form a horseshoe contains 2 unique nodes at the horseshoe ends.
(: open-path? (-> (Listof node) Boolean))
(define (open-path? node-lst)
  (= (no-of-unique-nodes node-lst) 2))

;a list of nodes that form a tree has more than 1 unique nodes as dead ends.
(: tree-path? (-> (Listof node) Boolean))
(define (tree-path? node-lst)
  (> (no-of-unique-nodes node-lst) 2))

;return all the start/end nodes given a list of entities. for an open path, this means the path ends, for a closed path, it can be any node along the path.
(: get-start/end-nodes (-> Entities (Listof node)))
(define (get-start/end-nodes entity-lst)
  (define node-lst (entities->nodes entity-lst))
  ;if there is a closed path containing 
  (if (closed-path? node-lst)
      node-lst
      (get-unique-nodes node-lst)))

;return all the nodes of the path given a list of entities
;TODO: can we use remove* to remove duplicates here without breaking the program?
(: entities->nodes (-> Entities (Listof node)))
(define (entities->nodes entity-lst)
  (let ([start-nodes (map get-entity-start entity-lst)]
        [end-nodes (map get-entity-end entity-lst)])
    (append start-nodes end-nodes)))

(: entities->connections (-> Entities (Listof Connection)))
(define (entities->connections entity-lst)
  (map (lambda ([x : Entity]) (list (get-entity-start x)
                                    (get-entity-end x))) entity-lst))

;; NODE OPERATIONS
(: node-equal? (-> node node Boolean))
(define (node-equal? n1 n2)
  (and (> 0.5 (cast (abs (- (node-x n1) (node-x n2))) Float))
       (> 0.5 (cast (abs (- (node-y n1) (node-y n2))) Float))))

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

(: make-mirror (-> Entities Void))
(define (make-mirror entity-lst)
  (let loop : Void
    ([x : Entities entity-lst])
    (cond ((empty? x) (void))
          (else
           (loop (rest x))
           (match (car x)
             [(dot highlighted selected visible layer p)
              (set-node-y! p (* -1 (node-y p)))]
             [(line highlighted selected visible layer p1 p2)
              (set-node-y! p1 (* -1 (node-y p1)))
              (set-node-y! p2 (* -1 (node-y p2)))]
             [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw)
              (set-node-y! p1 (* -1 (node-y p1)))
              (set-node-y! p2 (* -1 (node-y p2)))
              (set-node-y! p3 (* -1 (node-y p3)))
              (set-arc-start! (car x) (get-mirror-angle end))
              (set-arc-end! (car x) (get-mirror-angle start))
;              (set-arc-ccw! (car x) (not ccw))
              (set-node-y! center (* -1 (node-y center)))]
             [(path highlighted selected visible layer entities)
              (loop (path-entities (car x)))])))))

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