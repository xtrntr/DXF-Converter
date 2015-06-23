#lang typed/racket

(require "utils.rkt")

(provide (all-defined-out))

(define-type Entities (U line arc path dot))
(define-type Connection (List node node))
(define-type Connected-Entities (Listof Connection)) ;this is to refer to list of entities that share connected nodes but have not been made into a path yet
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

;; STRUCT OPERATIONS
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

;; NODE OPERATIONS
(: round-off-node (-> node node))
(define (round-off-node p)
  (node (round-3 (node-x p)) (round-3 (node-y p))))

(: get-start-node (-> Entities node))
(define (get-start-node a-struct)
  (round-off-node ((match-struct (dot p)
                                  (line p1)
                                  (arc p1)
                                  (path (lambda (x) (get-start-node (first x)))))
                    a-struct)))

(: get-end-node (-> Entities node))
(define (get-end-node a-struct)
  (round-off-node ((match-struct (dot p)
                                  (line p2)
                                  (arc p3)
                                  (path (lambda (x) (get-end-node (last x)))))
                    a-struct)))

(: get-nodes (-> (Listof Entities) (Listof node)))
(define (get-nodes entity-lst)
  (let loop : (Listof node)
    ([acc : (Listof node) '()]
     [lst : (Listof Entities) entity-lst])
    (cond ((empty? lst) acc)
          (else (loop (append (get-connection (car lst)) acc) (cdr lst))))))

;; CONNECTION OPERATIONS
(: connection-linked? (-> Connection Connection Boolean))
(define (connection-linked? c1 c2)
  (: node-equal? (-> node node Boolean))
  (define (node-equal? n1 n2)
    (and (= (node-x n1) (node-x n2))
         (= (node-y n1) (node-y n2))))
  (let ([start1 (car c1)]
        [end1 (cadr c1)]
        [start2 (car c2)]
        [end2 (cadr c2)])
    (or (node-equal? start1 end2)
        (node-equal? start1 start2)
        (node-equal? end1 end2)
        (node-equal? end1 start2))))

;; HASH TABLE OPERATIONS
;the hashtable's KEY:VALUE is NODE:(LISTOF ENTITIES)
(: make-ht (-> (Listof Entities) Node-Structs))
(define (make-ht entity-lst)
  ;add if no existing key or append to existing key
  (: ht-add (-> Node-Structs node Entities Node-Structs))
  (define (ht-add ht key val)
    (if (hash-has-key? ht key)
        (hash-set ht key (append (list val) (hash-ref ht key)))
        (hash-set ht key (list val))))
  (let loop : Node-Structs
    [(ht : Node-Structs (hash))
     (lst : (Listof Entities) entity-lst)]
     (cond ((empty? lst) ht)
           (else
            (let* ([entity (car lst)]
                   [n1 (get-start-node entity)]
                   [n2 (get-end-node entity)])
              (loop (ht-add (ht-add ht n1 entity) n2 entity) (cdr lst)))))))


;; ENTITY / CONNECTION / NODE OPERATIONS
(: get-connection (-> Entities Connection))
(define (get-connection a-struct)
  (list (get-start-node a-struct) (get-end-node a-struct)))

(: get-connection-lst (-> (Listof Entities) Connected-Entities))
(define (get-connection-lst lst)
  (let loop : Connected-Entities
    ([acc : Connected-Entities '()]
     [lst : (Listof Entities) lst])
    (cond ((empty? lst) acc)
          (else (loop (cons (get-connection (car lst)) acc) (cdr lst))))))

(: connections->nodes (-> Connected-Entities (Listof node)))
(define (connections->nodes conn-lst)
  (let loop : (Listof node)
    ([lst : Connected-Entities conn-lst]
     [acc : (Listof node) '()])
    (cond ((empty? lst) acc)
          (else
           (loop (cdr lst) (append (car lst) acc))))))

(: get-path-ends (-> (Listof node) (U (Listof node) Null)))
(define (get-path-ends lst)
  (let loop : (U (Listof node) Null)
    ([dupl : (Listof node) '()]
     [singles : (Listof node) '()]
     [lst : (Listof node) lst])
    (cond ((empty? lst) (remove* dupl singles))
          (((lambda ([x : (Listof node)]) (member (car lst) x)) singles)
           (loop (cons (car lst) dupl) (cons (car lst) singles) (cdr lst)))
          (else
           (loop dupl (cons (car lst) singles) (cdr lst))))))

(: closed-path? (-> Connected-Entities Boolean))
(define (closed-path? conn-lst)
  (define node-lst (connections->nodes conn-lst))
  (empty? (get-path-ends node-lst)))

(: get-start/end-nodes (-> Connected-Entities (Listof node)))
(define (get-start/end-nodes conn-lst)
  (define node-lst (connections->nodes conn-lst))
  (if (closed-path? conn-lst)
      node-lst
      (get-path-ends node-lst)))

#|
(: reorder-connections (-> Connected-Entities (Listof node)))
(define (reorder-connections conn-lst)
  (define node-lst (connections->nodes conn-lst))
  (let loop : (Listof node)
    ([acc : (Listof node)]
     [lst : (Listof node) node-lst]
     [current-conn (first conn-lst)])
    (cond ((empty? lst) acc)
          (e
|#

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