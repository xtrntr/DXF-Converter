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
(define-type Edge (List node node))

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
   [p2 : node]
   [mbr : rect])
  #:transparent)

(struct arc entity 
  ([center : node]
   [radius : Real]
   [start : Real]
   [end : Real]
   [p1 : node]
   [p2 : node]
   [p3 : node]
   [ccw : Boolean]
   [mbr : rect]) 
  #:transparent #:mutable)

(struct path entity
  ([entities : (Listof Path-Entity)])
  #:transparent)

;minimum bounding-rect
(struct rect
  ([x1 : Real]
   [y1 : Real]
   [x2 : Real]
   [y2 : Real]) 
  #:transparent)

;; CONSTRUCTORS
(: make-dot (-> String Real Real dot))
(define (make-dot layer x y)
  (dot #f #f #f layer (node x y)))

(: make-line (-> String Real Real Real Real line))
(define (make-line layer x1 y1 x2 y2)
  (let* ([xb (biggest (list x1 x2))]
         [yb (biggest (list y1 y2))]
         [xs (smallest (list x1 x2))]
         [ys (smallest (list y1 y2))]
         [mbr (rect xs ys xb yb)])
    (line #f #f #f layer (node x1 y1) (node x2 y2) mbr)))

;;mbr should be bigger for arcs bigger than a semicircle
(: make-arc (-> String Real Real Real Real Real Boolean arc))
(define (make-arc layer center-x center-y radius start end ccw?)
  (match (get-arc-points center-x center-y radius start end ccw?)
    [(list x1 y1 x2 y2 x3 y3)
     (let* ([arc-is-circle? (or (and (= 0 start) (= 360 end))
                                (and (= 360 start) (= 0 end)))]
            [x-points (if arc-is-circle?
                          (list (+ center-x radius) (- center-x radius))
                          (list x1 x2 x3))]
            [y-points (if arc-is-circle?
                          (list (+ center-y radius) (- center-y radius))
                          (list y1 y2 y3))]
            [xb (biggest x-points)]
            [yb (biggest y-points)]
            [xs (smallest x-points)]
            [ys (smallest y-points)]
            [mbr (rect xs ys xb yb)])
       (arc #f #f #f layer (node center-x center-y) radius start end (node x1 y1) (node x2 y2) (node x3 y3) ccw? mbr))]))

;; ENTITY OPERATIONS
(: make-highlighted! (-> Entity Entity))
(define (make-highlighted! an-entity)
  (set-entity-highlighted! an-entity #t)
  an-entity)

(: make-selected! (-> Entity Entity))
(define (make-selected! an-entity)
  (set-entity-selected! an-entity #t)
  (set-entity-highlighted! an-entity #f)
  (set-entity-visible! an-entity #t)
  an-entity)

(: arc-is-circle? (-> arc Boolean))
(define (arc-is-circle? an-arc)
  (or (and (= 0 (arc-start an-arc))
           (= 360 (arc-end an-arc)))
      (and (= 360 (arc-start an-arc))
           (= 0 (arc-end an-arc)))))

(: entity-to-string (-> Entity String))
(define (entity-to-string x)
  (if (arc? x)
      (if (arc-is-circle? x)
          "Circle"
          (capitalize (symbol->string (cast (object-name x) Symbol))))
      (capitalize (symbol->string (cast (object-name x) Symbol)))))

(: reverse-entity (-> Entity Entity))
(define (reverse-entity a-struct)
  (let* ([layer : String (entity-layer a-struct)]
         [highlighted? : Boolean (entity-highlighted a-struct)]
         [selected? : Boolean (entity-selected a-struct)]
         [visible? : Boolean (entity-visible a-struct)]
         [reversed-struct : Entity ((match-struct (dot (make-dot layer (node-x p) (node-y p)))
                                                  (line (make-line layer (node-x p2) (node-y p2) (node-x p1) (node-y p1)))
                                                  (arc (make-arc layer (node-x center) (node-y center) radius end start (not ccw)))
                                                  (path (lambda (x) (path #f #f #f layer(reverse x)))))
                                    a-struct)])
    (when highlighted? (set-entity-highlighted! reversed-struct #t))
    (when selected? (set-entity-selected! reversed-struct #t))
    (when visible? (set-entity-visible! reversed-struct #t))
    reversed-struct))

(: entities-connected? (-> Entity Entity Boolean))
(define (entities-connected? x y)
  (or (node-equal? (get-entity-end x) (get-entity-end y))
      (node-equal? (get-entity-end x) (get-entity-start y))
      (node-equal? (get-entity-start x) (get-entity-end y))
      (node-equal? (get-entity-start x) (get-entity-start y))))

(: entities-identical? (-> Entity Entity Boolean))
(define (entities-identical? x y)
  (and (equal? (object-name x) (object-name y))
       (or (and (node-equal? (get-entity-end x) (get-entity-end y))
                (node-equal? (get-entity-start x) (get-entity-start y)))
           (and (node-equal? (get-entity-end x) (get-entity-start y))
                (node-equal? (get-entity-start x) (get-entity-end y))))))

(: rect-intersect? (-> rect rect Boolean))
(define (rect-intersect? r1 r2)
  (and (< (rect-x1 r1) (rect-x2 r2))
       (> (rect-x2 r1) (rect-x1 r2))
       (< (rect-y1 r1) (rect-y2 r2))
       (> (rect-y2 r1) (rect-y1 r2))))

(: get-x-vals (-> Entities (Listof Real)))
(define (get-x-vals struct-lst)
  (let loop : (Listof Real)
    [(acc : (Listof Real) '())
     (lst : Entities struct-lst)]
    (cond ((empty? lst) acc)
          (else
           (match (car lst)
             [(struct* line  ([mbr mbr]))
              (loop (cons (rect-x1 mbr) (cons (rect-x2 mbr) acc)) (cdr lst))]
             [(struct* arc   ([mbr mbr]))
              (loop (cons (rect-x1 mbr) (cons (rect-x2 mbr) acc)) (cdr lst))]
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
             [(struct* line  ([mbr mbr]))
              (loop (cons (rect-y1 mbr) (cons (rect-y2 mbr) acc)) (cdr lst))]
             [(struct* arc   ([mbr mbr]))
              (loop (cons (rect-y1 mbr) (cons (rect-y2 mbr) acc)) (cdr lst))]
             [(struct* dot   ([p p]))
              (loop (cons (node-y p) acc) (cdr lst))]
             [(struct* path  ([entities entities]))
              (loop acc (append entities (cdr lst)))])))))

(: get-element-nodes (-> (Listof node) (Listof node)))
(define (get-element-nodes lst)
  (set->list (list->set lst)))

(: get-duplicate-nodes (-> (Listof node) (Listof node)))
(define (get-duplicate-nodes lst)
  (let* ([elements : (Listof node) (get-element-nodes lst)]
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
         [real-dupl : (Listof node) (remove-duplicates duplicates)]
         [uniques : (Listof node) (remove* real-dupl elements)])
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
  (or (> (no-of-unique-nodes node-lst) 2) (= (no-of-unique-nodes node-lst) 1)))

;return all the start/end nodes given a list of entities. for an open path, this means the path ends, for a closed path, it can be any node along the path.
(: get-start/end-nodes (-> Entities (Listof node)))
(define (get-start/end-nodes entity-lst)
  (define node-lst (entities->nodes entity-lst))
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

(: entities->edges (-> Entities (Listof Edge)))
(define (entities->edges entity-lst)
  (for/list ([x entity-lst])
            (list (get-entity-start x)
                  (get-entity-end x))))

;; NODE OPERATIONS
(: node-distance (-> node node Real))
(define (node-distance n1 n2)
  (sqrt (+ (sqr (- (node-x n2) (node-x n1)))
           (sqr (- (node-y n2) (node-y n1))))))

;given a start-n and a list of nodes, find a node in the list nearest to the given start-n
;nn -> nearest node
(: nn (-> node (Listof node) node))
(define (nn start-n node-lst)
  (let ([hs : (HashTable Real node) (make-hash)])
    (map (lambda ([n : node])
           (hash-set! hs (node-distance start-n n) n))
         node-lst)
    (hash-ref hs (smallest (hash-keys hs)))))

(: node-equal? (-> node node Boolean))
(define (node-equal? n1 n2)
  (and (= 0 (round-3 (abs (- (node-x n1) (node-x n2)))))
       (= 0 (round-3 (abs (- (node-y n1) (node-y n2)))))))

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

(: make-y-mirror (-> Entities Entities))
(define (make-y-mirror entity-lst)
  (for/list ([x : Entity entity-lst])
            (match x
              [(dot highlighted selected visible layer p)
               (dot highlighted selected visible layer
                    (node (node-x p) (* -1 (node-y p))))]
              [(line highlighted selected visible layer p1 p2 mbr)
               (let* ([xb (biggest (list (node-x p1) (node-x p2)))]
                      [yb (biggest (list (* -1 (node-y p1)) (* -1 (node-y p2))))]
                      [xs (smallest (list (node-x p1) (node-x p2)))]
                      [ys (smallest (list (* -1 (node-y p1)) (* -1 (node-y p2))))]
                      [mbr (rect xs ys xb yb)])
                 (line highlighted selected visible layer
                       (node (node-x p1) (* -1 (node-y p1)))
                       (node (node-x p2) (* -1 (node-y p2)))
                       mbr))]
              [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
               (let* ([xb (biggest (list (node-x p1) (node-x p2) (node-x p3)))]
                      [yb (biggest (list (* -1 (node-y p1)) (* -1 (node-y p2)) (* -1 (node-y p3))))]
                      [xs (smallest (list (node-x p1) (node-x p2) (node-x p3)))]
                      [ys (smallest (list (* -1 (node-y p1)) (* -1 (node-y p2)) (* -1 (node-y p3))))]
                      [mbr (rect xs ys xb yb)])
                 (arc highlighted selected visible layer
                      (node (node-x center) (* -1 (node-y center)))
                      radius
                      (get-y-mirror-angle start)
                      (get-y-mirror-angle end)
                      (node (node-x p1) (* -1 (node-y p1)))
                      (node (node-x p2) (* -1 (node-y p2)))
                      (node (node-x p3) (* -1 (node-y p3)))
                      (not ccw)
                      mbr))]
              [(path highlighted selected visible layer entities)
               (path highlighted selected visible layer (cast (make-y-mirror entities) Path-Entities))])))

(: make-x-mirror (-> Entities Entities))
(define (make-x-mirror entity-lst)
  (for/list ([x : Entity entity-lst])
            (match x
              [(dot highlighted selected visible layer p)
               (dot highlighted selected visible layer
                    (node (* -1 (node-x p)) (node-y p)))]
              [(line highlighted selected visible layer p1 p2 mbr)
               (let* ([xb (biggest (list (* -1 (node-x p1)) (* -1 (node-x p2))))]
                      [yb (biggest (list (node-y p1) (node-y p2)))]
                      [xs (smallest (list (* -1 (node-x p1)) (* -1 (node-x p2))))]
                      [ys (smallest (list (node-y p1) (node-y p2)))]
                      [mbr (rect xs ys xb yb)])
                 (line highlighted selected visible layer
                       (node (* -1 (node-x p1)) (node-y p1))
                       (node (* -1 (node-x p2)) (node-y p2))
                       mbr))]
              [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
               (let* ([xb (biggest (list (* -1 (node-x p1)) (* -1 (node-x p2)) (* -1 (node-x p3))))]
                      [yb (biggest (list (node-y p1) (node-y p2) (node-y p3)))]
                      [xs (smallest (list (* -1 (node-x p1)) (* -1 (node-x p2)) (* -1 (node-x p3))))]
                      [ys (smallest (list (node-y p1) (node-y p2) (node-y p3)))]
                      [mbr (rect xs ys xb yb)])
                 (arc highlighted selected visible layer
                      (node (* -1 (node-x center)) (node-y center))
                      radius
                      (get-x-mirror-angle start)
                      (get-x-mirror-angle end)
                      (node (* -1 (node-x p1)) (node-y p1))
                      (node (* -1 (node-x p2)) (node-y p2))
                      (node (* -1 (node-x p3)) (node-y p3))
                      (not ccw)
                      mbr))]
              [(path highlighted selected visible layer entities)
               (path highlighted selected visible layer (cast (make-x-mirror entities) Path-Entities))])))

(: rotate-90 (-> Entities Entities))
(define (rotate-90 entity-lst)
  (for/list ([x : Entity entity-lst])
            (match x
              [(dot highlighted selected visible layer p)
               (dot highlighted selected visible layer
                    (node (* -1 (node-y p)) (node-x p)))]
              [(line highlighted selected visible layer p1 p2 mbr)
               (let* ([xb (biggest (list (* -1 (node-y p1)) (* -1 (node-y p2))))]
                      [yb (biggest (list (node-x p1) (node-x p2)))]
                      [xs (smallest (list (* -1 (node-y p1)) (* -1 (node-y p2))))]
                      [ys (smallest (list (node-x p1) (node-x p2)))]
                      [mbr (rect xs ys xb yb)])
                 (line highlighted selected visible layer
                       (node (* -1 (node-y p1)) (node-x p1))
                       (node (* -1 (node-y p2)) (node-x p2))
                       mbr))]
              [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
               (let* ([xb (biggest (list (* -1 (node-y p1)) (* -1 (node-y p2)) (* -1 (node-y p3))))]
                      [yb (biggest (list (node-x p1) (node-x p2) (node-x p3)))]
                      [xs (smallest (list (* -1 (node-y p1)) (* -1 (node-y p2)) (* -1 (node-y p3))))]
                      [ys (smallest (list (node-x p1) (node-x p2) (node-x p3)))]
                      [mbr (rect xs ys xb yb)]
                      [rotated-start (get-rotated-angle start)]
                      [rotated-end (get-rotated-angle end)])
                 (arc highlighted selected visible layer
                      (node (* -1 (node-y center)) (node-x center))
                      radius
                      (if (arc-is-circle? x) start (get-rotated-angle start))
                      (if (arc-is-circle? x) end (get-rotated-angle end))
                      (node (* -1 (node-y p1)) (node-x p1))
                      (node (* -1 (node-y p2)) (node-x p2))
                      (node (* -1 (node-y p3)) (node-x p3))
                      ccw
                      mbr))]
              [(path highlighted selected visible layer entities)
               (path highlighted selected visible layer (cast (rotate-90 entities) Path-Entities))])))

(: display-entity (-> Entity Void))
(define (display-entity x)
  (match x
    [(line _ _ _ _ p1 p2 _)                                 (for ([x (list "line-> p1: " (round-off-node p1) " p2: " (round-off-node p2) "\n")])
                                                                 (display x))]
    [(arc _ _ _ _ center radius start end p1 p2 p3 ccw _)   (for ([x (list "arc-> p1: " (round-off-node p1) " p3: " (round-off-node p3) "\n")])
                                                                 (display x))]
    [(dot _ _ _ _ p)                                        (for ([x (list "dot-> p1: " (round-off-node p) "\n")])
                                                                 (display x))]
    [(path _ _ _ _ path-list)                               (for ([x path-list])
                                                                 (display-entity x))]))

(define-syntax match-struct
  (lambda (stx)
    (syntax-case stx ()
      [(_ (dot a) (line b) (arc c) (path d))
       (with-syntax  ([tmp0 (syntax->datum #'a)]
                      [tmp1 (syntax->datum #'b)]
                      [tmp2 (syntax->datum #'c)])
         #'(lambda (a-struct)
             (match a-struct
               [(dot highlighted selected visible layer p)                                        tmp0]
               [(line highlighted selected visible layer p1 p2 mbr)                               tmp1]
               [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr) tmp2]
               [(path highlighted selected visible layer entities)                               (d entities)])))])))
