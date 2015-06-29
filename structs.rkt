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
  #:transparent #:mutable)

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

;separate a group of entities according to whether they are connected or not. this does a node by node check so there may be "islands" that are actually connected"
(: separate-list-of-entities (-> Entities (Listof Entities)))
(define (separate-list-of-entities entity-lst)
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

;compare 2 list of entities for any connection
(: lists-of-entities-connected? (-> Entities Entities Boolean))
(define (lists-of-entities-connected? lst1 lst2)
  (cond ((empty? lst1) #f)
        (else 
         (let loop : Boolean
           ([entity-lst : Entities lst2])
           (cond [(empty? entity-lst) (lists-of-entities-connected? (cdr lst1) lst2)]
                 [else (let ([x (car lst1)]
                             [y (car entity-lst)])
                         (cond [(are-entities-connected? x y) #t]
                               [else (loop (cdr entity-lst))]))])))))

(: closed-path-entity-list? (-> Entities Boolean))
(define (closed-path-entity-list? entity-lst)
  (define node-lst (entities->nodes entity-lst))
  (empty? (get-path-ends node-lst)))

;check a list of entities(listof entity) if they are connected, if yes then join them together.
(: sort-list-of-entities (-> (Listof Entities) (Listof Entities)))
(define (sort-list-of-entities entity-lst)
  (let master : (Listof Entities)
    ([acc : (Listof Entities) '()]
     [checked : (Listof Entities) '()]
     [unchecked : (Listof Entities) (rest entity-lst)]
     [current-list : Entities (first entity-lst)])
    (cond ((and (empty? checked) (empty? unchecked)) 
           (cons current-list acc))
          ((empty? unchecked) 
           (master (cons current-list acc) '() (rest checked) (first checked)))
          ((lists-of-entities-connected? current-list (first unchecked))
           ;if new current-list, recheck everything in checked - put checked into unchecked
           (master acc '() (append checked (rest unchecked)) (append current-list (first unchecked))))
          (else
           (master acc (cons (first unchecked) checked) (rest unchecked) current-list)))))
 
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

(: clockwise-turn? (-> node node node Boolean))
(define (clockwise-turn? a b c)
  (let* ((ax (node-x a))
         (ay (node-y a))
         (bx (node-x b))
         (by (node-y b))
         (cx (node-x c))
         (cy (node-y c)))
    (not (positive? (- (* (- bx ax) (- cy ay)) (* (- cx ax) (- by ay)))))))

;; MIXED OPERATIONS
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

(: get-start/end-nodes (-> Entities (Listof node)))
(define (get-start/end-nodes entity-lst)
  (define node-lst (entities->nodes entity-lst))
  (if (closed-path-entity-list? entity-lst)
      node-lst
      (get-path-ends node-lst)))

(: entities->nodes (-> Entities (Listof node)))
(define (entities->nodes entity-lst)
  (let ([start-nodes (map get-entity-start entity-lst)]
        [end-nodes (map get-entity-end entity-lst)])
    (append start-nodes end-nodes)))

;given a node, get the entities(listof entity) that the node belongs to.
(: get-belonging-list (-> node (Listof Entities) Entities))
(define (get-belonging-list n lst)
  (let main : Entities
    [(connection-lst : (Listof Entities) lst)]
    (if (ormap (lambda ([x : Entity])
                 (or (equal? (get-entity-start x) n) 
                     (equal? (get-entity-end x) n))) (car lst))
        (car lst)
        (main (cdr lst)))))

(: reorder-entity (-> node Entity Entity))
(define (reorder-entity start-n x)
  (if (equal? start-n (get-entity-start x))
      x
      (reverse-direction x)))

(: find-entity-from-node (-> node Entities (Values Entity Entities)))
(define (find-entity-from-node start-n entity-lst)
  (define norm (findf (lambda ([x : Entity]) (equal? (get-entity-start x) start-n)) entity-lst))
  (if (not norm)
      (let ([reversed (reverse-direction (cast (findf (lambda ([x : Entity]) (equal? (get-entity-end x) start-n)) entity-lst) Entity))])
        (if (arc? reversed) 
            (values (cast reversed Entity) (remove (set-arc-ccw! (cast reversed arc) #f) entity-lst))
            (values (cast reversed Entity) (remove reversed entity-lst))))
      ;possible bug if reversed is not found in entity-lst because of incorrect copying
      (values norm (remove norm entity-lst))))

(: reorder-open-path (-> node Entities Entities))
(define (reorder-open-path start-n entity-lst)
  (define-values (first-entity new-lst) (find-entity-from-node start-n entity-lst))
  (let main : Entities
    ([current : Entity first-entity]
     [acc : Entities (list first-entity)]
     [unchecked : Entities new-lst])
    (cond ((empty? unchecked) acc)
          (else
           (let-values ([(next-entity new-lst) 
                         (find-entity-from-node (get-entity-end current) unchecked)])
             (main next-entity (append acc (list first-entity)) new-lst))))))

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