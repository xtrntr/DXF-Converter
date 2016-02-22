#lang typed/racket

(require "structs.rkt"
         "utils.rkt")

(provide (all-defined-out))

(: entities-to-strings (-> Entities (Listof String)))
(define (entities-to-strings struct-lst)
  (map (lambda ([x : Entity]) (capitalize (symbol->string (cast (object-name x) Symbol)))) struct-lst))

;given a node, get the entities(listof entity) that the node belongs to.
(: get-belonging-list (-> node (Listof Entities) Entities))
(define (get-belonging-list n lst)
  (let main : Entities
    [(connection-lst : (Listof Entities) lst)]
    (unless (path? (car connection-lst))
      (if (ormap (lambda ([x : Entity])
                   (or (node-equal? (get-entity-start x) n)
                       (node-equal? (get-entity-end x) n))) (car connection-lst))
          (car connection-lst)
          (main (cdr connection-lst))))))

;get all the base elements out of a path
(: get-base-elements (-> Entities Entities))
(define (get-base-elements lst)
  (let loop : Entities
    ([acc : Entities '()]
     [remaining : Entities lst])
    (if (empty? remaining)
        acc
        (if (path? (first remaining))
            (loop (loop acc (path-entities (first remaining)))
                  (rest remaining))
            (loop (cons (first remaining) acc)
                  (rest remaining))))))

;given a start node and an entity, reorder the entity if necessary
(: reorder-entity (-> node Entity Entity))
(define (reorder-entity start-n x)
  (cond [(node-equal? start-n (get-entity-start x))
         x]
        [(node-equal? start-n (get-entity-end x))
         (reverse-direction x)]
        [else (error "Expected the node to be start or end of the entity, but was given: " start-n x)]))

;;HELPER functions for finding (and reversing if needed) entity/entities in a list of entity.
;;they are a little dense, so feel free to remove the (cast .. ..) for readability. they were put in there for optimization.
;;names are self explanatory, and they use 1/2 node(s) to determine the matching entity
(: find-entity-with-starting-node (-> node Entities (U Entity False)))
(define (find-entity-with-starting-node start-n lst)
  (define result (findf (lambda ([x : Entity]) (node-equal? (get-entity-start x) start-n)) lst))
  ;(when (not result)
  ;  (display "start-n, ")
  ;  (display start-n)
  ;  (newline)
  ;  (display "length lst, ")
  ;  (display (length lst))
  ;  (newline))
  result)

(: find-entity-with-ending-node (-> node Entities (U Entity False)))
(define (find-entity-with-ending-node end-n lst)
  (define result (findf (lambda ([x : Entity]) (node-equal? (get-entity-end x) end-n)) lst))
  ;(when (not result)
  ;  (display "end-n:, ")
  ;  (display end-n)
  ;  (newline)
  ;  (display "(length lst) lst, ")
  ;  (display (length lst))
  ;  (newline))
  result)

(: find-entity-with-ending-and-starting-node (-> node node Entities (U Entity False)))
(define (find-entity-with-ending-and-starting-node start-n end-n lst)
  (findf (lambda ([x : Entity]) (and (node-equal? (get-entity-end x) end-n)
                                     (node-equal? (get-entity-start x) start-n))) lst))

;they also return the entity list with the found entity removed, because sometimes the entity is in the wrong direction
(: find-entity-from-node (-> node Entities (Values Entity Entities)))
(define (find-entity-from-node start-n entity-lst)
  (define norm (find-entity-with-starting-node start-n entity-lst))
  (if (not norm)
      (let* ([found? : (U Entity False) (find-entity-with-ending-node start-n entity-lst)]
             #|[a : Void (when (not found?)
                        (display "start-n, ")
                        (display start-n)
                        (newline)
                        (display "entity lst, ")
                        (display entity-lst)
                        (newline))]|#
             [reversed : Entity (reverse-direction (cast found? Entity))])
             (values reversed (remove found? entity-lst)))
      (values norm (remove norm entity-lst))))

(: find-entities-from-node (-> node Entities (Values Entities Entities)))
(define (find-entities-from-node start-n entity-lst)
  (let loop : (Values Entities Entities)
    ([acc : Entities '()]
     [culled : Entities entity-lst])
    (cond ((not (or (find-entity-with-ending-node start-n culled)
                    (find-entity-with-starting-node start-n culled)))
           (values acc culled))
          ((find-entity-with-starting-node start-n culled)
           (loop (append (list (cast (find-entity-with-starting-node start-n culled) Entity)) acc) 
                 (remove (cast (find-entity-with-starting-node start-n culled) Entity) culled)))
          ((find-entity-with-ending-node start-n culled)
           (loop (append (list (cast (find-entity-with-ending-node start-n culled) Entity)) acc) 
                 (remove (cast (find-entity-with-ending-node start-n culled) Entity) culled)))
          (else (error "Unexpected, given: " culled)))))

(: find-entity-from-nodes (-> node node Entities (Values Entity Entities)))
(define (find-entity-from-nodes start-n end-n entity-lst)
  (define norm (find-entity-with-ending-and-starting-node start-n end-n entity-lst))
  (if (not norm)
      (let ([reversed (reverse-direction (cast (find-entity-with-ending-and-starting-node end-n start-n entity-lst) Entity))])
        (values (cast reversed Entity) (remove (cast (find-entity-with-ending-and-starting-node end-n start-n entity-lst) Entity) entity-lst)))
      (values norm (remove norm entity-lst))))

;given 3 nodes a b c, find if a->b->c is in a clockwise or anticlockwise direction
(: clockwise-turn? (-> node node node Boolean))
(define (clockwise-turn? a b c)
  (match* (a b c) 
    [((node ax ay) (node bx by) (node cx cy)) 
     (not (positive? (- (* (- bx ax) (- cy ay)) (* (- cx ax) (- by ay)))))]))

;build a path,
;given the starting node and the list of entities to build the path
(: reorder-open-path (-> node Path-Entities Path-Entities))
(define (reorder-open-path start-n entity-lst)
  (define-values (first-entity new-lst) (find-entity-from-node start-n entity-lst))
  (define layer (entity-layer (first entity-lst)))
  #|
  (display "reorder-open-path, ")
  (newline)
  (display "start-n, ")
  (display start-n)
  (newline)
  (display "entity-lst, ")
  (display (length entity-lst))
  (newline)
  (newline)
  |#
  (cast (let main : Entities
          ([current : Entity first-entity]
           [acc : Entities (list first-entity)]
           [unchecked : Entities new-lst])
          (cond ((empty? unchecked) acc)
                (else
                 (let-values ([(next-entity new-lst)
                               (find-entity-from-node (get-entity-end current) unchecked)])
                   #|
                   (display "next-entity, ")
                   (display next-entity)
                   (newline)
                   (display "rest of lst, ")
                   (display (length unchecked))
                   (newline) |#
                   (main next-entity (append acc (list next-entity)) new-lst))))) Path-Entities))

;build a path,
;given a starting node, the list of entities to build the path and the direction CW/CCW of the path from the starting node
(: reorder-closed-path (-> node Path-Entities Boolean Path-Entities))
(define (reorder-closed-path start-n entity-lst ccw?)
  (define-values (possibilities not-used) (find-entities-from-node start-n entity-lst))
  #|
  (display "reorder-closed-path, ")
  (newline)
  (display "start-n, ")
  (display start-n)
  (newline)
  (display "entity-lst, ")
  (display entity-lst)
  (newline)
  (newline)
  |#
  (let* ([layer (entity-layer (first possibilities))]
         [nodes (remove start-n (remove-duplicates (entities->nodes possibilities)))] ;list of 2 nodes
         [a (first nodes)]
         [b start-n] ;starting node is middle node
         [c (second nodes)]
         [a>b>c-clockwise? (clockwise-turn? a b c)]
         [next-node (cond ((and (not ccw?) a>b>c-clockwise?) c)
                          ((and (not ccw?) (not a>b>c-clockwise?)) a)
                          ((and ccw? a>b>c-clockwise?) a)
                          ((and ccw? (not a>b>c-clockwise?)) c))])
    (let-values ([(first-entity new-lst) (find-entity-from-nodes start-n next-node entity-lst)])
      (cast (let main : Entities
              ([current : Entity first-entity]
               [acc : Entities (list first-entity)]
               [unchecked : Entities new-lst])
              (cond ((empty? unchecked) acc)
                    (else
                     (let-values ([(next-entity new-lst) 
                                   (find-entity-from-node (get-entity-end current) unchecked)])
                       (main next-entity (append acc (list next-entity)) new-lst))))) Path-Entities))))

(: reorder-jumbled-path (-> node Path-Entities Boolean Path-Entities))
(define (reorder-jumbled-path start-n entity-lst ccw?)
  #|
  (display "reorder-jumbled-path, ")
  (newline)
  (display "start-n, ")
  (display start-n)
  (newline)
  (display "entity-lst, ")
  (display entity-lst)
  (newline)
  (newline)
  |#
  (let-values ([(first-entity new-lst) (find-entity-from-node start-n entity-lst)])
    (cast (let main : Entities
            ([current : Entity first-entity]
             [acc : Entities (list first-entity)]
             [unchecked : Entities new-lst])
            (cond ((empty? unchecked) acc)
                  (else
                   (let-values ([(next-entity new-lst) 
                                 (find-entity-from-node (get-entity-end current) unchecked)])
                     (main next-entity (append acc (list next-entity)) new-lst))))) Path-Entities)))

(: make-path (-> Path-Entities path))
(define (make-path lst)
  (define layer (entity-layer (first lst)))
  (if (closed-path? (entities->nodes lst))
      (path #f #f #f layer (reorder-jumbled-path (get-entity-start (first lst)) lst #t))
      (path #f #f #f layer (reorder-open-path (first (get-start/end-nodes lst)) lst))))


(: reverse-direction (-> Entity Entity))
(define (reverse-direction a-struct)
  (let* ([layer : String (entity-layer a-struct)]
         [highlighted? : Boolean (entity-highlighted a-struct)]
         [selected? : Boolean (entity-selected a-struct)]
         [visible? : Boolean (entity-visible a-struct)]
         [reversed-struct : Entity ((match-struct (dot (make-dot layer (node-x p) (node-y p)))
                                                  (line (make-line layer (node-x p2) (node-y p2) (node-x p1) (node-y p1)))
                                                  (arc (make-arc layer (node-x center) (node-y center) radius end start (not ccw)))
                                                  (path (lambda (x) (make-path (reverse x)))))
                                    a-struct)])
    (when highlighted? (set-entity-highlighted! reversed-struct #t))
    (when selected? (set-entity-selected! reversed-struct #t))
    (when visible? (set-entity-visible! reversed-struct #t))
    reversed-struct))
