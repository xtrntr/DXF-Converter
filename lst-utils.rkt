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

;given a start node and an entity, reorder the entity if necessary
(: reorder-entity (-> node Entity Entity))
(define (reorder-entity start-n x)
  (cond [(node-equal? start-n (get-entity-start x))
         x]
        [(node-equal? start-n (get-entity-end x))
         (reverse-direction x)]
        [else (error "Expected the node to be start or end of the entity, but was given: " start-n x)]))

;;HELPER functions for finding (and reversing if needed) entity/entities in a list of entity.
;;names are self explanatory, and they use 1/2 node(s) to determine the matching entity
(: find-entity-with-starting-node (-> node Entities (U Entity False)))
(define (find-entity-with-starting-node start-n lst)
  (findf (lambda ([x : Entity]) (node-equal? (get-entity-start x) start-n)) lst))

(: find-entity-with-ending-node (-> node Entities (U Entity False)))
(define (find-entity-with-ending-node end-n lst)
  (let [(result : (U Entity False) (findf (lambda ([x : Entity]) (node-equal? (get-entity-end x) end-n)) lst))]
    (when (not result)
      (display "Error: node ")
      (display end-n)
      (display " cannot be found among: ")
      (display lst))
    result))

(: find-entity-with-ending-and-starting-node (-> node node Entities (U Entity False)))
(define (find-entity-with-ending-and-starting-node start-n end-n lst)
  (findf (lambda ([x : Entity]) (and (node-equal? (get-entity-end x) end-n)
                                     (node-equal? (get-entity-start x) start-n))) lst))

;they also return the entity list with the found entity removed, because sometimes the entity is in the wrong direction
(: find-entity-from-node (-> node Entities (Values Entity Entities)))
(define (find-entity-from-node start-n entity-lst)
  (define norm (find-entity-with-starting-node start-n entity-lst))
  (if (not norm)
      (let ([reversed (reverse-direction (cast (find-entity-with-ending-node start-n entity-lst) Entity))])
        (values (cast reversed Entity) (remove (cast (find-entity-with-ending-node start-n entity-lst) Entity) entity-lst)))
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
  (let* ((ax (node-x a))
         (ay (node-y a))
         (bx (node-x b))
         (by (node-y b))
         (cx (node-x c))
         (cy (node-y c)))
    (not (positive? (- (* (- bx ax) (- cy ay)) (* (- cx ax) (- by ay)))))))

;build a path,
;given the starting node and the list of entities to build the path
(: reorder-open-path (-> node Entities path))
(define (reorder-open-path start-n entity-lst)
  (define-values (first-entity new-lst) (find-entity-from-node start-n entity-lst))
  (define layer (entity-layer (first entity-lst)))
  (make-selected-path layer 
                      (cast (let main : Entities
                              ([current : Entity first-entity]
                               [acc : Entities (list first-entity)]
                               [unchecked : Entities new-lst])
                              (cond ((empty? unchecked) acc)
                                    (else
                                     (let-values ([(next-entity new-lst) 
                                                   (find-entity-from-node (get-entity-end current) unchecked)])
                                       (main next-entity (append acc (list next-entity)) new-lst))))) (Listof (U line arc)))))

;build a path,
;given a starting node, the list of entities to build the path and the direction CW/CCW of the path from the starting node
(: reorder-closed-path (-> node Entities Boolean path))
(define (reorder-closed-path start-n entity-lst ccw?)
  (define-values (possibilities not-used) (find-entities-from-node start-n entity-lst))
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
      (make-selected-path layer (cast (let main : Entities
                                        ([current : Entity first-entity]
                                         [acc : Entities (list first-entity)]
                                         [unchecked : Entities new-lst])
                                        (cond ((empty? unchecked) acc)
                                              (else
                                               (let-values ([(next-entity new-lst) 
                                                             (find-entity-from-node (get-entity-end current) unchecked)])
                                                 (main next-entity (append acc (list next-entity)) new-lst))))) (Listof (U line arc)))))))