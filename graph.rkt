#lang racket

(require graph
         "structs.rkt") ;stephen chang's graph library

(provide group-entities)

(define (make-node-hs entity-lst)
  (let ([hashy (hash)]
        [add-key-pair
         (lambda (entity hashy)
           (let ([ns (list (get-entity-start entity) (get-entity-end entity))]
                 [val (list entity)])
             (let loop
               ([keys ns]
                [ht hashy])
               (cond ([empty? keys] ht)
                     (else
                      (loop (rest keys)
                            (if (hash-has-key? ht (first keys))
                                (hash-set ht (first keys) (append (hash-ref ht (first keys)) val)) 
                                (hash-set ht (first keys) val))))))))])
    (let loop
      ([lst entity-lst]
       [ht hashy])
      (cond ([empty? lst] ht)
            (else
             (loop (rest lst)
                   (add-key-pair (first lst) ht)))))))


(define (sort-from-connections groups node-ht)
  (map
   (lambda (group)
     (remove-duplicates (flatten
                         (map
                          (lambda (element) (hash-ref node-ht element))
                          group))))
   groups))

(define (group-entities entity-lst)
  (let* ([connections (entities->connections entity-lst)] ;(Listof (List node node))
         [graph (unweighted-graph/undirected connections)]
         [groups (cc graph)]
         [node-hash (make-node-hs entity-lst)]
         [sorted (sort-from-connections groups node-hash)])
    sorted))