#lang racket

(require graph
         "structs.rkt") ;stephen chang's graph library

(provide group-entities
         reorder-entities)

;; creates a hash map of
;;key -> node
;;value -> entities connected to that node
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

;; we can use node-equal here to check for similar keys
;; (get-duplicate-nodes (hash-keys node-ht)) then link the 2 groups together.
(define (sort-from-edges groups node-ht)
  (map
   (lambda (group)
     (remove-duplicates (flatten
                         (map
                          (lambda (element) (hash-ref node-ht element))
                          group))))
   groups))

(define (make-graph entity-lst)
  (unweighted-graph/undirected
   (map (lambda (entity) (list (get-entity-start entity) (get-entity-end entity)))
        entity-lst)))

(define (group-entities entity-lst)
  (let* ([edges (entities->edges entity-lst)] ;(Listof (List node node))
         [graph (unweighted-graph/undirected edges)]
         [groups (cc graph)]
         [node-hash (make-node-hs entity-lst)]
         [sorted (sort-from-edges groups node-hash)]
         [sub-graphs (map make-graph sorted)])
    ;Standard textbook depth-first search algorith, ie like in [CLRS]. Consumes a graph and returns three hashes:
    ;one that maps a vertex to its "discovery time",
    ;another that maps a vertex to its predecessor in the search,
    ;and a third that maps a vertex to its "finishing time".
#|
 (for ([entity-lst sorted])
         (let*-values ([(graph) (make-graph entity-lst)]
                       [(discovery-time vertex-predecessor finishing-time) (dfs graph)])
           (display "entity-lst: ")
           (newline)
           (map display-entity entity-lst)
           (display "discovery-time: ")
           (display discovery-time)
           (newline)
           (display "vertex-predecessor: ")
           (display vertex-predecessor)
           (newline)
           (newline)))
    |#
    (display "length edges : ")
    (display (length edges))
    (newline)
    (display "edges : ")
    (display edges)
    (newline)
    (display "length groups : ")
    (display (length groups))
    (newline)
    (for/list ([group groups]
               [group-num (map add1 (range (length groups)))])
              (display (string-append "group " (number->string group-num) " : "))
              (newline)
              (for/list ([edge group])
                        (display edge)
                        (newline)))
    sorted))

(define (edges->entities edge-lst entity-lst)
  (for/list ([edge edge-lst])
    ((lambda (entity) 
       (if (and (equal? (first edge) (get-entity-start entity)) (equal? (second edge) (get-entity-end entity)))
           entity
           (reverse-entity entity)))
     (findf 
      (lambda (entity)
        (or (and (equal? (first edge) (get-entity-start entity)) (equal? (second edge) (get-entity-end entity)))
            (and (equal? (second edge) (get-entity-start entity)) (equal? (first edge) (get-entity-end entity)))))
      entity-lst))))

;; (-> node Entities (Listof Entities))
(define (reorder-entities start-n entity-lst)
  (display "entity-lst: ")
  (display entity-lst)
  (newline) 
  (let* ([edges (entities->edges entity-lst)]
         [x 
          (begin (display "edges: ")
                 (display edges)
                 (newline)
                 5)]
         [lst-of-e-lsts (reorder-edges start-n edges)]
         [new-entities (for/list ([e-lst lst-of-e-lsts])
                                 (edges->entities e-lst entity-lst))])
    new-entities))

(define (edge-connected? e1 e2)
  (or (equal? (first e1) (first e2))
      (equal? (first e1) (second e2))
      (equal? (second e1) (first e2))
      (equal? (second e1) (second e2))))

;e refers to edge
(define (e-find-edge start-e e-lst)
  (filter (lambda (e) (edge-connected? start-e e)) e-lst))

;n refers to node
(define (n-find-edge n e-lst)
  (filter (lambda (e) (or (equal? n (first e))
                          (equal? n (second e)))) e-lst))

(define (n-in-edge? edge n)
  (or (= (first edge) n)
      (= (second edge) n)))

(define (get-other-n edge n)
  (cond [(equal? (first edge) n) (second edge)]
        [(equal? (second edge) n) (first edge)]
        [else (error "node not found in edge" n edge)]))

(define (reorder-edges start-n edge-lst)
  (let ([hashy (make-hash)])
    (let loop
        ([acc '()]
         [e-lst edge-lst]
         [curr-n start-n]
         [curr-path '()])
      (if (empty? e-lst) 
          (if (empty? curr-path)
              acc
              (cons curr-path acc))
          (let ([edges (n-find-edge curr-n e-lst)])
            (cond 
              ;; backtracking
              [(and (empty? curr-path) (empty? edges)) (unless (hash-has-key? hashy curr-n)
                                                         (display "hashy: ")
                                                         (display hashy)
                                                         (newline)
                                                         (display "e-lst: ")
                                                         (display e-lst)
                                                         (newline))
                                                       (loop acc
                                                             e-lst
                                                             (hash-ref hashy curr-n)
                                                             curr-path)]
              ;; dead end
              [(empty? edges) (display (length e-lst))
                              (newline)
                              (loop (cons (reverse curr-path) acc)
                                    e-lst
                                    (hash-ref hashy curr-n)
                                    '())]
              ;; traversing one way
              [(= 1 (length edges)) (hash-set! hashy (get-other-n (first edges) curr-n) curr-n)
                                    (display (length e-lst))
                                    (newline)
                                    (loop acc
                                          (remove (first edges) e-lst)
                                          (get-other-n (first edges) curr-n)
                                          (cons (first edges) curr-path))]
              ;; at a junction/fork
              [else (hash-set! hashy (get-other-n (first edges) curr-n) curr-n)
                    (display (length e-lst))
                    (newline)
                    (loop acc
                          (remove (first edges) e-lst)
                          (get-other-n (first edges) curr-n)
                          (cons (first edges) curr-path))]))))))