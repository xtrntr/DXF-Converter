#lang racket

(require graph ;stephen chang's graph library
         "structs.rkt"
         "utils.rkt") 

(provide (all-defined-out))

;(: entities-to-strings (-> Entities (Listof String)))
(define (entities-to-strings struct-lst)
  (map (lambda (x) (capitalize (symbol->string (object-name x)))) struct-lst))

;given a node, get the entities(listof entity) that the node belongs to.
;(: get-belonging-list (-> node (Listof Entities) Entities))
(define (get-belonging-list n lst n-eq?)
  (let loop
    [(entity-lst lst)]
    (unless (path? (car entity-lst))
      (if (ormap (lambda (x)
                   (or (n-eq? (get-entity-start x) n)
                       (n-eq? (get-entity-end x) n))) (car entity-lst))
          (begin
            (display "n : ")
            (display n)
            (newline)
            (display "entities->nodes : ")
            (display (entities->nodes (car entity-lst)))
            (newline)
            (car entity-lst))
          (loop (cdr entity-lst))))))

;get all the base elements out of a path
;(: get-base-elements (-> Entities Entities))
(define (get-base-elements lst)
  (let loop
    ([acc '()]
     [remaining lst])
    (if (empty? remaining)
        acc
        (if (path? (first remaining))
            (loop (loop acc (path-entities (first remaining)))
                  (rest remaining))
            (loop (cons (first remaining) acc)
                  (rest remaining))))))

;(: make-path (-> Path-Entities path))
(define (make-path lst)
  (define layer (entity-layer (first lst)))
  (path #f #f #f layer lst))

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
(define (sort-from-nodes node-lsts node-ht)
  (map
   (lambda (node-lst)
     (remove-duplicates (flatten
                         (map
                          (lambda (node) (hash-ref node-ht node))
                          node-lst))))
   node-lsts))

(define (make-graph entity-lst)
  (unweighted-graph/undirected
   (map (lambda (entity) (list (get-entity-start entity) (get-entity-end entity)))
        entity-lst)))

#|
(define (link node-lsts node-hash n-eq?)
  (let ([linked-nodes (flatten (for/list ([node-pair (combinations (hash-keys node-hash) 2)]
                                          #:when (apply n-eq? node-pair))
                                         node-pair))])
    (let loop
      ([checked '()]
       [unchecked (rest node-lsts)]
       [current (first node-lsts)])
      (if (empty? unchecked)
          checked
          (if (let ([n (first (member 

(define (group-entities entity-lst n-eq?)
  (let* ([edges (entities->edges entity-lst)] ;(Listof (List node node))
         [graph (unweighted-graph/undirected edges)]
         [node-lsts (cc graph)]
         [node-lsts2 (link node-lsts n-eq?)]
         [node-hash (make-node-hs entity-lst)]
         [sorted (sort-from-nodes node-lsts node-hash)]
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
    |#
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


(define (do-optimization lst node-eq?)
    (let loop ([start (node 0 0)]
               [entity-lst lst]
               [acc '()]
               [individuals '()])
      (if (empty? entity-lst)
          (append (reverse acc) individuals) ;cuz order of the list matters here
          (let* ([groups (group-entities entity-lst)]
                 [start-n (nn start (flatten (map get-start/end-nodes groups)))]
                 [lst-to-reorder (get-belonging-list start-n groups)]
                 [base-elements (get-base-elements lst-to-reorder)]
                 ;we actually want to remove one instance of each element in base elements, not all instances
                 [rest-of-lst (remove* base-elements entity-lst)]
                 [nodes (entities->nodes base-elements)]
                 [single-entity? (= (length lst-to-reorder) 1)]
                 [closed-pattern? (closed-path? nodes)]
                 [open-pattern? (open-path? nodes)]
                 [tree-pattern? (tree-path? nodes)])
            ;possible bug where entity start and end node are equal to each other because they are too close
            ;need to fix node-equal?
            (cond [single-entity? (let ([x (first lst-to-reorder)])
                                    (loop (if (node-eq? start-n (get-entity-start x)) (get-entity-end x) (get-entity-start x))
                                          rest-of-lst
                                          (cons (reorder-entity start-n x) acc)
                                          individuals))]
                  [open-pattern? (let ([new-path (make-selected (make-path (reorder-open-path start-n base-elements)))])
                                   (loop (get-entity-end new-path)
                                         rest-of-lst
                                         (cons new-path acc)
                                         individuals))]
                  [closed-pattern? (let ([new-path (make-selected (make-path (reorder-closed-path start-n base-elements #f)))])
                                     (loop (get-entity-end new-path)
                                           rest-of-lst
                                           (cons new-path acc)
                                           individuals))]
                  [tree-pattern? (loop start-n
                                       rest-of-lst
                                       acc
                                       (append lst-to-reorder individuals))])))))
|#