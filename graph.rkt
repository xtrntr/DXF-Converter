#lang racket

(require graph ;stephen chang's graph library
         "structs.rkt"
         "utils.rkt"
         "lst-utils.rkt") 

(provide (all-defined-out))

;; we can use node-equal here to check for similar keys
;; (get-duplicate-nodes (hash-keys node-ht)) then link the 2 groups together.
(define (sort-from-nodes node-lst node-ht)
  (remove-duplicates (foldl append '() (map (lambda (node) (hash-ref node-ht node)) node-lst))))

(define (sort-from-edges edge-lst node-ht)
  (remove-duplicates (foldl append '() (map (lambda (edge) (hash-ref node-ht edge)) edge-lst))))

(define (make-graph entity-lst)
  (unweighted-graph/undirected
   (map (lambda (entity) (list (get-entity-start entity) (get-entity-end entity)))
        entity-lst)))

(define (add-node-entity entity ht)
  (let ([start-n (get-entity-start entity)]
        [end-n  (get-entity-end entity)])
  (hash-update (hash-update ht
                            start-n
                            (lambda (linked-entities) (cons entity linked-entities))
                            '())
               end-n
               (lambda (linked-entities) (cons entity linked-entities))
               '())))

(define (add-edge-entity entity ht)
  (let ([start-n (get-entity-start entity)]
        [end-n  (get-entity-end entity)])
  (hash-update ht
               (list (get-entity-start entity) (get-entity-end entity))
               (lambda (linked-entities) (cons entity linked-entities))
               '())))

(define (make-node-hs entity-lst)
  (foldl add-node-entity (hash) entity-lst))

(define (make-edge-hs entity-lst)
  (foldl add-edge-entity (hash) entity-lst))
                               
(define (group-entities entity-lst)
  (let* ([edges (entities->edges entity-lst)]
         [graph (unweighted-graph/undirected edges)]
         [node-lsts (cc graph)]
         [node-hash (make-node-hs entity-lst)]
         [entity-grps (for/list ([node-lst node-lsts])
                                (sort-from-nodes node-lst node-hash))]
         ;[sub-graphs (map make-graph entity-grps)]
         )
    entity-grps))

(define (edges->entities edge-lst entity-lst)
  (for/list ([edge edge-lst])
    ((lambda (entity) 
       (if (and (equal? (first edge) (get-entity-start entity))
                (equal? (second edge) (get-entity-end entity)))
           entity
           (reverse-entity entity)))
     (findf 
      (lambda (entity)
        (or (and (equal? (first edge) (get-entity-start entity))
                 (equal? (second edge) (get-entity-end entity)))
            (and (equal? (second edge) (get-entity-start entity))
                 (equal? (first edge) (get-entity-end entity)))))
      entity-lst))))

(define (get-edges lst)
  (map (lambda (e) (list (first e) (second e))) lst))

;e edge, ht hashtable
(define (add-edge e ht)
  (let ([start-n (first e)]
        [end-n  (second e)])
  (hash-update ht
               start-n
               (lambda (linked-ns) (cons end-n linked-ns))
               '())))

(define (n-in-edge? edge n)
  (or (equal? (first edge) n)
      (equal? (second edge) n)))

;; once we reach a point where we have still have some edges in e-lst (yet to traverse),
;; but no edges that don't have both in visited lst, then we 
(define (n-find-edges n original-n e-lst visited-ns)
  (let* ([edges (filter (lambda (e) (n-in-edge? e n)) e-lst)]
         ;return edges that don't have both nodes in visited lst
         [can-visit (filter (lambda (e) (if (= 1 (length visited-ns))
                                            (not (n-in-edge? e original-n))
                                            (not (and (member (first e) visited-ns) (member (second e) visited-ns))))) edges)])
    can-visit))

(define (reorder-entities start-n entity-lst)
  (let* ([1st (list (reorder-entity start-n (first entity-lst)))]
         [res (foldl (lambda (next prev)
                       (cons (reorder-entity (get-entity-end (first prev)) next) prev)) 1st (rest entity-lst))])
    (reverse res)))

(define (get-other-n edge n)
  (cond [(equal? (first edge) n) (second edge)]
        [(equal? (second edge) n) (first edge)]
        [else (error "node not found in edge" n edge)]))

;; returns a lst of paths
(define (reorder-tree-path start-n entity-lst)
  (let* ([edge-lst (entities->edges (remove-duplicates entity-lst entities-identical?))]
         [edge-hash (make-edge-hs entity-lst)]
         [node-lst (entities->nodes entity-lst)]
         [ht (make-hash)]) ;;hash will be original-n (key) previous-original-n (value)
    (map (lambda (e-lst)
           ;;assumes in every e-lst the first entity is ordered correctly.
           (let* ([entity-lst (reverse (sort-from-edges (rest e-lst) edge-hash))]
                  [uniques (get-unique-nodes (entities->nodes entity-lst))]
                  [open? (open-path? (entities->nodes entity-lst))]
                  [start-n (first e-lst)])
             (reorder-entities start-n entity-lst)))
           ;(reorder-open-path (first (first e-lst)) (reverse (sort-from-edges e-lst edge-hash))))
         (let loop
           ([acc '()]
            [e-lst edge-lst]
            [curr-n start-n]
            [curr-path '()]
            [visited-ns '()] ;if a current path encounters a visited node, choose another node if one is available
            [original-n start-n])
           (if (empty? e-lst) 
               (if (empty? curr-path)
                   (begin (reverse acc))
                   (begin (reverse (cons (cons original-n (reverse curr-path)) acc))))
               (let ([edges (n-find-edges curr-n original-n e-lst visited-ns)])
                 (cond
                   ;; dead end
                   [(empty? edges) (let ([prev-original-n (hash-ref ht original-n #f)])
                                     (if prev-original-n
                                         (begin (writeln (format "dead, moving from original node : ~a " original-n))
                                                (hash-remove! ht original-n)
                                                      ;; only cons curr-path into acc if it's not empty
                                                (loop (if (empty? curr-path)
                                                          acc
                                                          (cons (cons original-n (reverse curr-path)) acc))
                                                      e-lst
                                                      original-n
                                                      '()
                                                      ;try removing visited-ns immediately on a dead end,
                                                      ;not sure what implications this may have
                                                      '()
                                                      ;if no more prev original than start from original-n
                                                      ;but it means curr-n and original-n are the same. see what implications this has later.
                                                      (if prev-original-n prev-original-n original-n)))
                                         (begin (writeln (format "original-n: ~a" original-n))
                                                (for ([key (hash-keys ht)]
                                                      [val (hash-values ht)])
                                                     (writeln (cons key val))))))]
                   ;; traversing one way
                   [(= 1 (length edges)) (let ([next-n (get-other-n (first edges) curr-n)])
                                           (println (format "1way, moving from current node : ~a to next node : ~a" curr-n next-n))
                                           (loop acc
                                                 (remove (first edges) e-lst)
                                                 next-n
                                                 (cons (first edges) curr-path)
                                                 (cons next-n visited-ns)
                                                 original-n))]
                   ;; at a junction/fork
                   [else (let ([next-n (get-other-n (first edges) curr-n)])
                           (hash-set! ht curr-n original-n)
                           (println (format "fork, moving from current node : ~a to next node : ~a" curr-n next-n))
                           (loop acc
                                 (remove (first edges) e-lst)
                                 next-n
                                 (cons (first edges) curr-path)
                                 (cons next-n visited-ns)
                                 curr-n))])))))))

(define (do-optimization lst start-n)
    (let loop ([start start-n]
               [entity-lst lst]
               [acc '()]
               [individuals '()])
      (if (empty? entity-lst)
          (append (reverse acc) individuals) ;cuz order of the list matters here
          (let* ([groups (group-entities entity-lst)]
                 [start-n (nn start (flatten (map get-start/end-nodes groups)))]
                 [lst-to-reorder (get-belonging-list start-n groups)]
                 [base-elements (remove-duplicates (get-base-elements lst-to-reorder) entities-identical?)]
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
                                    (loop (if (equal? start-n (get-entity-start x)) (get-entity-end x) (get-entity-start x))
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