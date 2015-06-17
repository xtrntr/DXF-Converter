#lang typed/racket

(require "structs.rkt"
         "utils.rkt")

(require/typed (only-in racket/base hash-empty?)
               [hash-empty? (-> Node-Structs Boolean)])

(provide structs-to-strings
         get-selected
         get-visible
         select-highlighted
         highlight-lst
         unselect-all
         delete-selected
         get-nodes
         sort
         get-start/end-nodes
         make-ht)

(: structs-to-strings (-> (Listof Entities) (Listof String)))
(define (structs-to-strings struct-lst)
  (map (lambda ([x : Entities]) (capitalize (symbol->string (cast (object-name x) Symbol)))) struct-lst))

(: get-selected (-> (Listof Entities) (Listof Entities))) 
(define (get-selected lst)
  (filter (lambda ([i : Entities]) (and (entity-visible i) (entity-selected i))) lst))

(: get-visible (-> (Listof Entities) (Listof Entities))) 
(define (get-visible lst)
  (filter (lambda ([i : Entities]) (entity-visible i)) lst))

(: select-highlighted (-> (Listof Entities) Void))
(define (select-highlighted lst)
  (: select (-> Entities Void))
  (define (select x)
    (set-entity-selected! x #t)
    (set-entity-highlighted! x #f))
  (unless (empty? lst)
    (let ((current (first lst)))
      (cond ((path? current)
             (select-highlighted (path-entities current))
             (unless (empty? (filter entity-selected (path-entities current)))
               (select current))
             (select-highlighted (cdr lst)))
            (else (when (entity-highlighted current) 
                    (select current))
                  (select-highlighted (cdr lst)))))
    (void)))

(: highlight-lst (-> (Listof Entities) Void))
(define (highlight-lst lst)
  (: any-entity-highlighted? (-> (Listof Entities) Boolean))
  (define (any-entity-highlighted? lst)
    (cond ((empty? lst) #f)
          ((entity-highlighted (car lst)) #t)
          (else (any-entity-highlighted? (cdr lst)))))
  (let loop : Void
    ([x : (Listof path) (filter path? lst)])
    (cond ((empty? x) (void))
          (else (when (any-entity-highlighted? (path-entities (car x)))
                              (map (lambda ([i : Entities]) (set-entity-highlighted! i #t)) (path-entities (car x))))
                (loop (cdr x))))))

(: unselect-all (-> (Listof Entities) Void))
(define (unselect-all lst)
  (unless (empty? lst)
    (let ((current (first lst)))
      (cond ((path? current)
             (set-entity-selected! current #f)
             (unselect-all (path-entities current))
             (unselect-all (cdr lst)))
            (else (set-entity-selected! current #f)
                  (unselect-all (cdr lst))))))
  (void))

(: delete-selected (-> (Listof Entities) Void))
(define (delete-selected lst)
  (: delete (-> Entities Void))
  (define (delete x)
    (set-entity-selected! x #f)
    (set-entity-visible! x #f))
  (unless (empty? lst)
    (let ((current (first lst)))
      (cond ((and (path? current) (entity-selected current))
             (delete current)
             (delete-selected (path-entities current))
             (delete-selected (cdr lst)))
            ((entity-selected current)
             (delete current)
             (delete-selected (cdr lst)))
            (else (delete-selected (cdr lst)))))
    (void)))

(: get-node (-> Entities Connection))
(define (get-node a-struct)
  (list (get-end a-struct) (get-start a-struct)))

(: get-nodes (-> (Listof Entities) (Listof Connection)))
(define (get-nodes lst)
  (let loop : (Listof Connection)
    ([acc : (Listof Connection) '()]
     [lst : (Listof Entities) lst])
    (cond ((empty? lst) acc)
          (else (loop (cons (get-node (car lst)) acc) (cdr lst))))))

;for now: 6/16/15, this is only applicable to non path entities
;sort a list of nodes into a list of lists containing connected nodes
(: sort (-> (Listof Connection) (Listof (Listof Connection))))
(define (sort lst)
  (: is-connected? (-> Connection (Listof Connection) Boolean))
  (define (is-connected? node lst)
    (cond ((empty? lst) #f)
          (else
           (let ([comparison (car lst)])
             (cond ((connected? node comparison) #t)
                   (else (is-connected? node (cdr lst))))))))
  (: find-node (-> (Listof Connection) (Listof Connection) Connection))
  (define (find-node from to)
    (cond ((empty? to) (error "Expected a valid connection, given " from to))
          (else 
           (let loop : Connection
             ([node-lst : (Listof Connection) to])
             (cond [(empty? node-lst) (find-node (cdr from) to)]
                   [else (let ([node1 (car from)]
                               [node2 (car node-lst)])
                           (cond [(connected? node1 node2) node2]
                                 [else (loop (cdr node-lst))]))])))))
  (let loop : (Listof (Listof Connection))
    ([current-path : (Listof Connection) '()]
     [nodes : (Listof Connection) lst]
     [result : (Listof (Listof Connection)) '()]) 
    (cond ((empty? nodes) 
           (if (empty? current-path)
               result
               (if (empty? result)
                   (list current-path)
                   (cons current-path result))))
          (else
           (let ([current-node (car nodes)])
             (cond ((empty? current-path)
                    (loop (cons current-node current-path) (cdr nodes) result))
                   ((ormap (lambda ([x : Connection]) (is-connected? x nodes)) current-path) ;check if any connection in current-path to nodes
                    (define connected-node (find-node current-path nodes))
                    (loop (cons connected-node current-path) (remove connected-node nodes) result))
                   (else
                    (loop '() nodes (cons current-path result)))))))))


(: connections->nodes (-> (Listof Connection) (Listof node)))
(define (connections->nodes lst)
  (let loop : (Listof node)
    ([lst : (Listof Connection) lst]
     [acc : (Listof node) '()])
    (cond ((empty? lst) acc)
          (else
           (loop (cdr lst) (append (car lst) acc))))))

;from a list of duplicates and singles return a list of duplicates

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

(: closed-path? (-> (Listof Connection) Boolean))
(define (closed-path? lst)
  (define node-lst (connections->nodes lst))
  (empty? (get-path-ends node-lst)))

(: get-start/end-nodes (-> (Listof Connection) (Listof node)))
(define (get-start/end-nodes lst)
  (define node-lst (connections->nodes lst))
  (if (closed-path? lst)
      node-lst
      (get-path-ends node-lst)))

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
                   [n1 (get-start entity)]
                   [n2 (get-end entity)])
              (loop (ht-add (ht-add ht n1 entity) n2 entity) (cdr lst)))))))

;for lack of a more descriptive name
;take a starting/ending node and a ht and returns the path formed 
(: form-path (-> point Node-Structs Boolean (Listof Entities)))
(define (form-path node node-lst ht clockwise?)
  (let ([possibilities (hash-ref ht node)]
        [first-entity (car possibilities)]
        [layer (entity-layer first-entity)]
        [open-path? (= (length possibilities) 1)])
    (if open-path?
        (make-path layer 
                   ;this loop returns the ordered list of entities from a starting node
                   ;it needs a function that takes a node and returns an entity
                   ;needs another function that takes a node and returns the next node
                   ;an edge case is if a node forks into two paths, but we won't handle that now
                   (let loop : (Listof Entities)
                     ([acc : (Listof Entities) '()]
                      [prev : node]
                      [current : node node])
                     (cond ((equal? prev current) acc)
                           (else
                            (loop (append (list (start-node->entity current)) acc) current (next-node current))))))
        ;if it is a closed path, then we need to reorder for clockwise/anticlockwise directions
        ;a helper function that would be extremely useful is to determine the connections.
          




