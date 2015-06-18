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
         find-connection
         get-connections
         get-nodes
         sort
         get-start/end-nodes
         make-ht
         form-open-path
         form-closed-path)

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

(: get-connection (-> Entities Connection))
(define (get-connection a-struct)
  (list (get-end a-struct) (get-start a-struct)))

(: get-connections (-> (Listof Entities) (Listof Connection)))
(define (get-connections lst)
  (let loop : (Listof Connection)
    ([acc : (Listof Connection) '()]
     [lst : (Listof Entities) lst])
    (cond ((empty? lst) acc)
          (else (loop (cons (get-connection (car lst)) acc) (cdr lst))))))

(: get-nodes (-> (Listof Entities) (Listof node)))
(define (get-nodes entity-lst)
  (let loop : (Listof node)
    ([acc : (Listof node) '()]
     [lst : (Listof Entities) entity-lst])
    (cond ((empty? lst) acc)
          (else (loop (append (get-connection (car lst)) acc) (cdr lst))))))

;for now: 6/16/15, this is only applicable to non path entities
;sort a list of nodes into a list of lists containing connected nodes
;sort returns a list of connection, that is ordered logically i.e. (list (conn (0 0) (1 1)) (conn (1 1) (2 2)) (conn (2 2) (3 3)))
(: sort (-> (Listof Connection) (Listof (Listof Connection))))
(define (sort lst)
  (: is-connected? (-> Connection (Listof Connection) Boolean))
  (define (is-connected? node lst)
    (cond ((empty? lst) #f)
          (else
           (let ([comparison (car lst)])
             (cond ((connection-linked? node comparison) #t)
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
                           (cond [(connection-linked? node1 node2) node2]
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
                   [n1 (get-start entity)]
                   [n2 (get-end entity)])
              (loop (ht-add (ht-add ht n1 entity) n2 entity) (cdr lst)))))))

;given a chosen node, find the path(listof connection) within the path-lst(listof (listof connection))
(: find-connection (-> node (Listof (Listof Connection)) (Listof Connection)))
(define (find-connection n lst)
  (let main : (Listof Connection)
    [(connection-lst : (Listof (Listof Connection)) lst)]
    (if (ormap (lambda ([x : Connection])
                 (or (equal? (car x) n) (equal? (cadr x) n))) (car lst))
        (car lst)
        (main (cdr lst)))))

;given a chosen starting node and its path, return the reordered list of nodes from start and end
(: reorder-open-connection (-> node (Listof Connection) (Listof node)))
(define (reorder-open-connection n lst)
  (define conn-start (findf (lambda ([x : Connection]) (equal? (car x) n)) lst))
  (define-values (tail start) (break (lambda ([x : Connection]) (equal? x conn-start)) lst))
  (let flatten : (Listof node)
    ([lst : (Listof Connection) (append start tail)]
     [acc : (Listof node) '()])
    (cond ((empty? lst) (remove-duplicates acc))
          (else (flatten (cdr lst) (append acc (car lst)))))))

(: reorder-closed-connection (-> Connection (Listof Connection) (Listof node)))
(define (reorder-closed-connection conn-start lst)
  (define-values (tail start) (break (lambda ([x : Connection]) (equal? x conn-start)) lst))
  (let flatten : (Listof node)
    ([lst : (Listof Connection) (append start tail)]
     [acc : (Listof node) '()])
    (cond ((empty? lst) (remove-duplicates acc))
          (else (flatten (cdr lst) (append acc (car lst)))))))
  
;return the struct whose starting point matches the given node. if this is not possible, then return the reversed struct whose ending point matches the given node.
(: get-starting-struct (-> node (Listof Entities) Entities))
(define (get-starting-struct n entity-lst)
  (let loop : Entities
    ([lst : (Listof Entities) entity-lst])
    (cond ((empty? lst) (error "Expected a valid entity, given " node entity-lst))
          ((equal? (get-start (car lst)) n) (car lst))
          ((equal? (get-end (car lst)) n) (reverse-direction (car lst)))
          (else
           (loop (cdr lst))))))

;given the reordered list of nodes from start to end, return the equivalent in a list of entities
(: reorder-nodes (-> (Listof node) Node-Structs (Listof Entities)))
(define (reorder-nodes lst ht)
  (let loop : (Listof Entities)
    ([acc : (Listof Entities) '()]
     [node-lst : (Listof node) lst])
    (cond ((empty? node-lst) acc)
          (else
           (loop (append (list (get-starting-struct (car node-lst) (hash-ref ht (car node-lst)))) acc) (cdr node-lst))))))

;take a starting/ending node and the list of connection and returns the path formed 
(: form-open-path (-> node (Listof Connection) (Listof Entities) path))
(define (form-open-path n connection-lst entity-lst)
  (let* ([ht (make-ht entity-lst)] 
         [connected-entities (hash-ref ht n)] ;a node may have more than 1 entity connected to it
         [ordered-connection (reorder-open-connection n connection-lst)]
         [ordered-entities (reorder-nodes ordered-connection ht)]
         [layer (entity-layer (first connected-entities))])
    (cond ((not (= (length connected-entities) 1))
           (error "Only one entity expected, but got this instead:  " connected-entities))
          (else
           (make-path layer (cast ordered-entities (Listof (U arc line))))))))

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

(: form-closed-path (-> node (Listof Connection) (Listof Entities) Boolean path))
(define (form-closed-path n1 connection-lst entity-lst clockwise?)
  (let* ([ht (make-ht entity-lst)] 
         [connected-entities (hash-ref ht n1)] ;get the 2 entities connected to it
         [connected-nodes (get-nodes connected-entities)] ;get the 3 nodes of the 2 expected entities
         [possibilities (remove n1 (remove-duplicates connected-nodes))] ;list of 2 nodes
         [a (first possibilities)]
         [b n1] ;starting node is middle node
         [c (second possibilities)]
         [a>b>c-clockwise? (clockwise-turn? a b c)]
         [n2 (cond ((and clockwise? a>b>c-clockwise?) c)
                   ((and clockwise? (not a>b>c-clockwise?)) a)
                   ((and (not clockwise?) a>b>c-clockwise?) a)
                   ((and (not clockwise?) (not a>b>c-clockwise?)) c))]
         [layer (entity-layer (first connected-entities))]
         [conn-start (list n1 n2)]
         [ordered-connection (reorder-closed-connection conn-start connection-lst)]
         [ordered-entities (reorder-nodes ordered-connection ht)])
    (cond ((not (= (length connected-entities) 2))
           (error "Only two entities expected, but got this many entities instead:  " (length connected-entities)))
          (clockwise?
           (make-path layer (cast ordered-entities (Listof (U arc line)))))
          ((not clockwise?)
           (make-path layer (cast ordered-entities (Listof (U arc line))))))))