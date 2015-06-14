#lang typed/racket

(require "structs.rkt"
         "utils.rkt")

(require/typed (only-in racket/base hash-empty?)
               [hash-empty? (-> Header-Value Boolean)])

(provide structs-to-strings
         get-selected
         select-highlighted
         highlight-lst
         unselect-all
         delete-selected
         get-nodes
         sort)

(: structs-to-strings (-> (Listof Entities) (Listof String)))
(define (structs-to-strings struct-lst)
  (map (lambda ([x : Entities]) (capitalize (symbol->string (cast (object-name x) Symbol)))) struct-lst))

(: get-selected (-> (Listof Entities) (Listof Entities))) 
(define (get-selected lst)
  (filter (lambda ([i : Entities]) (and (entity-visible i) (entity-selected i))) lst))

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
                              (highlight-lst (path-entities  (car x))))
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

;sort a list of nodes into a list of lists containing connected nodes
(: sort (-> (Listof Connection) (Listof (Listof Connection))))
(define (sort lst)
  (: is-connected? (-> Connection (Listof Connection) Boolean))
  (define (is-connected? node lst)
    (cond ((empty? lst) #f)
          (else
           (let ([comparison (car lst)])
             (cond ((connected? node (car lst)) #t)
                   (else (is-connected? node (cdr lst))))))))
  (: find-node (-> (Listof Connection) (Listof Connection) Connection))
  (define (find-node from to)
    (cond ((empty? from) (error "Expected a valid connection, given " from to))
          (else
           (let ([node1 (car from)]
                 [node2 (car to)])
             (cond ((connected? node1 node2)
                    node2)
                   (else (find-node (cdr from) to)))))))
  (let loop : (Listof (Listof Connection))
    ([current-path : (Listof Connection) '()]
     [nodes : (Listof Connection) lst]
     [result : (Listof (Listof Connection)) '()]) 
    (cond ((empty? nodes) 
           (if (empty? current-path)
               result
               (cons current-path result)))
          (else
           (let ([current-node (car nodes)])
             (cond ((empty? current-path)
                    (loop (cons current-node current-path) (cdr nodes) result))
                   ((ormap (lambda ([x : Connection]) (is-connected? x nodes)) current-path) ;check if any connection in current-path to nodes
                    (loop (cons (find-node current-path nodes) current-path) (cdr nodes) result))
                   (else
                    (loop '() nodes (cons current-path result)))))))))
