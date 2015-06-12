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
         remove-singles
         get-nodes)

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

;member-nested only works for a specific edge case for now
(: member-nested (-> point (Listof Connection) Boolean))
(define (member-nested key lst)
  (let loop : Boolean
    ([acc : (Listof point) '()]
     [lst : (Listof Connection) lst])
    (cond ((empty? lst) (if (not (member key acc)) #f #t))
          (else (loop (append (car lst) acc) (cdr lst))))))

;from a list of duplicates and singles return a list of duplicates
(: remove-singles (-> (Listof Connection) (Listof Connection)))
(define (remove-singles lst)
  (let loop : (Listof Connection)
    ([lst : (Listof Connection) lst]
     [acc1 : (Listof Connection) '()]
     [acc2 : (Listof Connection) '()])
    (if (empty? lst)
        (remove-duplicates acc2)
        (let ((current (car lst)))
          (cond ((or (member-nested (car current) acc1) (member-nested (cadr current) acc1))
                 (loop (cdr lst) acc1 (cons current acc2)))
                (else
                 (loop (cdr lst) (cons current acc1) acc2)))))))

#|
(: get-paths (All [T] (-> (Listof Entities) (Listof (Listof point)))))
(define (get-paths a-list)
  (define linked-nodes (remove-singles (get-nodes a-list)))
  (let loop : (Listof (Listof point))
    ([acc : (Listof (Listof point)) '()]
     [
|#