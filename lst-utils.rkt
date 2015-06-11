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
         to-display
         get-linked-nodes
         get-linked-elements
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

(: to-display (-> Real String))
(define (to-display x)
  (format "~a" (number->string (round-off x))))

;from a list of duplicates and singles return a list of duplicates
(: remove-singles (-> (Listof point) (Listof point)))
(define (remove-singles lst)
  (let iter : (Listof point)
    ([lst : (Listof point) lst]
     [acc1 : (Listof point) '()]
     [acc2 : (Listof point) '()])
    (if (empty? lst)
        (remove-duplicates acc2)
        (let ((current (car lst)))
          (cond ((member current acc1)
                 (iter (cdr lst) acc1 (cons current acc2)))
                (else
                 (iter (cdr lst) (cons current acc1) acc2)))))))

(: get-linked-nodes (All [T] (-> (Listof Entities) (Listof point))))
(define (get-linked-nodes a-list)
  (remove-singles (get-nodes a-list)))

;returns a hashtable where keys are ENDPOINTS
(: end-ht (-> (Listof Entities) Header-Value))
(define (end-ht a-list)
  (coalesce a-list get-end))

;returns a hashtable where keys are STARTPOINTS
(: start-ht (-> (Listof Entities) Header-Value))
(define (start-ht a-list)
  (coalesce a-list get-start))

;struct-list -> hash-list
(: coalesce (-> (Listof Entities) (-> Entities point) Header-Value))
(define (coalesce a-list key)
  ;values of the hash table are the structs and the keys its starting points
  (let loop : Header-Value
    ([lst : (Listof Entities) a-list]
     [ht : Header-Value (hash)])
    (if (empty? lst)
        ht
        (let ((current (first lst)))
          (if (point? current)
              (loop (cdr lst) ht)
              (loop (cdr lst) (hash-set ht (key current) current)))))))

;takes a list of elements and sorts them into lists - in which they are inter-connected to other elements in the list, forming a path
(: sort (-> (Listof Entities) (Listof (Listof Entities))))
(define (sort lst)
  (let* ([separated (get-linked-elements lst)]
         (linked (first separated))
         (unlinked (second separated)))
    (let sort-paths : (Listof (Listof Entities))
      ([ht-s : Header-Value (start-ht linked)]
       [ht-e : Header-Value (end-ht linked)]
       [current : Entities (car linked)]
       [a-path : (U Null (Listof Entities)) '()]
       [result : (Listof (Listof Entities)) '()])
      (cond ((hash-empty? ht-s) result)
            (else
             (let ([current-end (get-end current)]
                   [current-start (get-start current)])
               (cond ((hash-has-key? ht-s current-end) ;normal case. end of current has a match in start of next
                      (sort-paths (hash-remove ht-s current-start) (hash-remove ht-e current-end) (hash-ref ht-s current-end) (cons current a-path) result))
                     ((hash-has-key? ht-e current-start) ;normal case. start of current has a match in end of next
                      (sort-paths (hash-remove ht-s current-start) (hash-remove ht-e current-end) (hash-ref ht-e current-end) (cons current a-path) result))
                     ((hash-has-key? ht-e current-end) ;weird case. end of current has a match in end of next. 1-2-1
                      (sort-paths (hash-remove ht-s current-start) (hash-remove ht-e current-end) (hash-ref ht-e current-end) (cons current a-path) result))
                     ((hash-has-key? ht-s current-start) ;weird case. start of current has a match in start of next. 2-1-2
                      (sort-paths (hash-remove ht-s current-start) (hash-remove ht-e current-end) (hash-ref ht-e current-end) (cons current a-path) result))
                     (else 
                      (sort-paths ht-s ht-e (car (hash-values ht-s)) '() (cons a-path result))))))))))
    
(: get-linked-elements (-> (Listof Entities) (List (Listof Entities) (Listof Entities))))
(define (get-linked-elements struct-list)
  
  (define node-list (get-linked-nodes struct-list))
  
  (: is-linked? (-> Entities (U True False (Listof point))))
  (define (is-linked? a-struct)
    (or (member (first (get-node a-struct)) node-list)
        (member (second (get-node a-struct)) node-list)))
  
  (let separate : (List (Listof Entities) (Listof Entities))
    ([unlinked : (Listof Entities) '()]
     [linked : (Listof Entities) '()]
     [lst : (Listof Entities) struct-list])
    (if (empty? lst) 
        (list linked unlinked)
        (let ((current (first lst)))
          (cond ((and (not (point? current)) (is-linked? current))
                 (separate unlinked (cons current linked) (cdr lst)))
                (else 
                 (separate (cons current unlinked) linked (cdr lst))))))))