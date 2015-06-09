#lang typed/racket

(require "structs.rkt"
         "utils.rkt")

(require/typed (only-in racket/base hash-empty?)
               [hash-empty? (-> Header-Value Boolean)])

(provide structs-to-strings
         get-relevant-list
         select-highlighted
         highlight-path
         unselect-all
         delete-selected
         to-display
         coalesce
         reorder
         get-linked-nodes
         separate-unlinked-elements)

(: structs-to-strings (-> (Listof Entities) (Listof String)))
(define (structs-to-strings struct-lst)
  (map (lambda ([x : Entities]) (capitalize (symbol->string (cast (object-name x) Symbol)))) struct-lst))

(: capitalize (-> String String))
(define (capitalize str)
  (let* ((dissected (string->list str))
         (first-letter (char-upcase (car dissected)))
         (capitalized (list->string (append (list first-letter) (cdr dissected)))))
    capitalized))

(: get-relevant-list (-> (Listof Entities) (Listof Entities))) 
(define (get-relevant-list lst)
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

(: highlight-path (-> (Listof Entities) Void))
(define (highlight-path lst)
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


(define-type Header-Value (HashTable point Entities))

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

(: separate-unlinked-elements (-> (Listof Entities) (Listof Entities)))
(define (separate-unlinked-elements struct-list)
  
  (define node-list (get-linked-nodes struct-list))
  
  (: is-linked? (-> Entities (U True False (Listof point))))
  (define (is-linked? a-struct)
    (or (member (first (get-node a-struct)) node-list)
        (member (second (get-node a-struct)) node-list)))
  
  (let separate : (Listof Entities)
    ([unlinked : (Listof Entities) '()]
     [linked : (Listof Entities) '()]
     [lst : (Listof Entities) struct-list])
    (if (empty? lst) 
        linked
        (let ((current (first lst)))
          (cond ((and (not (point? current)) (is-linked? current))
                 (separate unlinked (cons current linked) (cdr lst)))
                (else 
                 (separate (cons current unlinked) linked (cdr lst))))))))

(: end-ht (-> (Listof Entities) Header-Value))
(define (end-ht a-list)
  (coalesce a-list get-end))

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

(: reorder (-> (Listof Entities) (Listof (Listof Entities))))
(define (reorder a-list)
  ;in one DXF pattern i have a single path joined in a anti-clockwise fashion.. except for one arc that moves from start to end in a clockwise fashion.
  (let ((ht-start (start-ht a-list))
        (ht-end   (end-ht a-list)))
    (let sort : (Listof (Listof Entities))
      ([ht-s : Header-Value ht-start]
       [ht-e : Header-Value ht-end]
       [current : Entities (car a-list)]
       [a-path : (U Null (Listof Entities)) '()]
       [result : (Listof (Listof Entities)) '()])
      (cond ((hash-empty? ht-s) result)
            ((hash-has-key? ht-s (get-end current))
             (sort (hash-remove ht-s (get-start current)) (hash-remove ht-e (get-end current)) (hash-ref ht-s (get-end current)) (cons current a-path) result))
            ((hash-has-key? ht-e (get-start current))
             (sort (hash-remove ht-s (get-start current)) (hash-remove ht-e (get-end current)) (hash-ref ht-e (get-end current)) (cons (reverse-path current) a-path) result))
            (else
             (sort ht-s ht-e (car (hash-values ht-s)) '() (cons a-path result)))))))
    