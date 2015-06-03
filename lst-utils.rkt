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
         get-start-x
         get-start-y
         get-end-x
         get-end-y
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

(: get-start-x (-> Entities Real))
(define (get-start-x a-struct)
  (round-off (match a-struct
    [(struct* line  ([x1 x1]))               x1]
    [(struct* arc   ([x1 x1]))               x1]
    [(struct* point ([x x]))                 x]
    [(struct* path  ([entities entities]))  (get-start-x (first entities))])))

(: get-start-y (-> Entities Real))
(define (get-start-y a-struct)
  (round-off (match a-struct
    [(struct* line  ([y1 y1]))               y1]
    [(struct* arc   ([y1 y1]))               y1]
    [(struct* point ([y y]))                 y]
    [(struct* path  ([entities entities]))  (get-start-y (first entities))])))

(: get-end-x (-> Entities Real))
(define (get-end-x a-struct)
  (round-off (match a-struct
    [(struct* line  ([x2 x2]))               x2]
    [(struct* arc   ([x3 x3]))               x3]
    [(struct* point ([x x]))                 x]
    [(struct* path  ([entities entities]))  (get-end-x (first entities))])))

(: get-end-y (-> Entities Real))
(define (get-end-y a-struct)
  (round-off (match a-struct
    [(struct* line  ([y2 y2]))              y2]
    [(struct* arc   ([y3 y3]))              y3]
    [(struct* point ([y y]))                y]
    [(struct* path  ([entities entities]))  (get-end-y (first entities))])))

(: get-start (-> Entities (List Real Real)))
(define (get-start a-struct)
    (list (get-start-x a-struct) (get-start-y a-struct)))

(: get-end (-> Entities (List Real Real)))
(define (get-end a-struct)
    (list (get-end-x a-struct) (get-end-y a-struct)))

(: get-node (-> Entities (List (List Real Real) (List Real Real))))
(define (get-node a-struct)
  (list (get-end a-struct) (get-start a-struct)))

(: get-nodes (-> (Listof Entities) (Listof (List (List Real Real) (List Real Real)))))
(define (get-nodes a-list)
  (cond ((empty? a-list) '())
        (else (let ((current (car a-list)))
                (cons (get-node current)
                        (get-nodes (cdr a-list)))))))

;from a list of duplicates and singles return a list of duplicates
(: remove-singles (-> (Listof (List (List Real Real) (List Real Real))) (Listof (List (List Real Real) (List Real Real)))))
(define (remove-singles lst)
  (let iter : (Listof (List (List Real Real) (List Real Real)))
    ([lst : (Listof (List (List Real Real) (List Real Real))) lst]
     [acc1 : (Listof (List (List Real Real) (List Real Real))) '()]
     [acc2 : (Listof (List (List Real Real) (List Real Real))) '()])
    (if (empty? lst)
        (remove-duplicates acc2)
        (let ((current (car lst)))
          (cond ((member current acc1)
                 (iter (cdr lst) acc1 (cons current acc2)))
                (else
                 (iter (cdr lst) (cons current acc1) acc2)))))))

(: get-linked-nodes (All [T] (-> (Listof Entities) (Listof (List (List Real Real) (List Real Real))))))
(define (get-linked-nodes a-list)
  (remove-singles (get-nodes a-list)))

(: separate-unlinked-elements (-> (Listof Entities) (Listof Entities)))
(define (separate-unlinked-elements struct-list)
  (define node-list (get-linked-nodes struct-list))
  (: is-linked? (-> Entities (U False (Listof (List (List Real Real) (List Real Real))))))
  (define (is-linked? a-struct)
    (or (member (first (get-node a-struct)) node-list)
        (member (second (get-node a-struct)) node-list)))
  (let separate : (Listof Entities)
    ([unlinked : (Listof Entities) '()]
     [linked : (Listof Entities) '()]
     [lst : (Listof Entities) struct-list])
    (if (empty? lst) 
        unlinked
        (let ((current (first lst)))
          (cond ((is-linked? current)
                 (separate unlinked (cons current linked) (cdr lst)))
                (else (separate (cons current unlinked) linked (cdr lst))))))))

(define-type Header-Value (HashTable (List Real Real) Entities))
     
;struct-list -> hash-list
(: coalesce (-> (Listof Entities) Header-Value))
(define (coalesce a-list)
  ;values of the hash table are the structs and the keys its starting points
  (let loop : Header-Value
    ([lst : (Listof Entities) a-list]
     [ht : Header-Value (hash)])
    (if (empty? lst)
        ht
        (let ((current (first lst)))
          (if (point? current)
              (loop (cdr lst) ht)
              (loop (cdr lst) (hash-set ht (get-start current) current)))))))

(: reorder (-> (Listof Entities) (Listof Entities)))
(define (reorder a-list)
  (: is-end-connected? (-> Header-Value Entities Boolean))
  (define (is-end-connected? ht a-struct)
   (or (hash-has-key? ht (get-end a-struct))))
  (: is-start-connected? (-> Header-Value Entities Boolean))
  (define (is-start-connected? ht a-struct)
   (or (hash-has-key? ht (get-start a-struct))))
  ;acc1 stores the list of unlinked elements
  ;acc2 stores the list of linked elements according to has-connection?
  (: iter (-> (Listof Entities) Header-Value (Listof Entities) (Listof Entities) (-> Header-Value Entities Boolean) (List (Listof Entities) (Listof Entities))))
  (define (iter lst ht acc1 acc2 link-test)
    (cond ((empty? lst) (list acc1 acc2))
          (else
           (let* ((current (first lst))
                  (current-key (get-start current)))
             (cond ((link-test ht current)
                    (iter (cdr lst) ht acc1 (cons current acc2) link-test))
                   (else 
                    (iter (cdr lst) ht (cons current acc1) acc2 link-test)))))))
  (: sort (-> Header-Value Entities (Listof Entities) (Listof Entities)))
  (define (sort ht current-struct lst)
    (cond ((hash-empty? ht) lst)
          (else (sort (hash-remove ht current-struct) (hash-ref ht (get-end current-struct)) (cons current-struct lst)))))
  (let* ((ht-all (coalesce a-list))
         (result (iter a-list ht-all '() '() is-end-connected?)) ;sort a-list into linked and unlinked
         (unlinked (first result))
         (linked (second result))
         (ht-linked (coalesce linked))
         (result2 (iter unlinked ht-linked '() '() is-start-connected?)) ;further sort unlinked in new-linked and new-unlinked
         (new-unlinked (first result2)) ;final unlinked
         (new-linked (append linked (second result2)))
         (new-ht-linked (coalesce new-linked))
         (ordered (sort new-ht-linked (first new-linked) '())))
    ordered))
