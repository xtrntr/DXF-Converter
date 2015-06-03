#lang typed/racket

(require "structs.rkt"
         "utils.rkt")

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

(: structs-to-strings (-> (Listof (U line arc path point circle)) (Listof String)))
(define (structs-to-strings struct-lst)
  (map (lambda (x) (capitalize (symbol->string (object-name x)))) struct-lst))

(: capitalize (-> String String))
(define (capitalize str)
  (let* ((dissected (string->list str))
         (first-letter (char-upcase (car dissected)))
         (capitalized (list->string (append (list first-letter) (cdr dissected)))))
    capitalized))

(: get-relevant-list (-> (Listof (U line arc path point circle)) (Listof (U line arc path point circle)))) 
(define (get-relevant-list lst)
  (filter (lambda (i) (and (entity-visible i) (entity-selected i))) lst))
    
(: select-highlighted (-> (Listof (U line arc path point circle)) (Listof (U line arc path point circle))))
(define (select-highlighted lst)
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
    '()))
              
(: select-highlighted (-> (Listof (U line arc path point circle)) (Listof (U line arc path point circle))))
(define (highlight-path lst)
  (: any-entity-highlighted? (-> (Listof (U line arc path point circle)) Boolean))
  (define (any-entity-highlighted? lst)
    (cond ((empty? lst) #f)
          ((entity-highlighted (car lst)) #t)
          (else (any-entity-highlighted? (cdr lst)))))
  (map (lambda (x) (when (any-entity-highlighted? (path-entities x))
                     (foldl set-entity-highlighted! #t (path-entities x))))
       (filter path? lst)))

(: unselect-all (-> (Listof (U line arc path point circle)) (Listof (U line arc path point circle))))
(define (unselect-all lst)
  (unless (empty? lst)
    (let ((current (first lst)))
      (cond ((path? current)
             (set-entity-selected! current #f)
             (unselect-all (path-entities current))
             (unselect-all (cdr lst)))
            (else (set-entity-selected! current #f)
                  (unselect-all (cdr lst))))))
  '())

(: delete-selected (-> (Listof (U line arc path point circle)) (Listof (U line arc path point circle))))
(define (delete-selected lst)
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
    '()))

(: to-display (-> Real String))
(define (to-display x)
  (let ((x-string 0))
    (if (list? x)
        (set! x-string (number->string (round-off (first x))))
        (set! x-string (number->string (round-off x))))
    (format "~a" x-string)))

(: get-start-x (-> (U line arc path point circle) String))
(define (get-start-x a-struct)
  (round-off (match a-struct
    [(struct* line  ([x1 x1]))               x1]
    [(struct* arc   ([x1 x1]))               x1]
    [(struct* point ([x x]))                 x]
    [(struct* path  ([entities entities]))  (get-start-x (first entities))])))

(: get-start-y (-> (U line arc path point circle) String))
(define (get-start-y a-struct)
  (round-off (match a-struct
    [(struct* line  ([y1 y1]))               y1]
    [(struct* arc   ([y1 y1]))               y1]
    [(struct* point ([y y]))                 y]
    [(struct* path  ([entities entities]))  (get-start-y (first entities))])))

(: get-end-x (-> (U line arc path point circle) String))
(define (get-end-x a-struct)
  (round-off (match a-struct
    [(struct* line  ([x2 x2]))               x2]
    [(struct* arc   ([x3 x3]))               x3]
    [(struct* point ([x x]))                 x]
    [(struct* path  ([entities entities]))  (get-end-x (first entities))])))

(: get-end-y (-> (U line arc path point circle) String))
(define (get-end-y a-struct)
  (round-off (match a-struct
    [(struct* line  ([y2 y2]))              y2]
    [(struct* arc   ([y3 y3]))              y3]
    [(struct* point ([y y]))                y]
    [(struct* path  ([entities entities]))  (get-end-y (first entities))])))

(: get-start (-> (U line arc path point circle) (List String String)))
(define (get-start a-struct)
    (list (get-start-x a-struct) (get-start-y a-struct)))

(: get-end (-> (U line arc path point circle) (List String String)))
(define (get-end a-struct)
    (list (get-end-x a-struct) (get-end-y a-struct)))

(: get-node (-> (U line arc path point circle) (List (List String String) (List String String))))
(define (get-node a-struct)
  (list (get-end a-struct) (get-start a-struct)))

(: get-node (-> (Listof (U line arc path point circle)) (List (List String String) (List String String))))
(define (get-nodes a-list)
  (cond ((empty? a-list) '())
        (else (let ((current (car a-list)))
                (append (get-node current)
                        (get-nodes (cdr a-list)))))))

;from a list of duplicates and singles return a list of duplicates
(: remove-singles (All [T] (-> (Listof T) (Listof T))))
(define (remove-singles lst)
  (let iter ([lst lst]
             [acc1 '()]
             [acc2 '()])
    (if (empty? lst)
        (remove-duplicates acc2)
        (let ((current (car lst)))
          (cond ((member current acc1)
                 (iter (cdr lst) acc1 (cons current acc2)))
                (else
                 (iter (cdr lst) (cons current acc1) acc2)))))))

(: get-linked-nodes (All [T] (-> (Listof T) (Listof T))))
(define (get-linked-nodes a-list)
  (remove-singles (get-nodes a-list)))

(: separate-unlinked-elements (-> (Listof (U line arc path point circle)) (Listof (U line arc path point circle))))
(define (separate-unlinked-elements struct-list)
  (define node-list (get-linked-nodes struct-list))
  (: is-linked? (-> (U line arc path point circle) Boolean))
  (define (is-linked? a-struct)
    (or (member (first (get-node a-struct)) node-list)
        (member (second (get-node a-struct)) node-list)))
  (let separate : (Listof (U line arc path point circle))
    ([unlinked : (Listof (U line arc path point circle)) '()]
     [linked : (Listof (U line arc path point circle)) '()]
     [lst : (Listof (U line arc path point circle)) struct-list])
    (if (empty? lst) 
        unlinked
        (let ((current (first lst)))
          (cond ((is-linked? current)
                 (separate unlinked (cons current linked) (cdr lst)))
                (else (separate (cons current unlinked) linked (cdr lst))))))))
     
;struct-list -> hash-list
(define (coalesce a-list)
  ;values of the hash table are the structs and the keys its starting points
  (define (lst->ht lst ht)
    (if (empty? lst)
        ht
        (let ((current (first lst)))
          (if (point? current)
              (lst->ht (cdr lst) ht)
              (lst->ht (cdr lst) (apply hash-set ht (list (get-start current) current)))))))
  (let ((a-hash (hash)))
    (lst->ht a-list a-hash)))

(define (reorder a-list)
  (define (is-end-connected? ht a-struct)
   (or (hash-has-key? ht (get-end a-struct))))
  (define (is-start-connected? ht a-struct)
   (or (hash-has-key? ht (get-start a-struct))))
  ;acc1 stores the list of unlinked elements
  ;acc2 stores the list of linked elements according to has-connection?
  (define (iter lst ht acc1 acc2 link-test)
    (cond ((empty? lst) (list acc1 acc2))
          (else
           (let* ((current (first lst))
                  (current-key (get-start current)))
             (cond ((link-test ht current)
                    (iter (cdr lst) ht acc1 (cons current acc2) link-test))
                   (else 
                    (iter (cdr lst) ht (cons current acc1) acc2 link-test)))))))
  (define (sort ht current-struct acc)
    (cond ((hash-empty? ht) acc)
          (else (sort (hash-remove ht current-struct) (hash-ref ht (get-end current-struct)) (cons current-struct acc)))))
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
