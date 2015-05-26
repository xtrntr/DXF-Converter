#lang racket

(require "structs.rkt"
         "utils.rkt")

(provide struct-list->string-list
         filter-struct-list
         get-relevant-list
         select-highlighted
         highlight-path
         unselect-all
         delete-selected
         get-starting-points)

(define (struct-list->string-list struct-lst)
  (map (lambda (x) (capitalize (symbol->string (object-name x)))) struct-lst)
  #|(map (lambda (x) (if (path? x)
                       (struct-list->string-list (path-entities x))
                       (symbol->string (object-name x)))) struct-lst)|#)

(define (capitalize str)
  (let* ((dissected (string->list str))
         (first-letter (char-upcase (car dissected)))
         (capitalized (list->string (append (list first-letter) (cdr dissected)))))
    capitalized))

(define (filter-struct-list lst cond)
  (for/list ([i lst] 
             #:when (cond i))
    i))

(define (get-relevant-list lst)
  (filter-struct-list lst (lambda (i) (and (entity-visible i) (entity-selected i)))))
    
(define (select-highlighted lst)
  (define (select x)
    (set-entity-selected! x #t)
    (set-entity-highlighted! x #f))
  (unless (empty? lst)
    (let ((current (first lst)))
      (cond ((path? current)
             (select-highlighted (path-entities current))
             (unless (empty? (filter-struct-list (path-entities current) entity-selected))
               (select current))
             (select-highlighted (cdr lst)))
            (else (when (entity-highlighted current) 
                    (select current))
                  (select-highlighted (cdr lst)))))
    '()))
              
(define (highlight-path lst)
  (define (any-entity-highlighted? lst)
    (cond ((empty? lst) #f)
          ((entity-highlighted (car lst)) #t)
          (else (any-entity-highlighted? (cdr lst)))))
  (map (lambda (x) (when (any-entity-highlighted? (path-entities x))
                     (foldl set-entity-highlighted! #t (path-entities x))))
       (filter-struct-list lst path?)))

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

(define (delete-selected lst)
  (define (delete x)
    (set-entity-selected! x #f)
    (set-entity-visible! x #f))
  (unless (empty? lst)
    (let ((current (first lst)))
      (cond ((and (path? current) (entity-selected current))
             (delete current)
             (delete-selected (path-entities current)))
            ((entity-selected current)
             (delete current)
             (delete-selected (cdr lst)))
            (else (delete-selected (cdr lst)))))
    '()))

(define (get-starting-points lst)
  (define (to-display x y)
    (let* ((x-string (number->string (real->decimal x 3)))
           (y-string (number->string (real->decimal y 3)))
           (x-length (length (string->list x-string)))
           (num-spaces (- 7 x-length)))
    (format "~a, ~a" x-string y-string)))
  (flatten (cond ((empty? lst) '())
        (else (cons 
               (match (first lst)
                [(line _ _ _ _ x1 y1 _ _)              (to-display x1 y1)]
                [(arc _ _ _ _ _ _ _ _ _ x1 y1 _ _ _ _) (to-display x1 y1)]
                [(point _ _ _ _ x y)                   (to-display x y)]
                [(path _ _ _ _ path-list)              (get-starting-points (list (first path-list)))])
               (get-starting-points (cdr lst)))))))