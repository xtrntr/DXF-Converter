#lang racket

(require "structs.rkt")

(provide struct-list->string-list)

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