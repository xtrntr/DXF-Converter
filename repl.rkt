#lang racket

(require "structs.rkt"
         (for-syntax racket/base))

#|
(define (match-struct a-struct)
  (match a-struct
    [(struct* line  ([x1 x1]))               x1]
    [(struct* arc   ([x1 x1]))               x1]
    [(struct* point ([x x]))                 x]
    [(struct* path  ([entities entities]))   entities]))
|#

(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (names ...) (args ...) body0 body ...)
     (let* ([names/sym (map syntax-e (syntax->list #'(names ...)))]
            [names/str (map symbol->string names/sym)]
            [name/str (string-join names/str "-")]
            [name/sym (string->symbol name/str)])
       (with-syntax ([name (datum->syntax stx name/sym)])
         #`(define (name args ...)
             body0 body ...)))]))

(define-syntax (matcher stx)
  (syntax-case stx ()
    [(_ (list a-struct))
     #'(let ([result (match a-struct
                       [(struct* line  ([x1 x1]
                                        [y1 y1]))               (list x1 y1)]
                       [(struct* arc   ([x1 x1]))               x1]
                       [(struct* point ([x x]))                 x]
                       [(struct* path  ([entities entities]))   entities])])
         (printf "line's start position is ~a, ~a\n" (car result) (cadr result)))]))

(matcher (list (make-line "0" 1 2 3 4)))