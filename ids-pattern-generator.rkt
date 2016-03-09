#lang racket

(define filename "gooby.txt")

;; every line has 66 commas after z3.
(define after-trail ",,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,")


(require "structs.rkt"
         "geometric-functions.rkt")

(provide generate-ids-pattern)

(define (generate-ids-pattern struct-list port)
  (current-output-port port)
  (printf "[Page=Main]")
  (printf "~n")
  (for/list ([i struct-list])
    (printf (struct-to-format i))
    (printf "~n"))
  (close-output-port port))
         
;; list -> string
(define (struct-to-format a-struct)
  (match a-struct
    [(line layer highlighted selected visible x1 y1 x2 y2)                            (ids-line-format x1 y1 x2 y2)]
    [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3)  (ids-arc-format x1 y1 x2 y2 x3 y3)]
    [(point layer highlighted selected visible x y)                                   (ids-dot-format x y)]))

(define (ids-arc-format x1 y1 x2 y2 x3 y3)
  (string-append "Arc,Left,On," (real->decimal-string x1 3) "," (real->decimal-string y1 3) "," "0" "," (real->decimal-string x2 3) "," (real->decimal-string y2 3) "," "0" "," (real->decimal-string x3 3) "," (real->decimal-string y3 3) "," "0" "," after-trail))

(define (ids-dot-format x y)
  (string-append "Dot,Left,On," (real->decimal-string x 3) "," (real->decimal-string y 3) "," "0" "," "," "," "," "," "," "," after-trail))

(define (ids-line-format x1 y1 x2 y2)
  (string-append "Line,Left,On," (real->decimal-string x1 3) "," (real->decimal-string y1 3) "," "0" "," (real->decimal-string x2 3) "," (real->decimal-string y2 3) "," "0" "," "," "," "," after-trail))

(define newline (string #\newline))