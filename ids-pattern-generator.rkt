#lang racket

(require "structs.rkt"
         "utils.rkt")

;; every line has 66 commas after z3.
(define after-trail (make-string 66 #\,))

(provide generate-ids-pattern)

(define x-off 0)
(define y-off 0)

(define (ids-x x)
  ;25.4 inches debug
  (round-3 (+ x-off x)))

(define (ids-y y)
  ;25.4 inches debug
  (round-3 (+ y-off y)))

(define (insert-newline)
  ;CR LF for windows
  ; ~n or \n for windows
  (printf "\r\n"))

(define (generate-ids-pattern struct-list port x-offset y-offset)
  (set! x-off x-offset)
  (set! y-off y-offset)
  (current-output-port port)
  (printf "[Page=Main]")
  (insert-newline)
  (for ([i struct-list])
       (struct-to-format i))
  (close-output-port port))

;; list -> string
(define (struct-to-format a-struct)
  (match a-struct
    [(dot highlighted selected visible layer p)                                        (ids-dot-format (node-x p) (node-y p))]
    [(line highlighted selected visible layer p1 p2 mbr)                               (ids-line-format (node-x p1) (node-y p1) (node-x p2) (node-y p2))]
    [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr) (ids-arc-format (node-x p1) (node-y p1) (node-x p2) (node-y p2) (node-x p3) (node-y p3) (arc-is-circle? a-struct))]
    [(path highlighted selected visible layer entities)                                (ids-link entities)]))

(define (ids-arc-format x1 y1 x2 y2 x3 y3 circle?)
  (if circle?
      (printf (format "Circle,Left,On,~a,~a,0,~a,~a,0,~a,~a,0,~a"
          (ids-x x1) (ids-y y1) (ids-x x2) (ids-y y2) (ids-x x3) (ids-y y3) after-trail))
      (printf (format "Arc,Left,On,~a,~a,0,~a,~a,0,~a,~a,0,~a"
          (ids-x x1) (ids-y y1) (ids-x x2) (ids-y y2) (ids-x x3) (ids-y y3) after-trail)))
  (insert-newline))

(define (ids-dot-format x y)
  (printf (format "Dot,Left,On,~a,~a,0,,,,,,,~a"
          (ids-x x) (ids-y y) after-trail))
  (insert-newline))

(define (ids-line-format x1 y1 x2 y2)
  (printf (format "Line,Left,On,~a,~a,0,~a,~a,0,,,,~a"
          (ids-x x1) (ids-y y1) (ids-x x2) (ids-y y2) after-trail))
  (insert-newline))

(define (ids-link entity-lst)
  (printf (format "LinkStart,,,,,,,,,,,,~a" after-trail))
  (insert-newline)
  (for ([i entity-lst])
       (struct-to-format i))
  (printf (format "LinkEnd,,,~a,~a,,,,,,,,~a"
          (ids-x (node-x (get-entity-end (last entity-lst)))) (ids-y (node-y (get-entity-end (last entity-lst)))) after-trail))
  (insert-newline))