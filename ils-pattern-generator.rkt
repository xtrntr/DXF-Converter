#lang racket

(require "structs.rkt"
         "utils.rkt")

(provide generate-ils-pattern)

#|
First line is the parameters
Second line is "Dispense with right/left dispenser-type"
Third line determines whether to draw a small arc between two lines
Fourth line is *** Right-Needle ***
Fifth line onwards is the pattern file

Pattern format for:
Dot: x, y, z, xdeviation, number of repeat, ydeviation, retract height, retract speed, dispense time, retract delay, move height
LineStart: x, y, z, travel delay
LineEnd: x, y, z, travel speed, dispense on/off, retract delay, retract height, retract speed, move height
ArcStart: x, y, z, travel delay
ArcPoint: x, y, z
ArcEnd: x, y, z, travel speed, dispense on/off, retract delay, retract height, retract speed, move height
|#

(define x-deviation 0)
(define num-repeat 0)
(define y-deviation 0) 
(define ret-height 0)
(define ret-speed 0)
(define disp-dur 0)
(define ret-delay 0)
(define clear-height 0)
(define trav-delay 0)
(define trav-speed 0)
(define disp-onoff 0)

;135.224 represented as 135224
(define (ils-num x)
  (* 1000 (real->decimal x 3)))

(define (insert-newline)
  (printf "~n"))

(define (spacing)
  (printf "   "))

(define (generate-ils-pattern struct-list port)
  (current-output-port port)
  (printf "Without acr.")
  (insert-newline)
  (for/list ([i struct-list])
    (struct-to-format i))
  (close-output-port port))

(define (struct-to-format a-struct)
  (match a-struct
    [(line layer highlighted selected visible x1 y1 x2 y2)                            (ils-line x1 y1 x2 y2)]
    [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3)  (ils-arc x1 y1 x2 y2 x3 y3)]
    [(point layer highlighted selected visible x y)                                   (ils-dot x y)]
    [(path layer highlighted selected visible path-list)                              (ils-path path-list)]))

(define (ils-dot x y) ;what is between y-deviation and ret-height?
  (printf (format "dot(x=~a, y=~a, z=~a; ~a, ~a; ~a, ~a; z=~a; sp=~a; ~a; ~a; z=~a)" (ils-num x) (ils-num y) 0 x-deviation num-repeat y-deviation 0 ret-height ret-speed disp-dur ret-delay clear-height))
  (insert-newline))

(define (ils-line x1 y1 x2 y2)
  (ils-line-start x1 y1)
  (ils-line-end x2 y2))

(define (ils-arc x1 y1 x2 y2 x3 y3)
  (ils-arc-start x1 y1)
  (ils-arc-point x2 y2)
  (ils-arc-end x3 y3))

(define (ils-line-start x y)
  (printf (format "lineStart(x=~a, y=~a, z=~a; ~a)" (ils-num x) (ils-num y) 0 trav-delay))
  (insert-newline))

(define (ils-line-end x y) ;travel speed, dispense on/off, retract delay, retract height, retract speed, move height
  (printf (format "lineEnd(x=~a, y=~a, z=~a; sp=~a; ~a; ~a; ~a; sp=~a; ~a)" (ils-num x) (ils-num y) 0 trav-speed disp-onoff ret-delay ret-height ret-speed clear-height))
  (insert-newline))

(define (ils-line-point x y)
  (printf (format "linksLinePoint(x=~a, y=~a, z=~a; sp=~a; ~a)" (ils-num x) (ils-num y) 0 trav-speed disp-onoff))
  (insert-newline))

(define (ils-link-arc-restart x y) 
  (printf (format "linksArcRestart(x=~a, y=~a, z=~a; ~a)" (ils-num x) (ils-num y) 0 trav-delay))
  (insert-newline))

(define (ils-arc-start x y) 
  (printf (format "arcStart(x=~a, y=~a, z=~a; ~a)" (ils-num x) (ils-num y) 0 trav-delay))
  (insert-newline))

(define (ils-arc-point x y) 
  (printf (format "arcPoint(x=~a, y=~a, z=~a)" (ils-num x) (ils-num y) 0))
  (insert-newline))

(define (ils-link-arc-start x y) 
  (printf (format "linksArcStart(x=~a, y=~a, z=~a; ~a)" (ils-num x) (ils-num y) 0 trav-delay))
  (insert-newline))

(define (ils-arc-end x y) ;travel speed, dispense on/off, retract delay, retract height, retract speed, move height
  (printf (format "arcEnd(x=~a, y=~a, z=~a; sp=~a; ~a; ~a; z=~a; sp=~a; z=~a)" (ils-num x) (ils-num y) 0 trav-speed disp-onoff ret-delay ret-height ret-speed clear-height))
  (insert-newline))

(define (ils-link-arc-end x y) ;travel speed, dispense on/off, retract delay, retract height, retract speed, move height
  (printf (format "linksArcEnd(x=~a, y=~a, z=~a; sp=~a; ~a; ~a; z=~a; sp=~a; z=~a)" (ils-num x) (ils-num y) 0 trav-speed disp-onoff ret-delay ret-height ret-speed clear-height))
  (insert-newline))

;identify first
(define (ils-path path-list)
  (define (line-arc? x y)
    (and (equal? (object-name x) 'line)
         (equal? (object-name y) 'arc)))
  (define (arc-line? x y)
    (and (equal? (object-name x) 'arc)
         (equal? (object-name y) 'line)))
  (define (line-line? x y)
    (and (equal? (object-name x) 'line)
         (equal? (object-name y) 'line)))
  (define (arc-arc? x y)
    (and (equal? (object-name x) 'arc)
         (equal? (object-name y) 'arc)))
  (define (iterate lst prev)
    (define current (car lst))
    (spacing)
    (cond ((= (length lst) 1)
           (match current
             [(line layer highlighted selected visible x1 y1 x2 y2)                            (if (arc? prev)
                                                                                                   (ils-link-arc-end x1 y1)
                                                                                                   (ils-line-point x1 y1))
                                                                                               (ils-line-end x2 y2)]
             [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3)  (if (arc? prev)
                                                                                                   (ils-link-arc-restart x1 y1)
                                                                                                   (ils-link-arc-start x1 y1))
                                                                                               (spacing)(spacing)
                                                                                               (ils-arc-point x2 y2)
                                                                                               (ils-arc-end x3 y3)]))
          ((line-line? prev current)
           (ils-line-point (line-x1 current) (line-y1 current))
           (iterate (cdr lst) current))
          ((arc-line? prev current)
           (ils-link-arc-end (line-x1 current) (line-y1 current))
           (iterate (cdr lst) current))
          ((arc-arc? prev current)
           (ils-link-arc-restart (arc-x1 current) (arc-y1 current))
           (spacing)(spacing)
           (ils-arc-point (arc-x2 current) (arc-y2 current))
           (iterate (cdr lst) current))
          ((line-arc? prev current)
           (ils-link-arc-start (arc-x1 current) (arc-y1 current))
           (spacing)(spacing)
           (ils-arc-point (arc-x2 current) (arc-y2 current))
           (iterate (cdr lst) current))))
  (unless (= (length path-list) 1)
    (let ((first (car path-list)))
      (match first
        [(line layer highlighted selected visible x1 y1 x2 y2)                            (ils-line-start x1 y1)]
        [(arc layer highlighted selected visible x y radius start end x1 y1 x2 y2 x3 y3)  (ils-arc-start x1 y1)
                                                                                          (ils-arc-point x2 y2)])
      (iterate (cdr path-list) first))))

