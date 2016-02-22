#lang typed/racket

#|
First line is the parameters
Second line is "Dispense with right/left dispenser-type"
Third line determines whether to draw a small arc between two lines
Fourth line is *** Right-Needle ***
Fifth line onwards is the pattern file

Data types for parameters:
Float for dispense time, travel delay and retract delay
Integer for coordinates

Pattern format for:
Dot: x, y, z, xdeviation, number of repeat, ydeviation, retract height, retract speed, dispense time, retract delay, move height
LineStart: x, y, z, travel delay
LineEnd: x, y, z, travel speed, dispense on/off, retract delay, retract height, retract speed, move height
ArcStart: x, y, z, travel delay
ArcPoint: x, y, z
ArcEnd: x, y, z, travel speed, dispense on/off, retract delay, retract height, retract speed, move height
|#

(require "structs.rkt"
         "utils.rkt")

(provide generate-gr-pattern)

;1000 = 1mm
;1000 = 1second
(define x-deviation 0)
(define num-repeat 0)
(define y-deviation 0) 
(define ret-height -2000) 
(define ret-speed 0)
(define disp-dur 0)
(define ret-delay 0)
(define clear-height -5000)
(define trav-delay 0)
(define trav-speed 5)
(define disp-onoff 1)

(: x-off Real)
(define x-off 0)
(: y-off Real)
(define y-off 0)

;135.224 represented as 135224
(: ils-x (-> Real Real))
(define (ils-x x)
  ;25.4 inches debug
  (round-to-int (* 1000 (round-3 (+ x-off x)))))

(: ils-y (-> Real Real))
(define (ils-y y)
  ;25.4 inches debug
  (round-to-int (* 1000 (round-3 (+ y-off y)))))

(: insert-newline (-> Void))
(define (insert-newline)
  ;CR LF for windows
  ; ~n or \n for windows
  (printf "\r\n"))

(: spacing (-> Void))
(define (spacing)
  (printf "   "))

;7 spaces
(: arc-point-spacing (-> Void))
(define (arc-point-spacing)
  (printf "       "))

(: generate-gr-pattern (-> Entities Output-Port Void))
(define (generate-gr-pattern struct-list port)
  (let ([smallest-y (smallest (get-y-vals struct-list))]
        [smallest-x (smallest (get-x-vals struct-list))])
    (set! x-off (+ 1 (* -1 smallest-x)))
    (set! y-off (+ 1 (* -1 smallest-y))))
  (current-output-port port)
  (printf "Without acr.")
  (insert-newline)
  (let loop : Void
    ([lst : Entities struct-list])
    (cond ((empty? lst) (void))
          (else (struct-to-format (car lst))
                (loop (cdr lst)))))
  (close-output-port port))

(: struct-to-format (-> Entity Void))
(define (struct-to-format a-struct)
  (match a-struct
    [(dot highlighted selected visible layer p)                                        (ils-dot (node-x p) (node-y p))]
    [(line highlighted selected visible layer p1 p2 mbr)                               (ils-line (node-x p1) (node-y p1) (node-x p2) (node-y p2))]
    [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr) (ils-arc (node-x p1) (node-y p1) (node-x p2) (node-y p2) (node-x p3) (node-y p3))]
    [(path highlighted selected visible layer entities)                                (ils-path entities)]))

(: ils-dot (-> Real Real Void))
(define (ils-dot x y) ;what is between y-deviation and ret-height?
  (printf (format "dot(x=~a, y=~a, z=~a; ~a, ~a; ~a, ~a; z=~a; sp=~a; ~a; ~a; z=~a)" (ils-x x) (ils-y y) 0 x-deviation num-repeat y-deviation 0 ret-height ret-speed (exact->inexact disp-dur) (exact->inexact ret-delay) clear-height))
  (insert-newline))

(: ils-line (-> Real Real Real Real Void))
(define (ils-line x1 y1 x2 y2)
  (ils-line-start x1 y1)
  (ils-line-end x2 y2))

(: ils-arc (-> Real Real Real Real Real Real Void))
(define (ils-arc x1 y1 x2 y2 x3 y3)
  (ils-arc-start x1 y1)
  (arc-point-spacing)
  (ils-arc-point x2 y2)
  (ils-arc-end x3 y3))

(: ils-line-start (-> Real Real Void))
(define (ils-line-start x y)
  (printf (format "lineStart(x=~a, y=~a, z=~a; ~a)" (ils-x x) (ils-y y) 0 (exact->inexact trav-delay)))
  (insert-newline))

(: ils-line-end (-> Real Real Void))
(define (ils-line-end x y) ;travel speed, dispense on/off, retract delay, retract height, retract speed, move height
  (printf (format "lineEnd(x=~a, y=~a, z=~a; sp=~a; ~a; ~a; z=~a; sp=~a; z=~a)" (ils-x x) (ils-y y) 0 trav-speed disp-onoff (exact->inexact ret-delay) ret-height ret-speed clear-height))
  (insert-newline))

(: ils-line-point (-> Real Real Void))
(define (ils-line-point x y)
  (printf (format "linksLinePoint(x=~a, y=~a, z=~a; sp=~a; ~a)" (ils-x x) (ils-y y) 0 trav-speed disp-onoff))
  (insert-newline))

(: ils-link-arc-restart (-> Real Real Void))
(define (ils-link-arc-restart x y) 
  (printf (format "linksArcRestart(x=~a, y=~a, z=~a; sp=~a; ~a)" (ils-x x) (ils-y y) 0 trav-speed disp-onoff))
  (insert-newline))

(: ils-arc-start (-> Real Real Void))
(define (ils-arc-start x y) 
  (printf (format "arcStart(x=~a, y=~a, z=~a; ~a)" (ils-x x) (ils-y y) 0 (exact->inexact trav-delay)))
  (insert-newline))

(: ils-arc-point (-> Real Real Void))
(define (ils-arc-point x y) 
  (printf (format "arcPoint(x=~a, y=~a, z=~a)" (ils-x x) (ils-y y) 0))
  (insert-newline))

(: ils-link-arc-start (-> Real Real Void))
(define (ils-link-arc-start x y) 
  (printf (format "linksArcStart(x=~a, y=~a, z=~a; sp=~a; ~a)" (ils-x x) (ils-y y) 0 trav-speed disp-onoff))
  (insert-newline))

(: ils-arc-end (-> Real Real Void))
(define (ils-arc-end x y) ;travel speed, dispense on/off, retract delay, retract height, retract speed, move height
  (printf (format "arcEnd(x=~a, y=~a, z=~a; sp=~a; ~a; ~a; z=~a; sp=~a; z=~a)" (ils-x x) (ils-y y) 0 trav-speed disp-onoff (exact->inexact ret-delay) ret-height ret-speed clear-height))
  (insert-newline))

(: ils-link-arc-end (-> Real Real Void))
(define (ils-link-arc-end x y) ;travel speed, dispense on/off, retract delay, retract height, retract speed, move height
  ;(printf (format "linksArcEnd(x=~a, y=~a, z=~a; sp=~a; ~a; ~a; z=~a; sp=~a; z=~a)" (ils-x x) (ils-y y) 0 trav-speed disp-onoff (exact->inexact ret-delay) ret-height ret-speed clear-height))
  (printf (format "linksArcEnd(x=~a, y=~a, z=~a; sp=~a; ~a)" (ils-x x) (ils-y y) 0 trav-speed disp-onoff))
  (insert-newline))
  
;identify first
(: ils-path (-> Path-Entities Void))
(define (ils-path path-list)
  (: line-arc? (-> Entity Entity Boolean))
  (define (line-arc? x y)
    (and (equal? (object-name x) 'line)
         (equal? (object-name y) 'arc)))
  (: arc-line? (-> Entity Entity Boolean))
  (define (arc-line? x y)
    (and (equal? (object-name x) 'arc)
         (equal? (object-name y) 'line)))
  (: line-line? (-> Entity Entity Boolean))
  (define (line-line? x y)
    (and (equal? (object-name x) 'line)
         (equal? (object-name y) 'line)))
  (: arc-arc? (-> Entity Entity Boolean))
  (define (arc-arc? x y)
    (and (equal? (object-name x) 'arc)
         (equal? (object-name y) 'arc)))
  (: iterate (-> Path-Entities Path-Entity Void))
  (define (iterate lst prev)
    (define current (car lst))
    (spacing)
    (cond ((= (length lst) 1)
           (match current
             [(line highlighted selected visible layer p1 p2 mbr)
              (if (arc? prev)
                  (ils-link-arc-end (node-x p1) (node-y p1))
                  (ils-line-point (node-x p1) (node-y p1)))
              (ils-line-end (node-x p2) (node-y p2))]
             [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
              (if (arc? prev)
                  (ils-link-arc-restart (node-x p1) (node-y p1))
                  (ils-link-arc-start (node-x p1) (node-y p1)))
              (arc-point-spacing)
              (ils-arc-point (node-x p2) (node-y p2))
              (ils-arc-end (node-x p3) (node-y p3))]))
          ((line-line? prev current)
           (assert prev line?) (assert current line?)
           (ils-line-point (node-x (line-p1 current)) (node-y (line-p1 current)))
           (iterate (cdr lst) current))
          ((arc-line? prev current)
           (assert prev arc?) (assert current line?)
           (ils-link-arc-end (node-x (line-p1 current)) (node-y (line-p1 current)))
           (iterate (cdr lst) current))
          ((arc-arc? prev current)
           (assert prev arc?) (assert current arc?)
           (ils-link-arc-restart (node-x (arc-p1 current)) (node-y (arc-p1 current)))
           (arc-point-spacing)
           (ils-arc-point (node-x (arc-p2 current)) (node-y (arc-p2 current)))
           (iterate (cdr lst) current))
          ((line-arc? prev current)
           (assert prev line?) (assert current arc?)
           (ils-link-arc-start (node-x (arc-p1 current)) (node-y (arc-p1 current)))
           (arc-point-spacing)
           (ils-arc-point (node-x (arc-p2 current)) (node-y (arc-p2 current)))
           (iterate (cdr lst) current))))
  (unless (= (length path-list) 1)
    (let ([start : Path-Entity (car path-list)])
      (match start
        [(line highlighted selected visible layer p1 p2 mbr)
         (ils-line-start (node-x p1) (node-y p1))]
        [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
         (ils-arc-start (node-x p1) (node-y p1))
         (arc-point-spacing) 
         (ils-arc-point (node-x p2) (node-y p2))])
      (iterate (cdr path-list) start))))