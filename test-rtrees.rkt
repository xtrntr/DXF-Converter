#lang racket

(struct rect (x1 y1 x2 y2))

(struct tree-node (mbr n1 n2 entity))

(define (rect-intersect? r1 r2)
  (and (< (rect-x1 r1) (rect-x2 r2))
       (> (rect-x2 r1) (rect-x1 r2))
       (< (rect-y1 r1) (rect-y2 r2))
       (> (rect-y2 r1) (rect-y1 r2))))

(define (in-node? query-rect n)
  (rect-intersect? query-rect (tree-node-brect n)))

(define (insert entity master-n)
  (let ([entity-mbr (get-mbr entity)])
    ;loop either adds the entity to the current level node or dives one node deeper.
    (let loop ([current-n master-n])
      (let* ([n1 (tree-node-n1 current-n)]
             [n2 (tree-node-n2 current-n)]
             [n-mbr (tree-node-mbr current-n)])
        (cond ([not n1] (node n-mbr 