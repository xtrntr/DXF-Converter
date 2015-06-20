#lang typed/racket


(struct node ([x : Real]
              [y : Real]) #:transparent)
(define-type connection (List node node))

;square
(define closed-path-right (list (list (node 0 0) (node 0 1))
                                (list (node 0 1) (node 1 1))
                                (list (node 1 1) (node 1 0))
                                (list (node 1 0) (node 0 0))))

(define closed-path-wrong (list (list (node 0 0) (node 0 1))
                                (list (node 1 1) (node 0 1))
                                (list (node 1 1) (node 1 0))
                                (list (node 1 0) (node 0 0))))

;reverse c
(define open-path-right (list (list (node 0 0) (node 0 1))
                              (list (node 0 1) (node 1 1))
                              (list (node 1 1) (node 1 0))))

(define open-path-wrong (list (list (node 0 0) (node 0 1))
                              (list (node 0 1) (node 1 1))
                              (list (node 1 0) (node 1 1))))

(: get-next-node (-> node (Listof connection) node))
(define (get-next-node n conn-lst)
  (;2) then reverse it if the connection holds the node as the end part.
   (lambda ([x : connection]) 
     (if (equal? (second x) n)
         (first x)
         (second x)))
   ;1) find the connection that contains 1st node
   (cast (findf (lambda ([x : connection]) (or (equal? (first x) n) 
                                               (equal? (second x) n))) conn-lst) connection)))

(equal? (get-next-node (node 1 0) closed-path-right) (node 0 0))