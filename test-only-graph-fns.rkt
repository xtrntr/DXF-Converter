#lang racket

(require graph)

(struct n (x y) #:transparent)
(struct line (n1 n2) #:transparent)

(define line-lst (list
                  ;square
                  (line (n 0 0) (n 0 1))
                  (line (n 0 1) (n 1 1))
                  (line (n 1 1) (n 1 0))
                  (line (n 1 0) (n 0 0))

                  ;triangle /_\
                  (line (n 2 2) (n 2 4))
                  (line (n 2 4) (n 3 3))
                  (line (n 3 3) (n 2 2))

                  ;hump /|
                  (line (n 5 5) (n 6 6))
                  (line (n 6 6) (n 5 7))

                  ;graph
                  (line (n 10 10) (n 12 10))
                  (line (n 12 10) (n 12 8))
                  (line (n 12 10) (n 12 12))
                  ))

(define (line-contain? line n)
  (or (equal? (line-n1 n)) (equal? (line-n2 n))))

(define (lines-connected? l1 l2)
  (or (equal? (line-n1 l1) (line-n1 l2))
      (equal? (line-n1 l1) (line-n2 l2))
      (equal? (line-n2 l1) (line-n1 l2))
      (equal? (line-n2 l1) (line-n2 l2))))

;return a list
(define (line-ns line)
  (list (line-n1 line) (line-n2 line)))

(define (get-connections lst)
  (map line-ns lst))

(define (make-node-hs line-lst)
  (let ([hashy (hash)]
        [add-key-pair (lambda (line hashy)
                        (let ([ns (list (line-n2 line) (line-n1 line))]
                              [val (list line)])
                          (let loop
                            ([keys ns]
                             [ht hashy])
                            (cond ([empty? keys] ht)
                                  (else
                                   (loop (rest keys)
                                         (if (hash-has-key? ht (first keys))
                                             (hash-set ht (first keys) (append (hash-ref ht (first keys)) val)) 
                                             (hash-set ht (first keys) val))))))))])
    (let loop
      ([lst line-lst]
       [ht hashy])
      (cond ([empty? lst] ht)
            (else
             (loop (rest lst)
                   (add-key-pair (first lst) ht)))))))

(define (sort-from-connections groups node-ht)
  (map (lambda (group)
         (remove-duplicates (flatten (map (lambda (element) (hash-ref node-ht element)) group)))) groups))
           
(define (display-result)
  (let* ([connections (get-connections line-lst)] ;a (Listof (List node node))
         [graph (unweighted-graph/undirected connections)]
         [groups (cc graph)]
         [node-hash (time (make-node-hs line-lst))]
         [sorted (sort-from-connections groups node-hash)])
    (display (first connections))
;    (for/list ([entry sorted])
;              (display entry)
;              (display "\n"))
  ))

(display-result)

#|
(let ([lst '(3 5 7 9)])
(for/hash ([vals lst]
           [keys (build-list (length lst) values)])
          (values keys vals)))
|#

(apply append (for/list ([edge1 (permutations (list 1 2))])
             (apply append (for/list ([edge2 (permutations (list 3 4))])
                                   (permutations (list edge1 edge2))))))

