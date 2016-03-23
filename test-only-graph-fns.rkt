#lang racket

(require graph)

(struct n (x y) #:transparent)
;connections
(struct e (n1 n2) #:transparent)

(define a1 (n 5 5))
(define b1 (n 4 6))
(define c1 (n 6 6))
(define d1 (n 5 7))

(define e-lst (list
                  ;square
                  (e a1 b1)
                  (e a1 b1)
                  (e b1 d1)
                  (e d1 c1)
                  (e d1 c1)
                  (e c1 a1)
                  (e c1 a1)
                  ))

(define (e-contain? e n)
  (or (equal? (e-n1 n)) (equal? (e-n2 n))))

(define (e-connected? e1 e2)
  (or (equal? (e-n1 e1) (e-n1 e2))
      (equal? (e-n1 e1) (e-n2 e2))
      (equal? (e-n2 e1) (e-n1 e2))
      (equal? (e-n2 e1) (e-n2 e2))))

(define (display-result)
  (let* ([edges e-lst] 
         [graph (unweighted-graph/undirected edges)]
         [groups (cc graph)]
         [node-hash (make-node-hs e-lst)])
    (reorder-edges (e-n1 (first e-lst)) edges)))

(define (get-edges lst)
  (map (lambda (e) (list (e-n1 e) (e-n2 e))) lst))

;e edge, ht hashtable
(define (add-edge e ht)
  (let ([start-n (e-n1 e)]
        [end-n  (e-n2 e)])
  (hash-update ht
               start-n
               (lambda (linked-ns) (cons end-n linked-ns))
               '())))

(define (make-node-hs e-lst)
  (foldl add-edge (hash) e-lst))

(define (n-in-edge? edge n)
  (or (equal? (e-n1 edge) n)
      (equal? (e-n2 edge) n)))

(define (n-find-edges n original-n e-lst visited-ns)
  (let* ([edges (filter (lambda (e) (n-in-edge? e n)) e-lst)]
         ;return edges that don't have both nodes in visited lst
         [can-visit (filter (lambda (e) (if (= 1 (length visited-ns))
                                            (not (n-in-edge? e original-n))
                                            (not (and (member (e-n1 e) visited-ns) (member (e-n2 e) visited-ns))))) edges)])
    can-visit))

(define (get-other-n edge n)
  (cond [(equal? (e-n1 edge) n) (e-n2 edge)]
        [(equal? (e-n2 edge) n) (e-n1 edge)]
        [else (error "node not found in edge" n edge)]))

(define (reorder-edges start-n edge-lst)
  (let ([ht (make-hash)])
    (let loop
        ([acc '()]
         [e-lst edge-lst]
         [curr-n start-n]
         [curr-path '()]
         [visited-ns '()] ;if a current path encounters a visited node, choose another node if one is available
         [original-n start-n]
         )
      (if (empty? e-lst) 
          (if (empty? curr-path)
              (reverse acc)
              (cons (reverse curr-path) acc))
          (let ([edges (n-find-edges curr-n original-n e-lst visited-ns)])
            (cond 
              ;; dead end
              [(empty? edges) (display "dead end, starting from original node : ")
                              (writeln original-n)
                              (loop (cons (reverse curr-path) acc)
                                    e-lst
                                    original-n
                                    '()
                                    '()
                                    original-n)]
              ;; traversing one way
              [(= 1 (length edges)) (let ([next-n (get-other-n (first edges) curr-n)])
                                      (display "moving from current node : ")
                                      (display curr-n)
                                      (display " to next node : ")
                                      (writeln next-n)
                                      (loop acc
                                            (remove (first edges) e-lst)
                                            next-n
                                            (cons (first edges) curr-path)
                                            (cons next-n visited-ns)
                                            original-n))]
              ;; at a junction/fork
              [else (let ([next-n (get-other-n (first edges) curr-n)])
                      (display "possible paths : ")
                      (for ([edge edges])
                           (display edge)
                           (display " , "))
                      (newline)
                      (display "moving from current node : ")
                      (display curr-n)
                      (display " to next node : ")
                      (writeln next-n)
                      (loop acc
                            (remove (first edges) e-lst)
                            next-n
                            (cons (first edges) curr-path)
                            (cons next-n visited-ns)
                            original-n))]))))))

(for ([result (reverse (display-result))])
     (writeln result))