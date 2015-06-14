#lang racket

(struct connection (start end)
  #:transparent)

(define lst (list (connection 1 2)
                  (connection 2 3)
                  (connection 3 4)
                  (connection 5 6)
                  (connection 6 7)
                  (connection 8 9)))

;sort a list of nodes into a list of lists containing connected nodes
(define (sort-into-groups lst)
  (define (is-connected? node lst)
    (cond ((empty? lst) #f)
          ((or (= (connection-start node) (connection-start (car lst)))
               (= (connection-start (car lst)) (connection-end node))
               (= (connection-end (car lst)) (connection-start node))
               (= (connection-end node) (connection-end (car lst)))) #t)
          (else (is-connected? node (cdr lst)))))
  (define (find-node from to)
    (cond ((empty? from) '())
          (else
           (let ([node1 (car from)]
                 [node2 (car to)])
             (cond ((or (= (connection-start node1) (connection-start node2))
                        (= (connection-start node2) (connection-end node1))
                        (= (connection-end node2) (connection-start node1))
                        (= (connection-end node1) (connection-end node2)))
                    node2)
                   (else (find-node (cdr from) to)))))))
  (let loop
    ([current-path '()]
     [nodes lst]
     [result '()]) 
    (cond ((empty? nodes) 
           (if (empty? current-path)
               result
               (cons current-path result)))
          (else
           (let ([current-node (car nodes)])
             (cond ((empty? current-path)
                    (loop (cons current-node current-path) (cdr nodes) result))
                   ((ormap (lambda (x) (is-connected? x nodes)) current-path) ;check if any connection in current-path to nodes
                    (loop (cons (find-node current-path nodes) current-path) (cdr nodes) result))
                   (else
                    (loop '() nodes (cons current-path result)))))))))

(sort-into-groups lst)
                   