#lang racket

(require graph)
;; test algorithms here

(define (split str [ptn #rx"[;]+"])
  (regexp-split ptn (string-trim str)))

(define (read-chunks input-port)
  (let loop
    ([accu '()])
    (define nxt (read-line input-port 'any))
    (if (eof-object? nxt)
        accu
        (loop (cons nxt accu)))))

(define (reader input-port)
  (define lines (read-chunks input-port))
  (foldl (lambda (f r)
           (define fst (filter (compose not (curry string=? "")) (split f)))
           (append fst r))
         '() lines))

(struct node
  (x y)
  #:transparent)

(define (decimalstr2num str)
  (let* ([parts (string-split str ".")]
         [p1 (string->number (first parts))]
         [p2 (exact->inexact (/ (string->number (string-trim (second parts))) 1000))])
    (+ p1 p2)))
  
(define (str2node str)
  (let* ([n1 (string-split str " , ")]
         [n1-x (decimalstr2num (first n1))]
         [n1-y (decimalstr2num (second n1))])
    (node n1-x n1-y)))
  
(define (make-edge str)
  (let* ([str-nodes (string-split str " - ")]
         [str-n1 (first str-nodes)]
         [str-n2 (second str-nodes)])
    (list (str2node str-n1) (str2node str-n2))))
    
(define (make-node str)
  (node (decimalstr2num str)))

(define path-str "/Users/kangrenchia/Documents/Github/DXF-Converter/edges.txt")
(define edge-lst
  (for/list ([edge-or-n (reader (open-input-file path-str))])
           (when (string-contains? edge-or-n "-")
             (make-edge edge-or-n)
             ;(make-node edge-or-n)
             )))

(define (add-edge e ht)
  (let ([start-n (first e)]
        [end-n  (second e)])
  (hash-update (hash-update ht
                            start-n
                            (lambda (linked-edges) (cons e linked-edges))
                            '())
               end-n
               (lambda (linked-edges) (cons e linked-edges))
               '())))

(define (sort-from-nodes node-lsts node-ht)
  (for/list ([node-lst node-lsts])
            ((lambda (node-lst)
               (remove-duplicates (foldl append '() (map (lambda (node) (hash-ref node-ht node)) node-lst))))
             node-lst)))

(define (make-node-hs edge-lst)
  (foldl add-edge (hash) edge-lst))
  
(define (make-connected-graphs e-lst)
  (let* ([graph (unweighted-graph/undirected e-lst)]
         [node-hash (make-node-hs e-lst)]
         [node-lsts (cc graph)]
         [edge-grps (sort-from-nodes node-lsts node-hash)])
    edge-grps))

(define graph-lst (make-connected-graphs edge-lst))

(define (get-unique-nodes lst)
  (let* ([elements (set->list (list->set lst))]
         [duplicates (for/fold ([x lst]) ([y elements])
                                       (remove y x))]
         [real-dupl (remove-duplicates duplicates)]
         [uniques (remove* real-dupl elements)])
    uniques))

(define (n-in-edge? edge n)
  (or (equal? (first edge) n)
      (equal? (second edge) n)))

(for/sum ([i (map length graph-lst)]) i)
(length edge-lst)