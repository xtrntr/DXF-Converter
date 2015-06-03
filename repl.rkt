#lang typed/racket

(: remove-singles (-> (Listof (List (List Real Real) (List Real Real))) (Listof (List (List Real Real) (List Real Real)))))
(define (remove-singles lst)
  (let iter : (Listof (List (List Real Real) (List Real Real)))
    ([lst : (Listof (List (List Real Real) (List Real Real))) lst]
     [acc1 : (Listof (List (List Real Real) (List Real Real))) '()]
     [acc2 : (Listof (List (List Real Real) (List Real Real))) '()])
    (if (empty? lst)
        (remove-duplicates acc2)
        (let ((current (car lst)))
          (cond ((member current acc1)
                 (iter (cdr lst) acc1 (cons current acc2)))
                (else
                 (iter (cdr lst) (cons current acc1) acc2)))))))