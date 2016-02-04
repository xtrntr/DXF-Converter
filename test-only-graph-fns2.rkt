#lang typed/racket

(require/typed graph 
               [unweighted-graph/undirected (-> (Listof Connection) Graph)]
               [cc (-> Graph (Listof (Listof vertices)))]
               [#:struct vertices
                         ([x : Any]
                          [y : Any])]
               [#:struct Graph
                         
               [)

(define-type Connection (List vertices vertices))

(struct vertices 
  ([x : Real]
   [y : Real])
  #:mutable #:transparent)

(define graph (unweighted-graph/undirected (list (list (vertices 1 2) (vertices 3 4)))))
(cc graph)