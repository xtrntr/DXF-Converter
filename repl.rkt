#lang typed/racket

(define entity-types '("3DFACE"  "3DSOLID"  "ACAD_PROXY_ENTITY" "ARC" "ARCALIGNEDTEXT"  "ATTDEF"  "ATTRIB"  "BODY"  "CIRCLE" "DIMENSION" "ELLIPSE"  "HATCH" "IMAGE"  "INSERT"  "LEADER"  "LINE" "LWPOLYLINE" "MLINE"  "MTEXT"  "OLEFRAME"  "OLE2FRAME"  "POINT" "POLYLINE" "RAY"  "REGION"  "RTEXT"  "SEQEND"  "SHAPE"  "SOLID" "SPLINE" "TEXT"  "TOLERANCE"  "TRACE"  "VERTEX"  "VIEWPORT" "WIPEOUT" "XLINE"))
(define lst '("ENTITIES" "0" "LINE" "5" "BA1C" "330" "1F" "100" "AcDbEntity" "8" "0" "370" "0" "100" "AcDbLine" "10" "253.2926653556182" "20" "1478.621431186484" "30" "0.0" "11" "253.2876653556182" "21" "1478.621431186484" "31" "0.0" "0" "LINE" "5" "BA1D" "330" "1F" "100" "AcDbEntity" "8" "0" "370" "0" "100" "AcDbLine" "10" "253.2876653556182" "20" "1478.621431186484" "30" "0.0" "11" "253.2876653556182" "21" "1476.655431186484" "31" "0.0" "0" "LINE" "5" "BA1E" "330" "1F" "100" "AcDbEntity" "8" "0" "370" "0" "100" "AcDbLine" "10" "253.2926653556182" "20" "1476.655431186484" "30" "0.0" "11" "253.2876653556182" "21" "1476.655431186484" "31" "0.0" "0" "LINE" "5" "BA1F" "330" "1F" "100" "AcDbEntity" "8" "0" "370" "0" "100" "AcDbLine" "10" "253.2926653556182" "20" "1476.655431186484" "30" "0.0" "11"))

(: break (All [T] (-> (T -> Any) (Listof T) 
                      (Values (Listof T) (Listof T)))))
(define (break pred lst)
  (splitf-at lst (negate pred)))

(break (lambda ([x : String]) (member x entity-types)) lst)

(: cast (All (T) ((Any -> Boolean : T) Any -> T)))
(define (cast p? x)
   (if (p? x)
       x
       (error "Cast failed")))

(cast exact-integer? (truncate 3))