#lang typed/racket

#|

This module is where i parse the DXF file into lines and remove whitespace,
then read the relevant ENTITIES section where supported DXF elements are ARC, LINE, DOT, CIRCLE, LWPOLYLINE.
It will loop through the data of this section and create a list of structs that contain the DXF information

|#

(require "structs.rkt"
         "utils.rkt")

(provide file->struct-list)

(define supported-types '("LWPOLYLINE" "ARC" "POINT" "CIRCLE" "LINE"))
(define entity-types '("3DFACE"  "3DSOLID"  "ACAD_PROXY_ENTITY" "ARC" "ARCALIGNEDTEXT"  "ATTDEF"  "ATTRIB"  "BODY"  "CIRCLE" "DIMENSION" "ELLIPSE"  "HATCH" "IMAGE"  "INSERT"  "LEADER"  "LINE" "LWPOLYLINE" "MLINE"  "MTEXT"  "OLEFRAME"  "OLE2FRAME"  "POINT" "POLYLINE" "RAY"  "REGION"  "RTEXT"  "SEQEND"  "SHAPE"  "SOLID" "SPLINE" "TEXT"  "TOLERANCE"  "TRACE"  "VERTEX"  "VIEWPORT" "WIPEOUT" "XLINE"))

(: split (-> String (Pairof String (Listof String))))
(define (split str [ptn #rx"[ ]+"])
  (regexp-split ptn (string-trim str)))

(: read-chunks (-> Input-Port (Listof String)))
(define (read-chunks input-port)
  (let loop : (Listof String)
    ([accu : (Listof String) '()])
    (define nxt (read-line input-port 'any))
    (if (eof-object? nxt)
        accu
        (loop (cons nxt accu)))))

(: reader (-> Input-Port (Listof String)))
(define (reader input-port)
  (define lines (read-chunks input-port))
  (foldl (lambda ([f : String]
                  [r : (Listof String)])
           (define fst (filter (compose not (curry string=? "")) (split f)))
           (append fst r))
         '() lines))

(: extract-section (-> (Listof String) String (Listof String)))
(define (extract-section lst section-name)
  (: extract-until (-> (U False (Listof String)) String (Listof String)))
  (define (extract-until lst keyword)
    ;need to handle error properly
    (cond ((not lst) '())
          ((equal? (car lst) keyword) '())
          (else (cons (car lst) (extract-until (cdr lst) keyword)))))
  (extract-until (member section-name lst) "ENDSEC"))

(: separate-entities (-> (Listof String) (Listof (Listof String))))
(define (separate-entities lst)
  (if (empty? lst)
      '()
      (let-values ([(data tail) (break (lambda ([element : String]) (member element entity-types)) (rest lst))])
        (if (member (first lst) supported-types)
            (begin ((inst cons (Listof String) (Listof (Listof String))) ((inst cons String (Listof String)) (first lst) data)
                                                                         (separate-entities tail)))
            (separate-entities tail)))))


(: extract-str (-> (HashTable String String) String String))
(define (extract-str ht header)
  (hash-ref ht header))

(: extract-val (-> (HashTable String String) String Real))
(define (extract-val ht header)
  (define val (string->number (hash-ref ht header)))
  (assert val real?))

(: separate (-> (Listof String) (Listof (Pairof String String))))
(define (separate lst)
  (let loop : (Listof (Pairof String String))
    ([acc : (Listof (Pairof String String)) '()]
     [lst : (Listof String) lst])
    (cond ((> 2 (length lst)) acc)
          (else (loop (cons (cons (car lst) (cadr lst)) acc) (cddr lst))))))

(: dxf-line (-> (Listof String) line))
(define (dxf-line lst)
  (let* ([ht : (HashTable String String) (make-hash (separate lst))]
         [layer : String (extract-str ht "8")]
         [x1 : Real (extract-val ht "10")]
         [y1 : Real (extract-val ht "20")]
         [x2 : Real (extract-val ht "11")]
         [y2 : Real (extract-val ht "21")])
    (make-line layer x1 y1 x2 y2)))

(: dxf-point (-> (Listof String) dot))
(define (dxf-point lst)
  (let* ([ht : (HashTable String String) (make-hash (separate lst))]
         [layer : String (extract-str ht "8")]
         [x : Real       (extract-val ht "10")]
         [y : Real       (extract-val ht "20")])
    (make-dot layer x y)))

(: dxf-arc (-> (Listof String) arc))
(define (dxf-arc lst)
  (let* ([ht : (HashTable String String)        (make-hash (separate lst))]
         [layer : String        (extract-str ht "8")]
         [center-x : Real       (extract-val ht "10")]
         [center-y : Real       (extract-val ht "20")]
         [radius : Real         (extract-val ht "40")]
         [start : Real          (extract-val ht "50")]
         [end : Real            (extract-val ht "51")])
    (make-arc layer center-x center-y radius start end #f)))

(: dxf-circle (-> (Listof String) arc))
(define (dxf-circle lst)
  (let* ([ht : (HashTable String String)        (make-hash (separate lst))]
         [layer : String        (extract-str ht "8")]
         [center-x : Real       (extract-val ht "10")]
         [center-y : Real       (extract-val ht "20")]
         [radius : Real         (extract-val ht "40")])
    (make-arc layer center-x center-y radius 0 0 #f)))

(: dxf-path (-> (Listof String) path))
(define (dxf-path lst)
  
  ;string-make
  (: smake-line (-> String String String String String line))
  (define (smake-line layer x1 y1 x2 y2)
    (make-line layer (string->real x1) (string->real y1) (string->real x2) (string->real y2)))
  
  (: smake-arc (-> String String String String String String arc))
  (define (smake-arc layer center-x center-y radius start end)
      (make-arc2 layer (string->real center-x) (string->real center-y) (string->real radius) (string->real start) (string->real end)))
  
  (let* ([ht : (HashTable String String)    (make-hash (separate lst))]
         [layer : String                    (extract-str ht "8")]
         [closed? : Boolean                 (if (equal? (or 1 129) (extract-val ht "70")) #t #f)]
         [first-x : String                  (cadr (cast (memf (lambda ([x : String]) (equal? x "10")) lst) (Listof String)))]
         [first-y : String                  (cadr (cast (memf (lambda ([x : String]) (equal? x "20")) lst) (Listof String)))])
    (if closed?
        (path #f #f #f layer (let closed-polyline : Path-Entities
                               ([path-lst : (Listof String) lst]
                                [acc : Path-Entities null])
                               (match path-lst
                                 [(list "10" x1 "20" y1 "42" bulge "10" x2 "20" y2 rest ...) (closed-polyline (append (list "10" x2 "20" y2) rest) (cons (smake-arc layer x1 y1 x2 y2 bulge) acc))]
                                 [(list "10" x1 "20" y1 "10" x2 "20" y2 rest ...)            (closed-polyline (append (list "10" x2 "20" y2) rest) (cons (smake-line layer x1 y1 x2 y2) acc))]
                                 [(list "10" x1 "20" y1 "42" bulge "0")                      (cons (smake-arc layer x1 y1 first-x first-y bulge) acc)]
                                 [(list "10" x1 "20" y1 "0")                                 (cons (smake-line layer x1 y1 first-x first-y) acc)]
                                 [(list _ _ rest ...)                                        (closed-polyline rest acc)]
                                 [_ (error "This is not expected, given: " path-lst)])))
        (path #f #f #f layer (let open-polyline : Path-Entities
                               ([path-lst : (Listof String) lst]
                                [acc : Path-Entities null])
                               (match path-lst
                                 [(list "10" x1 "20" y1 "42" bulge "10" x2 "20" y2 "0")      (cons (smake-arc layer x1 y1 x2 y2 bulge) acc)]
                                 [(list "10" x1 "20" y1 "10" x2 "20" y2 "0")                 (cons (smake-line layer x1 y1 x2 y2) acc)]
                                 [(list "10" x1 "20" y1 "42" bulge "10" x2 "20" y2 rest ...) (open-polyline (append (list "10" x2 "20" y2) rest) (cons (smake-arc layer x1 y1 x2 y2 bulge) acc))]
                                 [(list "10" x1 "20" y1 "10" x2 "20" y2 rest ...)            (open-polyline (append (list "10" x2 "20" y2) rest) (cons (smake-line layer x1 y1 x2 y2) acc))]
                                 [(list _ _ rest ...)                                        (open-polyline rest acc)]
                                 [_ (error "This is not expected, given: " path-lst)]))))))

;; 1) determine the center point of the arc given the angle and the 2 arc points.
;; 1.1) calculate the 2 possible center points using vectors. the 2 arc points form a line/chord.
;; 1.2) a perpendicular line bisecting the line/chord at the midpoint with a magnitude (calculated using trigonometry) extending in either direction are the 2 possible center points.
;; 1.3) the arc goes from the first to the second point in a CW or CCW fashion depending on the sign of the bulge (-ve bulge means CW fashion, +ve means CCW fashion)
;; 1.41) if the angle (calculated using the bulge) is smaller than 180, then for a CW arc, the centerpoint is CCW with respect to the vector formed from arc point 1 to arc point 2
;; 1.42) if the angle (calculated using the bulge) is bigger than 180, then for a CW arc, the centerpoint is CW with respect to the vector formed from arc point 1 to arc point 2
;; 2) determine the quadrant where the first arc point is located by separating the bounding box of the circle (with the center points and radius) into 4 areas.
;; 3) calculate the angle from start point to x axis.
;; 4) when creating an arc from DXF files, the arcs go from start angle to end angle in a clockwise fashion. we want to represent that here.
(: make-arc2 (-> String Real Real Real Real Real arc))
(define (make-arc2 layer x1 y1 x2 y2 bulge)
  (: get-center (-> Real Boolean (List Real Real Real Real Real)))
  (define (get-center angle big-bulge?)
    (let* ((chord-length (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))
           (small-angle (if (< angle pi) angle (- (* 2 pi) angle)))
           ;negative bulge indicates point 1 goes to point 2 in a CW fashion
           (is-cw? (negative? bulge))
           (radius (abs (/ (/ chord-length 2) (sin (/ small-angle 2)))))
           (midpoint-x (/ (+ x1 x2) 2))
           (midpoint-y (/ (+ y1 y2) 2))
           ;normalizing a vector -> calculate x and y length, then divide both x and y component length by the vector length
           (vector-x (- x1 x2))
           (vector-y (- y1 y2))
           (magnitude chord-length)
           (unit-vector-x (* vector-x (/ 1 magnitude)))
           (unit-vector-y (* vector-y (/ 1 magnitude)))
           ;the normal is perpendicular to the vector formed by the 2 arc points
           (normal-vector-x (* 1 unit-vector-y))
           (normal-vector-y (* -1 unit-vector-x))
           (adj (/ (/ chord-length 2) (tan (/ angle 2))))
           ;2 possible center points
           (center1-x (+ midpoint-x (* adj normal-vector-x)))
           (center1-y (+ midpoint-y (* adj normal-vector-y)))
           (center2-x (- midpoint-x (* adj normal-vector-x)))
           (center2-y (- midpoint-y (* adj normal-vector-y)))
           (ax1 (radians->degrees (cos (/ (- x1 center2-x) radius))))
           (ax2 (radians->degrees (cos (/ (- x2 center2-x) radius))))
           (cross-product1 (- (* (- x2 x1) (- center1-y y1)) (* (- y2 y1) (- center1-x x1))))
           (cross-product2 (- (* (- x2 x1) (- center2-y y1)) (* (- y2 y1) (- center2-x x1)))))
      (if big-bulge? 
          (if is-cw?                         ;big angle -> CW center and CW arc or CCW center and CCW arc
              (if (positive? cross-product1) ;positive cross product means center is CW with respect to point1 -> point 2
                  (list center1-x center1-y center2-x center2-y radius)
                  (list center2-x center2-y center1-x center1-y radius))
              (if (negative? cross-product2)
                  (list center2-x center2-y center1-x center1-y radius)
                  (list center1-x center1-y center2-x center2-y radius)))
          (if is-cw?                         ;small angle -> CW center and CCW arc or CCW center and CW arc
              (if (positive? cross-product1) ;positive cross product means center is CW with respect to point1 -> point 2
                  (list center2-x center2-y center1-x center1-y radius)
                  (list center1-x center1-y center2-x center2-y radius))
              (if (negative? cross-product2)
                  (list center1-x center1-y center2-x center2-y radius)
                  (list center2-x center2-y center1-x center1-y radius))))))
  (let* ((arc-angle-rad (abs (* 4 (atan bulge))))
         (big-bulge? (> arc-angle-rad pi))
         (small-angle (if (< arc-angle-rad pi) arc-angle-rad (- (* 2 pi) arc-angle-rad)))
         (is-cw? (negative? bulge))
         (centerpoints (get-center arc-angle-rad big-bulge?))
         (center-x (first centerpoints))
         (center-y (second centerpoints))
         [x3 (third centerpoints)]
         [y3 (fourth centerpoints)]
         (radius (fifth centerpoints))
         [xb (biggest (list x1 x2 x3))]
         [yb (biggest (list y1 y2 y3))]
         [xs (smallest (list x1 x2 x3))]
         [ys (smallest (list y1 y2 y3))]
         [mbr (rect xs ys xb yb)]
         (top (+ center-y radius))
         (bottom (- center-y radius))
         (left (- center-x radius))
         (right (+ center-x radius))
         (quad-num (cond ((and (in-between? x1 left center-x) (in-between? y1 top center-y)) 2)
                         ((and (in-between? x1 left center-x) (in-between? y1 bottom center-y)) 3)
                         ((and (in-between? x1 right center-x) (in-between? y1 top center-y)) 1)
                         ((and (in-between? x1 right center-x) (in-between? y1 bottom center-y)) 4)
                         ;0 is for edge cases.
                         ((or (reasonable-equal? x1 left) (reasonable-equal? x1 right) (reasonable-equal? y1 top) (reasonable-equal? y1 bottom)) 0)
                         (else (error "Quadrant not found: " x1 y1))))
         (angle-to (acos (/ (abs (- x1 center-x)) radius)))
         (start (radians->degrees (cond ((= quad-num 0) 
                                         (cond ((reasonable-equal? x1 left) (degrees->radians 180))
                                               ((reasonable-equal? x1 right) (degrees->radians 0))
                                               ((reasonable-equal? y1 top) (degrees->radians 90))
                                               ((reasonable-equal? y1 bottom) (degrees->radians 270))
                                               (error "Quadrant not found: " x1 y1)))
                                        ((= quad-num 1) angle-to)
                                        ((= quad-num 2) (- (degrees->radians 180) angle-to))
                                        ((= quad-num 3) (+ (degrees->radians 180) angle-to))
                                        ((= quad-num 4) (- (degrees->radians 360) angle-to))
                                        (else (error "Quadrant given is: " quad-num)))))
         (end (if is-cw? 
                  (if (negative? (- start (radians->degrees arc-angle-rad)))
                      (+ 360 (- start (radians->degrees arc-angle-rad)))
                      (- start (radians->degrees arc-angle-rad)))
                  (if (< 360 (+ start (radians->degrees arc-angle-rad)))
                      (- (+ start (radians->degrees arc-angle-rad)) 360)
                      (+ start (radians->degrees arc-angle-rad))))))
    ;DXF is CW
    ;in this case x3 y3 is the arc center point
    (arc #f #f #f layer (node center-x center-y) radius start end (node x1 y1) (node x3 y3) (node x2 y2) is-cw? mbr)
    ))

(: create-structs (-> (Listof (Listof String)) Entities))
(define (create-structs entity-list)
  (map (lambda ([x : (Listof String)])
         (match x
           [(list "LINE" lst ...)       (dxf-line lst)]
           [(list "POINT" lst ...)      (dxf-point lst)]
           [(list "ARC" lst ...)        (dxf-arc lst)]
           [(list "LWPOLYLINE" lst ...) (dxf-path lst)]
           [(list "CIRCLE" lst ...)     (dxf-circle lst)]
           [_ (error "This is not expected, given: " x)]))
       entity-list))

(: apply-unit-scale (-> Entity Real Entity))
(define (apply-unit-scale entity dxf-scale)
  (define *dxf-scale (lambda ([x : Real]) (* x dxf-scale)))
  (match entity
    [(line highlighted selected visible layer p1 p2 mbr)
     (make-line layer (*dxf-scale (node-x p1)) (*dxf-scale (node-y p1)) (*dxf-scale (node-x p2)) (*dxf-scale (node-y p2)))]
    [(arc highlighted selected visible layer center radius start end p1 p2 p3 ccw mbr)
     (make-arc layer (*dxf-scale (node-x center)) (*dxf-scale (node-y center)) (*dxf-scale radius) start end ccw)]
    [(dot highlighted selected visible layer p)
     (make-dot layer (*dxf-scale (node-x p)) (*dxf-scale (node-y p)))]
    [(path highlighted selected visible layer path-list)
     (path #f #f #f layer (for/list ([x path-list]) (cast (apply-unit-scale x dxf-scale) Path-Entity)))]))

(: file->struct-list (-> Path-String Entities))
(define (file->struct-list path-string)
  (let* ([file-list (reader (open-input-file path-string))]
         [entity-section (extract-section file-list "ENTITIES")]
         [header-section (extract-section file-list "HEADER")]
         ;;either 25.4 or 1
         [dxf-scale (let ([scale (cast (string->number (third (cast (member "$DIMALTF" header-section) (Listof String)))) Real)])
                      (if (> scale 1) scale 1))]
         [entity-list (separate-entities entity-section)]
         [struct-lst (create-structs entity-list)])
    (map (lambda ([x : Entity]) (apply-unit-scale x dxf-scale)) struct-lst)))