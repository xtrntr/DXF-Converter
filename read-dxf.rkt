#lang racket

;; This module reads DXF files and extracts any relevant information. Also creates structs from lists containing DXF entity information

(require srfi/1
         "structs.rkt"
         "geometric-functions.rkt")

(define supported-types '("LWPOLYLINE" "ARC" "POINT" "CIRCLE" "LINE"))
(define entity-types '("3DFACE"  "3DSOLID"  "ACAD_PROXY_ENTITY" "ARC" "ARCALIGNEDTEXT"  "ATTDEF"  "ATTRIB"  "BODY"  "CIRCLE" "DIMENSION" "ELLIPSE"  "HATCH" "IMAGE"  "INSERT"  "LEADER"  "LINE" "LWPOLYLINE" "MLINE"  "MTEXT"  "OLEFRAME"  "OLE2FRAME"  "POINT" "POLYLINE" "RAY"  "REGION"  "RTEXT"  "SEQEND"  "SHAPE"  "SOLID" "SPLINE" "TEXT"  "TOLERANCE"  "TRACE"  "VERTEX"  "VIEWPORT" "WIPEOUT" "XLINE"))
(define sections (list "ENTITIES" "ENDSEC"))

(provide file->struct-list)

;; parsing functions
(define (split str [ptn #rx"[ ]+"])
  (regexp-split ptn (string-trim str)))

(define (reader input-port)
  (define lines (read-chunks input-port))
  (foldl (lambda (f r)
           (define fst (filter (compose not (curry string=? "")) (split f)))
           (append fst r))
         '() lines))

(define (read-chunks input-port)
  (let loop ([accu '()])
    (define nxt (read-line input-port 'any))
    (if (eof-object? nxt)
        ((lambda (x) x) accu)
        (loop (cons nxt accu)))))

;; extract the values in one section into a list.
(define (extract-section lst header)
  (define (extract-until lst keyword)
    (cond ((equal? (car lst) keyword) '())
          (else (cons (car lst) (extract-until (cdr lst) keyword)))))
  (extract-until (member (car header) lst) (cadr header)))

;; extract individual entities in the ENTITIES section of a DXF file.
;; this returns a list of lists, with each containing the information of a single entity.
(define (separate-entities lst)
  (if (empty? lst)
      '()
      (let-values ([(data tail) (break (lambda (element) (member element entity-types)) (rest lst))])
        (if (member (first lst) supported-types)
            (begin (cons (cons (first lst) data)
                         (separate-entities tail)))
            (separate-entities tail)))))

;; lists of DXF entities come in the format: header-keyvalue header-keyvalue ... header-keyvalue

;forgotten purpose
(define (string-contains-alphabet? str)
  (ormap char-alphabetic? (string->list str)))

;; take the pair of the relevant headers for drawing using take-pair,
(define (take-pair lst)
  (cond ((> 2 (length lst)) '())
        (else (cons (list (first lst)
                          (if (string-contains-alphabet? (second lst)) (second lst) (string->number (second lst))))
                    (take-pair (cddr lst))))))

;; and extract the parameters using filter-header
(define (filter-header lst key)
  (cond ((empty? lst) '())
        ((member (car (car lst)) key)
         (cons (car lst)
               (filter-header (cdr lst) key)))
        (else
         (filter-header (cdr lst) key))))

(define (list->line lst)
  (apply create-line
         (map cadr (filter-header (take-pair lst) '("8" "10" "20" "11" "21")))))

(define (list->arc lst)
  (apply create-arc
         (map cadr (filter-header (take-pair lst) '("8" "10" "20" "40" "50" "51")))))
  
(define (list->circle lst)
  (apply create-circle
         (map cadr (filter-header (take-pair lst) '("8" "10" "20" "40")))))

(define (list->point lst)
  (apply create-point
         (map cadr (filter-header (take-pair lst) '("8" "10" "20")))))
 
; 1) if 70 = 1 or 129 then closed. store x y value
; 2) create line for 10 20 10 20
; 3) create arc for 10 20 42 10 20
(define (separate-lwpolyline lst layer)
  (define (closed-polyline lst first-x first-y)
    (match lst
      [(list (list "10" x1) (list "20" y1) (list "42" bulge) (list "10" x2) (list "20" y2) a ...)  (cons (create-arc2 layer x1 y1 x2 y2 bulge) (closed-polyline (append (list (list "10" x2) (list "20" y2)) a) first-x first-y))]
      [(list (list "10" x1) (list "20" y1) (list "10" x2) (list "20" y2) a ...)                    (cons (create-line layer x1 y1 x2 y2) (closed-polyline (append (list (list "10" x2) (list "20" y2)) a) first-x first-y))]
      [(list (list "10" x1) (list "20" y1) (list "42" bulge))                                      (create-arc2 layer x1 y1 first-x first-y bulge)]
      [(list (list "10" x1) (list "20" y1))                                                        (create-line layer x1 y1 first-x first-y)]
      [_ (void)]))
  (define (open-polyline lst)
    (match lst
      [(list (list "10" x1) (list "20" y1) (list "42" bulge) (list "10" x2) (list "20" y2))        (create-arc2 layer x1 y1 x2 y2 bulge)]
      [(list (list "10" x1) (list "20" y1) (list "10" x2) (list "20" y2))                          (create-line layer x1 y1 x2 y2)]
      [(list (list "10" x1) (list "20" y1) (list "42" bulge) (list "10" x2) (list "20" y2) a ...)  (cons (create-arc2 layer x1 y1 x2 y2 bulge) (open-polyline (append (list (list "10" x2) (list "20" y2)) a)))] 
      [(list (list "10" x1) (list "20" y1) (list "10" x2) (list "20" y2) a ...)                    (cons (create-line layer x1 y1 x2 y2) (open-polyline (append (list (list "10" x2) (list "20" y2)) a)))]
      [(list (list "10" x1) (list "20" y1) (list "42" bulge))                                      (display "error, there should not be a 42 point at the end of an open polyline")]
      [_ (void)]))
  (let* ((polyline-flag (cadr (findf (lambda (x) (equal? (car x) "70")) lst)))
         (closed? (if (equal? polyline-flag (or 1 129)) #t #f))
         (first-x (cadr (findf (lambda (x) (equal? (car x) "10")) lst)))
         (first-y (cadr (findf (lambda (x) (equal? (car x) "20")) lst)))) ;1 or 129 for closed, 0 for open
    (if closed? 
        (closed-polyline (cdr lst) first-x first-y)
        (open-polyline (cdr lst)))))

(define (list->path lst)
  (define layer (cadr (car (filter-header (take-pair lst) '("8")))))
  (define list-of-arcs-and-lines (separate-lwpolyline (filter-header (take-pair lst) '("70" "10" "20" "42")) layer))
  (path (layer->string layer) (flatten list-of-arcs-and-lines)))

(define (layer->string x)
  (if (string? x) x (number->string x)))

;; 1) determine the center point of the arc given the angle and the 2 arc points.
;; 1.1) calculate the 2 possible center points using vectors. the 2 arc points form a line/chord.
;; 1.2) a perpendicular line bisecting the line/chord at the midpoint with a magnitude (calculated using trigonometry) extending in either direction are the 2 possible center points.
;; 1.3) the arc goes from the first to the second point in a CW or CCW fashion depending on the sign of the bulge (-ve bulge means CW fashion, +ve means CCW fashion)
;; 1.41) if the angle (calculated using the bulge) is smaller than 180, then for a CW arc, the centerpoint is CCW with respect to the vector formed from arc point 1 to arc point 2
;; 1.42) if the angle (calculated using the bulge) is bigger than 180, then for a CW arc, the centerpoint is CW with respect to the vector formed from arc point 1 to arc point 2
;; 2) determine the quadrant where the first arc point is located by separating the bounding box of the circle (with the center points and radius) into 4 areas.
;; 3) calculate the angle from start point to x axis.
;; 4) when creating an arc from DXF files, the arcs go from start angle to end angle in a clockwise fashion. we want to represent that here.
(define (create-arc2 layer x1 y1 x2 y2 bulge)
  (define (get-center angle big-bulge?)
    (let* ((chord-length (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))
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
                  (list center1-x center1-y radius)
                  (list center2-x center2-y radius))
              (if (negative? cross-product2)
                  (list center2-x center2-y radius)
                  (list center1-x center1-y radius)))
          (if is-cw?                         ;small angle -> CW center and CCW arc or CCW center and CW arc
              (if (positive? cross-product1) ;positive cross product means center is CW with respect to point1 -> point 2
                  (list center2-x center2-y radius)
                  (list center1-x center1-y radius))
              (if (negative? cross-product2)
                  (list center1-x center1-y radius)
                  (list center2-x center2-y radius))))))
  (let* ((arc-angle-rad (abs (* 4 (atan bulge))))
         (big-bulge? (> arc-angle-rad pi))
         (small-angle (if (< arc-angle-rad pi) arc-angle-rad (- (* 2 pi) arc-angle-rad)))
         (is-cw? (negative? bulge))
         (centerpoints (get-center arc-angle-rad big-bulge?))
         (center-x (car centerpoints))
         (center-y (cadr centerpoints))
         (radius (caddr centerpoints))
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
                         (else (display "unaccounted for"))))
         (angle-to (acos (/ (abs (- x1 center-x)) radius)))
         (start (radians->degrees (cond ((= quad-num 0) 
                                         (cond ((reasonable-equal? x1 left) (degrees->radians 180))
                                               ((reasonable-equal? x1 right) (degrees->radians 0))
                                               ((reasonable-equal? y1 top) (degrees->radians 90))
                                               ((reasonable-equal? y1 bottom) (degrees->radians 270))
                                               (else (display "unaccounted for"))))
                                        ((= quad-num 1) angle-to)
                                        ((= quad-num 2) (- (degrees->radians 180) angle-to))
                                        ((= quad-num 3) (+ (degrees->radians 180) angle-to))
                                        ((= quad-num 4) (- (degrees->radians 360) angle-to)))))
         (end (if is-cw? 
                  (if (negative? (- start (radians->degrees arc-angle-rad)))
                      (+ 360 (- start (radians->degrees arc-angle-rad)))
                      (- start (radians->degrees arc-angle-rad)))
                  (if (< 360 (+ start (radians->degrees arc-angle-rad)))
                      (- (+ start (radians->degrees arc-angle-rad)) 360)
                      (+ start (radians->degrees arc-angle-rad))))))
    ;DXF is CW
    (list (if is-cw?
              (create-arc layer center-x center-y radius end start)
              (create-arc layer center-x center-y radius start end)))))

(define (create-line layer x1 y1 x2 y2)
  (line (layer->string layer) x1 y1 x2 y2))

(define (create-point layer x y)
  (point (layer->string layer) x y))

(define (create-arc layer x y radius start end)
  (define list-of-3-points (get-arc-points x y radius start end))
  (apply arc (append (list (layer->string layer) x y radius start end) list-of-3-points)))

(define (create-circle layer x y radius) ; creating 2 semicircles with create-arc
  (create-arc (layer->string layer) x y radius 0 360))

;; convert entity list to their respective structs using above functions
(define (create-structs entity-list)
  (map (lambda (x) (case (first x)
                     [("LINE")       (list->line (rest x))]
                     [("LWPOLYLINE") (list->path (rest x))]
                     [("CIRCLE")     (list->circle (rest x))]
                     [("POINT")      (list->point (rest x))]
                     [("ARC")        (list->arc (rest x))]))
       entity-list)) 

(define (file->struct-list input-port)
  (let* ((file-list (reader (open-input-file input-port)))
         (section-list (extract-section file-list sections))
         (entity-list (separate-entities section-list)))
    (flatten (create-structs entity-list))))