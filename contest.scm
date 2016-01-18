;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: Cal dente
;;;
;;; Description:
;;;    spheres, planes, reflections
;;;    Cal's colors, tail recursion
;;;    put in pot and stir

; Notes:
;	* Requires tail recursion
;	* Takes about 3 hours to render
;	* 841 tokens

(define (min a b) (if (> a b) b a))
(define (max a b) (if (> a b) a b))
(define (clamp val) (max 0 (min 1 val)))

(define (convert-color clr)
    (rgb (clamp (car clr)) (clamp (cadr clr)) (clamp (caddr clr))))

(define (x v) (car v))
(define (y v) (cadr v))
(define (z v) (caddr v))

(define (dot a b) (+ (* (x a) (x b)) (* (y a) (y b)) (* (z a) (z b))))

(define (cross a b)
    (list (- (* (z a) (y b)) (* (y a) (z b)))
          (- (* (x a) (z b)) (* (z a) (x b)))
          (- (* (y a) (x b)) (* (x a) (y b)))))

(define (addv a b) (list (+ (x a) (x b)) (+ (y a) (y b)) (+ (z a) (z b))))
(define (subv a b) (addv a (scale b (- 1))))
(define (scale a k) (list (* (x a) k) (* (y a) k) (* (z a) k)))
(define (norm a) (scale a (/ 1 (sqrt (dot a a)))))
(define (mix a b r) (addv (scale a r) (scale b (- 1 r))))

(define (illuminate-sphere sphere direction surface depth)
    (define center (cadr sphere))
    (define color (caddr sphere))
    (define type color)
    (if (eq? type 'mirror)
        (define color '(1 1 1)))

    (define to_surface (norm (subv surface center)))
    (define to_light (norm (subv light surface)))
    (define intensity (max ambient (dot (norm to_light) (norm to_surface))))

    (if (not (null? (get-hits spheres surface to_light))) (define intensity ambient))

    (define direct (scale color intensity))
    (if (and (<= depth 1) (not (eq? type 'mirror)))
        direct
        (begin
            (define cosine (dot direction to_surface))
            (define bounce (subv direction (scale to_surface (* 2 cosine))))
            (define reflected (trace-ray surface bounce (- depth 1)))
            (if (eq? type 'mirror)
                reflected
                (mix direct reflected (+ 0.7 (* 0.3 (expt intensity 300))))))))

(define (illuminate-plane plane direction surface depth)
    (define origin (car plane))
    (define u1 (cadr plane))
    (define u2 (caddr plane))
    (define normal (cross u1 u2))

    (define to_light (norm (subv light surface)))
    (define intensity (max ambient (dot to_light normal)))
    
    (if (not (null? (get-hits spheres surface to_light))) (define intensity ambient))

    (define offset (subv surface origin))
    (define px (dot u1 offset))
    (define py (dot u2 offset))
    (if (< px 0) (- px 1))
    (if (< py 0) (- py 1))
    (define color '(0.9 0.9 0.9))
    (if (or (and (< (abs (modulo px 2)) 1) (< (abs (modulo py 2)) 1))
            (and (>= (abs (modulo px 2)) 1) (>= (abs (modulo py 2)) 1)))
        (define color '(0.5 0.5 0.5)))
    (scale color intensity))

(define (intersect-sphere sphere source direction)
    (define radius (car sphere))
    (define center (cadr sphere))
    (define v (subv source center))
    (define b (- (dot v direction)))
    (define v2 (dot v v))
    (define r2 (* radius radius))
    (define d2 (+ (- (* b b) v2) r2))
    (if (< d2 0)
        nil
        (begin
            (define d (- b (sqrt d2)))
            (if (> d min_distance)
                (list d)
                (begin
                    (define d (+ b (sqrt d2)))
                    (if (> d min_distance)
                        (list d)
                        nil))))))

(define (intersect-plane plane source direction)
    (define normal (cross (cadr plane) (caddr plane)))
    (if (< (abs (dot direction normal)) min_distance)
        nil
        (begin
            (define h (/ (dot (subv (car plane) source) normal) (dot direction normal)))
            (if (>= h 0) (list h) nil))))

; Data Abstraction
(define (scene-object data intersect-fn illuminate-fn) (append data (list intersect-fn) (list illuminate-fn)))
(define (sphere data) (scene-object data intersect-sphere illuminate-sphere))
(define (plane  data) (scene-object data intersect-plane  illuminate-plane))

(define (intersect object source direction)
    ((car (cdddr object)) object source direction))

(define (illuminate object direction surface depth)
    ((car (cddddr object)) object direction surface depth))

(define min_distance 0.01)

(define (get-hits objects source direction)
    (if (null? objects)
        nil
        (begin
            (define object (car objects))
            (define hit (intersect object source direction))
            (if (not (null? hit))
                (cons (cons (car hit) (list object)) (get-hits (cdr objects) source direction))
                (get-hits (cdr objects) source direction)))))

(define (get-nearest lst)
    (cond
        ((null? (cdr lst)) (car lst))
        ((< (car (car lst)) (car (get-nearest (cdr lst)))) (car lst))
        (else (get-nearest (cdr lst)))))

(define (trace-ray source direction depth)
    (define hits (get-hits (append spheres planes) source direction))
    (if (null? hits)
        black
        (begin
            (define nearest (get-nearest hits))
            (define distance (car nearest))
            (define object (cadr nearest))
            (define surface (addv source (scale direction distance)))
            (illuminate object direction surface depth))))

; Scene
(define camera '(0 1 -4.5))
(define light '(2 3 0))
(define ambient 0.2)
(define black '(0 0 0))

; (<size> <position> <color>)
(define spheres
    (list
        (sphere '(1 (0 1 2) mirror))
        (sphere '(1 (2.1 1 2) (0.03137 0.12157 0.39216)))
        (sphere '(1 (-2.1 1 2) (0.98824 0.70196 0.14902)))
    )
)
; (<origin> <u1> <u2>)
(define planes
    (list
        (plane (list '(0 0 10) (norm '(1 0 -1)) (norm '(1 0 1))))
        (plane (list '(0 1 10) (norm '(0 -1 0)) (norm '(1 0 -1))))
        (plane (list '(0 0 10) (norm '(-1 0 -1)) (norm '(0 -1 0))))
    )
)

(define (draw)
    (bgcolor "black")
    (define block 1)
    (pixelsize block)
    (define size (min (screen_width) (screen_height)))

    (define (draw-pixel x y)
        (define direction (norm (list (- (/ x size) 0.5) (- (/ y size) 0.5) 1)))
        (define color (scale (trace-ray camera direction 4) 0.95))
        (pixel x y (convert-color color))
        (cond
            ((< (+ 1 y) size) (draw-pixel x (+ 1 y)))
            ((< (+ 1 x) size) (draw-pixel (+ 1 x) 0))))
    (draw-pixel 0 0)
    (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)
