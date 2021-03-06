;;; Scheme Recursive Art Contest Entry
;;;
;;; Title: Tail Recursive Ray Tracer (T.R.R.T)
;;;
;;; Description:
;;;		Recursive raytracer traces
;;;		Recursively stacked spheres
;;;		Everything recursive!


;; Features:
;; - Raytracing of colored spheres
;; - 3D coordinate system
;; - Point lighting system
;; - Reflection and shadows with adjustable depth
;; - Multiprocessing!!
;; - Pixelisation and adjustable output image size
;; - Tail recursive
;;
;; Inspired by http://www.gabrielgambetta.com/tiny_raytracer.html

(define canv-size 200) ;;set output image size
(define pen-size 2) ;; set pixel size.
(define n-processors 16) ;; must be >= 1. For the best speed set to (2 * number of cores).
;; If set to a number greater then 1, this code will use a non-standard scheme multiprocessing extension
;; Which you want! unless you hate yourself and like excessively slow programs.

;; It took over 17.6 hours to render the final image 800x800px with pen-size 1
;; on an i7-2600K CPU @ 3.40GHz (with 16 processes)
;; Note: pen-size 1 on Mac OSX would result in small artifacts because Mac can't
;; draw straight lines precisely...

(define demo #f) ;;set to true to render a simple (fast) 4 sphere scene
;;It takes 1min to render the demo with canv-size 200 and pen-size 2 (with i7)

(define lights '((10 (0 0 -50))
                 (10 (0 0 -20))))

(define camera (list -16.5 5.5 -80))

(define reflection-depth 3)
(define reflectance 5) ;;default sphere reflection. can be set individually instead.
(define ambient-light 2)

(define circle-depth 13) ;;below level 13 there are no more circles of radius > 0.3
(define color-list '((2.0 8.0 10.0) (6.0 10.0 4.0) (10.0 10.0 4.0) (10.0 6.0 2.0) (10.0 0.0 0.0) (9.3 2.5 0.0) (10.0 5.1 9.8) (1.3 5.4 1.3)))
(define color-length (length color-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitutions
;;
;; Similar to c preprocessor macros.
;; All substitutions are defined in the list `substitutions'
;; format: (symbol value)
;;          replace 'symbol' with 'value'
;;    OR:  ((name args) def)
;;         replace call to 'name' with 'def', substituting 'args'
;;
;; Calling other substitions is OK
;; but recursion or mutual recursion is not
;;
;;
;; Example:
;;  with substitution definition:
;;     ((dot-product a b)
;;      (+
;;       (* (car a) (car b))
;;       (* (cadr a) (cadr b))
;;       (* (caddr a) (caddr b))))
;;
;; the code:
;;   (dot-product a b)
;;
;; will be replaced with:
;;  (+ (* (car a) (car b))
;;     (* (car (cdr a)) (car (cdr b)))
;;     (* (car (cdr (cdr a))) (car (cdr (cdr b)))))
;;
;;
;; Substitutions are only expanded with macros `define*' and `mu*'

(define substitutions
  (list '((cadr x) (car (cdr x)))
        '((caddr x) (car (cdr (cdr x))))
        '((cadddr x) (caddr (cdr x)))
        '((square x) (* x x))
        (list 'half (/ canv-size 2))
        '((in-255 value) (min 255 value))

        ;;you must be careful with the functions to be inlined,
        ;;functions like 'dot-project' and 'a-minus-bk'
        ;;seem like good candidates but doing so will
        ;;cause the token count of some functions to increase 2-3 times
        ;;upon expansion, ultimately slowing down the program
        ;;instead of speeding it up. presumably due to the
        ;;increased volume of code the system must move about

        ))

(define-macro (for var in lst : code)
  ;;This is why lisp is amazing!
  (list 'let '()
        (list 'define (list '_call_ var)
              code)
        '(define (_do_ lst)
           (if (not (null? lst))
               (begin
                 (_call_ (car lst))
                 (_do_ (cdr lst)))))
        (list '_do_ lst)))

(define-macro (define-substitutions)
  ;;bind the substitutions to actual function names so
  ;;that they may be used outside of define* and mu* macros

  (define subs nil)
  (for s in substitutions :
       (set! subs (cons (list 'define (car s) (car (cdr s))) subs)))
  (cons 'begin subs))

(define-substitutions)

(define (find-subst thing list)
  ;;return the first element in LIST whose caar for caaar is THING
  (if (null? list)
      nil
      (if (or (equal? (car (car list)) thing)
              (and (list? (car (car list)))
                   (equal? (car (car (car list))) thing)))
          (car list)
          (find-subst thing (cdr list)))))

(define (subst-args subst)
  (cdr (car subst)))

(define (subst-body subst)
  (car (cdr subst)))

(define (zip x y)
  (if (or (null? x)
	  (null? y))
      nil
      (cons (list (car x) (car y))
            (zip (cdr x) (cdr y)))))

;;tail recursive with correct ordering
(define (map proc items)
  (define results nil)
  (for thing in items :
       (set! results (cons (proc thing) results)))
  (reverse results))

(define (reverse lst)
  (define newlist nil)
  (for i in lst :
       (set! newlist (cons i newlist)))
  newlist)

(define (make-substitutions subs form)
  (if (and (list? form)
	   (> (length form) 0))
      (begin
        (define subst (find-subst (car form) subs))
        (if (and (not (null? subst))
                 (list? (car subst))) ;;must be function subst
            (make-substitutions subs
                                (make-substitutions (zip (subst-args subst) (cdr form))
                                                    (subst-body subst)))
            (map (lambda (x) (make-substitutions subs x)) form)))
      (begin ;;else: symbol substitution
        (define subst (find-subst form subs))
        (if (null? subst)
            form
            (subst-body subst)))))

(define-macro (define* formals body)
  (list 'define formals
        (make-substitutions substitutions body)))

(define-macro (mu* formals body)
  (list 'mu formals
        (make-substitutions substitutions body)))

(print "loading and expanding...")
(define load-start-time (time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sphere positioning
;;;;

(define circles nil)

;; these function are not inlined because
;; doing so takes more time then is saved
;; during calculations.
;; (2.22 additional seconds to expand for a speed up
;;  of only 1.14 seconds (this is important!))

;;circle abstraction
(define (make-circle center radius color reflection)
  (list radius center color reflection))
(define (radius c) (car c))
(define (center c) (cadr c))
(define (x-coor c) (car (center c)))
(define (y-coor c) (cadr (center c)))
(define (curvature c) (/ 1 (radius c)))
(define (circle-color c) (caddr c))
(define (reflectiveness c) (cadddr c))

;;complex number abstraction
(define (complex real imag)
  (list real imag))
(define (real z) (car z))
(define (imag z) (cadr z))
(define (magnitude z) (sqrt (+ (* (real z) (real z)) (* (imag z) (imag z)))))

(define spheres nil)

;; fun fact:
;; if you do define  c+ and c* as substitutions,
;; it will take 1 hour 45min to finish the expansion
;; and the resulting code will have 855766 tokens
(define* (c+ z1 z2)
  (complex (+ (real z1) (real z2))
           (+ (imag z1) (imag z2))))

(define* (c* z1 z2)
  (if (number? z2)
      (complex (* (real z1) z2) (* (imag z1) z2))
      (complex (- (* (real z1) (real z2)) (* (imag z1) (imag z2)))
               (+ (* (real z1) (imag z2)) (* (imag z1) (real z2))))))

(define* (c-sqrt z)
  (let ((x (real z))
        (y (imag z))
        (r (magnitude z)))
    (complex (sqrt (/ (+ r x) 2))
             (* (if (= y 0) 1 (/ y (abs y))) (sqrt (/ (- r x) 2))))))


;;return list of all triples of mutually tangential circles
(define* (find-new-tangencies circles)
  ;;circles is a list of lists of 3 circles with format (c1 c2 c3 new)
  ;;where 'new' is the newly calculated circle that is mutually
  ;;tangential to c1, c2, and c2
  (begin
    (define (find circles sofar)
      (if (null? circles)
          sofar
          (let ((c1 (car (car circles)))
                (c2 (cadr (car circles)))
                (c3 (caddr (car circles)))
                (new (cadddr (car circles))))
            (find (cdr circles ) (cons (list c1 c2 new)
                                       (cons (list c1 c3 new)
                                             (cons (list c2 c3 new)
                                                   sofar)))))))
    (find circles nil)))

(define (get-color level)
  (nth color-list (modulo level color-length)))

(define (find-tangent-circles circles level)
  (define (find-iter circles sofar)
    (if (null? circles)
        sofar
        (let ((lst (car circles)))
          (define new (find-tangent-circle (car lst) (cadr lst) (caddr lst) level))
          (if (null? new)
              (find-iter (cdr circles) sofar)
              (find-iter (cdr circles) (cons new sofar))))))
  (find-iter circles nil))

(define* (find-spheres)
  (let ((c1 (make-circle (list 0 (/ (* 250 (sqrt 3)) 3)) 125 (car color-list) reflectance ))
        (c2 (make-circle (list 125 (- (/ (* 125 (sqrt 3)) 3))) 125 (car color-list) reflectance))
        (c3 (make-circle (list -125 (- (/ (* 125 (sqrt 3)) 3))) 125 (car color-list) reflectance))
        (c4 (make-circle '(0 0) (- (+ 125 (/ (* 250 (sqrt 3)) 3))) (car color-list) reflectance)))
    (print "calculating sphere positions...")
    (define (find circles tangencies level)
      (if (< level circle-depth)
          (let ((new-circles (find-tangent-circles tangencies level)))
            (find (append circles (map (lambda (lst) (cadddr lst)) new-circles))
                  (find-new-tangencies new-circles)
                  (+ level 1)))))

    (find (list c1 c2 c3 c4)
          (list (list c1 c2 c3) (list c1 c2 c4) (list c1 c3 c4) (list c2 c3 c4)) ;;lists of tangent circles
          0)

    (filter-circles)
    (set! spheres (convert))
    (if demo nil (print (list 'rendering (length spheres) 'spheres)))))

(define* (nth lst i)
  (if (= i 0)
      (car lst)
      (nth (cdr lst) (- i 1))))

;; ref: http://en.wikipedia.org/wiki/Descartes%27_theorem
(define* (find-tangent-circle c1 c2 c3 level)
  ;;returns a new circle that is mutually tangent to c1, c2, and c3
  (let ((z1 (center c1))
        (z2 (center c2))
        (z3 (center c3))
        (k1 (curvature c1))
        (k2 (curvature c2))
        (k3 (curvature c3)))

    (define k4 (+ k1 k2 k3 (* 2 (sqrt (+ (* k1 k2)
                                         (* k2 k3)
                                         (* k3 k1))))))

    (define _1 (c+ (c+ (c* z1 k1)
                       (c* z2 k2))
                   (c* z3 k3)))

    (define _2 (c-sqrt (c+ (c+ (c* (c* z1 z2)
                                   (* k1 k2))
                               (c* (c* z2 z3)
                                   (* k2 k3)))
                           (c* (c* z3 z1)
                               (* k3 k1)))))

    ;;;z4 has two possible solutions, the one whose magnitude is largest is the one we keep

    (define z4 (c* (c+ _1
                       (c* _2
                           2))
                   (/ 1 k4)))

    (define z42 (c* (c+ _1
                        (c* _2
                            -2))
                    (/ 1 k4)))

    (define new (if (> (magnitude z4) (magnitude z42))
                    (make-circle z4 (/ 1 k4) (get-color level) reflectance)
                    (make-circle z42 (/ 1 k4) (get-color level) reflectance)))

    ;;throw out circles that are to far from the origin
    ;;or have a radius that is too small
    ;;to prevent massive circle accumulation
    (if (or (> (magnitude (center new)) 72)
            (< (radius new) 0.3))

        nil
        (begin
          (set! circles (cons new circles))
          (list c1 c2 c3 new)))))


(define good (list (make-circle (list 125 (- (/ (* 125 (sqrt 3)) 3))) 125 '(8.0 2.0 10.0) reflectance)))

(define* (filter-circles)
    (for c in circles :
         (if (and (< (magnitude (center c)) 72) ;; radius*tan(180/6) ~ 72
                  (or (and (>= (x-coor c) 0) ;;only in 1st quadrant
                           (>= (y-coor c) 0))
                      (< (magnitude (center c)) 1) ;;execpt for the middle one
                      (and (> (x-coor c) 0) ;; or the ones in the forth quadrant
                           (< (y-coor c) 0) ;; that are close to the the x-axes
                           (> (y-coor c) (- 3)))
                      ))
             (set! good (cons c good)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sphere format: radius (cx  cy  cz) (R  G  B) specular_exponent reflectiveness
(define* (convert)
  (begin
    (define out nil)
    (for c in good :
         (set! out (cons (list (radius c)
                               (list
                                ;;(- (y-coor c)) ;;refect 90deg
                                ;;(x-coor c)
                                (- (x-coor c)) ;;reflect about y-axis
                                (y-coor c)
                                0)
                               (circle-color c)
                               9
                               (reflectiveness c))
                         out)))
    out))

;;end sphere generation code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RAYTRACING


(define* (dot-product a b)
  (+
   (* (car a) (car b))
   (* (cadr a) (cadr b))
   (* (caddr a) (caddr b))))

(define* (a-minus-bk a b k)
  (list
   (- (car a) (* (car b) k))
   (- (cadr a) (* (cadr b) k))
   (- (caddr a) (* (caddr b) k))))

;; returns closest sphere (without radius) which a ray hits and magnitude of distance to it
;; returns zero if no intersection
(define (closest-intersection source direction t_min t_max)
    (get-closest-sphere 0 spheres canv-size))

(define (sum-lists a b) ; have to be equal size
  (if (null? a) nil (cons (+ (car a) (car b)) (sum-lists (cdr a) (cdr b)))))

(define get-closest-sphere
  (mu* (v spheres-list distance)
      (if (null? spheres-list)
          (cons v distance) ; if out of spheres, pass closest sphere and magnitude
          (begin
            (define curr-sphere (car spheres-list))
            (define radius (car curr-sphere))
            (define curr-sphere (cdr curr-sphere))
            ;; solve quadratic equation for curr-sphere to fine intersects
            (define j (a-minus-bk source (car curr-sphere) 1))
            (define a (* 2 (dot-product direction direction)))
            (define b (* -2 (dot-product j direction)))
            (define discr (- (square b) (* 2 a (- (dot-product j j) (square radius)))))
            (if (> discr 0)
                (begin
                  (define discr (sqrt discr))
                  (define sol1 (/ (- b discr) a))
                  (if (and (< t_min sol1) (< sol1 t_max) (< sol1 distance))
                      (begin
                        (define v curr-sphere)
                        (define distance sol1)))
                  (define sol2 (/ (- b (- discr)) a))
                  (if (and (< t_min sol2) (< sol2 t_max) (< sol2 distance))
                      (begin
                        (define v curr-sphere)
                        (define distance sol2)))
                  ))
            (get-closest-sphere v (cdr spheres-list) distance)))))

;; Mu <3 procedure to calculate illumination based on all light for a specific point
;; trace-ray-iter is parent
(define get-illumination
  (mu* (lights-list illumination)
      (if (null? lights-list)
          illumination
          (begin
            (define light (car lights-list))
            (define light-vector (a-minus-bk (cadr light) intersection 1))
            (define k (dot-product normal light-vector))
            (define m (a-minus-bk light-vector normal (/ (* 2 k) n)))
            (define illumination (+ illumination
                                    (*
                                     (if (number? (car (closest-intersection intersection light-vector (/ 1 canv-size) 1))) 1 0)
                                     (car light)
                                     (+
                                      (max 0 (/ k (sqrt (* (dot-product light-vector light-vector) n))))
                                      (max 0 (pow
                                              (/
                                               (dot-product m direction)
                                               (sqrt (* (dot-product m m) (dot-product direction direction))))
                                              (cadr closest-sphere))))))) ;; get specular_exponent 4th element
            (get-illumination (cdr lights-list) illumination)))))

; where magic happens ;)
(define (trace-ray-iter source direction t_min t_max depth prev-color prev-ref) ;; tail-recursive hell yeah!!!
       (begin
         (define closest-sphere (closest-intersection source direction t_min t_max)) ;; get sphere without radius
         (define distance (cdr closest-sphere))
         (define closest-sphere (car closest-sphere))
         (if (number? closest-sphere) ; if no intersection
             prev-color ; return whatever color was computed before, default is black (no intersections at all)
             (begin
               (define intersection (a-minus-bk source direction (- distance)))
               (define normal (a-minus-bk intersection (car closest-sphere) 1))
               (define closest-sphere (cdr closest-sphere)) ;; get to color
               (define n (dot-product normal normal))
               (define illumination (get-illumination lights ambient-light))
               (define new-color (map (lambda (channel) (* channel 2.833 illumination prev-ref)) (car closest-sphere)))
               (define curr-ref (/ (caddr closest-sphere) 9)) ;; get reflection
               (if (> depth 0)
                   (begin
                     (define new-color (map (lambda (channel) (* channel (- 1 curr-ref))) new-color))
                     (define new-color (sum-lists new-color prev-color))
                     ;; otherwise, shoot another ray from intersection relative to normal to find reflections
                     (trace-ray-iter
                      intersection
                      (a-minus-bk direction normal (/ (* 2 (dot-product normal direction)) n))
                      (/ 1 canv-size)
                      canv-size
                      (- depth 1)
                      new-color
                      (* curr-ref prev-ref)))
                   (sum-lists new-color prev-color))))))

(define (trace-ray direction)
  (trace-ray-iter camera direction 1 canv-size reflection-depth (list 0 0 0) 1))

;;;;;;;;;;;;;;;;;;;; end raytracing ;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Multiprocessing!
;;
;;a non-standard scheme extension.
;;also uses vectors and (non-standard) queues.
;;to bypass all this non-standard code just set n-processors to 1
;;but if you do that you will suffer.


(define* (worker work-Q results-Q)
  ;; Each worker repeatedly gets a line number from work-Q, calculates all
  ;; pixel colors for that line and then puts the array of color
  ;; values in results-Q to be drawn by the main proces.
  (begin
    (define (calc-line x-left y-coor results)
      (if (< x-left half)
          (calc-line (+ x-left pen-size)
                     y-coor
                     (cons (list->vector (trace-ray
                                          (list (/ x-left canv-size)
                                                (/ y-coor canv-size) 1)))
                           results))
          (list->vector results)))

    (define (worker-iter)
      (if (queue-empty? work-Q)
          (begin
            (queue-put results-Q 'done)
            0)
          (let ((y-coor (queue-get work-Q)))
            (queue-put results-Q (vector y-coor (calc-line (- half) y-coor nil)))
            (worker-iter))))

    (worker-iter)))

(define completed-lines 0)
(define* (async-draw-y y-top y-bottom)
  ;; creates a bunch of processes and communicates to them via Queues.
  (begin
    (define results-Q (queue)) ;; used to get color vectors from the workers
    (define work-Q (queue))  ;; used to send y-coordinates to the workers
    (define finished 0)

    (define (spawn-workers num)
      (if (= num 0)
          nil
          (cons (async worker (list work-Q results-Q)) ;; y-bottom ???
                (spawn-workers (- num 1)))))

    (define (init-work-Q top bottom)
      ;;populate the work-Q with the line numbers to be calculated
      (if (< top bottom)
          nil
          (begin
            (queue-put work-Q top)
            (init-work-Q (- top pen-size) bottom))))
    (init-work-Q y-top y-bottom)

    (define workers (spawn-workers n-processors))
    (map async-start workers)

    (define (get-draw-repeat)
      (if (= finished n-processors)
          ;;each time a process is finished it puts the symbol 'done in results-Q
          ;;here we the number of 'done' symbols to know when to quite
          ;;This seems clumsy but I'm not sure how this sort of thing is normally done
          nil
          (begin
            (define results (queue-get results-Q))

            (if (equal? results 'done)
                (begin
                  (set! finished (+ finished 1))
                  (get-draw-repeat))

                (begin
                  (draw-points results)

                  (set! completed-lines (+ completed-lines pen-size))
                  (print (* (/ completed-lines canv-size) 100))

                  (get-draw-repeat))))))

    (get-draw-repeat)))

(define* (draw-points colors)
  ;;COLORS is a vector with format: [y-coor colors]
  ;;Where 'y-coor' is the y-coordinate of the line
  ;;     and 'colors' is a vector of rgb color values (reversed)
  (begin
    (define y-coor (vector-ref colors 0))
    (define colors (vector-ref colors 1))
    (penup)
    (setpos (- half) y-coor)
    (pendown)
    (define draw-point (mu (index)
                           (if (< index 0) nil
                               (begin (set-color
                                       (vector->list (vector-ref colors index)))
                                      (fd pen-size)
                                      (draw-point (- index 1))))))
    (draw-point (- (vector-length colors) 1))
    (update)))

(define (range from to step)
  ;;return a list of all multiples of STEP in the range [FROM, TO], inclusive
  ;;(range 1 5 1) => (1 2 3 4 5)
  ;;(range 1 5 2) => (1 3 5)
  (define (range-iter from to step accum)
    (if (< to from)
        accum
        (range-iter from (- to step) step (cons to accum))))
  (range-iter from
              (if (= (modulo (- to from) step) 0)
                  to
                  (- to (modulo (- to from) step)))
              step
              nil))

;;end multiprocessing code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Drawing procedures
(define (set-color colors) (color (in-255 (car colors))
                                  (in-255 (cadr colors))
                                  (in-255 (caddr colors))))

(define* (draw-y y-top y-bottom)
  (if (> y-top y-bottom)
      (begin
        (setpos (- half) y-top)
        (pendown)
        (draw-x (- half) half)
        (penup)
        (draw-y (- y-top pen-size) y-bottom))))

(define draw-x (mu* (x-left x-right)
                              (if (< x-left x-right)
                                  (begin
                                    (define new-color (trace-ray
                                                       (list (/ x-left canv-size) (/ y-top canv-size) 1)))
                                    (set-color new-color)
                                    (fd pen-size)
                                    (draw-x (+ x-left pen-size) x-right)))))

(define (draw) (speed 0)
  (pensize pen-size)
  (setheading 90)

  (find-spheres)

  (if demo
      (let ((keepers nil))
        (for s in spheres : (if (> (car s) 4) (set! keepers (cons s keepers))))
        (set! spheres keepers)))

  (if (= n-processors 1)
      (draw-y half (- half)) ;;avoid all multiprocessing code
      (async-draw-y half (- half)))
  (exitonclick)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; functions for testing

(define-macro (time-eval code)
  (list 'let '((begin-time (time)))
        code
        '(let ((dif (- (time) begin-time)))
           (if (> dif 60)
               (print (list 'run 'time: (quotient dif 60) 'minutes (modulo dif 60) 'seconds))
               (print (list 'run 'time: dif 'seconds))))))

(define (time-draw)
  (time-eval (draw)))

(print (list 'expansion 'time: (- (time) load-start-time) 'seconds))

; Please leave this last line alone.  You may add additional procedures above
; this line.  All Scheme tokens in this file (including the one below) count
; toward the token limit.
(draw)