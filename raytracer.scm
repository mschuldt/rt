(define canv-size 100)
(define dot-size 1)
(define n-processors 8) ;; must be >= 1

;;run times for 4 spheres:
;;200/10 (run time: 4.971529722213745 seconds)

;;;run times for 64 spheres:
;; 200/10 (run time: 2 minutes 7.498538970947266 seconds)


(define basic #f) ;;set to true to render the basic 4 sphere scene

(define lights '((10 (0 0 -50))
                 (10 (0 0 -20))))

(if basic
    (define lights (list (list 8 (list 2 2 0))))
    )

(define camera (list -15 5 -80))
(if basic
    (define camera (list 0 1 0)))

(define reflection-depth 2)

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
;;          replace 'sym' with 'value'
;;    OR:  ((name args) def)
;;         (replace call to 'name' with 'def', substituting 'args'
;;
;; Calling other substitions is OK
;; but recursion or mutual recursion is not
;;
;; Example:
;;  with substitution definition:
;;       '((set-color colors) (color (in-255 (car colors))
;;                            (in-255 (cadr colors))
;;                            (in-255 (caddr colors))))
;;
;; the code:
;;   (set-color red)
;;
;; will be expanded as:
;;   (color (min 255 (car red))
;;          (min 255 (car (cdr red)))
;;          (min 255 (car (cdr (cdr red)))))
;;
;;
;; Substitutions are only expanded with macros `define*' and `mu*'


(define substitutions
  (list '((cadr x) (car (cdr x)))
        '((caddr x) (car (cdr (cdr x))))
        '((cadddr x) (caddr (cdr x)))
        '((square x) (* x x))
        (list  'half (/ canv-size 2))
        '((in-255 value) (min 255 value))
        '((set-color colors) (color (in-255 (car colors))
                                    (in-255 (cadr colors))
                                    (in-255 (caddr colors))))
        '((dot-product a b) ;for some reason works faster than tail recursive implementation
          (+
           (* (car a) (car b))
           (* (cadr a) (cadr b))
           (* (caddr a) (caddr b))))

        '((a-minus-bk a b k)
          (list
           (- (car a) (* (car b) k))
           (- (cadr a) (* (cadr b) k))
           (- (caddr a) (* (caddr b) k))))

        ;;circles


        '((curvature c) (/ 1 (radius c)))
        '((make-circle center radius color reflection)
          (list radius center color reflection))
        ;;complex numbers
        '((complex real imag)
          (list real imag))
        '((real z) (car z))
        '((imag z) (cadr z))
        '((magnitude z) (sqrt (+ (* (real z) (real z)) (* (imag z) (imag z)))))

        '((abs n)
          (if (< n 0) (- n) n))

        ))

(define (center c) (cadr c))
(define (x-coor c) (car (center c)))
(define (y-coor c) (cadr (center c)))


(define (radius c) (car c)) ;;conflict

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sphere: radius (cx  cy  cz) R  G  B specular_exponent reflectiveness


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;sphere generation

(define circles nil)


(define (circle-color c) (car (cdr (cdr c))))
(define (reflectiveness c) (car (cdr (cdr (cdr c)))))

(define (make-circle center radius color reflection)
  (list radius center  color reflection))


;; fun fact:
;; if you do make substitutions for c+ and c*,
;; it will take 1 hour 45min to finish
;; the the resulting code will have 855766 tokens
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


;;return list of all triples of mutually tangental circles
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
  (let ((c1 (make-circle (list 0 (/ (* 250 (sqrt 3)) 3)) 125 (car color-list) 7 ))
        (c2 (make-circle (list 125 (- (/ (* 125 (sqrt 3)) 3))) 125 (car color-list) 7))
        (c3 (make-circle (list -125 (- (/ (* 125 (sqrt 3)) 3))) 125 (car color-list) 7))
        (c4 (make-circle '(0 0) (- (+ 125 (/ (* 250 (sqrt 3)) 3))) (car color-list) 7)))
    (print "finding spheres...")
    (define (find circles tangencies level)
      (if (< level circle-depth)
          (let ((new-circles (find-tangent-circles tangencies level)))
            (find (append circles (map (lambda (lst) (cadddr lst)) new-circles))
                  (find-new-tangencies new-circles)
                  (+ level 1)))))

    (find (list c1 c2 c3 c4)
          (list (list c1 c2 c3) (list c1 c2 c4) (list c1 c3 c4) (list c2 c3 c4)) ;;lists of tangent circles
          0)

    (print "filtering good spheres...")
    (filter-circles)
    (set! spheres (convert))
    (print (list 'rendering (length spheres) 'spheres))
    ))

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
                    (make-circle z4 (/ 1 k4) (get-color level) 7)
                    (make-circle z42 (/ 1 k4) (get-color level) 7)))

    ;;throw out circles that are to far from the origin
    ;;or have a radius that is too small
    ;;to prevent massive circle accumulation
    (if (or (> (magnitude (center new)) 72)
            (< (radius new) 0.3))

        nil
        (begin
          (set! circles (cons new circles))
          (list c1 c2 c3 new)))))





(define good (list (make-circle (list 125 (- (/ (* 125 (sqrt 3)) 3))) 125 '(8.0 2.0 10.0) 7)))

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
             ;;TODO: throw out the small ones
             (set! good (cons c good)))))


(define* (convert)
  (begin
    (define out nil)
    (for c in good :
         (set! out (cons (list (radius c)
                               (list
                                ;;(- (y-coor c)) ;;refect 90deg
                                ;;(x-coor c)
                                (- (x-coor c))
                                (y-coor c)
                                0)
                               (circle-color c)
                               9
                               (reflectiveness c))
                         out)))
    out))



;;end sphere generation code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spheres (list
                 (list canv-size (list 0 (- canv-size) 0)  (list 9 9 0)  canv-size  2)  ;; Yellow sphere
                 (list 1 (list 0  0 3)  (list 9 0 0)  canv-size  3)  ;; Red sphere
                 (list 1 (list -2  1 4)  (list 0 9 0)  9  4)  ;; Green sphere
                 (list 1 (list 2  1 4)  (list 0 0 9)  canv-size  5)   ;; Blue sphere
                 ))


(define half (/ canv-size 2))

(define closest-intersection (mu* (source direction t_min t_max) ; MU Procedure I LOVE YOU <3
                                 (get-closest-sphere 0 spheres)))

(define (sum-lists a b) ; have to be equal size
  (if (null? a) nil (cons (+ (car a) (car b)) (sum-lists (cdr a) (cdr b)))))


(define get-closest-sphere
  (mu* (v spheres-list)
      (if (null? spheres-list)
          v
          (begin
            (define curr-sphere (car spheres-list))
            (define radius (car curr-sphere))
            (define curr-sphere (cdr curr-sphere))
            (define j (a-minus-bk source (car curr-sphere) 1))
            (define a (* 2 (dot-product direction direction)))
            (define b (* -2 (dot-product j direction)))
            (define discr (- (square b) (* 2 a (- (dot-product j j) (square radius)))))
            (if (> discr 0)
                (begin
                  (define discr (sqrt discr))
                  (define sol1 (/ (- b discr) a))
                  (if (and (< t_min sol1) (< sol1 t_max) (< sol1 min-dist))
                      (begin
                        (define v curr-sphere)
                        (set! min-dist sol1))) ;set is because mu
                  (define sol2 (/ (- b (- discr)) a))
                  (if (and (< t_min sol2) (< sol2 t_max) (< sol2 min-dist))
                      (begin
                        (define v curr-sphere)
                        (set! min-dist sol2))) ;set is because mu
                  ))
            (get-closest-sphere v (cdr spheres-list))))))

(define get-illumination
  (mu* (lights-list illumination)
      (if (null? lights-list)
          illumination
          (begin
            (define light (car lights-list))
            (define light-vector (a-minus-bk (cadr light) intersection 1))
            (print (cdr light))
            (define k (dot-product normal light-vector))
            (define m (a-minus-bk light-vector normal (/ (* 2 k) n)))
            (define min-dist canv-size)
            (define illumination (+ illumination
                                    (*
                                     (if (number? (closest-intersection intersection light-vector (/ 1 canv-size) 1)) 1 0)
                                     (car light)
                                     (+
                                      (max 0 (/ k (sqrt (* (dot-product light-vector light-vector) n))))
                                      (max 0 (pow
                                              (/
                                               (dot-product m direction)
                                               (sqrt (* (dot-product m m) (dot-product direction direction))))
                                              (cadr closest-sphere))))))) ;; get specular_exponent 4th element
            (get-illumination (cdr lights-list) illumination)))))


(define trace-ray-iter
  (mu* (source direction t_min t_max depth prev-color prev-ref) ;; tail-recursive fuck yeah!!!
       (begin
         (define min-dist canv-size)
         (define closest-sphere (closest-intersection source direction t_min t_max)) ;; get sphere without radius
         (if (number? closest-sphere) ; if no intersection
             prev-color
             (begin
               (define intersection (a-minus-bk source direction (- min-dist)))
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
                     (trace-ray-iter
                      intersection
                      (a-minus-bk direction normal (/ (* 2 (dot-product normal direction)) n))
                      (/ 1 canv-size)
                      canv-size
                      (- depth 1)
                      new-color
                      (* curr-ref prev-ref)))
                   (sum-lists new-color prev-color)))))))

(define (trace-ray source direction t_min t_max depth)
  (trace-ray-iter source direction t_min t_max depth (list 0 0 0) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;multiprocessing

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


(define* (worker work-Q results-Q)
  (begin
    (define (calc-line x-left y-coor results)
      (if (< x-left half)
          (calc-line (+ x-left dot-size)
                     y-coor
                     (cons (list->vector (trace-ray
                                          camera
                                          (list (/ x-left canv-size)
                                                (/ y-coor canv-size) 1)
                                          1
                                          canv-size
                                          reflection-depth
                                          ))
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
  ;;this version starts processing the next batch of lines
  ;;as it draws the results from the previous batch
  ;;=> This is faster then normal but still slower then doing  all calculations first
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
            (init-work-Q (- top dot-size) bottom))))
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

                  (set! completed-lines (+ completed-lines dot-size))
                  (print (* (/ completed-lines canv-size) 100))

                  (get-draw-repeat))))))

    (get-draw-repeat)))

(define* (draw-points colors)
  ;; Draws points with lines
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
                                      (fd dot-size)
                                      (draw-point (- index 1))))))
    (draw-point (- (vector-length colors) 1))
    (update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define* (draw-y y-top y-bottom)
  (if (> y-top y-bottom)
      (begin
        (setpos (- half) y-top)
        (pendown)
        (draw-x (- half) half)
        (penup)
        (draw-y (- y-top dot-size) y-bottom))))

(define draw-x (mu* (x-left x-right)
                              (if (< x-left x-right)
                                  (begin
                                    (define new-color (trace-ray
                                                       camera
                                                       (list (/ x-left canv-size) (/ y-top canv-size) 1)
                                                       1
                                                       canv-size
                                                       reflection-depth
                                                       ))
                                    (set-color new-color)
                                    (fd dot-size)
                                    (draw-x (+ x-left dot-size) x-right)))))

;;NOTE: (exitonclick) commented out just for testing
(define (normal-draw)
  (speed 0)
  (pensize dot-size)
  (if basic nil (find-spheres))
  (draw-y half (- half)) ;(exitonclick)
  )

(define (fast-draw) (speed 0)
  (pensize dot-size)
  (setheading 90)
  (if basic nil (find-spheres))
  (async-draw-y half (- half)) ;(exitonclick)
  )

(define (draw)
  (if (= n-processors 1)
      (normal-draw)
      (fast-draw)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; testing functions

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
