(define canv-size 300)
(define dot-size 5)
(define n-processors 8) ;; must be >= 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substititions
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
;;   (set-color "red")
;;
;; will be expanded as:
;;   (color (min 255 (car "red"))
;;          (min 255 (car (cdr "red")))
;;          (min 255 (car (cdr (cdr "red")))))
;;
;;
;; Substitutions are only expanded with macros `define-with-subst' and `mu-with-subst'
;;
(define substitutions
  (list '((cadr x) (car (cdr x)))
        '((caddr x) (car (cdr (cdr x))))
        '((square x) (* x x))
        (list  'half (/ canv-size 2))
        '((in-255 value) (min 255 value))
        '((set-color colors) (color (in-255 (car colors))
                                    (in-255 (cadr colors))
                                    (in-255 (caddr colors))))
        '((set-dot x y colors) (begin (setpos x y) (dot dot-size
                                                        (in-255 (car colors))
                                                        (in-255 (cadr colors))
                                                        (in-255 (caddr colors)))))
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

        ))

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
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))


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

(define-macro (define-with-subst formals body)
  (list 'define formals 
	(make-substitutions substitutions body)))

(define-macro (mu-with-subst formals body)
  (list 'mu formals 
	(make-substitutions substitutions body)))


(print "loading and expanding...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sphere: radius (cx  cy  cz) R  G  B specular_exponent reflectiveness
(define spheres (list
                 (list canv-size (list 0 (- canv-size) 0)  (list 9 9 0)  canv-size  2)  ;; Yellow sphere
                 (list 1 (list 0  0 3)  (list 9 0 0)  canv-size  3)  ;; Red sphere
                 (list 1 (list -2  1 4)  (list 0 9 0)  9  4)  ;; Green sphere
                 (list 1 (list 2  1 4)  (list 0 0 9)  canv-size  5)   ;; Blue sphere
                 ))

(define lights (list (list 8 (list 2 2 0)))) ;; always even size
(define camera (list 0 1 0))
(define ambient-light 2)

(define half (/ canv-size 2))

(define closest-intersection (mu-with-subst (source direction t_min t_max) ; MU Procedure I LOVE YOU <3
                                 (get-closest-sphere 0 spheres)))

(define (sum-lists a b) ; have to be equal size
  (if (null? a) nil (cons (+ (car a) (car b)) (sum-lists (cdr a) (cdr b)))))


(define get-closest-sphere
  (mu-with-subst (v spheres-list)
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
  (mu-with-subst (lights-list illumination)
      (if (null? lights-list)
          illumination
          (begin
            (define light (car lights-list))
            (define light-vector (a-minus-bk (cadr light) intersection 1))
            (define k (dot-product normal light-vector))
            (define m (a-minus-bk light-vector normal (/ (* 2 k) n)))
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
  (mu-with-subst (source direction t_min t_max depth prev-color prev-ref) ;; tail-recursive fuck yeah!!!
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


(define-with-subst (worker y-coor)
  (begin
    (define ret nil)
    (define sub-contractor
      (mu (x-left)
          (if (< x-left half)
              (begin
                (set! ret (cons (list->vector (trace-ray
                                               camera
                                               (list (/ x-left canv-size)
                                                     (/ y-coor canv-size) 1)
                                               1
                                               canv-size
                                               2 ;reflection depth
                                               )) 
                                ret))
                (sub-contractor (+ x-left dot-size))))))
    
    (sub-contractor (- half))
    (vector y-coor (list->vector ret))))

(define-with-subst (async-draw-y y-top y-bottom previous-lines)
  ;;this version starts processing the next batch of lines
  ;;as it draws the results from the previous batch
  ;;=> This is faster then normal but still slower then doing  all calculations first
  (begin
    (define (spawn-workers y-coor num)
      (if (or (= num 0)
              (< y-coor y-bottom))
          nil
          (cons (async worker (list y-coor))
                (spawn-workers (- y-coor dot-size) (- num 1)))))
    (if (> y-top y-bottom)
        (begin
          (define workers (spawn-workers y-top n-processors))
          (map async-start workers)
          (map draw-points previous-lines)
          
          (set! completed-lines (+ completed-lines (* n-processors dot-size)))
          (print (* (/ completed-lines canv-size) 100))
          
          (async-draw-y (- y-top (* dot-size n-processors))
                        y-bottom
                        (map async-get workers)))
        (map draw-points previous-lines))))



(define-with-subst (draw-points colors)
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

(define-with-subst (draw-y y-top y-bottom)
  (if (> y-top y-bottom)
      (begin
        (setpos (- half) y-top)
        (pendown)
        (draw-x (- half) half)
        (penup)
        (draw-y (- y-top dot-size) y-bottom))))

(define draw-x (mu-with-subst (x-left x-right)
                              (if (< x-left x-right)
                                  (begin
                                    (define new-color (trace-ray
                                                       camera
                                                       (list (/ x-left canv-size) (/ y-top canv-size) 1)
                                                       1
                                                       canv-size
                                                       2 ; reflection depth
                                                       ))
                                    (set-color new-color)
                                    (fd dot-size)
                                    (draw-x (+ x-left dot-size) x-right)))))

;;NOTE: (exitonclick) commented out just for testing
(define (normal-draw) (speed 0) (penup) (pensize dot-size) (draw-y half (- half)) ;(exitonclick)
  )
(define (fast-draw) (speed 0) (penup) (pensize dot-size) (setheading 90) (async-draw-y half (- half) nil) ;(exitonclick)
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
