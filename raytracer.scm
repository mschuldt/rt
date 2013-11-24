(define canv-size 300)
(define dot-size 5)

;; Sphere: radius (cx  cy  cz) R  G  B specular_exponent reflectiveness
(define spheres (list
	(list canv-size (list 0 (- canv-size) 0)  (list 9 9 0)  canv-size  2)  ;; Yellow sphere
	(list 1 (list 0  0 3)  (list 9 0 0)  canv-size  3)  ;; Red sphere
	(list 1 (list (- 2)  1 4)  (list 0 9 0)  9  4)  ;; Green sphere
	(list 1 (list 2  1 4)  (list 0 0 9)  canv-size  5)   ;; Blue sphere
	))
(define lights (list (list 8 (list 2 2 0)))) ;; always even size
(define camera (list 0 1 0))
(define ambient-light 2)
(define lights-length (length lights)) ;some optimization
(define (set-dot x y colors) (setpos x y) (dot dot-size
	(in-255 (car colors))
	(in-255 (cadr colors))
	(in-255 (caddr colors))))

(define (cadr x)
  (car (cdr x)))
(define (caddr x)
  (car (cdr (cdr x))))

(define (in-255 value) (min 255 value))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (sum-lists a b) ; have to be equal size
	(if (null? a) nil (cons (+ (car a) (car b)) (sum-lists (cdr a) (cdr b)))))

(define (square x) (* x x))

(define (dot-product a b) ;for some reason works faster than tail recursive implementation
	(+
		(* (car a) (car b))
		(* (cadr a) (cadr b))
		(* (caddr a) (caddr b))))

(define (a-minus-bk a b k)
	(list
		(- (car a) (* (car b) k))
		(- (cadr a) (* (cadr b) k))
		(- (caddr a) (* (caddr b) k))))

(define closest-intersection (mu (source direction t_min t_max) ; MU Procedure I LOVE YOU <3
	(get-closest-sphere 0 spheres)))

(define get-closest-sphere
  (mu (v spheres-list)
      (if (null? spheres-list)
          v
          (begin
            (define curr-sphere (car spheres-list))
            (define radius (car curr-sphere))
            (define curr-sphere (cdr curr-sphere))
            (define j (a-minus-bk source (car curr-sphere) 1))
            (define a (* 2 (dot-product direction direction)))
            (define b (- (* 2 (dot-product j direction))))
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
  (mu (lights-list illumination)
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


(define (trace-ray source direction t_min t_max depth prev-color prev-ref) ;; tail-recursive fuck yeah!!!
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
			(define curr-ref (/ (caddr closest-sphere) 9)) ;; get reflectance
			(if (> depth 0)
				(begin
					(define new-color (map (lambda (channel) (* channel (- 1 curr-ref))) new-color))
					(define new-color (sum-lists new-color prev-color))
					(trace-ray
						intersection
						(a-minus-bk direction normal (/ (* 2 (dot-product normal direction)) n))
						(/ 1 canv-size)
						canv-size
						(- depth 1)
						new-color
						(* curr-ref prev-ref)))
				(sum-lists new-color prev-color)))))

(define half (/ canv-size 2))
(define (draw-y y-top y-bottom)
	(if (> y-top y-bottom)
		(begin
			(draw-x (- half) half)
			(draw-y (- y-top dot-size) y-bottom))))

(define draw-x (mu (x-left x-right)
                   (if (< x-left x-right)
                       (begin
                         (set-dot
                          x-left
                          y-top
                          (trace-ray
                           camera
                           (list (/ x-left canv-size) (/ y-top canv-size) 1)
                           1
                           canv-size
                           2 ; reflection depth
                           (list 0 0 0)
                           1))
                         (draw-x (+ x-left dot-size) x-right)))))

(define (draw) (speed 0) (penup) (draw-y half (- half)) (exitonclick))


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




