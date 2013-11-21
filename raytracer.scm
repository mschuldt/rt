(define canv-size 200)
(define dot-size 5)
;; Sphere: radius (cx  cy  cz) R  G  B specular_exponent reflectiveness
(define spheres (list
	(list canv-size (list 0 (- canv-size) 0)  (list 9 9 0)  canv-size  2)  ;; Yellow sphere
	(list 1 (list 0  0 3)  (list 9 0 0)  canv-size  3)  ;; Red sphere
	(list 1 (list (- 2)  1 4)  (list 0 9 0)  9  4)  ;; Green sphere
	(list 1 (list 2  1 4)  (list 0 0 9)  canv-size  5)   ;; Blue sphere
	))
(define spheres-length (length spheres)) ;some optimization
(define camera (list 0 1 0))
(define ambient-light 2)
(define lights (list (list 8 (list 2 2 0)))) ;; always even size
(define lights-length (length lights)) ;some optimization
(define (set-dot x y colors) (setpos x y) (dot dot-size
	(to-255 (car colors))
	(to-255 (cadr colors))
	(to-255 (caddr colors))))

(define (cadr x)
  (car (cdr x)))
(define (caddr x)
  (car (cdr (cdr x))))

(define (to-255 value) (min 255 value))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(define (sum-lists a b) ; have to be equal size
	(if (null? a) nil (cons (+ (car a) (car b)) (sum-lists (cdr a) (cdr b)))))

(define (get-by-index alist start index list-length)
		(cond
			((>= index list-length) nil)
			((= start index) (car alist))
			(else (get-by-index (cdr alist) (+ start 1) index list-length))))

(define (spheres-by-index index)
	(get-by-index spheres 0 index spheres-length))

(define (param-by-index sphere index)
	(get-by-index sphere 0 index 7))

(define (lights-by-index index)
	(get-by-index lights 0 index lights-length))

(define (square x) (* x x))

(define (dot-product a b)
	(+
		(* (car a) (car b))
		(* (cadr a) (cadr b))
		(* (caddr a) (caddr b))))

(define (a-minus-bk a b k)
	(list
		(- (car a) (* (car b) k))
		(- (cadr a) (* (cadr b) k))
		(- (caddr a) (* (caddr b) k))))


(define closest-intersection (mu (source direction t_min t_max)
	(define (get-closest-sphere v q)
		(cond
			((null? (spheres-by-index q)) v)
			(else
				(define curr-sphere (spheres-by-index q))
				(define radius (car curr-sphere))
				(define j (a-minus-bk source (cadr curr-sphere) 1))
				(define a (* 2 (dot-product direction direction)))
				(define b (- (* 2 (dot-product j direction))))
				(define discr (- (square b) (* 2 a (- (dot-product j j) (square radius)))))
				(if (> discr 0) (begin
					(define discr (sqrt discr))
					(define sol1 (/ (- b discr) a))
					(if (and (< t_min sol1) (< sol1 t_max) (< sol1 min-dist))
						(begin
							(define v curr-sphere)
							(set! min-dist sol1)))
					(define sol2 (/ (- b (- discr)) a))
					(if (and (< t_min sol2) (< sol2 t_max) (< sol2 min-dist))
						(begin
							(define v curr-sphere)
							(set! min-dist sol2)))
					))
				(get-closest-sphere v (+ q 1)))))
	(get-closest-sphere 0 0)))


(define (trace-ray source direction t_min t_max depth) ; MU Procedure I LOVE YOU <3
	(define min-dist canv-size)
	(define closest-sphere (closest-intersection source direction t_min t_max)) ;; get sphere
	(if (number? closest-sphere) ; if no intersection
		(list 0 0 0)
		(begin
			(define intersection (a-minus-bk source direction (- min-dist)))
			(define normal (a-minus-bk intersection (cadr closest-sphere) 1))
			(define n (dot-product normal normal))
			(define (get-illumination index illumination)
				(define light (lights-by-index index))
				(cond
					((null? light) illumination)
					(else (begin
						(define intensity (car light))
						(define light-vector (a-minus-bk (cadr light) intersection 1))
						(define k (dot-product normal light-vector))
						(define m (a-minus-bk light-vector normal (/ (* 2 k) n)))
						(define illumination (+ illumination
							(*
								(if (number? (closest-intersection intersection light-vector (/ 1 canv-size) 1)) 1 0)
								intensity
								(+
									(max 0 (/ k (sqrt (* (dot-product light-vector light-vector) n))))
									(max 0 (pow
										(/
											(dot-product m direction)
											(sqrt (* (dot-product m m) (dot-product direction direction))))
										(param-by-index closest-sphere 3))))))) ;; get specular_exponent 4th element
						(get-illumination (+ index 1) illumination)))))
			(define illumination (get-illumination 0 ambient-light))
			(define local-color (map (lambda (channel) (* channel 2.833 illumination)) (caddr closest-sphere)))
			(define reflection (/ (param-by-index closest-sphere 4) 9)) ;; get reflectance
			(if (> depth 0)
				(begin
					(define new-ray (map (lambda (channel) (* channel reflection))
						(trace-ray
							intersection
							(a-minus-bk direction normal (/ (* 2 (dot-product normal direction)) n))
							(/ 1 canv-size)
							canv-size
							(- depth 1))))
					(define local-color (map (lambda (channel) (* channel (- 1 reflection))) local-color))
					(sum-lists new-ray local-color))
				local-color))))

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
					(list (/ x-left canv-size)(/ y-top canv-size) 1)
					1
					canv-size
					2))
			(draw-x (+ x-left dot-size) x-right)))))

(define (draw) (penup) (draw-y half (- half)) (exitonclick))
