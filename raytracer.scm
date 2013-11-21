(define canv-width 100)
;; Sphere: radius (cx  cy  cz) R  G  B specular_exponent reflectiveness
(define spheres (list
	(list canv-width (list 0 (- canv-width) 0)  (list 9 9 0)  canv-width  2)  ;; Yellow sphere
	(list 1 (list 0  0 3)  (list 9 0 0)  canv-width  3)  ;; Red sphere
	(list 1 (list (- 2)  1 4)  (list 0 9 0)  9  4)  ;; Green sphere
	(list 1 (list 2  1 4)  (list 0 0 9)  canv-width  5)   ;; Blue sphere
	))
(define spheres-length (length spheres)) ;some optimization
(define camera (list 0 1 0))
(define ambient-light 2)
(define lights (list 8 (list 2 2 0))) ;; always even size
(define lights-length (length lights)) ;some optimization
(define (set-dot x y r g b) (setpos x y) (dot 1 r g b))
(penup)

(define (cadr x)
  (car (cdr x)))
(define (caddr x)
  (car (cdr (cdr x))))

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
	(define (closest-sphere-index v q)
		(cond
			((null? (spheres-by-index q)) v)
			(else
				(define sphere (spheres-by-index q))
				(define radius (car sphere))
				(define j (a-minus-bk source (cadr sphere) 1))
				(define a (* 2 (dot-product direction direction)))
				(define b (- (* 2 (dot-product j direction))))
				(define discr (- (square b) (* 2 a (- (dot-product j j) (square radius)))))
				(if (> discr 0) (begin
					(define discr (sqrt discr))
					(define sol1 (/ (- b discr) a))
					(if (and (< t_min sol1) (< sol1 t_max) (< sol1 min-dist))
						(begin
							(define v q)
							(set! min-dist sol1)))
					(define sol2 (/ (- b (- discr)) a))
					(if (and (< t_min sol2) (< sol2 t_max) (< sol2 min-dist))
						(begin
							(define v q)
							(set! min-dist sol2)))
					))
				(closest-sphere-index v (+ q 1)))))
	(closest-sphere-index (- 1) 0)))


(define trace-ray (mu (source direction t_min t_max depth) ; MU Procedure I LOVE YOU <3
	(define min-dist canv-width)
	(define closest-sphere (closest-intersection source direction t_min t_max)) ;; get index
	(if (= (- 1) closest-sphere) 0 (begin
		(define closest-sphere (spheres-by-index closest-sphere)) ;; get actual sphere list
		(define intersection (a-minus-bk source direction (- min-dist)))
		(define normal (a-minus-bk intersection (cadr closest-sphere) 1))
		(define n (dot-product normal normal))
		(define (get-illumination index illumination)
			(cond
				((null? (lights-by-index index)) illumination)
				(else (begin
					(define intensity (lights-by-index index))
					(define index (+ index 1))
					(define light-vector (a-minus-bk (lights-by-index index) intersection 1))
					(define k (dot-product normal light-vector))
					(define m (a-minus-bk light-vector normal (/ (* 2 k) n)))
					(define illumination (+ illumination
						(*
							(if (= -1 (closest-intersection intersection light-vector (/ 1 canv-width) 1)) 1 0)
							intensity
							(+
								(max 0 (/ k (sqrt (* (dot-product light-vector light-vector) n))))
								(max 0 (pow
									(/
										(dot-product m direction)
										(sqrt (* (dot-product m m) (dot-product direction direction))))
									(param-by-index closest-sphere 3))))))) ;; get specular_exponent 4th element
					(get-illumination (+ index 1) illumination)))))
		(define color (caddr closest-sphere))
		(define local-color (*
			(color-channel color)
			(get-illumination 0 ambient-light)
			2.833))
		(define reflection (/ (param-by-index closest-sphere 4) 9)) ;; get reflectance
		(if (> depth 0)
			(+
				(*
					(trace-ray
						intersection
						(a-minus-bk direction normal (/ (* 2 (dot-product normal direction)) n))
						(/ 1 canv-width)
						canv-width
						(- depth 1))
					reflection)
				(* local-color (- 1 reflection)))
			local-color)))))

(define half (/ canv-width 2))
(define draw-y (mu (y-top y-bottom)
	(if (> y-top y-bottom)
		(begin
			(draw-x (- half) half)
			(draw-y (- y-top 1) y-bottom)))))

(define draw-x (mu (x-left x-right)
	(if (< x-left x-right)
		(begin
			(define color-channel car)
			(define direct (list (/ x-left canv-width) (/ y-top canv-width) 1))
			(define r (trace-ray camera direct 1 canv-width 2))
			(define color-channel cadr)
			(define g (trace-ray camera direct 1 canv-width 2))
			(define color-channel caddr)
			(define b (trace-ray camera direct 1 canv-width 2))
			(set-dot x-left y-top (if (> r 255) 255 r) (if (> g 255) 255 g) (if (> b 255) 255 b))
			(draw-x (+ x-left 1) x-right)))))

(define (draw) (draw-y half (- half)) (exitonclick))
