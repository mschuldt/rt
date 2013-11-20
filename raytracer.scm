(define canv-width 20)
;; Sphere: radius (cx  cy  cz) R  G  B specular_exponent reflectiveness
(define spheres (list
	canv-width (list 0 (- canv-width) 0)  9 9 0  canv-width  2  ;; Yellow sphere
	1 (list 0  0 3)  9 0 0  canv-width  3  ;; Red sphere
	1 (list (- 2)  1 4)  0 9 0  9  4  ;; Green sphere
	1 (list 2  1 4)  0 0 9  canv-width  5   ;; Blue sphere
	))

(define ambient-light 2)
(define lights (list 8 (list 2 2 0))) ;; always even size
(define (set-dot x y r g b) (setpos x y) (dot 1 r g b))
(penup)

(define (cadr x)
  (car (cdr x)))
(define (caddr x)
  (car (cdr (cdr x))))
(define (cadddr x)
  (car (cdr (cdr (cdr x)))))

(define (get-by-index alist start index list-length)
		(cond
			((>= index list-length) nil)
			((= start index) (car alist))
			(else (get-by-index (cdr alist) (+ start 1) index list-length))))

(define (spheres-by-index index)
	(get-by-index spheres 0 index (length spheres)))

(define (lights-by-index index)
	(get-by-index lights 0 index (length lights)))

(define (max a b)
	(if (>= a b) a b))

(define (square x) (* x x))

(define (pow b n)
  (if (= n 0)
    1
    (if (odd? n)
      (* b (pow b (- n 1)))
      (square (pow b (/ n 2))))))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

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
	(define min-dist canv-width)
	(define (closest-sphere-index v q)
		(cond
			((null? (spheres-by-index q)) v)
			(else
				(define radius (spheres-by-index q))
				(define q (+ q 1))
				(define j (a-minus-bk source (spheres-by-index q) 1))
				(define a (* 2 (dot-product direction direction)))
				(define b (- (* 2 (dot-product j direction))))
				(define discr (- (square b) (* 2 a (- (dot-product j j) (square radius)))))
				(if (>= discr 0) (begin
					(define discr (sqrt discr))
					(define sol1 (/ (- b discr) a))
					(if (and (< t_min sol1) (< sol1 t_max) (< sol1 min-dist))(begin
						(set! v q)
						(set! min-dist sol1)))
					(define sol2 (/ (- b (- discr)) a))
					(if (and (< t_min sol2) (< sol2 t_max) (< sol2 min-dist))(begin
						(set! v q)
						(set! min-dist sol2)))
					))
				(closest-sphere-index v (+ q 6)))))
	(closest-sphere-index 0 0)))


(define trace-ray (mu (source direction t_min t_max depth) ; MU Procedure I LOVE YOU <3
	(define closest-sphere (closest-intersection source direction t_min t_max))
	(if (= 0 closest-sphere) 0 (begin
		(define intersection (a-minus-bk source direction (- min-dist)))
		(define normal (a-minus-bk intersection (spheres-by-index closest-sphere) 1))
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
							(closest-intersection intersection light-vector (/ 1 canv-width) 1)
							intensity
							(+
								(max 0 (/ k (sqrt (* (dot-product light-vector light-vector) n))))
								(max 0 (pow
									(/
										(dot-product m direction)
										(sqrt (* (dot-product m m) (dot-product direction direction))))
									(spheres-by-index (+ closest-sphere 4))))))))
					(get-illumination (+ index 1) illumination)))))
		(define local-color (*
			(spheres-by-index (+ closest-sphere color-channel))
			(get-illumination 0 ambient-light)
			2.8))
		(define reflection (/ (spheres-by-index (+ closest-sphere 5)) 9))
		(if (> depth 0)
			(+ (*
				(trace-ray
					intersection
					(a-minus-bk direction normal (* 2 (/ (dot-product normal direction) n)))
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
			(define color-channel 1)
			(define r (trace-ray ;; thank you mu!
				(list 0 1 0)
				(list (/ x-left canv-width) (/ y-top canv-width) 1)
				1
				canv-width
				2))
			(define color-channel 2)
			(define g (trace-ray ;; thank you mu!
				(list 0 1 0)
				(list (/ x-left canv-width) (/ y-top canv-width) 1)
				1
				canv-width
				2))
			(define color-channel 3)
			(define b (trace-ray ;; thank you mu!
				(list 0 1 0)
				(list (/ x-left canv-width) (/ y-top canv-width) 1)
				1
				canv-width
				2))
			(set-dot x-left y-top r g b)
			(draw-x (+ x-left 1) x-right)))))

(draw-y half (- half))







