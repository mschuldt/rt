(define canv-size 300)
(define dot-size 5)
(define n-processors 8) ;; must be >= 1


;; Runtimes with a Intel® Core™ i7-2600K CPU @ 3.40GHz × 8
;;;
;;with canv-size=300, dot-size=5
;;processes   time
;; 1:       (run time: 1 minutes 26.63226556777954 seconds)
;; 2:       (run time: 47.16749954223633 seconds)
;; 3:       (run time: 34.66832447052002 seconds)
;; 4:       (run time: 32.659693002700806 seconds)
;; 6:       (run time: 28.001237154006958 seconds)
;; 8:       (run time: 25.460663080215454 seconds)
;; 10:      (run time: 25.95758295059204 seconds)
;; 150:     (run time: 25.428292989730835 seconds)
;; 300:     (run time: 25.336638689041138 seconds)
;; '600'    (run time: 25.634010791778564 seconds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;with canv-size=300, dot-size=10
;; 1: (run time: 20.87380290031433 seconds)
;; 6: (run time: 6.624558687210083 seconds)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;with canv-size=300, dot-size=2
;; 1:  (run time: 9 minutes 11.588381052017212 seconds)
;; 8:  (run time: 2 minutes 52.476383447647095 seconds)
;;
;;
;;canv-size=500, dot-size=1
;;8: (run time: 81 minutes 37.223453521728516 seconds)

;;function expansion
(define-macro (expand-rcalls def ntimes)
  (append  (list (car def))
           (list (cadr def))
           (expand-in (car (car (cdr def))) ;;name
                      (cdr (car (cdr def))) ;;args
                      (cdr (cdr def)) ;;body
                      (cdr (cdr def)) ;;form
                      ntimes)))

(define-macro (expand-mu-rcalls name def ntimes)
  (append '(mu)
          (list (car (cdr def)))
          (expand-in name  ;;name
                     (car (cdr def)) ;;args
                     (cdr (cdr def)) ;;body
                     (cdr (cdr def)) ;;form
                     ntimes)))

(define (zip a b)
  (if (or (null? a) (null? b))
      nil
      (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))

(define (expand-in name args body form ntimes)
  (if (and (list? form)
           (> ntimes 0))
      (if (equal? (car form) name)
          ;;expand
          (append '(begin) (map (lambda (x)
                                  (list 'set! (car x) (cdr x)))
                                (zip args (cdr form)))
                  (expand-in name args body body (- ntimes 1))) ;;or (,@body?

          ;;else, attempt to expand body
          (map (lambda (bod)
                 (expand-in name args body bod ntimes))
               form))
      form))

(expand-rcalls
 (define (fact n)
   (if (= n 0)
       1
       (* n (fact (- n 1)))))
 5)

(define fact2
  (expand-mu-rcalls
   fact2
   (mu (n)
       (if (= n 0)
           1
           (* n (fact2 (- n 1)))))
   5))
;;end function expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(define (set-dot x y colors) (setpos x y) (dot dot-size
                                               (in-255 (car colors))
                                               (in-255 (cadr colors))
                                               (in-255 (caddr colors))))

(define (set-color colors) (color (in-255 (car colors))
                                  (in-255 (cadr colors))
                                  (in-255 (caddr colors))) )

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


(define trace-ray-iter (mu (source direction t_min t_max depth prev-color prev-ref) ;; tail-recursive fuck yeah!!!
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
				(sum-lists new-color prev-color))))))

(define (trace-ray source direction t_min t_max depth)
	(trace-ray-iter source direction t_min t_max depth (list 0 0 0) 1))

(define half (/ canv-size 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (worker y-coor) 
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
  (vector y-coor (list->vector ret)))

(define (async-draw-y y-top y-bottom)
  ;;this version calculates each line, then draws them
  ;;it does not raytrace while the turtle works
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
        (map draw-points (map async-get workers))
        (async-draw-y (- y-top (* dot-size n-processors)) y-bottom))))

(define lines nil)
(define completed-lines 0)
(define (async-draw-y y-top y-bottom)
  ;;this version saves all calculated colors in 'lines'
  ;;then drawn them when all raytracing is finished
  ;;; it improves time from 81min to 54min for 500/1
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
        (map (lambda (x) (set! lines (cons (async-get x) lines)))
             workers)
        
        (set! completed-lines (+ completed-lines (* n-processors dot-size)))
        (print (* (/ completed-lines canv-size) 100))
        
        (async-draw-y (- y-top (* dot-size n-processors)) y-bottom))
      (let ((start (time)))
        (print "Drawing points...")
        
        (define (to-each proc items)
          ;;tail recursively map a function - nothing returned
          (define (iter proc items)
            (if (null? items)
                nil
                (begin (proc (car items))
                       (iter proc (cdr items)))))
          (iter proc items))
        (to-each draw-points lines)
        (print (list 'drawing 'took (- (time) start) 'seconds)))))

(define (async-draw-y y-top y-bottom previous-lines)
  ;;this version starts processing the next batch of lines
  ;;as it draws the results from the previous batch
  ;;=> This is faster then normal but still slower then doing  all calculations first
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
      (map draw-points previous-lines)))

(define (draw-points colors)
  ;;COLORS is a vector with format: [y-coor colors]
  ;;Where 'y-coor' is the y-coordinate of the line
  ;;     and 'colors' is a vector of rgb color values (reversed)
  (let ((y-coor (vector-ref colors 0))
        (colors (vector-ref colors 1))
        (draw-point (mu (index x-coor)
                        (if (< index 0) nil
                            (begin (set-dot
                                    x-coor
                                    y-coor
                                    (vector->list (vector-ref colors index)))
                                   (draw-point (- index 1)
                                               (+ x-coor dot-size)))))))
    (draw-point (- (vector-length colors) 1) (- half))
    (update)
    ))

;;; new draw-points with lines
(define (draw-points colors)
	;;; Draws points with lines
  ;;COLORS is a vector with format: [y-coor colors]
  ;;Where 'y-coor' is the y-coordinate of the line
  ;;     and 'colors' is a vector of rgb color values (reversed)
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
    (update))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-y y-top y-bottom)
	(if (> y-top y-bottom)
		(begin
			(setpos (- half) y-top)
			(pendown)
			(draw-x (- half) half)
			(penup)
			(draw-y (- y-top dot-size) y-bottom))))

(define draw-x (mu (x-left x-right)
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
