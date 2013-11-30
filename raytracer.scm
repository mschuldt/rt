(define canv-size 500)
(define dot-size 1)
(define n-processors 8) ;; must be >= 1


(define testing-new #t) ;;set to false to use use the old spheres and orientation


(define lights (list (list 8 (list 2 2 0)))) ;; always even size
(if testing-new
    (define lights '((8 (2 2 0))
                     (10 (0 0 -50))
                     (10 (0 0 -20)))))

(define camera (list 0 1 0))
(if testing-new
    (define camera (list -15 5 -80)))


(define reflection-depth 2)

(define ambient-light 2)


(define spheres (list
                 (list canv-size (list 0 (- canv-size) 0)  (list 9 9 0)  canv-size  2)  ;; Yellow sphere
                 (list 1 (list 0  0 3)  (list 9 0 0)  canv-size  3)  ;; Red sphere
                 (list 1 (list -2  1 4)  (list 0 9 0)  9  4)  ;; Green sphere
                 (list 1 (list 2  1 4)  (list 0 0 9)  canv-size  5)   ;; Blue sphere
                 ))



(if testing-new
    (define spheres '((125 (-125 -72.16878364870321 0) (8.0 2.0 10.0) 9 7) (0.32549258922520563 (-54.68275498983455 31.571103187305667 0) (10.0 0.0 0.0) 9 7) (0.37813562991079136 (-54.07339507724316 31.219289068611484 0) (10.0 6.0 2.0) 9 7) (0.44467351214795175 (-53.36082145775421 30.807884487038613 0) (10.0 10.0 4.0) 9 7) (0.5304679212539934 (-52.51632420414535 30.320313758021214 0) (6.0 10.0 4.0) 9 7) (0.6437428481060288 (-51.4994278484823 29.733208358767676 0) (2.0 8.0 10.0) 9 7) (0.7976373958837171 (-50.251155940674174 29.012518219266525 0) (1.3 5.4 1.3) 9 7) (0.3974660517417542 (-5.167058672642805 19.046601455740404 0) (10.0 5.1 9.8) 9 7) (0.3843667460069968 (-46.124009520839614 25.298223595303053 0) (10.0 5.1 9.8) 9 7) (0.3843667460069968 (-44.970909282818624 27.295451793775168 0) (10.0 5.1 9.8) 9 7) (1.0142093874692784 (-48.682050598525365 28.106594806181036 0) (10.0 5.1 9.8) 9 7) (0.3407066126724756 (-27.256529013798048 6.294625288393284 0) (9.3 2.5 0.0) 9 7) (0.3407066126724756 (-19.079570309658635 20.457533215355316 0) (9.3 2.5 0.0) 9 7) (0.555745624377355 (-6.113201868150905 18.930732767402823 0) (9.3 2.5 0.0) 9 7) (0.3934434590902016 (-31.47547672721613 11.357733928712236 0) (9.3 2.5 0.0) 9 7) (0.3934434590902016 (-25.573824840863104 21.579694844460374 0) (9.3 2.5 0.0) 9 7) (0.33483755461753206 (-40.18050655410385 20.105129109190813 0) (9.3 2.5 0.0) 9 7) (0.3895014727116597 (-38.56064579845431 18.66493840170463 0) (9.3 2.5 0.0) 9 7) (0.33483755461753206 (-37.50180611716359 24.744774364228288 0) (9.3 2.5 0.0) 9 7) (0.3895014727116597 (-35.44463401676103 24.062029124580615 0) (9.3 2.5 0.0) 9 7) (0.5402979518477707 (-43.22383614782166 23.08364613786322 0) (9.3 2.5 0.0) 9 7) (0.5402979518477707 (-41.60294229227835 25.891116649340443 0) (9.3 2.5 0.0) 9 7) (1.3328412882676501 (-46.64944508936775 26.93306944156254 0) (9.3 2.5 0.0) 9 7) (0.3224398680214002 (-20.63615155336961 5.957143337105627 0) (10.0 0.0 0.0) 9 7) (0.34792108209285205 (-19.135659515106862 4.620063101565141 0) (10.0 0.0 0.0) 9 7) (0.4634647783446199 (-18.538591133784795 6.957118880722827 0) (10.0 0.0 0.0) 9 7) (0.34688624911439203 (-23.93515118889305 5.407420995669364 0) (10.0 0.0 0.0) 9 7) (0.39611607917480635 (-24.9553129880128 3.430465401921539 0) (10.0 0.0 0.0) 9 7) (0.553109960876242 (-26.549278122059615 5.748086864909592 0) (10.0 0.0 0.0) 9 7) (0.4391575938325384 (-19.762091722464227 0.76064276770684 0) (10.0 0.0 0.0) 9 7) (0.4659944179725466 (-22.367732062682236 -4.984098995428828e-07 0) (10.0 0.0 0.0) 9 7) (0.8317694276863223 (-19.962466264471736 -2.8813343136436065 0) (10.0 0.0 0.0) 9 7) (0.3224398680214002 (-15.477113665027208 14.892859077887518 0) (10.0 0.0 0.0) 9 7) (0.34792108209285205 (-13.56892220162123 14.26193496091173 0) (10.0 0.0 0.0) 9 7) (0.4634647783446199 (-15.294337685372456 12.576330686003532 0) (10.0 0.0 0.0) 9 7) (0.34688624911439203 (-16.650539957490817 18.024737761844918 0) (10.0 0.0 0.0) 9 7) (0.39611607917480635 (-15.448527087817448 19.896701597739316 0) (10.0 0.0 0.0) 9 7) (0.553109960876242 (-18.252628708915985 20.118305181060865 0) (10.0 0.0 0.0) 9 7) (0.4391575938325384 (-10.53978225198092 16.73415133370755 0) (10.0 0.0 0.0) 9 7) (0.4659944179725466 (-11.183866031341118 19.37102370525646 0) (10.0 0.0 0.0) 9 7) (0.8317694276863223 (-7.485924849176901 18.728669320384352 0) (10.0 0.0 0.0) 9 7) (0.4580045573578863 (-31.602314457694156 15.601313804870427 0) (10.0 0.0 0.0) 9 7) (0.5479129281220673 (-34.51851447169024 14.86787040565846 0) (10.0 0.0 0.0) 9 7) (0.6657926711538377 (-31.95804821538421 12.300658076599218 0) (10.0 0.0 0.0) 9 7) (0.4580045573578863 (-29.312291670904724 19.567749622080196 0) (10.0 0.0 0.0) 9 7) (0.5479129281220673 (-30.1352110467137 22.459974642708502 0) (10.0 0.0 0.0) 9 7) (0.6657926711538377 (-26.631706846153506 21.52615194656277 0) (10.0 0.0 0.0) 9 7) (0.8143459210779348 (-39.08860421174087 19.746838803636816 0) (10.0 0.0 0.0) 9 7) (0.8143459210779348 (-36.64556644850707 23.978304334367195 0) (10.0 0.0 0.0) 9 7) (1.8296116864575847 (-43.91068047498203 25.35184291989713 0) (10.0 0.0 0.0) 9 7) (0.9235054586676283 (-19.393614632020196 5.8650468813513505 0) (10.0 6.0 2.0) 9 7) (1.050759905878541 (-25.218237741084984 4.85325164637485 0) (10.0 6.0 2.0) 9 7) (1.3801598473898486 (-20.70239771084773 -0.7968361529988659 0) (10.0 6.0 2.0) 9 7) (0.9235054586676283 (-14.776087338682053 13.862838758749009 0) (10.0 6.0 2.0) 9 7) (1.050759905878541 (-16.812158494056657 19.413007994678246 0) (10.0 6.0 2.0) 9 7) (1.3801598473898486 (-9.661118931728941 18.327219672966937 0) (10.0 6.0 2.0) 9 7) (1.3640180483542248 (-32.73643316050139 14.175290966633698 0) (10.0 6.0 2.0) 9 7) (1.3640180483542248 (-28.64437901543872 21.262936653205074 0) (10.0 6.0 2.0) 9 7) (2.667724852208609 (-40.01587278312914 23.10317460517984 0) (10.0 6.0 2.0) 9 7) (2.7301750707082078 (-21.841400565665662 3.1525341358230325 0) (10.0 10.0 4.0) 9 7) (2.7301750707082078 (-13.65087535354104 17.338939943896733 0) (10.0 10.0 4.0) 9 7) (4.252813811554711 (-34.02251049243769 19.64290521108458 0) (10.0 10.0 4.0) 9 7) (7.847715036835193 (-23.543145110505577 13.59264069936055 0) (6.0 10.0 4.0) 9 7) (19.33756729740644 (0 -5.763044147910607e-07 0) (2.0 8.0 10.0) 9 7))))


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


(define-macro (define* formals body)
  (list 'define formals
        (make-substitutions substitutions body)))
        

(define-macro (mu* formals body)
  (list 'mu formals
        (make-substitutions substitutions body)))


(print "loading and expanding...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sphere: radius (cx  cy  cz) R  G  B specular_exponent reflectiveness


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
                                          3 ;reflection depth
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
(define (normal-draw) (speed 0) (penup) (pensize dot-size) (draw-y half (- half)) ;(exitonclick)
  )
(define (fast-draw) (speed 0) (penup) (pensize dot-size) (setheading 90) (async-draw-y half (- half)) ;(exitonclick)
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
