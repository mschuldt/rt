;;; Test cases for Scheme.
;;;
;;; In order to run only a prefix of these examples, add the line
;;;
;;; (exit)
;;;
;;; after the last test you wish to run.
;;; *** Add more of your own here! ***

(define (assert-equal v1 v2)
  (if (equal? v1 v2)
      (print 'ok)
      (print (list v2 'does 'not 'equal v1))))

(assert-equal #t True)


;;test our version of `try'
(assert-equal (try 3 4)
              3)
(assert-equal(try (/ 1 0) 9)
             9)
(assert-equal (try (/ 1 0))
              'okay)

;;question 2
'(1 2 . 3 3)
;;reader error (illegal use of `.')

(assert-equal '(1 2 . '(1 3))
	      '(1 2 quote (1 3)))

(assert-equal '(1 2 . (list 1 3))
	      '(1 2 1 3))

;;question 3
(assert-equal (try (modulo 1)
		   "error")
	      "error")
(assert-equal (modulo 12 3) 0)

;;question 4
(define (test4 x y) (+ x y z))
(assert-equal (try (test4 1 2) ;;test erroneous lookup
		   "error")
	      "error")
(define x 5)
(define (test y)
  (+ x y))
(assert-equal (test 2) 7) ;; test lookup

;;question A5
(define x 23)
(assert-equal x 23) ;;test define
(assert-equal (try (define x) ;;test too few operands
		   "err")
	      "err")

(assert-equal (try (define x 1 2) ;;test too many operands
		   "err")
	      "err")
(assert-equal (try (define 3 4) ;;test bad arguement to define
		   "err")
	      "err")

;;question B6
(assert-equal 's (quote s))
(assert-equal ''s (quote (quote s))) ;;test double application

;; question 7

(assert-equal (begin 12 'a) 'a)
(define (test) "end")
(assert-equal (begin (display 1)
		     (display 2)
		     (display 3)
		     (print 4)
		     (test))
	      "end")
(assert-equal (begin (+ 1 2 3))
	      6)

;; question 8
(define add  (lambda (a b) (+ a b))) ;;test lambda
(assert-equal (add 1 2)  3)
(assert-equal
 (lambda (x) 1 2)
 '(lambda (x) (begin 1 2)))

(assert-equal
 (lambda (x) 1 )
 '(lambda (x) 1))

(assert-equal ((lambda (a b) (+ a b)) 3 4) 7)

(define test  (lambda (a b) ;;test multiple body forms
	       (display 4)
	       5))

(assert-equal (test 3 4) 5)


(assert-equal
 (try (lambda x 1) ;;check for malformed formals list
      "err")
 "err")

(assert-equal
 (try (lambda (x)) ;;check min operands
      "err")
 "err")

;;question A9

(define (f x) (* x 2))  ;;test equivalence to lambda def
(define g (lambda (x) (* x 2)))

(assert-equal 
 (define (f x) (* x 2))
 (define f (lambda (x) (* x 2))))
(assert-equal (f 3) (g 3))
(assert-equal (f 3) 6)

(assert-equal  ;;test formals list
 (try (define (7 x) 
	 (* x 8))
      "err")
 "err")

(assert-equal 
 (try (define (x)) ;;check for min operands
      "err")
 "err")

;;problem 10
(define (test x) (+ x 2))

(assert-equal   ;;test for correct args
 (try (test 1 2)
      "err")
 "err")

(assert-equal 
 (try (test)
      "err")
 "err")

;;problem 11
(assert-equal
 (try (define (x 1) (+ x 1))
      "err")
 "err")
(assert-equal  ;;repeated formals
 (try (define (test x x) (+ x 1))
      "err")
 "err")
(assert-equal (define (x x) (+ x 1))
	      '(x x))

(assert-equal
 (try (define (+ x 1))
      "err")
 "err")


;;problem 12 tested above

;;problem 13
      
(assert-equal
  (if False "yes" "no") "no")
(assert-equal
  (if True "yes" "no") "yes")
(assert-equal
  (if false "yes") 'okay)
(assert-equal ;;check for correct operands 
 (try
  (if false)
  "err")
 "err")

(assert-equal 
 (try
  (if false 1 2 2 33 4)
  "err")
 "err")

(assert-equal 
  (if nil "yes" "no")
  "yes")



;;; These are examples from several sections of "The Structure
;;; and Interpretation of Computer Programs" by Abelson and Sussman.

;;; License: Creative Commons share alike with attribution

;;; 1.1.1



(assert-equal 10 10)

(assert-equal (+ 137 349)
486)

(assert-equal (- 1000 334)
666)

(assert-equal (* 5 99)
495)

(assert-equal (/ 10 5)
2)

(assert-equal (+ 2.7 10)
12.7)

(assert-equal (+ 21 35 12 7)
 75)

(assert-equal (* 25 4 12)
 1200)

(assert-equal (+ (* 3 5) (- 10 6))
 19)

(assert-equal (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))
 57)

(assert-equal (+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))
 57)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Move the following (exit) line to run additional tests. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(exit)


;;; 1.1.2

(assert-equal (define size 2)
 'size)
(assert-equal size
 2)

(assert-equal (* 5 size)
 10)

(define pi 3.14159)
(define radius 10)
(assert-equal (* pi (* radius radius))
 314.159)

(define circumference (* 2 pi radius))
(assert-equal circumference
 62.8318)

;;; 1.1.4

(assert-equal (define (square x) (* x x))
 '(square x))
(assert-equal (square 21)
 441)

(define square (lambda (x) (* x x))) ; See Section 1.3.2
(assert-equal (square 21)
 441)

(assert-equal (square (+ 2 5))
 49)

(assert-equal (square (square 3))
 81)

(define (sum-of-squares x y)
  (+ (square x) (square y)))
(assert-equal (sum-of-squares 3 4)
 25)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
(assert-equal (f 5)
 136)

;;; 1.1.6

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(assert-equal (abs -3)
 3)

(assert-equal (abs 0)
 0)

(assert-equal (abs 3)
 3)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
(assert-equal (a-plus-abs-b 3 -2)
 5)

;;; 1.1.7

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
  (sqrt-iter 1.0 x))
(assert-equal (sqrt 9)
 3.00009155413138)

(assert-equal (sqrt (+ 100 37))
 11.704699917758145)

(assert-equal (sqrt (+ (sqrt 2) (sqrt 3)))
 1.7739279023207892)

(assert-equal (square (sqrt 1000))
 1000.000369924366)

;;; 1.1.8

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
(assert-equal (sqrt 9)
 3.00009155413138)

(assert-equal (sqrt (+ 100 37))
 11.704699917758145)

(assert-equal (sqrt (+ (sqrt 2) (sqrt 3)))
 1.7739279023207892)

(assert-equal (square (sqrt 1000))
 1000.000369924366)

;;; 1.3.1

(define (cube x) (* x x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))
(assert-equal (sum-cubes 1 10)
 3025)

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(assert-equal (sum-integers 1 10)
 55)

;;; 1.3.2

(assert-equal ((lambda (x y z) (+ x y (square z))) 1 2 3)
 12)

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
(assert-equal (f 3 4)
 456)

(define x 5)
(assert-equal (+ (let ((x 3))
     (+ x (* x 10)))
   x)
 38)

(assert-equal (let ((x 3)
      (y (+ x 2)))
  (* x y))
 21)

;;; 2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x (cons 1 2))
(assert-equal (car x)
 1)

(assert-equal (cdr x)
 2)

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))
(assert-equal (car (car z))
 1)

(assert-equal (car (cdr z))
 3)

(assert-equal z
 '((1 . 2) 3 . 4))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display '/)
  (display (denom x))
  (newline))
(define one-half (make-rat 1 2))
(assert-equal (print-rat one-half)
 'okay)
;mbs
(define one-third (make-rat 1 3))
(assert-equal (print-rat (add-rat one-half one-third))
'okay)

(assert-equal (print-rat (mul-rat one-half one-third))
 'okay)

(assert-equal (print-rat (add-rat one-third one-third))
 'okay)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(assert-equal (print-rat (add-rat one-third one-third))
'okay)

(define one-through-four (list 1 2 3 4))
(assert-equal one-through-four
 '(1 2 3 4))

(assert-equal (car one-through-four)
 1)

(assert-equal (cdr one-through-four)
 '(2 3 4))

(assert-equal (car (cdr one-through-four))
 2)

(assert-equal (cons 10 one-through-four)
 '(10 1 2 3 4))

(assert-equal (cons 5 one-through-four)
 '(5 1 2 3 4))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
(assert-equal (map abs (list -10 2.5 -11.6 17))
 '(10 2.5 11.6 17))

(assert-equal (map (lambda (x) (* x x))
     (list 1 2 3 4))
 '(1 4 9 16))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))
(assert-equal (scale-list (list 1 2 3 4 5) 10)
 '(10 20 30 40 50))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(define x (cons (list 1 2) (list 3 4)))
(assert-equal (count-leaves x)
 4)

(assert-equal (count-leaves (list x x))
 8)

;;; 2.2.3

(define (odd? x) (= 1 (remainder x 2)))
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(assert-equal (filter odd? (list 1 2 3 4 5))
 '(1 3 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(assert-equal (accumulate + 0 (list 1 2 3 4 5))
 15)

(assert-equal (accumulate * 1 (list 1 2 3 4 5))
 120)

(assert-equal (accumulate cons nil (list 1 2 3 4 5))
 '(1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(assert-equal (enumerate-interval 2 7)
 '(2 3 4 5 6 7))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(assert-equal (enumerate-tree (list 1 (list 2 (list 3 4)) 5))
 '(1 2 3 4 5))

;;; 2.3.1

(define a 1)

(define b 2)

(assert-equal (list a b)
 '(1 2))

(assert-equal (list 'a 'b)
 '(a b))

(assert-equal (list 'a b)
 '(a 2))

(assert-equal (car '(a b c))
 'a)

(assert-equal (cdr '(a b c))
 '(b c))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
(assert-equal (memq 'apple '(pear banana prune))
 False)

(assert-equal (memq 'apple '(x (apple sauce) y apple pear))
 '(apple pear))

(define (equal? x y)
  (cond ((pair? x) (and (pair? y)
                        (equal? (car x) (car y))
                        (equal? (cdr x) (cdr y))))
        ((null? x) (null? y))
        (else (eq? x y))))
(assert-equal (equal? '(1 2 (three)) '(1 2 (three)))
 True)

(assert-equal (equal? '(1 2 (three)) '(1 2 three))
 False)

(assert-equal (equal? '(1 2 three) '(1 2 (three)))
 False)

;;; Peter Norvig tests (http://norvig.com/lispy2.html)

(define double (lambda (x) (* 2 x)))
(assert-equal (double 5)
 10)

(define compose (lambda (f g) (lambda (x) (f (g x)))))
(assert-equal ((compose list double) 5)
 '(10))

(define apply-twice (lambda (f) (compose f f)))
(assert-equal ((apply-twice double) 5)
 20)

(assert-equal ((apply-twice (apply-twice double)) 5)
 80)

(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
(assert-equal (fact 3)
 6)

(assert-equal (fact 50)
 30414093201713378043612608166064768844377641568960512000000000000)

(define (combine f)
  (lambda (x y)
    (if (null? x) nil
      (f (list (car x) (car y))
         ((combine f) (cdr x) (cdr y))))))
(define zip (combine cons))
(assert-equal (zip (list 1 2 3 4) (list 5 6 7 8))
 '((1 5) (2 6) (3 7) (4 8)))

(define riff-shuffle (lambda (deck) (begin
    (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
    (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
    (define mid (lambda (seq) (/ (length seq) 2)))
    ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))
(assert-equal (riff-shuffle (list 1 2 3 4 5 6 7 8))
 '(1 5 2 6 3 7 4 8))

(assert-equal ((apply-twice riff-shuffle) (list 1 2 3 4 5 6 7 8))
 '(1 3 5 7 2 4 6 8))

(assert-equal (riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))
 '(1 2 3 4 5 6 7 8))

;;; Additional tests

(assert-equal (apply square '(2))
 4)

(assert-equal (apply + '(1 2 3 4))
 10)

(assert-equal (apply (if false + append) '((1 2) (3 4)))
 '(1 2 3 4))

(assert-equal (if 0 1 2)
 1)

(assert-equal (if '() 1 2)
 1)

(assert-equal (or false true)
 True)

(assert-equal (or)
 False)

(assert-equal (and)
 True)

(assert-equal (or 1 2 3)
 1)

(assert-equal (and 1 2 3)
 3)

(assert-equal (and False (/ 1 0))
 False)

(assert-equal (and True (/ 1 0))
 Error)

(assert-equal (or 3 (/ 1 0))
 3)

(assert-equal (or False (/ 1 0))
 Error)

(assert-equal (or (quote hello) (quote world))
 'hello)

(assert-equal (if nil 1 2)
 1)

(assert-equal (if 0 1 2)
 1)

(assert-equal (if (or false False #f) 1 2)
 2)

(define (loop) (loop))
(assert-equal (cond (false (loop))
      (12))
 12)

(assert-equal ((lambda (x) (display x) (newline) x) 2)
 2) ; 2)

(define g (mu () x))
(define (high f x)
  (f))

(assert-equal (high g 2)
 2)

(define (print-and-square x)
  (print x)
  (square x))
(assert-equal (print-and-square 12)
144) ;;12

(assert-equal (/ 1 0)
 Error)

(define addx (mu (x) (+ x y)))
(define add2xy (lambda (x y) (addx (+ x x))))
(assert-equal (add2xy 3 7)
 13)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme Implementations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; len outputs the length of list s
(define (len s)
  (if (eq? s '())
    0
    (+ 1 (len (cdr s)))))
(assert-equal (len '(1 2 3 4))
 4)


;;;;;;;;;;;;;;;;;;;;
;;; Extra credit ;;;
;;;;;;;;;;;;;;;;;;;;

;(exit)

; Tail call optimization test
(define (sum n total)
  (if (zero? n) total
    (sum (- n 1) (+ n total))))
(assert-equal (sum 1001 0)
 501501)


      
