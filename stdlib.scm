
(define (range from to)
  "integer range [FROM, TO)"
  (set! from (- from 1))
  (define (make-range num range)
    (if (= num from)
        range
        (make-range (- num 1) (cons num range))))
  (make-range (- to 1) nil))

(define (reverse lst)
  "reverse a list"
  (define newlist nil)
  (for i in lst :
       (push  i newlist))
  newlist)

(define-macro (for var in lst : code)
  "This is why lisp is amazing!"
  (list 'let '()
        (list 'define (list '_call_ var)
              code)
        '(define (_do_ lst)
           (if (not (null? lst))
               (begin
                 (_call_ (car lst))
                 (_do_ (cdr lst)))))

        (list '_do_ lst)))

(define-macro (push newelt place)
  "Add NEWELT to the existing list PLACE"
  (list 'set! place (list 'cons newelt place)))

(define-macro (time-eval code)
  (list 'let '((begin-time (time)))
        code
        '(let ((dif (- (time) begin-time)))
           (if (> dif 60)
               (print (list 'run 'time: (quotient dif 60) 'minutes (modulo dif 60) 'seconds))
               (print (list 'run 'time: dif 'seconds))))))


(define-macro (dolist var--list code)
  "Loop over a list"
  "(dolist (VAR LIST) CODE)"
  "Evaluate CODE with VAR bound to each `car' from LIST, in turn"
  (list 'let '()
        (list 'define (list '_call_ (car var--list))
              code)
        '(define (_do_ lst)
           (if (not (null? lst))
               (begin
                 (_call_ (car lst))
                 (_do_ (cdr lst)))))
        (list '_do_ (cadr var--list))))

(define (map proc items)
  (define (mapp proc items mapped)
    (if (null? items)
        mapped
        (mapp proc (cdr items)
              (cons (proc (car items)) mapped))))
  (reverse (mapp proc items nil)))

(define (cadr x)
  (car (cdr x)))

(define (caar x)
  (car (car x)))

(define (caaar x)
;;  "Return the `car' of the `car' of the `car' of X."
  (car (car (car x))))

(define (caadr x)
;;  "Return the `car' of the `car' of the `cdr' of X."
  (car (car (cdr x))))

(define (cadar x)
;;  "Return the `car' of the `cdr' of the `car' of X."
  (car (cdr (car x))))

(define (caddr x)
;;  "Return the `car' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr x))))

(define (cdaar x)
;;  "Return the `cdr' of the `car' of the `car' of X."
  (cdr (car (car x))))

(define (cdadr x)
;;  "Return the `cdr' of the `car' of the `cdr' of X."
  (cdr (car (cdr x))))

(define (cddar x)
;;  "Return the `cdr' of the `cdr' of the `car' of X."
  (cdr (cdr (car x))))

(define (cdddr x)
;;  "Return the `cdr' of the `cdr' of the `cdr' of X."
  (cdr (cdr (cdr x))))

(define (caaaar x)
;;  "Return the `car' of the `car' of the `car' of the `car' of X."
  (car (car (car (car x)))))

(define (caaadr x)
;;  "Return the `car' of the `car' of the `car' of the `cdr' of X."
  (car (car (car (cdr x)))))

(define (caadar x)
;;  "Return the `car' of the `car' of the `cdr' of the `car' of X."
  (car (car (cdr (car x)))))

(define (caaddr x)
;;  "Return the `car' of the `car' of the `cdr' of the `cdr' of X."
  (car (car (cdr (cdr x)))))

(define (cadaar x)
;;  "Return the `car' of the `cdr' of the `car' of the `car' of X."
  (car (cdr (car (car x)))))

(define (cadadr x)
;;  "Return the `car' of the `cdr' of the `car' of the `cdr' of X."
  (car (cdr (car (cdr x)))))

(define (caddar x)
;;  "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
  (car (cdr (cdr (car x)))))

(define (cadddr x)
;;  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X."
  (car (cdr (cdr (cdr x)))))

(define (cdaaar x)
;;  "Return the `cdr' of the `car' of the `car' of the `car' of X."
  (cdr (car (car (car x)))))

(define (cdaadr x)
;;  "Return the `cdr' of the `car' of the `car' of the `cdr' of X."
  (cdr (car (car (cdr x)))))

(define (cdadar x)
;;  "Return the `cdr' of the `car' of the `cdr' of the `car' of X."
  (cdr (car (cdr (car x)))))

(define (cdaddr x)
;;  "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X."
  (cdr (car (cdr (cdr x)))))

(define (cddaar x)
;;  "Return the `cdr' of the `cdr' of the `car' of the `car' of X."
  (cdr (cdr (car (car x)))))

(define (cddadr x)
;;  "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X."
  (cdr (cdr (car (cdr x)))))

(define (cdddar x)
;;  "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X."
  (cdr (cdr (cdr (car x)))))

(define (cddddr x)
;;  "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (cdr (cdr (cdr (cdr x)))))

;;;; testing functions -------------------------------------
(define tests-passed 0) ;;number of tests passed
(define tests-failed nil) ;;list of failed tests

(define (assert-equal2 v1 v2 quoted)
  "prints and saves the results of testings V1 and V2 for equality"
  (if (equal? v1 v2)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (print 'ok))
      (begin
        (set! tests-failed (cons (list quoted v1 v2)
                                 tests-failed))
        (print (list v2 'does 'not 'equal v1)))))

;;NOTE: now that we have `okay?' we can get rid of this macro entirely
;;      but why not just leave it as an additional test
(define-macro (assert-equal form expect)
  (if (equal? expect 'okay)
      (list 'assert-equal2
            (list 'okay? form)
            'True
            (list 'quote form))
      (list 'assert-equal2 form expect (list 'quote form))))

(define (test-report)
  (let ((passed tests-passed)
        (failed (length tests-failed))
        (total nil))
    (set! total (+ passed failed))
    (print (list passed '/ total 'tests 'passed))
    (if (> (length tests-failed) 0)
        (print (list failed 'tests 'failed. 'Run '(fail-report) 'for 'details)))))

(define (fail-report)
  (let ((count 0))
    (for test in tests-failed :
         (begin
           (newline)
           (print (list  'failed 'test count '--------------------------------))
           (print (list  'test: (car test)))
           (print (list  'expected: (caddr test)))
           (print (list  'got: (cadr test)))
           (set! count (+ count 1))))))

