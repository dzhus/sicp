#lang scheme

;;; Tests for our generic arithmetics system

(require srfi/1 srfi/27
         (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui)
         "get-put.ss"
         "ddp-shared.ss"
         (prefix-in 2.78: "ex2.78.ss")
         "complex.ss"
         "generic-arith.ss"
         "ex2.79.ss"
         "ex2.80.ss"
         "coercion-shared.ss"
         (prefix-in 2.81: "ex2.81.ss")
         (prefix-in 2.82: "ex2.82.ss"))

(define epsilon 1e-8)

(define-test-suite get-put-test
  (test-case
   "Test put/get operations"
   (let ((op1 (lambda (x y) (* x y)))
         (op2 (lambda (x y) (+ x y))))
     (put 'mul 'complex op1)
     (put 'add 'complex op2)
     ;; Make sure we get what we've put
     (let ((res1 (get 'mul 'complex))
           (res2 (get 'add 'complex))
           (res3 (get 'sub 'rational)))
       (check-equal? op1 res1)
       (check-equal? op2 res2)
       (check-false res3)))))

(define-test-suite ddp-shared-test
  (test-case
   "Functions to attach tags to data"
   (let ((tags '(rational user-type type2))
         (data-pieces (list "test1" (list "foo" '@ "bar") -1 5 (cons 2 2))))
     (for-each
      (lambda (tag data)
        (let ((compound (attach-tag tag data)))
          (check-equal? tag (type-tag compound))
          (check-equal? data (contents compound))))
      tags data-pieces)))
  (test-case
   "Special version for Scheme numbers (ex. 2.78)"
   (let ((numbers '(1 -2.0 0 5/2)))
     (for-each
      (lambda (n)
        (check-equal? 'scheme-number (2.78:type-tag n))
        (check-equal? n (2.78:contents n)))
      numbers))))

(define (all-true? list)
  (every (lambda (t) t) list))

(define-simple-check (check-generic-operations numbers operations)
  (all-true?
   (map (lambda (op)
          (all-true?
           (map
            (lambda (n1 n2)
              (check-equal? (contents ((car op) n1 n2))
                            ((cdr op) (contents n1) (contents n2))))
            numbers numbers)))
        operations)))

(define-test-suite arithmetics
  (test-case
   "Primitives for rectangular and polar representations of complex"
   (let* ((k (list (- (random-integer 100) 50)
                   (random-integer 100)
                   (random-integer 1234567890))))
     (for-each
      (lambda (k1 k2)
        (let ((c1 (make-from-real-imag k1 k2))
              (c2 (make-from-mag-ang k1 k2)))
          (check-equal? k1 (real-part c1))
          (check-equal? k2 (imag-part c1))
          (check-equal? k1 (magnitude c2))
          (check-equal? k2 (angle c2))))
      k k)))

  (test-case
   "Integers"
   (check-generic-operations
    (list (make-integer (random-integer 500))
          (make-integer (- (random-integer 500) 250)))
    (list (cons add +) (cons sub -) (cons mul *))))

  (test-case
   "Rationals"
   (check-equal? (sub (make-rational -1 5) (make-rational 21 6))
                 (make-rational -37 10))
   (check-equal? (add (make-rational -1 5) (make-rational 21 6))
                 (make-rational 33 10))
   (check-equal? (add (make-rational 1 3) (make-rational 1 4))
                 (make-rational 7 12))
   (check-equal? (add (make-rational -100 1) (make-rational 201 99))
                 (make-rational -3233 33))
   (check-equal? (add (make-rational -0 5) (make-rational 0 8))
                 (make-rational 0 101)))

  (test-case
   "Reals"
   (check-generic-operations
    (list (make-real (random-integer 500))
          (make-real (- (random-integer 500) 250)))
    (list (cons add +) (cons sub -) (cons mul *) (cons div /))))
  
  (test-case
   "Complex representations interoperability test"
   (let ((c1 (make-complex-from-real-imag 1 1))
         (c2 (make-complex-from-mag-ang (sqrt 2) (/ pi 4))))
     (for-each
      (lambda (selector)
        (check-= (selector c1) (selector c2) epsilon))
      (list real-part imag-part magnitude angle))))

  (test-case
   "Complex numbers"
   (check-true #f))

  (test-case
   "=zero? predicate (excercise 2.80)"
   (let ((numbers (list (make-integer 0)
                        (sub (make-integer 5) (make-integer 5))
                        (make-rational 0 10)
                        (make-rational -0 1)
                        (make-complex-from-real-imag 0.0 -0.0)
                        (make-complex-from-real-imag -0.0 0e5)
                        (make-complex-from-mag-ang 0.0 0)
                        (make-complex-from-mag-ang 0.0 (/ pi -4)))))
     (for-each
      (lambda (n)
        (check-pred =zero? n))
      numbers)))
  
  (test-case
   "equ? predicate (excercise 2.79)"
   (check-true #f)))

(define-test-suite coercion
  (test-case
   "Shared coercion functions"
   (let ((coer1 (lambda (x) (make-integer x)))
         (coer2 (lambda (x) (make-rational x 1))))
     (put-coercion 'limbo 'integer coer1)
     (put-coercion 'integer 'rational coer2)
     ;; Make sure we get what we've put
     (let ((res1 (get-coercion 'limbo 'integer))
           (res2 (get-coercion 'integer 'rational))
           (res3 (get 'limbo 'complex)))
       (check-equal? coer1 res1)
       (check-equal? coer2 res2)
       (check-false res3))))

  (test-case
   "Simple type coercion (excercise 2.81)"
   (put-coercion 'integer 'rational (lambda (x) (make-rational (contents x) 1)))
   (put-coercion 'integer 'real (lambda (x) (make-real (contents x))))
   (put-coercion 'real 'complex (lambda (x) (make-complex-from-real-imag (contents x) 0)))
   (let ((c1 (make-integer 5))
         (c2 (make-rational 1 8))
         (c3 (make-real 10))
         (c4 (make-complex-from-real-imag 0 -5)))
     (check-equal? (2.81:apply-generic 'add c1 c2)
                   (make-rational 41 8))
     (check-equal? (2.81:apply-generic 'mul c1 c3)
                   (make-real 50))
     (check-equal? (2.81:apply-generic 'sub c4 c3)
                   (make-complex-from-real-imag -10 -5))))

  (test-case
   "Advanced type coercion (excercise 2.82)"
   (check-true #f)))

(exit (run-tests (test-suite "All tests"
                             get-put-test
                             ddp-shared-test
                             arithmetics
                             coercion)))
