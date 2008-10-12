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
         (prefix-in 2.82: "ex2.82.ss")
         "ex2.83.ss"
         (prefix-in 2.84: "ex2.84.ss")
         (prefix-in 2.85: "ex2.85.ss"))

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
  (not (any false? list)))

(define-simple-check (check-equ? x y)
  (check-true (equ? x y)))

;; Make sure generic operations do what's expected
(define-simple-check (check-generic-operations constructor numbers operations)
  (all-true?
   (map (lambda (op)
          (all-true?
           (map
            (lambda (n1 n2)
              (check-equ? ((car op) (constructor n1) (constructor n2))
                          (constructor ((cdr op) n1 n2))))
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
   "=zero? predicate (excercise 2.80)"
   (let ((numbers (list (make-integer 0)
                        (sub (make-integer 5) (make-integer 5))
                        (make-rational 0 10)
                        (make-rational -0 1)
                        (make-complex-from-real-imag 0.0 -0.0)
                        (make-complex-from-real-imag -0.0 0e5)
                        (make-complex-from-mag-ang -0.0 0)
                        (make-complex-from-mag-ang 0.0 (/ pi -4)))))
     (for-each
      (lambda (n)
        (check-pred =zero? n))
      numbers)))
  
  (test-case
   "equ? predicate (excercise 2.79)"
   (check-true (equ? (make-rational 3 9)
                     (make-rational 21 63)))
   (check-true (equ? (make-rational 27 27)
                     (make-rational 100000 100000)))
   (check-true (equ? (make-integer 0)
                     (make-integer -0)))
   (check-false (equ? (make-integer 5)
                      (make-integer -5)))
   (check-true (equ? (make-real 5)
                     (make-real 5.0)))
   (check-false (equ? (make-real 1.7)
                      (make-real 1.71)))
   (let ((c1 (make-complex-from-real-imag 1 1))
         (c2 (make-complex-from-mag-ang (sqrt 2) (/ pi 4))))
     (check-true (equ? c1 c2))))

  (test-case
   "Integers"
    (check-generic-operations
     make-integer
     (list (random-integer 500)
           (- (random-integer 500) 250))
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
    make-real
    (list (* (random-real) 500)
          (- (* (random-real) 500))
          (* (random-real) 123456))
    (list (cons add +) (cons sub -) (cons mul *) (cons div /))))

  (test-case
   "Complex representations interoperability test"
   (let ((c1 (make-complex-from-real-imag 1 1))
         (c2 (make-complex-from-mag-ang (sqrt 2) (/ pi 4)))
         (c3 (make-complex-from-real-imag -1 0))
         (c4 (make-complex-from-mag-ang 1 pi)))
     (check-equ? c1 c2)
     (check-equ? c3 c4)
     (check-false (equ? c1 c3))
     (for-each
      (lambda (selector)
        (check-= (selector c1) (selector c2) epsilon))
      (list real-part imag-part magnitude angle))))

  (test-case
   "Complex numbers"
   (let ((c1 (make-complex-from-real-imag 1 -0))
         (c2 (make-complex-from-real-imag (/ (sqrt 2) 2)
                                          (/ (sqrt 2) 2)))
         (c3 (make-complex-from-mag-ang 1 pi))
         (c4 (make-complex-from-mag-ang 1 (* 7 (/ pi 4))))
         (c5 (make-complex-from-mag-ang (sqrt 2) 0.0)))
     (check-equ? c1 (div c2 c2))
     (check-equ? c3 (mul (mul c2 c2) (mul c2 c2)))
     (check-equ? c5 (add c2 c4))
     (check-equ? c4 (sub c5 c2))
     (check-equ? c1 (div c5 c5))
     (check-equ? c1 (mul c2 c4)))))

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
     (check-equ? (2.81:apply-generic 'add c1 c2)
                   (make-rational 41 8))
     (check-equ? (2.81:apply-generic 'mul c1 c3)
                   (make-real 50))
     (check-equ? (2.81:apply-generic 'sub c4 c3)
                   (make-complex-from-real-imag -10 -5))))

  (test-case
   "Advanced type coercion (excercise 2.82)"
   (let ((add-3-real (lambda (x y z) (make-real (+ x y z))))
         (add-3-complex (lambda (x y z) 
                  (make-complex-from-real-imag (+ (real-part x)
                                                  (real-part y)
                                                  (real-part z))
                                               0))))
     ;; Add generic operation which is implemented only for complex
     ;; numbers
     (put 'add '(real real real) add-3-real)
     (put 'add '(complex complex complex) add-3-complex)
     ;; Install coercion operations (integer->real and real->complex
     ;; have already been installed)
     (put-coercion 'rational 'real
                   (lambda (x)
                     (make-real (/ (numer x)
                                   (denom x)))))
     (put-coercion 'integer 'complex 
                   (lambda (x) 
                     (make-complex-from-real-imag (contents x) 0)))
     (let ((c1 (make-integer 1))
           (c2 (make-real 757.13))
           (c3 (make-rational 2 5))
           (c4 (make-complex-from-mag-ang 13.37 0)))
       ;; Add real and rational (coercing to real)
       (check-equ? (2.82:apply-generic 'add c2 c3)
                   (make-real 757.53))
       ;; Add integer, real and rational (coercing to real)
       (check-equ? (2.82:apply-generic 'add c1 c2 c3)
                   (make-real 758.53))
       ;; Add integer, real and complex (coercing to latter)
       (check-equ? (2.82:apply-generic 'add c1 c2 c4)
                   (make-complex-from-mag-ang 771.5 0))))))

(define-test-suite tower
  (test-case
   "Raising"
   (check-equ? (raise (make-integer 5)) (make-rational 10 2))
   (check-equ? (raise (make-integer 0)) (make-rational 0 1))
   (check-equ? (raise (make-rational 30 6)) (make-real 5.0))
   (check-equ? (raise (make-real 13.37)) 
               (make-complex-from-mag-ang 13.37 0)))

  (test-case
   "Coercion via raising"
   (let ((c1 (make-integer 5))
         (c2 (make-rational 7 10))
         (c3 (make-real 101.25))
         (c4 (make-complex-from-real-imag 13.37 5)))
     ;; Add integer and rational (raising to rational)
     (check-equ? (2.84:apply-generic 'add c1 c2)
                 (make-rational 57 10))
     ;; Add integer, real and rational (raising to real) (relies on
     ;; add-3-real installed few tests ago)
     (check-equ? (2.84:apply-generic 'add c1 c2 c3)
                 (make-real 106.95))
     ;; Multiply integer and rational, resulting in rational
     (check-equ? (2.84:apply-generic 'mul c1 c2)
                 (make-rational 35 10))
     ;; Subtract rational and complex (raising to latter)
     (check-equ? (2.84:apply-generic 'sub c2 c4)
                 (make-complex-from-real-imag -12.67 -5))))

  (test-case
   "Dropping"
   (check-equ? (2.85:drop (make-complex-from-real-imag 5 0))
               (make-integer 5))
   (check-equ? (2.85:drop (make-complex-from-real-imag 1 1))
               (make-complex-from-mag-ang (sqrt 2) (/ pi 4)))
   (check-equ? (2.85:drop (make-complex-from-mag-ang 5.2 0))
               (make-rational 52 10))
   (check-equ? (2.85:drop (make-real 20.0))
               (make-integer 20))
   (check-equ? (2.85:drop (make-real 0.6))
               (make-rational 3 5))
   (check-false (equ? (2.85:drop (make-real pi))
                      (make-rational 22 7)))
   (check-equ? (2.85:drop (make-rational 5 2))
               (make-rational 10 4))
   (check-equ? (2.85:drop (make-integer 9))
               (make-integer 9)))

  (test-case
   "Simplification"
   (check-equ? (2.85:apply-generic 'add 
                                   (make-complex-from-real-imag 5.0 0)
                                   (make-real 5.0))
               (make-integer 10))
   (check-equ? (2.85:apply-generic 'mul (make-real 17.5) (make-rational 2 5))
               (make-integer 7))
   (check-equ? (2.85:apply-generic 'add (make-rational 1 2) (make-real 1))
               (make-rational 3 2))
   (check-equ? (2.85:apply-generic 'sub (make-real 7.1) (make-real 5.1))
               (make-integer 2))
   (check-equ? (2.85:apply-generic 'add 
                                   (make-complex-from-real-imag 5 1)
                                   (make-real 5))
               (make-complex-from-real-imag 10 1))))

(exit (run-tests (test-suite "All tests"
                             get-put-test
                             ddp-shared-test
                             arithmetics
                             coercion
                             tower)))
