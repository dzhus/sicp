#lang scheme

;;; Tests for polynomials

(require (planet schematics/schemeunit:3)
         (planet schematics/schemeunit:3/text-ui))

(require "../ex2.79.ss"
         "../generic-arith.ss"
         "../packages.ss"
         "polynomial.ss")

;; `M-w C-y` from `../tests.ss`
(define-simple-check (check-equ? x y)
  (check-true (equ? x y)))

(define-test-suite polynomial-test
  (test-case
   "Addition and multiplication"
   (check-equ? (add (make-polynomial
                     'y (make-terms (map make-real '(10 3))))
                    (make-polynomial
                     'y (make-terms (map make-real '(9 5 3)))))
               (make-polynomial
                'y (make-terms (map make-real '(9 15 6)))))
   (check-equ? (mul (make-polynomial
                     'x (make-terms (list (make-real 1)
                                          (make-real -1))))
                    (make-polynomial
                     'x (make-terms (list (make-real 1)
                                          (make-real 1)))))
               (make-polynomial
                'x (make-terms (list (make-real 5) (make-rational 0 5)
                                     (make-real -1)))))))

(exit (run-tests (test-suite "Polynomial tests"
                             polynomial-test)))