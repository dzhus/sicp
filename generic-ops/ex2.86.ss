#lang scheme

;;; Advanced complex numbers package

(require (prefix-in basic: scheme/base))

(require "generic-arith.ss"
         "ddp-shared.ss"
         "get-put.ss"
         "ex2.85.ss")

(provide sin cos sqrt atan)

;; To implement complex numbers arbitary (integer, rational and real
;; (complex numbers with complex parts don't seem much sense to me
;; here)) with real and imaginary parts, following changes are needed:
;;
;; - Change all package to use our custom generic arithmetics instead
;;   of built-in one (`+` to `add`, `/` to `div` etc.)
;;
;; - Implement generic `sin`, `cos`, `sqrt`, `atan`


;;; Generic operations

;; Note that an advanced version of `apply-generic` from 2.85 is used
;; here, so operations are implemented for real numbers only.
;;
;; `generic-arith.ss` is a better place for these definitions

(put 'sin '(real) (lambda (x) (make-real (basic:sin x))))
(define (sin x) (apply-generic 'sin x))

(put 'cos '(real) (lambda (x) (make-real (basic:cos x))))
(define (cos x) (apply-generic 'cos x))

(put 'sqrt '(real) (lambda (x) (make-real (basic:sqrt x))))
(define (sqrt x) (apply-generic 'sqrt x))

(put 'sqrt '(real real) (lambda (x y) (make-real (basic:atan x y))))
(define (atan x y) (apply-generic 'atan x y))