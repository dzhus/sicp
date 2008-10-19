#lang scheme

;;; Advanced complex numbers package

(require (prefix-in std: scheme/base))

(require (except-in "generic-arith.ss"
                    add sub mul div
                    make-complex-from-real-imag
                    make-complex-from-mag-ang)
         "ddp-shared.ss"
         "get-put.ss"
         "ex2.80.ss"
         "ex2.85.ss")

(provide sin cos sqrt atan
         real-part imag-part magnitude angle
         make-complex-from-real-imag
         make-complex-from-mag-ang)

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

(put 'sin '(real) (lambda (x) (make-real (std:sin x))))
(define (sin x) (apply-generic 'sin x))

(put 'cos '(real) (lambda (x) (make-real (std:cos x))))
(define (cos x) (apply-generic 'cos x))

(put 'sqrt '(real) (lambda (x) (make-real (std:sqrt x))))
(define (sqrt x) (apply-generic 'sqrt x))

(put 'atan '(real real) (lambda (x y) (make-real (std:atan x y))))
(define (atan x y) (apply-generic 'atan x y))

;; To allow complex numbers with mixed parts (e. g. with integer real
;; part and rational imaginary part), in our advanced complex packages
;; we'll need to use versions of `add`, `sub`, `mul` and `div` which
;; know how to handle arguments of mixed type (those defined in
;; `generic-arith.ss` use standard `apply-generic`)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))



;;; Implementation
(define (install-complex-rectangular-package)
  ;; Constructors&selectors
  (define (make-from-real-imag x y) (cons x y))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  ;; Dependant selectors
  (define (magnitude z)
    (define (square x) (mul x x))
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (tag x) (attach-tag 'adv-rectangular x))
  (put 'real-part '(adv-rectangular) real-part)
  (put 'imag-part '(adv-rectangular) imag-part)
  (put 'magnitude '(adv-rectangular) magnitude)
  (put 'angle '(adv-rectangular) angle)
  (put 'make-from-real-imag 'adv-rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (install-complex-polar-package)
  (define (make-from-mag-ang mag ang) (cons mag ang))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (mul (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin (angle z))))

  (define (tag x) (attach-tag 'adv-polar x))
  (put 'real-part '(adv-polar) real-part)
  (put 'imag-part '(adv-polar) imag-part)
  (put 'magnitude '(adv-polar) magnitude)
  (put 'angle '(adv-polar) angle)
  (put 'make-from-mag-ang 'adv-polar
       (lambda (m a) (tag (make-from-mag-ang m a))))
  'done)

(install-complex-rectangular-package)
(install-complex-polar-package)

;;
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (define make-from-real-imag 
    (get 'make-from-real-imag 'adv-rectangular))
  (define make-from-mag-ang
    (get 'make-from-mag-ang 'adv-polar))

  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'adv-complex z))
  (put 'add '(adv-complex adv-complex)
       (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(adv-complex adv-complex)
       (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(adv-complex adv-complex)
       (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(adv-complex adv-complex)
       (lambda (x y) (tag (div-complex x y))))
  (put '=zero? '(adv-complex)
       (lambda (x) (=zero? (magnitude x))))
  (put 'make-from-real-imag 'adv-complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'adv-complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)

(define make-complex-from-real-imag
  (get 'make-from-real-imag 'adv-complex))

(define make-complex-from-mag-ang
  (get 'make-from-mag-ang 'adv-complex))

;; 2.77
(put 'real-part '(adv-complex) real-part)
(put 'imag-part '(adv-complex) imag-part)
(put 'magnitude '(adv-complex) magnitude)
(put 'angle '(adv-complex) angle)

