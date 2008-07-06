(load "../simple/get-put.scm")
(load "ddp-shared.scm")

;;; Complex numbers package, see 2.4.3
(define (complex-rectangular-package)
  ;; Constructors&selectors
  (define (make-from-real-imag x y) (cons x y))
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))

  ;; Dependant selectors
  (define (magnitude z)
    (define (square x) (* x x))
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (complex-polar-package)
  (define (make-from-mag-ang mag ang) (cons mag ang))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (cos (angle z))))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-mag-ang 'polar
       (lambda (m a) (tag (make-from-mag-ang m a))))
  'done)

(complex-rectangular-package)
(complex-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define make-from-real-imag
  (get 'make-from-real-imag 'rectangular))
(define make-from-mag-ang
  (get 'make-from-mag-ang 'polar))