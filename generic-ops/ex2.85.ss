#lang scheme

;;; Dropping

(require "generic-arith.ss"
         "get-put.ss"
         "ddp-shared.ss"
         "ex2.83.ss" ; raise
         "ex2.79.ss" ; equ?
         "complex.ss")

(provide drop)

(define (project-complex z)
  (make-real (real-part z)))

(define (project-real x)
  (let ((c (contents x)))
    (make-rational (numerator c)
                   (denominator c))))

(define (project-rational r)
  (make-integer (numer r)))

(put 'project 'complex project-complex)
(put 'project 'real project-real)
(put 'project 'rational project-rational)
(put 'project 'integer (lambda (x) 'bottom))

;; The same as `raise` from 2.83 solution (similar assumption made (an
;; attempt to project argument already on bottom must return
;; `'bottom`))
(define (project x)
  (let* ((type (type-tag x))
         (proc (get 'project type)))
    (if proc
        (proc x)
        (error (format "PROJECTION IS NOT IMPLEMENTED FOR TYPE: ~a" type)))))

(define (drop x)
  (let ((projected (project x)))
    (if (or (eq? projected 'bottom)
            (not (equ? (raise projected) x)))
        x
        (drop projected))))
