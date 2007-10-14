(use-modules (srfi srfi-1))

(load "2.46.scm")

(define (make-segment from to)
  (cons from to))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-path . vectors)
  (map
   (lambda (from to)
     (make-segment from to))
   (drop-right vectors 1)
   (cdr vectors)))