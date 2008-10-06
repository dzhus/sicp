#lang scheme

;;; Few basic procedures for data-driven programming (see 2.4.2)

(provide attach-tag type-tag contents)

;; See also `ex2.78.ss`

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "INCORRECT DATUM " datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "INCORRECT DATUM" datum)))
