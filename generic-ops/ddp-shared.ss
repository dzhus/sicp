#lang scheme

;;; Few basic procedures for data-driven programming (see 2.4.2)

(require "get-put.ss")
(provide attach-tag type-tag contents
         apply-generic)

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

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error "NOT IMPLEMENTED!"))))
