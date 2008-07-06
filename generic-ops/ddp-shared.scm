(load "../simple/get-put.scm")

;;; Few basic procedures for data-driven programming (see 2.4.2)

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

;; Shadow these definitions with a more advanced version
(load "2.78.scm")
  
    