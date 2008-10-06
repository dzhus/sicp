#lang scheme

(require "get-put.ss")

(provide put-coercion get-coercion)

(define (put-coercion type1 type2 coercion)
  (put 'coerce (list type1 type2) coercion))

(define (get-coercion type1 type2)
  (if (eq? type1 type2)
      (lambda (t) t)
      (get 'coerce (list type1 type2))))

