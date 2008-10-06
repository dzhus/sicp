#lang scheme

;;; Simple coercion

(require "get-put.ss"
         "ddp-shared.ss"
         "coercion-shared.ss")

(provide apply-generic)

;; Version of `apply-generic` which introduces very simple type
;; coercion strategy, trying to coerce arguments of _binary_
;; operations to each other's type
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags))
                  (a1 (car args))
                  (a2 (cadr args)))
              ;; Here's the difference:
              (if (not (eq? type1 type2))
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                           (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic op a1 (t2->t1 a2)))
                          (else
                           (error "NOT IMPLEMENTED FOR GIVEN TYPES"))))
                  (error "NOT IMPLEMENTED FOR GIVEN TYPE")))
            (error "COULD NOT COERCE MORE THAN 2 ARGUMENTS")))))