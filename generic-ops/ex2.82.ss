#lang scheme

;;; Advanced coercion

(require srfi/1
         "ddp-shared.ss"
         "get-put.ss"
         "coercion-shared.ss")

(provide apply-generic)

;; True if list does not contain any #f
(define (non-false-list? list)
  (not (any false? list)))

(define (get-coercions target-type types)
  (map (lambda (type)
         (get-coercion type target-type))
       types))

;; This version tries to coerce all arguments to the type of the first
;; one, then second one etc.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    ;; Return a list of coerced arguments
    (define (coerce-args coercions)
      (map apply coercions
           (map list args)))
    ;; Apply operation to coerced arguments
    (define (apply-coercing coercions)
      (let* ((coerced-args (coerce-args coercions))
             (new-type-tags (map type-tag coerced-args))
             (proc (get op new-type-tags)))
        (apply proc (map contents coerced-args))))
    ;; Try to apply operation to arguments coerced to the head of
    ;; `target-types` list. If it's impossible, try the same with next
    ;; type in list.
    (define (try-apply-generic-coercing target-types)
      (if (null? target-types)
          (error "NOT IMPLEMENTED FOR GIVEN TYPES")
          (let ((coercions (get-coercions (car target-types)
                                          type-tags)))
            (if (non-false-list? coercions)
                (apply-coercing coercions)
                (try-apply-generic-coercing (cdr target-types))))))

    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-apply-generic-coercing type-tags)))))

;;; Example:
;;
;; Extend complex package with some ternary operation:
;; > (define (complex-ternary z1 z2 z3) (make-complex-from-real-imag (+ (real-part z1) (real-part z2)) (- (imag-part z2) (imag-part z3))))
;; > (put 'ternary '(complex complex complex) complex-ternary)
;; > (define (ternary x y z) (apply-generic 'ternary x y z))
;; > (define z1 (make-complex-from-real-imag 1 2))
;; > (define z2 (make-complex-from-real-imag 3 0))
;; > (define z3 (make-complex-from-real-imag 8 -7))
;; > (ternary z1 z2 z3)
;; (complex rectangular 4 . 7)
;;
;; Add a scheme-number->complex coercion:
;; > (put 'scheme-number 'complex (lambda (x) (make-complex-from-real-imag (contents x) 0)))
;;
;; Try to call ternary with mixed arguments:
;; > (ternary z1 3 z3)
;; (complex rectangular 4 . 7)
;; > (ternary 1 2 z1)
;; (complex rectangular 3 . -2)
