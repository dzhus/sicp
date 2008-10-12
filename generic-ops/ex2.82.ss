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
        (if proc
            (apply proc (map contents coerced-args))
            ;; Report unsuccessful application attempt (occurs when
            ;; all arguments are coerced to single type but operation
            ;; is not implemented for it)
            ;;
            ;; This leads to a limitation that generic operations must
            ;; not return `'fail` symbol as a result. Normally
            ;; reporting may be done using exceptions (we don't want
            ;; to use exceptions here in SICP solutions, though)
            'fail)))
    ;; Try to apply operation to arguments coerced to the head of
    ;; `target-types` list. If it's impossible, try the same with next
    ;; type in list.
    (define (try-apply-generic-coercing target-types)
      (if (null? target-types)
          (error (format "OPERATION ~a NOT IMPLEMENTED FOR TYPES: ~a" op type-tags))
          (let ((coercions (get-coercions (car target-types)
                                          type-tags)))
            (if (non-false-list? coercions)
                (let ((result (apply-coercing coercions)))
                  (if (eq? result 'fail)
                      ;; Proceed to next type if coerced to wrong one
                      (try-apply-generic-coercing (cdr target-types))
                      result))
                ;; Proceed to next type in case coercion failed at all
                (try-apply-generic-coercing (cdr target-types))))))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-apply-generic-coercing type-tags)))))


;;; Limitation:
;;
;; Given the t_1 to t_2 coercion is installed, an attempt to apply
;; generic operation which is defined only for t_2 to all-t_1
;; arguments will fail!
;;
;; For example, this won't work:
;;
;; > (put-coercion 'integer 'real (lambda (x) (make-real (contents x))))
;; > (define (add-3-reals (lambda (x y z) (make-real + x y z))))
;; > (put 'add '(real real real) add-3-reals)
;; > (apply-generic 'add (make-integer 1) (make-integer 2) (make-integer 3))
;; NOT IMPLEMENTED FOR GIVEN TYPES