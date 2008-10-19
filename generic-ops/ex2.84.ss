#lang scheme

;;; Coercion via raising

(require "get-put.ss"
         "ddp-shared.ss"
         "ex2.83.ss")

(provide apply-generic)

;; Count how many steps will it take to raise argument to the top of
;; the tower (zero if it's already there)
(define (distance-to-top x)
  (define (count x acc)
    (let ((raised (raise x)))
      (if (not (eq? raised 'top))
          (count raised (add1 acc))
          acc)))
  (count x 0))

;; Check if first argument is lower than second
(define (lower x y)
  (> (distance-to-top x)
     (distance-to-top y)))

;; Select highest member of list
(define (highest args)
  (last (sort args lower)))

;; Raise second argument to the type of the first one (return
;; `'beyond` if it's higher)
(define (raise-to target x)
  (if (lower target x)
      'beyond
      (if (eq? (type-tag target) (type-tag x))
          x
          (raise-to target (raise x)))))

(define (apply-generic op . args)
  (let* ((raised-args (map (lambda (arg)
                             (raise-to (highest args) arg))
                           args))
         (raised-types (map type-tag raised-args))
         (proc (get op raised-types)))
    (if proc
        (apply proc (map contents raised-args))
        ;; If top of the tower is not reached, raise all arguments and
        ;; try again (allows operations work even if arguments are of
        ;; type «B» and operation is defined only for type «A» (given
        ;; «A» is higher than «B»))
        (if (= (distance-to-top (first raised-args)) 0)
            (error (format "OPERATION ~a IS NOT IMPLEMENTED FOR TYPES: ~a" op raised-types))
            (apply apply-generic (append (list op) (map raise raised-args)))))))
