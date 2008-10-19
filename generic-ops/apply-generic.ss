#lang scheme

;;; Standard version of `apply-generic`

(require "ddp-shared.ss"
         "get-put.ss")

(provide apply-generic)

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error (format "OPERATION ~a IS NOT IMPLEMENTED FOR TYPES: ~a" op type-tags)))))
