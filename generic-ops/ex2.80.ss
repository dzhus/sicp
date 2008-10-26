#lang scheme

;;; Testing for zero

(require "apply-generic.ss")

(provide =zero?)

;; See packages in `generic-arith.ss` for `=zero?` implementations

(define (=zero? x) (apply-generic '=zero? x))
