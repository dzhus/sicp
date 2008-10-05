#lang scheme

(require "generic-arith.ss")

(provide =zero?)

(define (=zero? x)
  ;; Probably I should compare contents
  (equal? (sub x x) (add x x)))