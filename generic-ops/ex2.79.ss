#lang scheme

(require "ex2.80.ss") ; Time paradox
(require "generic-arith.ss")

(provide equ?)

(define (equ? x y)
  (=zero? (sub x y)))