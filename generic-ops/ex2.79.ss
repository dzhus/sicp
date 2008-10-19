#lang scheme

(require "ex2.80.ss" ; Time paradox (`=zero?` predicate)
         "generic-arith.ss")

(provide equ?)

(define (equ? x y)
  (=zero? (sub x y)))