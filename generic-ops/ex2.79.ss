#lang scheme

(require "ex2.80.ss" ; Time paradox (`=zero?` predicate)
         "apply-generic.ss")

(provide equ?)

;; I can't use `sub` from `generic-arith.ss` here because its
;; implementation depends on `equ?` (that would be possible if *my*
;; `sub` used standard `apply-generic`, not version from 2.85)
(define (sub x y) (apply-generic 'sub x y))

;; Note that I cannot compare two different types (see comments above)
;; Probably I should check operand types before attempting subtraction
(define (equ? x y)
  (=zero? (sub x y)))
