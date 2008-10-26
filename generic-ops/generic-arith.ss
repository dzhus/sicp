#lang scheme

;;; Four Horsemen of generic arithmetics

;; Now with advanced `apply-generic` from 2.85

(require "ex2.85.ss")

(provide add sub mul div)

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
