#lang racket

(define (make-accumulator initial)
  (define (increase n)
    (set! initial (+ initial n))
    initial)
  increase)
