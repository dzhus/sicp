#lang racket

(require "ex3.3.ss")

(define (make-joint source password proxy-password)
  (define (dispatch p m)
    (if (eq? p proxy-password)
        (source password m)
        (error "Bad password")))
  dispatch)
