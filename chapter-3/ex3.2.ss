#lang racket

(define (make-monitored f)
  (define counter 0)
  (define (dispatch arg)
    (cond ((eq? arg 'how-many-calls?) counter)
          ((eq? arg 'reset-count) (set! counter 0))
          (else (set! counter (add1 counter))
                (f arg))))
  dispatch)

