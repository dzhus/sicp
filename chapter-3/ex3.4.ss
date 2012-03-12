#lang racket

;; make-account which calls the cops after 7 failed access attempts
(define (make-account balance password)
  (define tries 0)
  (define (call-the-cops)
    (error "Stay where you are"))
  (define (with-password given-password on-success)
    (if (eq? given-password password)
        on-success
        (begin (set! tries (add1 tries))
               (if (> tries 7)
                   (call-the-cops)
                   (error "Bad password")))))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Not enough funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (with-password
     p
     (cond ((eq? m 'withdraw) withdraw)
           ((eq? m 'deposit) deposit)
           (else (error "Bad request: make-account" m)))))
  dispatch)
