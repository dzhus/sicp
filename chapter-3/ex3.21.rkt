#lang racket

(define (front-ptr q) (mcar q))

(define (rear-ptr q) (mcdr q))

(define (set-front-ptr! q p) (set-mcar! q p))

(define (set-rear-ptr! q p) (set-mcdr! q p))

(define (make-queue) (mcons '() '()))

(define (empty-queue? q) (null? (front-ptr q)))

(define (add-to-queue q i)
  (let ((new-pair (mcons i '())))
    (if (empty-queue? q)
        (begin
          (set-front-ptr! q new-pair)
          (set-rear-ptr! q new-pair)
          q)
        (begin
          (set-mcdr! (rear-ptr q) new-pair)
          (set-rear-ptr! q new-pair)
          q))))

(define (delete-from-queue q)
  (let ((head (front-ptr q)))
    (set-front-ptr! q (mcdr head))
    (mcar head)))

(define (print-queue q)
  (print (mcar q)))
