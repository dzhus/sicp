;; Recursive
(define (adjoin-set x set)
  (let ((y (car set)))
    (cond ((null? set) x)
          ((= x y) set)
          ((< x y) (cons x set))
          (else (cons y (adjoin-set x (cdr set)))))))

;; Iterative
(define (adjoin-set x set)
  (define (adjoin-step preceding following)
    (if (null? following)
        (append preceding (list x))
        (let ((y (car following)))
          (cond
           ((= x y) (append preceding following))
           ((< x y) (append preceding (cons x following)))
           (else (adjoin-step (append preceding (list y))
                             (cdr following)))))))
  (adjoin-step '() set))