(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (list-pair (cdr l))))