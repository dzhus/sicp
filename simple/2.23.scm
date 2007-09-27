(define (for-each proc items)
  (if (not (null? items)) (proc (car items)))
  (if (null? items)
      #t
      (for-each proc (cdr items))))