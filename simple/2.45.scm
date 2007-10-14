(define (split combine step)
  (lambda (painter)
    (combine painter (step painter))))