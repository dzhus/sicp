(define (list-reverse l)
  (if (null? (cdr l))
      l
      ;; Using plain `cons` won't work — compare `(cons (list 4) 1)`
      ;; and `(append (list 4) (list 1))` results
      (append (list-reverse (cdr l)) (list (car l)))))