;; Using high order filtering function. Beware of recursion.
(define (filter-list-recursive filter l)
  (if (null? l)
      l
      (append (if (filter (car l))
                  (list (car l))
                  (list))
              (filter-list filter (cdr l)))))

(define (same-parity-highorder . l)
  (define (same-parity? x)
    (= (remainder (car l) 2) (remainder x 2)))
  (filter-list-recursive same-parity? l))