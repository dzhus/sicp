;; Using high order filtering function. Beware of recursion.
(define (filter-list-recursive predicate items)
  (if (null? items)
      items
      (append (if (predicate (car items))
                  (list (car items))
                  (list))
              (filter-list-recursive predicate (cdr items)))))

(define (same-parity-highorder . items)
  (define (same-parity? x)
    (= (remainder (car items) 2) (remainder x 2)))
  (filter-list-recursive same-parity? items))