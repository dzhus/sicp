;; Using high order filtering function. Beware of recursion.
(define (filter-list-recursive predicate items)
  (define empty-list (list))
  (if (null? items)
      items
      (append (if (predicate (car items))
                  (list (car items))
                  empty-list)
              (filter-list-recursive predicate (cdr items)))))

;; Iterative version
(define (filter-list-iterative predicate items)
  (define empty-list (list))
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (append result
                      (if (predicate (car items))
                          (list (car items))
                          empty-list)))))
  (iter items empty-list))

(define (same-parity-highorder . items)
  (define (same-parity? x)
    (= (remainder (car items) 2) (remainder x 2)))
  (filter-list-recursive same-parity? items))