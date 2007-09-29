(define (list-reverse l)
  (if (null? (cdr l))
      l
      ;; Using plain `cons` won't work — compare `(cons (list 4) 1)`
      ;; and `(append (list 4) (list 1))` results
      (append (list-reverse (cdr l)) (list (car l)))))

(define (map function items)
  (define empty-list (list))
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (append result
                                  (list (function (car items)))))))
  (iter items empty-list))

(define (deep-reverse items)
  (list-reverse (map list-reverse items)))