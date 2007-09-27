(define (map-recursive function items)
  (if (null? items)
      items
      (cons (function (car items))
            (map-recursive function (cdr items)))))

(define (map-iterative function items)
  (define empty-list (list))
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items) (append result
                                  (list (function (car items)))))))
  (iter items empty-list))

(define (square-list items)
  (map-iterative (lambda (n) (* n n)) items))
  