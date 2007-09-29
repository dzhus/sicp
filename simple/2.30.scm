(define (square x) (expt x 2))

(define (square-tree tree)
  (cond ((null? tree)
         tree)
        ((not (pair? tree))
         (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))

(define (square-tree-highorder tree)
  (map (lambda (item)
         (if (pair? item)
             (square-tree-highorder item)
             (square item)))
       tree))
             