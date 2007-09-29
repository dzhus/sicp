(define (square x) (expt x 2))

(define (tree-map proc tree)
  (cond ((null? tree)
         tree)
        ((not (pair? tree))
         (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (tree-map-high proc tree)
  (map (lambda (item)
         (if (not (pair? item))
             (proc item)
             (tree-map-high proc item)))
       tree))

(define (square-tree tree)
  (tree-map-high square tree))