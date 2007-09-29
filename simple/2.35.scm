(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

;; This one doesn't really follow template given in SICP
(define (count-leaves tree)
  (accumulate (lambda (subtree rest)
                (+ (if (not (pair? subtree))
                       1
                       (count-leaves subtree))
                   rest))
              0
              tree))

;; Version following SICP template
(define (count-leaves-2 tree)
  (accumulate (lambda (x y) (+ x y))
              0
              (map (lambda (item)
                     (if (not (pair? item))
                         1
                         (count-leaves-2 item)))
                   tree)))