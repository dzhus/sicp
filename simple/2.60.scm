(load "2.59.scm")

;; `intersection-set` and `element-of-set?` are _the same_

(define (adjoin-set x set)
  (cons x set))
  
(define (union-set set1 set2)
  (append set1 set2))