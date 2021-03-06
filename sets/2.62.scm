#lang scheme

;;; Intersection and union of sets represented by sorted lists

;; Taken from the book
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))

;; 2.62
(define (union-set set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        (else
         (let ((x1 (car set1)) 
               (x2 (car set2)))
           (cond
            ((= x1 x2)
             (cons x1 (union-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (cons x1 (union-set (cdr set1) set2)))
            ((> x1 x2)
             (cons x2 (union-set set1 (cdr set2)))))))))