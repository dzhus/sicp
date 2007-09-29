(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (kettle-weight structure)
  structure)

(define (mobile? structure)
  (pair? structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (total-weight structure)
  (if (mobile? structure)
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))
      structure))
      
(define (balanced? mobile)
  ;; Single weight is always balanced
  (if (not (mobile? mobile))
      #t
      (and (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile)))
           (= (* (branch-length (left-branch mobile))
                 (total-weight (branch-structure (left-branch mobile))))
              (* (branch-length (right-branch mobile))
                 (total-weight (branch-structure (right-branch mobile))))))))