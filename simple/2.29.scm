(define (make-mobile left right)
  (cons left right))

;; Kettle cannot have zero or negative weight (nonsense!)
;; @assert (not (make-branch 5 -5))
;; @assert (not (make-branch 5 0))
(define (make-branch length structure)
  (if (and (not (mobile? structure)) (<= structure 0))
      #f
      (cons length structure)))

;; @assert (= (branch-length (make-branch 10 15)) 10)
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;; @assert (mobile? (branch-structure (make-branch 5 (make-mobile (make-branch 1 1) (make-branch 2 2)))))
(define (mobile? structure)
  (pair? structure))

;; @assert (kettle? (branch-structure (make-branch 1 1)))
(define (kettle? structure)
  (not (mobile? structure)))

;; @assert (= (kettle-weight (branch-structure (make-branch 5 15))) 15)
(define (kettle-weight structure)
  structure)

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (total-weight structure)
  (if (mobile? structure)
      (+ (total-weight (branch-structure (left-branch structure)))
         (total-weight (branch-structure (right-branch structure))))
      (kettle-weight structure))
      
(define (balanced? mobile)
  ;; Single weight is always balanced
  (if (not (mobile? mobile))
      #t
      ;; Recursive call is ineffective as most of submobiles will
      ;; be weighed several times!
      (and (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile)))
           (= (* (branch-length (left-branch mobile))
                 (total-weight (branch-structure (left-branch mobile))))
              (* (branch-length (right-branch mobile))
                 (total-weight (branch-structure (right-branch mobile))))))))