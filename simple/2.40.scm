(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op
                      initial
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-n n)
  (enumerate-interval 1 n))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (list j i))
          (enumerate-n (- i 1))))
   (enumerate-n n)))

;; Untested
(define (prime-sum-pairs n)
  (map (make-pair-sum
        (filter prime-sum?
                (unique-pairs n)))))