(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op
                      initial
                      (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; @assert (= (reverse (list 1 2 3)) (list 3 2 1))
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))

;; @assert (= (reverse-2 (list 1 2 3)) (list 3 2 1))
(define (reverse-2 sequence)
  (fold-left (lambda (x y) (append (list y) x)) (list) sequence))

;; @assert (= (reverse (list 1 2 3)) (reverse-2 (list 1 2 3)))
;; @assert (= (reverse (reverse-2 (list 1 2 3))) (list 1 2 3))