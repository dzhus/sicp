;; @assert (= (length (boolean (list 1 2 3 4 5))) (expt 2 5))
(define (boolean set)
  (if (null? set)
      (list set)
      (let ((rest (boolean (cdr set))))
        (append rest (map (lambda (s)
                            (append (list (car set)) s))
                          rest)))))
        