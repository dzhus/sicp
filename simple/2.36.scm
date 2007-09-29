(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op initial (map car seqs))
            (accumulate-n op initial (map cdr seqs)))))