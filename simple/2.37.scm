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

(define (matrix-*-vector m v)
  (map (lambda (row) (accumulate + 0 (map * row v))) m))

(define (transpose m)
  (accumulate-n cons (list) m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col) 
                  (accumulate + 0 (map * row col)))
                cols))
         m)))

(define (matrix-*-vector-2 m v)
  (map (lambda (row) (car row)) 
       (matrix-*-matrix m (map (lambda (item) (list item))
                               v))))