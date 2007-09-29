(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
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

(define (find-triples n s)
  (define (sums-to-s? triple)
    (= (+ (car triple) (cadr triple) (caddr triple)) s))
  (filter sums-to-s?
          (flatmap 
           (lambda (i)
             (flatmap
              (lambda (j)
                (map 
                 (lambda (k)
                   (list k j i))
                 (enumerate-n (- j 1))))
              (enumerate-n (- i 1))))
           (enumerate-n n))))