(define (max a b) (if (> a b) a b))
(define (min a b) (if (< a b) a b))

(define (fun a b c)
  (+
   (sqr (max a b))
   (sqr (cond ((> c (max a b)) c)
              (else (if (> c (min a b)) c (min a b)))
              ))
   )
  )