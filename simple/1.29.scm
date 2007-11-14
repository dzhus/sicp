(define (cube x) (* x x x))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integrate f a b n)
  (define h (/ (- b a) n))
  (define (next-k a k) (+ a (* k h)))
  (define (prev-k a k) (- a (* k h)))
  (define (next a) (next-k a 1))
  (define (next-2 a) (next-k a 2))
  (* (/ h 3) (+ (f a)
                (* 4 (sum f (next a) next-2 (prev-k b 1)))
                (* 2 (sum f (next-2 a) next-2 (prev-k b 2)))
                (f b))))
