#lang sicp

;; Library

(define (stream-ref s n)
  (if (= n 0)
      (car s)
      (stream-ref (force (cdr s)) (dec n))))

(define (stream-take s n)
  (define (go s left acc)
    (if (= left 0)
        acc
        (go (force (cdr s)) (dec left) (cons (car s) acc))))
  (reverse (go s n '())))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; Ex 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map (cons proc (map (lambda (s) (force (cdr s))) argstreams))))))

;; Ex 3.54

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams integers factorials)))

;; Ex 3.55

(define (partial-sums s)
  (stream-map + s (cons-stream 0 (partial-sums s))))

;; Ex 3.59

(define (integrate-series s)
  (mul-streams (stream-map (lambda (n) (/ 1 n)) integers) s))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define (const-stream c)
  (cons-stream c (const-stream c)))

(define (scale-stream c s)
  (mul-streams (const-stream c) s))

(define cosine-series
  (cons-stream 1 (scale-stream -1 (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Ex 3.60

(define (mul-series s1 s2)
  (cons-stream
   (* (car s1) (car s2))
   (add-streams
    (add-streams (scale-stream (car s1) (force (cdr s2)))
                 (scale-stream (car s2) (force (cdr s1))))
    (cons-stream 0
                 (mul-series (force (cdr s1)) (force (cdr s2)))))))

(define is-one
  (add-streams (mul-series sine-series sine-series)
               (mul-series cosine-series cosine-series)))

;; Ex 3.61

(define (invert-unit-series s)
  (cons-stream
   1
   (mul-series (scale-stream -1 (force (cdr s))) (invert-unit-series s))))

;; Ex 3.62

(define (div-series s1 s2)
  (if (= 0 (car s2))
      (error "div-series: zero constant term in divisor")
      (mul-series s1 (invert-unit-series s2))))

(define tan-series
  (div-series sine-series cosine-series))
