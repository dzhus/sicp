#lang racket

;;; Monte-Carlo 2D-integral esitmation

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (integral-experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo trials integral-experiment))

(define (estimate-pi trials)
  (let ((disc-area (estimate-integral
                    (lambda (x y)
                      (< (+ (expt x 2) (expt y 2)) 1))
                    -1 1 -1 1 trials))
        (square-area 4))
    (* disc-area square-area)))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
