(define (no-more? coin-list)
  (= (length coin-list) 0))

(define (first-value coin-list)
  (car coin-list))

(define (except-first coin-list)
  (cdr coin-list))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount 
                     (except-first coin-values))
                 (cc (- amount (first-value coin-values)) 
                     coin-values)))))

         