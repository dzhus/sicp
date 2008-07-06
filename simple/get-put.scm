  
;; Dummy dispatching table (really slow)
(define (make-key operation type)
  (cons operation type))

;;; Yes, we have one single dispatching table without any way to
;;; work with mulitple independent tables at once
(define table '())

;; Empty result should be handled properly in calling function
(define (get operation type)
  (assoc-ref table (make-key operation type)))

(define (put operation type implementation)
  (set! table (acons (make-key operation type) implementation
                              table)))