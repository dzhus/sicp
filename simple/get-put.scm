  
;; Dummy dispatching table (really slow)
(define (make-key operation type)
  (cons operation type))

;;; Yes, we have one single dispatching table without any way to
;;; work with mulitple independent tables at once
(define table '())

(define (get operation type)
  (let ((implementation (assoc-ref table (make-key operation type))))
    (if implementation
        implementation
        (error "NOT IMPLEMENTED YET!"))))

(define (put operation type implementation)
  (set! table (acons (make-key operation type) implementation
                              table)))