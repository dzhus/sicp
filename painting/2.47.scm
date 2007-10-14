(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  ;; Also: (caddr frame)
  ;; Also (SRFI-1): (third frame)
  (car (cdr (cdr frame))))

;; There is no need to define selectors for second representation
;; because `(list a b c .. k)` is (by definition!) the same as:
;;
;; `(cons a (cons b (cons c .. k) .. ))`