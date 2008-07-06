(load "2.68.scm")

;;; Huffman tree generation
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge
       (adjoin-set 
        (make-code-tree (car leaves)
                        (cadr leaves))
        (cddr leaves)))))


;;; Sample

;; > (define rock-tree (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1))))
;; > (define message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
;; > (length (encode message rock-tree))
;; 84
;;
;; > (length message)
;; 36
;; 
;; Using fixed-length code (3 bits per symbol (2³=8)) would take 36×3=108 bits.
;;
;; 2.71 — 1 bit for the most frequent symbol, n-1 for the least one.