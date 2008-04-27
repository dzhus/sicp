(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (lookup given-key tree)
  (cond ((null? tree) false)
        ((equal? given-key (key (entry tree)))
         (entry tree))
        (else
         (if (< given-key (key (entry tree)))
             (lookup given-key (left-branch tree))
             (lookup given-key (right-branch tree))))))

(define (key record)
  (car record))

;; > (define db (list->tree '((1 Chuck) (2 Bob) (3 Dylan) (4 Helga) (5 John) (6 Spike))))
;; > (lookup 5 db)
;; (5 John)