(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (lookup given-key set)
  (cond ((null? set) #f)
        ((match? given-key (key (first-record set)))
         (first-record set))
        (else
         (lookup given-key (drop-unmatched-record set given-key)))))

;; Tree version
(define first-record entry)

(define (key record)
  (car record))

(define match? equal?)

(define (drop-unmatched-record tree given-key)
  (if (< given-key (key (entry tree)))
      (left-branch tree)
      (right-branch tree)))

;;;                      Abstraction levels:

;;;                       +---------------+
;;;                       |    lookup     |
;;;                       +------+--------+
;;;                              |
;;;                  +-----------+--------------+
;;;                  |         match?           |
;;;                  |          key             |
;;;                  |      first-record        |
;;;                  |   drop-unmatched-record  |
;;;                  +---------------+----------+
;;;                                  |
;;;                                 -+--
;;;              +-----------------/    \+--------------+
;;;              |    :TREES:      |     |   :LISTS:    |
;;;              |     equal?      |     |    equal?    |
;;;              |      car        |     |     car      |
;;;              |     entry       |     |     car      |
;;;              |  |left-branch   |     |     cdr      |
;;;              |  |right-branch  |     |              |
;;;              +-----------------+     +--------------+

;; > (define db (list->tree '((1 Chuck) (2 Bob) (3 Dylan) (4 Helga) (5 John) (6 Spike))))
;; > (lookup 5 db)
;; (5 John)