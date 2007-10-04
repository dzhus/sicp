(define (enumerate-n n)
  (define (enumerate-interval low high)
    (if (> low high)
        (list)
        (cons low (enumerate-interval (+ low 1) high))))
  (enumerate-interval 1 n))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (flatmap proc seq)
  (fold append (list) (map proc seq)))

(define (make-position row column)
  (list row))

(define (get-row position)
  position)

(define (make-board . positions)
  positions)

(define (add-position position board)
  (append board position))

(define (from-column column board)
  (list-ref board (- column 1)))

(define (first-from-column column board)
  (from-column column board))

(define (column-safe? k board)
  (accumulate
   and
   #t
   (let ((new-row (get-row
                   (first-from-column k board))))
     (map
      (lambda (column)
        (let ((row (get-row (first-from-column column board))))
          (if (not (or
                    (= new-row row)
                    (= (abs (- new-row row)) (- k column))))
              #t
              #f)))
      (enumerate-n (- k 1))))))

(define (queens board-size)
  (define (adjoin-position new-row k rest-of-queens)
    (add-position (make-position new-row k) rest-of-queens))
  (define empty-board
    (list))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (column-safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-n board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(use-modules (srfi srfi-1))
(let ((b 5))
  (display (queens b)))