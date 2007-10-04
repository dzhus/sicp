(use-modules (srfi srfi-1))

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
  (cons row column))

(define (get-row position)
  (car position))

(define (get-column position)
  (cdr position))

(define (make-board . positions)
  positions)

(define empty-board
  (make-board))

(define (add-position position board)
  (append board (list position)))

(define (from-column column board)
  (filter (lambda (position)
            (= (get-column position)
               column))
          board))

(define (from-row row board)
  (filter (lambda (position)
            (= (get-row position)
               row))
          board))

(define (first-from-column column board)
  (car (from-column column board)))

(define (column-safe? k board)
  (fold-right
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
