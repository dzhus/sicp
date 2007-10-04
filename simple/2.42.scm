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

;; Return a subboard of one column
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

;; Return sides (width, height) of a minimum rectangle surrounding all
;; positions
(define (board-dimensions board)
  (fold (lambda (position bound)
          (cons (max (get-row position)
                     (get-row bound))
                (max (get-column position)
                     (get-column bound))))
        (cons 1 1)
        board))

(define (print-board board)
  (let ((size (board-dimensions board)))
    (let ((width (car size))
          (height (cdr size)))
      (for-each
       (lambda (current)
         (let ((row-positions (from-row current board)))
           (for-each
            (lambda (current-column)
              (if (not
                   (null? (from-column current-column row-positions)))
                  (display "*")
                  (display ".")))              
            (enumerate-n width)))
         (newline))
       (enumerate-n height)))))

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
