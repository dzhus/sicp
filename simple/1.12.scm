(define (pascal row pos)
  (if (or (= pos 1) (= pos row))
      1
      (+ (pascal (- row 1) (- pos 1)) (pascal (- row 1) pos)))
  )
)