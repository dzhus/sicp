(load "2.51.scm")

(define (hor-split painter)
  (below painter painter))

(define (vert-split painter)
  (beside painter painter))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (hor-split painter))
            (right (vert-split painter)))
        (let ((top-left up)
              (top-right (corner-split painter (- n 1)))
              (bottom-left painter)
              (bottom-right right))
          (beside (below top-left bottom-left)
                  (below top-right bottom-right))))))
  
(define (square-limit painter n)
  (let ((corner (corner-split painter n)))
    (let ((halve (beside (flip-horiz corner) corner)))
      (below halve (flip-vert halve)))))

;; Example:
;; (define f (make-frame (make-vect 0 0) (make-vect 0 950) (make-vect 950 0)))
;; 
;; (save-picture (((square-limit 
;;                  (square-limit wave 3) 2) 
;;                 f)
;;                (make-picture 1000 1000)))
               