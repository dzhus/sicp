#lang racket

;;; Library

;; Wires

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation - WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; Queue

(define (front-ptr q) (mcar q))

(define (rear-ptr q) (mcdr q))

(define (set-front-ptr! q p) (set-mcar! q p))

(define (set-rear-ptr! q p) (set-mcdr! q p))

(define (make-queue) (mcons '() '()))

(define (empty-queue? q) (null? (front-ptr q)))

(define (add-to-queue! q i)
  (let ((new-pair (mcons i '())))
    (if (empty-queue? q)
        (begin
          (set-front-ptr! q new-pair)
          (set-rear-ptr! q new-pair)
          q)
        (begin
          (set-mcdr! (rear-ptr q) new-pair)
          (set-rear-ptr! q new-pair)
          q))))

(define (delete-from-queue! q)
  (let ((head (front-ptr q)))
    (set-front-ptr! q (mcdr head))
    (mcar head)))

(define (print-queue q)
  (print (mcar q)))

;; Agenda

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (mcons 0 '()))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (current-time agenda) (mcar agenda))

(define (set-current-time! agenda time)
  (set-mcar! agenda time))

(define (segments agenda) (mcdr agenda))

(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (first-segment agenda) (mcar (segments agenda)))

(define (rest-segments agenda) (mcdr (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (add-to-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (add-to-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment time action)
                      (mcdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (mcons (make-new-time-segment time action)
                segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-from-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty - FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (mcar (front-ptr (segment-queue first-seg))))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
;;; Gates

(define (logical-not i1) (if (= i1 1) 0 1))

(define (inverter a output)
  (define (inverter-action-procedure)
    (let ((new-value (logical-not (get-signal a))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a inverter-action-procedure))

(define (logical-and i1 i2) (if (= i1 i2 1) 1 0))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;;; Ex 3.29

(define (logical-or i1 i2) (if (= i1 i2 0) 0 1))

(define (my-or-gate a1 a2 output)
  (let ((inverted-a1 (make-wire))
        (inverted-a2 (make-wire))
        (and-of-inverted (make-wire)))
    (inverter a1 inverted-a1)
    (inverter a2 inverted-a2)
    (and-gate inverted-a1 inverted-a2 and-of-inverted)
    (inverter and-of-inverted output)))

(require srfi/1)

(define (test-boolean-gate gate logical-function)
  (let ((a (make-wire))
        (b (make-wire))
        (output (make-wire)))
    (gate a b output)
    (let
        ((inputs '((0 . 0)
                   (1 . 0)
                   (0 . 1)
                   (1 . 1)))
         (check-with-inputs
          (lambda (i1 i2)
            (set-signal! a i1)
            (set-signal! b i2)
            (let ((propagation-result (propagate)))
              (print (format "f(~a, ~a) = ~a (t=~a)" (get-signal a) (get-signal b) (get-signal output) (current-time the-agenda)))
              (newline)
              (= (get-signal output) (logical-function (get-signal a) (get-signal b)))))))
      (if
       (every (lambda (input-pair)
                (check-with-inputs (car input-pair) (cdr input-pair)))
              inputs)
       "PASS"
       "FAIL"))))

(test-boolean-gate and-gate logical-and)
(test-boolean-gate my-or-gate logical-or)
