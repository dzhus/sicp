#lang scheme

;;; Polynomial package, see 2.5.3

;; TODO Add raising/dropping procedures for polynomials to this
;; package. We need this because our generic operations use smart
;; `apply-generic` (which attempts type coercion and simplifications)
;; by default

(require "../ex2.80.ss" ; `=zero?`
         "../get-put.ss"
         "../ddp-shared.ss"
         "../generic-arith.ss"
         "../packages.ss")

(provide make-polynomial
         make-terms)

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (install-polynomial-package)
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable? v) (symbol? v))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2)
         (eq? v1 v2)))

  (define (add-terms l1 l2)
    (cond ((empty-termlist? l2) l1)
          ((empty-termlist? l1) l2)
          (else
           (let ((t1 (first-term l1))
                 (t2 (first-term l2)))
             (cond
              ((> (order t1) (order t2))
               (adjoin-term t1 (add-terms (rest-terms l1) l2)))
              ((< (order t1) (order t2))
               (adjoin-term t2 (add-terms l1 (rest-terms l2))))
              (else
               (adjoin-term
                (make-term (order t1)
                           (add (coeff t1) (coeff t2)))
                (add-terms (rest-terms l1) (rest-terms l2)))))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Different variables")))

  (define (mul-term-by-all-terms t1 l)
    (if (empty-termlist? l)
        (the-empty-termlist)
        (let ((t2 (first-term l)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms l))))))

  (define (mul-terms l1 l2)
    (if (empty-termlist? l1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term l1) l2)
                   (mul-terms (rest-terms l1) l2))))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Different variables")))

  (define (inverse-coeffs term-list)
    (if (empty-termlist? term-list)
        (the-empty-termlist)
        (let ((first (first-term term-list)))
          (adjoin-term
           (make-term
            (order first)
            (mul (make-integer -1) (coeff first)))
           (rest-terms term-list)))))
  
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1
                                 (make-poly (variable p2)
                                            (inverse-coeffs (term-list p2)))))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p) (empty-termlist? (term-list p))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;; Helper function a proper dense term list given a sequence of
;; coefficients.
;; `(make-polynomial 'x (make-terms 1 2 3))` gives x²+2x+3
(define (make-terms coeffs)
  (if (null? coeffs)
      (the-empty-termlist)
      (adjoin-term
       (make-term (sub1 (length coeffs))
                  (first coeffs))
       (make-terms (drop coeffs 1)))))