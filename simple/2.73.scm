(use-modules (srfi srfi-1))


;;; Operators
(define operator car)

(define operands cdr)

(define (attach-operator operator . contents)
  (append (list operator) contents))



;;; Derivation
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp number)
  (and (number? exp) (= exp number)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
;;; Sum
(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (attach-operator '+  a b))))

(define addend first)
(define augend second)

(define (deriv-sum operands var)
  (make-sum (deriv (first operands) var)
            (deriv (second operands) var)))

;;; Product
(define (make-product a b)
  (cond ((=number? a 1) b)
        ((=number? b 1) a)
        ((or (=number? a 0) (=number? b 0)) 0)
        ((and (number? a) (number? b)) (* a b))
        (else (attach-operator '* a b))))

(define multiplier first)
(define multiplicand second)

(define (deriv-product operands var)
  (make-sum
   (make-product (deriv (multiplier operands) var) (multiplier operands))
   (make-product (multiplier operands) (deriv (multiplicand operands) var))))

;; Indefinite integral
(define (make-antideriv expression var)
  (attach-operator '∫ expression var))

(define integrated-expr first)
(define integration-var second)

(define (deriv-antideriv integral var)
  (if (same-variable? var (integration-var integral))
      (integrated-expr integral)
      (make-antideriv (deriv
                       (integrated-expr integral)
                       var)
                      (integration-var integral))))

;; P0W3R!
(define (make-exponentiation base exponent)
  (cond ((or (=number? exponent 0) (=number? base 1)) 1)
        ((=number? base 0) 0)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (expt base exponent))
        (else (attach-operator '^ (list base exponent)))))

(define base first)
(define exponent second)

(define (deriv-expon operands var)
  (make-product
   (make-product (exponent operands)
                 (make-expon
                  (base operands) (- (exponent operands) 1)))
   (deriv (base operands) var)))



;;; Sample installation:
;;; (put 'deriv '+ deriv-sum)
;;; (put 'deriv '* deriv-product)
;;; (put 'deriv '∫ deriv-antideriv)
;;; (put 'deriv '^ deriv-expon)


  
;; Dispatching table (really slow)
(define howto '())

(define (make-key operation operator)
  (cons operation operator))

(define (get operation operator)
  (let ((implementation (assoc-ref howto (cons operation operator))))
    (if implementation
        implementation
        (error "NOT IMPLEMENTED YET! LEIBNITZ GOES NASTY!"))))

(define (put operation operator rule)
  (set! howto (acons (cons operation operator) rule
                              howto)))