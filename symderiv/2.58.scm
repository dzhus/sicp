(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp number)
  (and (number? exp) (= exp number)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list a '+ b))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (make-subtraction a b)
  (make-sum a (make-product b -1)))

(define (make-product a b)
  (cond ((=number? a 1) b)
        ((=number? b 1) a)
        ((or (=number? a 0) (=number? b 0)) 0)
        ((and (number? a) (number? b)) (* a b))
        (else (list a '* b))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-division a b)
  (cond ((=number? b 1) a)
        ((=number? b 0) (error "Division by zero"))
        ((and (number? a) (number? b))
         (/ a b))
        (else (list a '/ b))))

(define (division? x)
  (and (pair? x) (eq? (cadr x) '/)))

(define (divident x) (car x))

(define (divisor x) (caddr x))

(define (make-exponentiation base exponent)
  (cond ((or (=number? exponent 0) (=number? base 1)) 1)
        ((=number? base 0) 0)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (expt base exponent))
        (else (list base '^ exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '^)))

(define (base e) (car e))

(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ;;@ $(u+v)' = u' + v'$
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ;;@ $(uv)' = u'v + uv'$
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ;;@ $\left ( \frac{u}{v} \right )' = \frac{u'v - vu'}{v^2}$
        ((division? exp)
         (make-division
          (make-subtraction
           (make-product (deriv (divident exp) var) (divisor exp))
           (make-product (deriv (divisor exp) var) (divident exp)))
          (make-exponentiation (divisor exp) 2)))
        ;;@ $(u^n)' = nu^{n-1}u'$
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation
                         (base exp) (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "Unknown expression type"))))