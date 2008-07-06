(use-modules (srfi srfi-1))


;;; put&get are now in separate file
(load "get-put.scm")
(define (dispatch proc dept)
  (let ((impl (get proc dept)))
    (if impl
        impl
        (error "NOT IMPLEMENTED! ASSET SOLD!"))))

(define plug put)

;; Departments might make use of this function (the existence of this
;; function in this file does not break layer abstraction as
;; departments may write their function without using `make-lookup`).
(define (make-lookup
         empty?
         match?
         key
         first-record
         drop-unmatched-record)
  (define (lookup given-key set)
    (cond ((null? set) #f)
          ((match? given-key (key (first-record set)))
           (first-record set))
          (else
           (lookup given-key (drop-unmatched-record set given-key)))))
  lookup)


;;; Generic functions
(define (get-record name department)
  ((dispatch 'get-record department) name))

(define (get-salary name department)
  (let ((record (get-record name department)))
    (if record
        ((dispatch 'get-salary department) record)
        (error "Employee not found"))))

(define (find-employee-record name . depts)
  (map (lambda (dept) (get-record name dept)) depts))


;;;; DEPARMENTS

;;; Each package `plug`s two functions — `get-record` and
;;; `get-salary`. No changes to generic functions needed.

;;; London
(define (london-dept-package)
  (define db
    '(("John" "Baker st., 12" 3500)
      ("Robert" "Westminster Abbey, 3-12a" 6000)
      ("Helen" "Chestfield, Sun st., 13" 7500)))
  
  (define (get-record name)
    ((make-lookup null? string=? car car
                  (lambda (s k) (cdr s)))
     name db))
  
  (define (get-salary record)
    (third record))

  (plug 'get-record 'london get-record)
  (plug 'get-salary 'london get-salary))

;;; India department stores yearly salaries (not monthly)
(define (india-dept-package)
  (define db
    '(("Shiram" . ((salary . 31337) (address . "--")))
      ("Rakhaj" . ((salary . 10000) (address . "Taj-Mahal")))))

  (define (get-record name)
    ((make-lookup null? string=? car car
                  (lambda (s k) (cdr s)))
     name db))

  (define (get-salary record)
    (/ ((make-lookup null? eq? car car
                     (lambda (s k) (cdr s)))
        'salary (cdr record))
       12.0))

  (plug 'get-record 'india get-record)
  (plug 'get-salary 'india get-salary))
  
;;; Chelyabinsk programmers are so austere that they use binary tree
(define (russia-dept-package)
  (define db
    '(("Petya" . 100)
      (("Boris" . 200)
       ()
       (("Grisha" . 550)
        ()
        ()))
      (("Sergey" . 750)
       (("Sasha" . 200)
        ()
        ())
       (("Slava" . 900)
        ()
        ()))))

  (define (get-record name)
    (define key car)
    (define entry car)
    (define left-branch cadr)
    (define right-branch caddr)
    (define (drop-unmatched-record tree given-key)
      (if (string<? given-key (key (entry tree)))
          (left-branch tree)
          (right-branch tree)))    
    ((make-lookup null? string=? key entry drop-unmatched-record)
     name db))

  (define get-salary cdr)
  
  (plug 'get-record 'russia get-record)
  (plug 'get-salary 'russia get-salary))

;; Example:
;; > (russia-dept-package) (london-dept-package) (india-dept-package)
;; > (get-salary "John" 'london)
;; 3500
;; > (find-employee-record "Shiram" 'russia 'india)
;; (#f ("Shiram" (salary . 31337) (address . "--")))
;; > (get-record "Boris" 'india)
;; #f
;; > (get-record "Boris" 'russia)
;; ("Boris" . 200)