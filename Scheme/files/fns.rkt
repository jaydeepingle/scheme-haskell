#lang racket
;;-*- mode: scheme; -*-
;; :set filetype=scheme

;;Return a 2-element list containing the 2 roots of the quadratic
;;equation a*x^2 + b*x + c = 0 using the classical formula for the
;;roots of a quadratic equation.  The first element in the returned list
;;should use the positive square-root of the discriminant, the second
;;element should use the negative square-root of the discriminant.
(define (quadratic-roots a b c)
  (let ([x (sqrt (- (* b b) (* 4 a c)))])
    (let ([y (* 2 a)])
      (list (/ (+ (- b) x) y) (/ (- (- b) x) y)))))
;;Return the list resulting by multiplying each element of `list` by `x`.
(define (mul-list list x)
  (if (null? list)
    '()
    (cons
      (* x (car list)) (mul-list (cdr list) x))))
;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.
(define (sum-lengths list)
  (if (null? list)
    0
    (+ (length (car list)) (sum-lengths (cdr list)))))
;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  The computation should reflect the traditional
;;representation of the polynomial.
(define (poly-eval coeffs x)
  (if (null? coeffs)
    0
    (+ (* (car coeffs) (expt x (- (length coeffs) 1))) (poly-eval
                                                         (cdr coeffs) x))))
;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x using Horner's method.
(define (poly-eval-horner coeffs x)
  (poly-eval-horner-aux coeffs x 0))
(define (poly-eval-horner-aux coeffs x acc)
  (if (null? coeffs)
    acc
    (poly-eval-horner-aux (cdr coeffs) x (+ (car coeffs) (* acc x)))
    ))

;;Return count of occurrences equal? to x in exp
(define (count-occurrences s-exp x)
  (if (null? s-exp)
    0
    (if(equal? x (car s-exp))
      (+ 1 (count-occurrences (cdr s-exp) x))
      (if(list? (car s-exp))
        (+ (count-occurrences (cdr s-exp) x) (count-occurrences (car s-exp) x))
        (count-occurrences (cdr s-exp) x)))))


;;Return result of evaluating arith expression over Scheme numbers
;;with fully parenthesized prefix binary operators 'add, 'sub, 'mul
;;and 'div.
(define (eval-arith exp)
  (if (number? exp)
    exp
    (cond ((equal? (car exp) 'add)
           (+ (eval-arith (cadr exp)) (eval-arith (caddr exp))))
          ((equal? (car exp) 'sub)
           (- (eval-arith (cadr exp)) (eval-arith (caddr exp))))
          ((equal? (car exp) 'mul)
           (* (eval-arith (cadr exp)) (eval-arith (caddr exp))))
          ((equal? (car exp) 'div)
           (/ (eval-arith (cadr exp)) (eval-arith (caddr exp)))))
    ))


;;Given a proper-list list of proper-lists, return sum of lengths of
;;all the contained lists.  Must be tail-recursive.
;;tail
(define (sum-lengths-tr list)
  (sum-length-aux list 0))
(define (sum-length-aux list a)
  (if (null? list)
    a
    (sum-length-aux (cdr list) (+ a (length (car list))))))


;;Evaluate polynomial with list of coefficients coeffs (highest-order
;;first) at x.  Must be tail-recursive.
;;tail
(define (poly-eval-tr coeffs x)
  (poly-eval-tr-aux coeffs x 0))
(define (poly-eval-tr-aux coeffs x a)
  (if (null? coeffs)
    a
    (poly-eval-tr-aux (cdr coeffs) x (+ a (* (car coeffs) (expt x (-
                                                                    (length coeffs) 1)))))))

;;Return the list resulting by multiplying each element of `list` by `x`.
;;Cannot use recursion, can use one or more of `map`, `foldl`, or `foldr`.
(define (mul-list-2 list x)
  (map (lambda (n) (* n x)) list))

;;Given a proper-list list of proper-lists, return the sum of the
;;lengths of all the contained lists.  Cannot use recursion, can use
;;one or more of `map`, `foldl`, or `foldr`.
(define (sum-lengths-2 list)
  (foldl + 0 (map (lambda (n) (length n)) list)))
