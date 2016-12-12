#lang scheme
;;; exercise 1.1
10

(+ 5 3 4)

(- 9 1)

(/ 6 2)

(+ (* 2 4) (- 4 6))

(define a 3)
(define b (+ a 1))

(+ a b (* a b))

(= a b)

(if (and (> b a) (< b (* a b)))
b
a)
(cond (
       (= a 4) 6)
       ((= b 4) (+ 6 7 a))
       (else 25))

(+ 2 (if (> b a) b a))

(* (cond
        ((> a b) a)
	((< a b) b)
	(else -1)
   )
   (+ a 1)
   )


;;; exercise 1.2

( /
  (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
  (* 3 (- 6 2) (- 2 7))
  )
					; => -37/150


;;; exercise 1.3

(define (sum-of-largest-two-squares x y z)
  (define (square x) (* x x))
  (define (larger-of p q) (if (> p q) p q) )
  (if (> x y)
       (+ (square x) (square (larger-of y z)))
       (+ (square y) (square (larger-of x z)))
  )
)

;; > (sum-of-largest-two-squares 2 3 4)
;; 25
;; > (sum-of-largest-two-squares 2 5 4)
;; 41


;;; exercise 1.4
;; Either add (if b >= 0), or subtract (if b < 0) a from b
;; If b is negative, subtracting from a is the same as adding
;; its absolute value

;;; exercise 1.5
;; applicative order - infinitely recursive evaluation of (p) even though not needed
;; normal order (lazy evaluation) - return 0, (p) not needed so not evaluated


;;; exercise 1.6
;; Because of applicative order evaluation new-if will expand out the recursive
;; call to (sqrt-iter (improve guess x) ) and never complete

;;; exercise 1.7
;; For small numbers, 0.001 is large wrt. the guess and will terminate too soon
;;  (sqrt-iter 1.0 0.00000000000000000000000000002)
;; => Value: .03125
;;
;; For large numbers, the average fails dividing two v. similar large numbers
;; as they converge
;; (sqrt-iter 1.0 2e200)
;;Floating-point overflow

(define (sqrt x)
  (define (average x y) (/ (+ x y) 2))

  (define (improve guess x)
    (average guess (/ x guess)))

  (define (good-enough? prev-guess guess)
    (< (abs (- guess prev-guess)) 0.001))

  (define (sqrt-iter prev-guess guess x)
    (if (good-enough? prev-guess guess)
        guess
        (sqrt-iter guess (improve guess x) x)))
  
  (sqrt-iter 0 1.0 x)
)

;; Not for small numbers
;; > (sqrt 0.00000000000000000000000000002)
;; => Value: .0009765625
;;
;; Better for large 
;;> (sqrt 2e200)
;; => Value: 1.414213562373095e100

;;; exercise 1.8

(define (cube-root x)
  (define (square x) (* x x))
  (define (improve-cube-root-guess guess x)
    (/
     (+ (/ x (square guess)) (* 2 guess))
     3
     )
    )

  (define (good-enough? prev-guess guess)
    (< (abs (- guess prev-guess)) 0.001))

  (define (cube-root-iter prev-guess guess x)
    (if (good-enough? prev-guess guess)
        guess
        (cube-root-iter guess (improve-cube-root-guess guess x) x)))

  (cube-root-iter 0 1.0 x)
)
