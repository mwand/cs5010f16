(require rackunit)
(require "extras.rkt")

;; zero-of-quadratic? : Real^4 -> Boolean
;; RETURNS: whether ax^2 + bx + c < tolerance
(define (zero-of-quadratic? a b c x tol)
  (< (+ (* a x x)
       (* b x)
       c)
    tol))

(define (quadratic-solution1 a b c)
  (/ (+ (- b) (sqrt (- (* b b) (* 4 a c))))
    (* 2 a)))

(define (quadratic-solution2 a b c)
  (/ (- (- b) (sqrt (- (* b b) (* 4 a c))))
    (* 2 a)))

(begin-for-test
  (check-true (zero-of-quadratic? 1 0 4 (quadratic-solution1 1 0 4)))
  (check-true (zero-of-quadratic? 1 0 4 (quadratic-solution2 1 0 4)))
  (check-true (zero-of-quadratic? 1 0 1 (quadratic-solution1 1 0 1)))
  (check-true (zero-of-quadratic? 1 0 1 (quadratic-solution2 1 0 1)))
  (check-true (zero-of-quadratic? 13 58 6 (quadratic-solution1 3 5 6))))



