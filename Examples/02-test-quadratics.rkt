;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 02-test-quadratics) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

;; zero-of-quadratic? : Real^4 -> Boolean
;; RETURNS: whether abs(ax^2 + bx + c) < .01
(define (zero-of-quadratic? a b c x)
  (< (magnitude
      (+ (* a x x)
         (* b x)
         c))
     .01))

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
  (check-true (zero-of-quadratic? 13 58 6 (quadratic-solution1 13 58 6))))



