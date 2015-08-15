;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ps01-regexp-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "regexp.rkt")                      

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define state-1 (initial-state 10))

(begin-for-test
  ;; this only tests to see if functions were provided.  Does not test correctness AT ALL
  (check-provided state-1)  
  (check-provided (next-state state-1 "a"))
  (check-provided (accepting-state? state-1))
  (check-provided (error-state? state-1))
  (check-provided (next-state (next-state state-1 "a") "b")))


