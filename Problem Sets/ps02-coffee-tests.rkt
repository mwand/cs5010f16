;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01-snacks-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "snacks.rkt")

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))
  

(begin-for-test
  (check-provided (initial-machine 3 4))
  (check-provided (machine-next-state (initial-machine 3 4) 12))
  (check-provided (machine-next-state 
                   (machine-next-state (initial-machine 3 4) 12)
                   "chocolate"))
  (check-equal? (machine-chocolates (initial-machine 3 4)) 3)
  (check-equal? (machine-carrots (initial-machine 3 4)) 4)
  (check-equal? (machine-bank (initial-machine 3 4)) 0))
