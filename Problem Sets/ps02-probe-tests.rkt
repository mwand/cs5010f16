;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ps01-probe-qualification-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "probe.rkt")                      

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define probe1 (initial-probe 30 40))

(begin-for-test
  (check-provided (probe-turned-left probe1))
  (check-provided (probe-turned-right probe1))
  (check-provided (probe-forward probe1 20))
  (check-provided (probe-north? probe1))
  (check-provided (probe-south? probe1))
  (check-provided (probe-east? probe1))
  (check-provided (probe-west? probe1))
  (check-provided
   (probe-forward
    (probe-left 
     (probe-forward probe1 20))
    5))

  )
