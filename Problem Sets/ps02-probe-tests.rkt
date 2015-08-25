;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ps01-robot-qualification-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "robot.rkt")                      

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val)
  (check-true true))

(define robot1 (initial-robot 30 40))

(begin-for-test
  (check-provided (robot-left robot1))
  (check-provided (robot-right robot1))
  (check-provided (robot-forward robot1 20))
  (check-provided (robot-north? robot1))
  (check-provided (robot-south? robot1))
  (check-provided (robot-east? robot1))
  (check-provided (robot-west? robot1))
  (check-provided
   (robot-forward
    (robot-left 
     (robot-forward robot1 20))
    5))

  )
