;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ps01q1-tests) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "editor.rkt")  

(check-location "02" "editor.rkt")                    

;; this only tests to see if its argument evaluates successfully.
(define (check-provided val) (check-true true))



(define editor1 (make-editor "a" "b"))


(begin-for-test
  (check-provided editor1)
  (check-equal? (editor-pre editor1) "a")
  (check-equal? (editor-post editor1) "b")
  (check-provided (edit editor1 "a"))
  (check-true (editor? (edit editor1 "a")))
  )
