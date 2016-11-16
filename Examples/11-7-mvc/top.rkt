#lang racket

; (require "Interfaces.rkt")
(require "Model.rkt")
(require "World.rkt")
(require "ControllerFactory.rkt")


(define (run rate)
  (let* ((m (new Model%))
         (w (make-world m 400 300)))
    (begin
      (send w add-widget
        (new ControllerFactory% [m m][w w]))
      (send w run rate))))

    
        


