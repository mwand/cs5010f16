#lang racket

;; the model consists of a particle, bouncing with its center from x=0
;; to x=200.  It accepts commands and reports when its status changes

(require "extras.rkt")
(require "Interfaces.rkt")

(provide Model%)

(define Model%
  (class* object% (Model<%>)

    ;; boundaries of the field
    (field [lo 0])
    (field [hi 200])

    ;; position and velocity of the object
    (init-field [x (/ (+ lo hi) 2)])
    (init-field [v 0])

    ; ListOfController<%>
    (init-field [controllers empty])   

    (super-new)

    ;; -> Void
    ;; moves the object by v.
    ;; if the resulting x is >= 200 or <= 0
    ;; reports x at ever tick
    ;; reports velocity only when it changes
    (define/public (after-tick)
      (set! x (within-limits lo (+ x v) hi))
      (publish-position)
      (if (or (= x hi) (= x lo))
        (begin
          (set! v (- v))
          (publish-velocity))
        "model.rkt after-tick"))

    (define (within-limits lo val hi)
      (max lo (min val hi)))

    ;; Controller -> Void
    ;; register the new controller and send it some data
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position x))
        (send c receive-signal (make-report-velocity v))))

    ;; Command -> Void
    ;; decodes the command, executes it, and sends updates to the
    ;; controllers. 
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           (set! x (set-position-pos cmd))
           (publish-position))]
        [(incr-velocity? cmd)
         (begin
           (set! v (+ v (incr-velocity-dv cmd)))
           (publish-velocity))]))

    ;; report position or velocity to each controller:

    (define (publish-position)
      (let ((msg (make-report-position x)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)
        ))

    (define (publish-velocity)
      (let ((msg (make-report-velocity v)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)))

    ))




    

    