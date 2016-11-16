#lang racket

;; displays as an outline rectangle with text showing the x
;; coordinate and velocity of the particle.

;; the rectangle is draggable

;; +,- increments or decrements the speed of the particle

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")

(provide VelocityController%)

;; a VelocityController% is a (new VelocityController% [model Model<%>])

(define VelocityController%
  (class* object% (Controller<%>)

    (init-field model)  ; the model

    ; Nats -- the position of the center of the controller
    (init-field [x 150] [y 100])   

    (init-field [width 120][height 50])

    (field [half-width  (/ width  2)])
    (field [half-height (/ height 2)])

    ;; the position of the particle
    (field [particle-x 0])
    (field [particle-v 0])

    ;; fields for dragging
    ;; It there has ever been a button-down in this object, then these
    ;; contain the position of last button-down relative to
    ;; center of viewer.  Else any value
    (field [selected? false])
    (field [saved-mx 0])
    (field [saved-my 0])

    (super-new)

    (send model register this)
    
    ;; Signal -> Void
    ;; decodes signal and updates local data
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (set! particle-x (report-position-pos sig))]
        [(report-velocity? sig)
         (set! particle-v (report-velocity-v sig))]))

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; STRATEGY: Cases on whether the event is in this object
    (define/public (after-button-down mx my)
      (if (in-this? mx my)
        (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
        3742))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    (define/public (after-button-up mx my)
      (set! selected? false))
      

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    (define/public (after-drag mx my)
      (if selected?
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        2744))

    (define (in-this? other-x other-y)
      (and
        (<= (- x half-width) other-x (+ x half-width))
        (<= (- y half-height) other-y (+ y half-height))))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    (define/public (add-to-scene scene)
      (place-image (viewer-image) x y scene))
    
    (define/public (after-tick) 'viewer1-after-tick-trap)

    ;; KeyEvent -> Void
    ;; interpret +,- as commands to the model
    ;; +/- alter velocity of the particle
    (define/public (after-key-event kev)
      (if selected?
        (cond
        [(key=? "+" kev)
         (send model execute-command
           (make-incr-velocity 1))]
        [(key=? "-" kev)
         (send model execute-command
           (make-incr-velocity -1))])
        3456))

    (define (current-color)
      (if selected? "red" "black"))

    ;; assemble the image of the viewer
    (define (viewer-image)
      (let ((the-data-image (data-image)))
        (overlay 
          the-data-image
          (rectangle
            (max width (+ (image-width the-data-image) 10))
            (max height (+ (image-height the-data-image) 10))
            "outline" 
            (current-color)))))

    (define (data-image)
      (above
        (text "+/- : Change velocity" 10 "black")
        (text (string-append
                "X = "
                (number->string particle-x)
                " Velocity = "
                (number->string particle-v))
          12
          "black")))

    ))

