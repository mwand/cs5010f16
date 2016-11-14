#lang racket

(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
;; (require "extras.rkt")

(provide 
  make-world
  Widget<%>
  SWidget<%>)

;; the problem set says we provide StatefulWorld<%>, so we'll provide it,
;; even though there's no logical necessity to do so.
(provide StatefulWorld<%>)


;; The World implements the StatefulWorld<%> interface

(define StatefulWorld<%>
  (interface ()

   ; Widget -> Void
   ; GIVEN: A widget
   ; EFFECT: add the given widget to the world
   add-widget

   ; SWidget -> Void
   ; GIVEN: A stateful widget
   ; EFFECT: add the given widget to the world
   add-stateful-widget

   ; PosReal -> Void
   ; GIVEN: a framerate, in secs/tick
   ; EFFECT: runs this world at the given framerate
   run

    ))

;; Every functional object that lives in the world must implement the
;; Widget<%> interface.

(define Widget<%>
  (interface ()

    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event and a time
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;; Every stable (stateful) object that lives in the world must implement the
;; SWidget<%> interface.

(define SWidget<%>
  (interface ()

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this widget to the state it should have
    ; following a tick.
    after-tick          

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; KeyEvent : KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))




; NonNegInt NonNegInt -> StatefulWorld<%>
(define (make-world w h)
  (new WorldState% [canvas-width w][canvas-height h]))

(define WorldState%
  (class* object% (StatefulWorld<%>)

    (init-field canvas-width)
    (init-field canvas-height)
       
    (init-field [objs empty])  ; ListOfWidget
    (init-field [sobjs empty])  ; ListOfSWidget

    (field [EMPTY-CANVAS (empty-scene canvas-width canvas-height)])

    (super-new)

    ; run : PosReal -> World
    ; GIVEN: a frame rate, in secs/tick
    ; EFFECT: runs this world at the given frame rate
    ; RETURNS: the world in its final state of the world
    ; Note: the (begin (send w ...) w) idiom
    (define/public (run rate)
      (big-bang this
        (on-tick
          (lambda (w) (begin (after-tick) w))
          rate)
        (on-draw
          (lambda (w) (to-scene)))
        (on-key
          (lambda (w kev)
            (begin
              (after-key-event kev)
              w)))
        (on-mouse
          (lambda (w mx my mev)
            (begin
              (after-mouse-event mx my mev)
              w)))))

    (define/public (add-widget w)
      (set! objs (cons w objs)))

   (define/public (add-stateful-widget w)
      (set! sobjs (cons w sobjs)))

    ;; (Widget or SWidget -> Void) -> Void
    (define (process-widgets fn)
      (begin
        (set! objs (map fn objs))
        (for-each fn sobjs)))

    ;; after-tick : -> Void
    ;; Use map on the Widgets in this World; use for-each on the
    ;; stateful widgets

    (define (after-tick)
      (process-widgets
        (lambda (obj) (send obj after-tick))))

    ;; to-scene : -> Scene
    ;; Use HOFC foldr on the Widgets and SWidgets in this World
    ;; Note: the append is inefficient, but clear..
      
    (define (to-scene)
      (foldr
        (lambda (obj scene)
          (send obj add-to-scene scene))
        EMPTY-CANVAS
        (append objs sobjs)))

    ;; after-key-event : KeyEvent -> WorldState
    ;; STRATEGY: Pass the KeyEvents on to the objects in the world.

    (define (after-key-event kev)
      (process-widgets
        (lambda (obj) (send obj after-key-event kev))))

    ;; world-after-mouse-event : Nat Nat MouseEvent -> WorldState
    ;; STRATGY: Cases on mev
    (define (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [else this]))

    ;; the next few functions are local functions, not in the interface.

    (define (world-after-button-down mx my)
      (process-widgets
       (lambda (obj) (send obj after-button-down mx my))))
    
     
    (define (world-after-button-up mx my)
      (process-widgets
        (lambda (obj) (send obj after-button-up mx my))))


    (define (world-after-drag mx my)
      (process-widgets
        (lambda (obj) (send obj after-drag mx my))))

    ))
