#lang racket

; (require "Interfaces.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide World% make-world)

(define World<%>
  (interface ()

    ; SWidget<%> -> Void
    add-widget                          ; all the widgets are stateful

    ; PosReal -> Void
    run
    ))

;; A World is a (make-world model canvas-width canvas-height) 

(define (make-world m w h)
  (new World%
    [model m]
    [canvas-width w]
    [canvas-height h]))



;; A World% consists of a model and some stateful widgets.  

;; it distributes after-tick to the model and to the controllers

;; it distributes mouse events and keyboard events to the controllers
;; with for-each.

;; it distributes add-to-scene to the controllers, and collects the
;; results with foldr

(define World%
  (class* object%
    (World<%>)

    (init-field canvas-width)
    (init-field canvas-height)
    
    ;; the model, initialized to a garbage value
    (init-field model) ; [model false])     ; MaybeModel     
    (init-field [widgets empty])   ; ListOfSWidget

    ;; (Widget -> Void) -> Void
    (define (for-each-widget fn)
      (for-each fn widgets))

    ;; (Widget Y -> Y) Y ListOfWidget -> Y
    (define (foldr-widgets fn base)
      (foldr fn base widgets))

    (super-new)

    (define empty-canvas (empty-scene canvas-width canvas-height))

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

    ;; these are not public.  They are just called from run.

    (define (after-tick)
      (send model after-tick)
      (for-each-widget
        (lambda (c) (send c after-tick))))

    (define (after-key-event kev)
      (for-each-widget
        (lambda (c) (send c after-key-event kev))))

    (define (to-scene)
      (foldr-widgets
       (lambda (widget scene) (send widget add-to-scene scene))
       empty-canvas))

    ;; demux the mouse event and send button-down/drag/button-up
    ;; events to each widget
    (define (after-mouse-event mx my mev)
      (for-each-widget 
        (mouse-event->message mx my mev)))

    ;; Nat Nat MouseEvent -> (Widget -> Void)
    (define (mouse-event->message mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (lambda (obj) (send obj after-button-down mx my))]
        [(mouse=? mev "drag")
         (lambda (obj) (send obj after-drag mx my))]
        [(mouse=? mev "button-up")
         (lambda (obj) (send obj after-button-up mx my))]
        [else (lambda (obj) 'dont-look-at-this-126)]))

    (define/public (add-widget c)
      (set! widgets (cons c widgets)))

  
    ))

