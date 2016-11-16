#lang racket

(require "Interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require 2htdp/universe)

(provide ControllerFactory%)



(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the world in which the controllers will live
    (init-field w)   ; World<%>

    ; the model to which the controllers will be connected
    (init-field m)   ; Model<%>

    (super-new)

    ; KeyEvent -> Void
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "v") (add-viewer VelocityController%)]
        [(key=? kev "p") (add-viewer PositionController%)]
        ))


    (define/public (add-viewer viewer-class)
      (send w add-widget (new viewer-class [model m])))

    (define/public (add-to-scene s) s)
    (define/public (after-tick) 'controller-factory-after-tick-trap)

    (define/public (after-button-down mx my)
      'controller-factory-after-button-down-trap)
    (define/public (after-drag mx my)
      'controller-factory-after-drag-trap)
    (define/public (after-button-up mx my)
      'controller-factory-after-button-up-trap)

    ))



