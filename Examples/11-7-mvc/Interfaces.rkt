#lang racket

;; new version, based on WidgetWorks

(provide World<%> SWidget<%> Controller<%> Model<%>)

(define World<%>
  (interface ()

    ; SWidget<%> -> Void
    add-widget                          ; all the widgets are stateful

    ; PosReal -> Void
    run
    ))

(define SWidget<%>
  (interface ()
    add-to-scene           ; Scene -> Scene
    after-tick             ; -> Void
    after-button-up        ; Nat Nat -> Void
    after-button-down      ; Nat Nat -> Void
    after-drag             ; Nat Nat -> Void
    after-key-event        ; KeyEvent -> Void
    ))

(define Controller<%>    
  (interface (SWidget<%>)

    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly 
    receive-signal
    
    ))

(define Model<%>
  (interface ()

    ;; -> Void
    after-tick        

    ;; Controller<%> -> Void
    ;; Registers the given controller to receive signal
    register          

    ;; Command -> Void
    ;; Executes the given command
    execute-command   
))

;; protocol: 
;; model sends the controller an initialization signal as soon as it registers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Command is one of 
;; -- (make-set-position n)
;; -- (make-incr-velocity dv)

;; A Signal is one of
;; -- (make-report-position n)
;; -- (make-report-velocity v)

;; provide the structs for Command and Signal
;; the syntax of provide in #lang racket has more options in it.
(provide 
  (struct-out set-position) 
  (struct-out incr-velocity)
  (struct-out report-position)
  (struct-out report-velocity))

(define-struct set-position (pos) #:transparent)
(define-struct incr-velocity (dv) #:transparent)
(define-struct report-position (pos) #:transparent)
(define-struct report-velocity (v) #:transparent)










