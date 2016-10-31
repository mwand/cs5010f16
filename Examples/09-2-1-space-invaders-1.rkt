#lang racket

;; space-invaders-1.rkt

;; the world will consist of a list of Widget<%>'s, and a tick
;; counter to indicate the current time.


(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")


;; Press space to drop a new bomb.  
;; Bombs fall at a constant rate. 
;; Bombs are draggable. 

;; Helicopter just rises at a constant rate.

;; start with (run framerate).  Typically: (run 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CONSTANTS

(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 400)

(define EMPTY-CANVAS (empty-scene 200 400))

;; some arbitrary choices
(define BOMB-INITIAL-X 75)  
(define BOMB-INITIAL-Y 0)
(define BOMB-INITIAL-RADIUS 10)

(define HELI-INITIAL-X 100)
(define HELI-INITIAL-Y 300)

(define NEW-BOMB-EVENT "b")
(define NEW-HELI-EVENT "h")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Time is a NonNegative Integer

;; A Widget is an object whose class implements Widget<%>

(define-struct world-state (widgets time))

;; A WorldState is a (make-world-state ListOfWidget Time)

;; INTERP: (make-world-state lst t) represents a world containing the
;; widgets in lst at time t (in ticks).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERFACEs

;; Every object that lives in the world must implement the Widget<%>
;; interface.

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

    ; KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world : -> WorldState
;; RETURNS: a world with a helicopter and no bombs
(define (initial-world)
  (make-world-state
    (list (new-heli))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; run : PosReal -> World
; GIVEN: a frame rate, in secs/tick
; EFFECT: runs an initial world at the given frame rate
; RETURNS: the final state of the world
(define (run rate)
  (big-bang (initial-world)
    (on-tick world-after-tick rate)
    (on-draw world-to-scene)
    (on-key  world-after-key-event)
    (on-mouse world-after-mouse-event)))

;; GP: Let's say the world should launch a new helicopter every 10
;; ticks.  How could we accomplish that?

;; world-after-tick : WorldState -> WorldState
;; Use HOFC map on the Widget's in w
(define (world-after-tick w)
  (let ((objs (world-state-widgets w))
        (t (world-state-time w)))
    (make-world-state
      (map
        (lambda (obj) (send obj after-tick))
        objs)
      (+ 1 t))))

;; world-to-scene : WorldState -> Scene
;; Use HOFC foldr on the Widgets in w
(define (world-to-scene w)
  (foldr
    ;; Widget Scene -> Scene
    (lambda (obj scene)
      (send obj add-to-scene scene))
    EMPTY-CANVAS
    (world-state-widgets w)))


;; world-after-key-event : WorldState KeyEvent -> WorldState
;; STRATEGY: Cases on kev
;; "b" and "h" create new bomb and new helicopter;
;; other keystrokes are passed on to the widgets in the world.

(define (world-after-key-event w kev)
  (let ((objs (world-state-widgets w))
        (t (world-state-time w)))
    (cond
      [(key=? kev NEW-BOMB-EVENT)
       (make-world-state
        (cons (new-bomb t) objs)
        t)]
      [(key=? kev NEW-HELI-EVENT)
       (make-world-state
        (cons (new-heli) objs)
        t)]
      [else
       (make-world-state
        (map
         (lambda (obj) (send obj after-key-event kev))
         (world-state-widgets w))
        t)])))

;; world-after-mouse-event : WorldState Nat Nat MouseEvent -> WorldState
;; STRATGY: Cases on mev
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev "button-down")
     (world-after-button-down w mx my)]
    [(mouse=? mev "drag")
     (world-after-drag w mx my)]
    [(mouse=? mev "button-up")
     (world-after-button-up w mx my)]
    [else w]))

; WorldState Nat Nat -> WorldState
; STRATEGY: Use HOF map on the widgets in w
(define (world-after-button-down w mx my)
  (let ((objs (world-state-widgets w))
        (t (world-state-time w)))
    (make-world-state
      (map
        (lambda (obj) (send obj after-button-down mx my))
        objs)
      t)))
    
     
(define (world-after-button-up w mx my)
  (let ((objs (world-state-widgets w))
        (t (world-state-time w)))
    (make-world-state
      (map
        (lambda (obj) (send obj after-button-up mx my))
        objs)
      t)))

(define (world-after-drag w mx my)
  (let ((objs (world-state-widgets w))
        (t (world-state-time w)))
    (make-world-state
      (map
        (lambda (obj) (send obj after-drag mx my))
        objs)
      t)))

;; Challenge question: Could these 3 functions be generalized? 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Here's what a class definition looks like:

;; classes are like data definitions.  They should have a purpose statement
;; describing what information they are supposed to represent, and
;; interpretations of the fields describing the meaning of each piece of data.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We have two classes of Widget<%>s:  Helicopters and Bombs

;; Helicopters start at the bottom of the screen and rise slowly.
;; They are selectable and draggable.

;; A Heli is a (new Heli% [x Integer][y Integer]
;;                        [selected? Boolean][mx Integer][my Integer])  ;; these 3 are optional
;; A Heli represents a heli.
(define Heli%
  (class* object% (Widget<%>)

    ;; the init-fields are the values that may vary from one heli to
    ;; the next.

    ; the x and y position of the center of the heli
    (init-field x y)   

    ; is the heli selected? Default is false.
    (init-field [selected? false]) 

    ;; if the heli is selected, the position of
    ;; the last button-down event inside the heli, relative to the
    ;; heli's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; private data for objects of this class.
    ;; these can depend on the init-fields.

    ; the heli's radius
    (field [r 15])   

    ; image for displaying the heli
    (field [HELI-IMG (circle r "outline" "blue")])
    ; the heli's speed, in pixels/tick
    (field [HELISPEED -4])                      
       
    (super-new)
    
    ;; after-tick : -> Heli
    ;; RETURNS: A heli like this one, but as it should be after a tick
    ;; a selected heli doesn't move.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
        this
        (new Heli%
          [x x]
          [y (+ y HELISPEED)]
          [selected? selected?]
          [saved-mx saved-mx]
          [saved-my saved-my])))
    
    ;; after-key-event : KeyEvent -> Heli
    ;; RETURNS: A heli like this one, but as it should be after the
    ;; given key event.
    ;; DETAILS: a heli ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ; after-button-down : Integer Integer -> Heli
    ; GIVEN: the location of a button-down event
    ; STRATEGY: Cases on whether the event is in the helicopter
    (define/public (after-button-down mx my)
      (if (in-heli? mx my)
        (new Heli%
          [x x][y y]
          [selected? true]
          [saved-mx (- mx x)]
          [saved-my (- my y)])
        this))

    ; after-button-up : Integer Integer -> Heli
    ; GIVEN: the location of a button-up event
    ; STRATEGY: Cases on whether the event is in the helicopter.
    ; If the heli is selected, then unselect it.
    (define/public (after-button-up mx my)
      (if (in-heli? mx my)
        (new Heli%
          [x x][y y]
          [selected? false]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   

    ; after-drag : Integer Integer -> Heli
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether the heli is selected.
    ; If it is selected, move it so that the vector from the center to
    ; the drag event is equal to (mx, my)
    (define/public (after-drag mx my)
      (if selected?
        (new Heli%
          [x (- mx saved-mx)]
          [y (- my saved-my)]
          [selected? true]
          [saved-mx saved-mx]
          [saved-my saved-my])
        this))   


    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this heli painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image HELI-IMG x y scene))
    
    ;; in-heli? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this heli.
    (define (in-heli? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))

    ;; test methods, to probe the heli state.  Note that we don't have
    ;; a probe for radius.
    ;; -> Int
    (define/public (for-test:x) x)
    ;; -> Int
    (define/public (for-test:y) y)
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)

    ;; -> (list Int Int Boolean)
    (define/public (for-test:heli-state) (list x y selected?))
    
    ))

;; make-heli: -> Heli
;; RETURNS: a new heli near the bottom of the screen
(define (new-heli)
  (new Heli% [x HELI-INITIAL-X][y HELI-INITIAL-Y]))



;; A Bomb is a (new Bomb% [x Integer][y Integer])
;; A Bomb represents a bomb.
;; in this version, the bomb just falls.

;; Handy to have a functional interface.
;; We don't use t, now but we might do so later.
(define (new-bomb t)
  (new Bomb% [x BOMB-INITIAL-X][y BOMB-INITIAL-Y]))

(define Bomb%
  (class* object% (Widget<%>)
    (init-field x y)  ; the bomb's x and y position
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.

    ;; image for displaying the bomb
    (field [BOMB-IMG (circle 10 "solid" "red")])
    ; the bomb's speed, in pixels/tick
    (field [BOMB-SPEED 8])
   
    (super-new)
    
    ;; after-tick : -> Bomb
    ;; RETURNS: A bomb like this one, but as it should be after a tick
    ;; DETAILS: the bomb moves vertically by BOMB-SPEED
    (define/public (after-tick)
      (new Bomb% [x x][y (+ y BOMB-SPEED)]))
    
    ;; to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this bomb painted
    ;; on it.
    (define/public (add-to-scene scene)
      (place-image BOMB-IMG x y scene))   

    ;; the bomb doesn't have any other behaviors
    (define/public (after-button-down mx my) this)
    (define/public (after-drag mx my) this)
    (define/public (after-button-up mx my) this)
    (define/public (after-key-event kev) this)
    
    ;; test methods, to probe the bomb state.
    (define/public (for-test:x) x)
    (define/public (for-test:y) y)

    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Testing Framework

;; bomb-equal? : Bomb Bomb -> Boolean
;; GIVEN: two bombs
;; RETURNS: true iff they have the same x, y, and selected? fields
;; STRATEGY: morally, this is SD on the two bombs
(define (bomb-equal? b1 b2)
  (and
    (= 
      (send b1 for-test:x)
      (send b2 for-test:x))
    (=
      (send b1 for-test:y)
      (send b2 for-test:y))))  


;; Heli Heli -> Boolean
(define (heli-equal? heli1 heli2)
  (and
    (= (send heli1 for-test:x) (send heli2 for-test:x))
    (= (send heli1 for-test:y) (send heli2 for-test:y))))

(define (world-equal? w1 w2)
  (and
     (heli-equal? (send w1 for-test:heli) (send w2 for-test:heli))
     (andmap
      (lambda (b1 b2) (bomb-equal? b1 b2))
      (send w1 for-test:bombs)
      (send w2 for-test:bombs))))

;; here we've used the 2-argument version of andmap, which is available in
;; #lang racket.

;; In what way does world-equal? equate two worlds that might be different?

;; In what way does world-equal? fail to equate two worlds that might
;; reasonably considered similar? 

(begin-for-test
  
  ;; using check-equal? as we would in the functional version
  (local
    ((define b1 (new Bomb% [x 20][y 30])))
    (check-equal? 
     (send b1 after-tick)
     (new Bomb% [x 20][y (+ 30 8)])  ; BOMB-SPEED is local
     "Surprise!"))
                  
 

  (local
    ((define b1 (new Bomb% [x 20][y 30]))
     (define b2 (send b1 after-button-down 21 31))
     (define b3 (send b1 after-tick)))
    (check bomb-equal? b1 (new Bomb% [x 20][y 30]))
    (check bomb-equal? b2 (new Bomb% [x 20][y 30]))               
    (check bomb-equal? b3 (new Bomb% [x 20][y (+ 30 8)])))
                               

  )






