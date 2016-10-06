;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 07-3-fredexps) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; foombles-- with and without accumulators

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

;; The information:

;; FredExp ::= Variable 
;;          | (lambda (Variable) FredExp) 
;;          | (FredExp FredExp)
;; Variable = x | y | z | ... | xx | yy | zz | ... 

;; A variable is free if it occurs in a place that is not inside a lambda
;; with the same name. 

;; Information Analysis:

;; The information does not specify the external representation
;; (information) incomplete detail, so we have some freedom in designing
;; the internal representation (data).

;; IMPORTANT: We are NOT writing functions to convert from an external
;; representation to this internal representation (or vice versa).
;; That will come next week.

;; Data Design:

;; We represent foombles using recursive structures.

(define-struct var (name))
(define-struct lam (var body))
(define-struct app (fn arg))

;; A FredExp is one of
;; (make-var Symbol)     
;; (make-lam Symbol FredExp)  
;; (make-app FredExp FredExp)
;; interpretation: the cases represent variables, lambdas, and
;; applications, repectively.

;; We could have represented variables using strings instead of
;; symbols, but using symbols makes it a little easier to build
;; examples. 

;; We also could have used a naked symbol rather than a symbol in a
;; struct.  But the representation we chose makes the template
;; clearer.

;; template:
;; fredexp-fn : FredExp -> ?
#;
(define (fredexp-fn f)
  (cond
    [(var? f) (... (var-name f))]
    [(lam? f) (...
                (lam-var f)
                (fredexp-fn (lam-body f)))]
    [(app? f) (...
                (fredexp-fn (app-fn f))
                (fredexp-fn (app-arg f)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; version without accumulators

;; free-vars : FredExp -> SetOfSymbol
;; RETURNS: the set of names that occur free in the given FredExp
;; EXAMPLE:
;; (free-vars (z (lambda (x) (x y)))) = {y, z}
;; STRATEGY: Use template for FredExp on f
#;
(define (free-vars f) 
  (cond
    [(var? f) (list (var-name f))]
    [(lam? f) (set-minus
                (free-vars (lam-body f))
                (lam-var f))]
    [(app? f) (set-union
                (free-vars (app-fn f))
                (free-vars (app-arg f)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; version with accumulator

;; free-vars-in-subexp : FredExp ListOfSymbol -> SetOfSymbol
;; GIVEN: a FredExp f that is part of a larger FredExp f0
;; WHERE: los is the list of symbols that occur in lam's above f in
;; f0
;; RETURNS: the set of symbols from f that are free in f0.

;; EXAMPLE: 
;; (free-vars-in-subexp (z (lambda (x) (x y))) (list z)) = (list y) 

;; STRATEGY: Use template for FredExp on f
(define (free-vars-in-subexp f bvars)
  (cond
    [(var? f) (if (my-member? (var-name f) bvars)
                empty
                (list (var-name f)))]     
    [(lam? f) (free-vars-in-subexp (lam-body f)
                   (cons (lam-var f)
                     bvars))]                                           
    [(app? f) (set-union
                (free-vars-in-subexp (app-fn f) bvars)
                (free-vars-in-subexp (app-arg f) bvars))]))



;; free-vars : FredExp -> SetOf<Symbol>
;; RETURNS: the set of names that occur free in the given FredExp
;; EXAMPLE:
;; (free-vars (z (lambda (x) (x y)))) = {y, z}

;; Strategy: Call a more general function
(define (free-vars f)
  (free-vars-in-subexp f empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; correctness tests

(begin-for-test

  (check set-equal?
    (free-vars (make-var 'x))
    (list 'x))

  (check set-equal?
    (free-vars
      (make-lam 'x (make-var 'x)))
    empty)

  (check set-equal?
    (free-vars
      (make-lam 'x (make-app
                           (make-var 'x)
                           (make-var 'y))))
    (list 'y))

  (check set-equal?
    (free-vars
      (make-app
        (make-var 'z)
        (make-lam 'x (make-app
                           (make-var 'x)
                           (make-var 'y)))))
    (list 'z 'y))

  (check set-equal?
    (free-vars
      (make-app
        (make-var 'x)
        (make-lam 'x (make-app
                           (make-var 'x)
                           (make-var 'y)))))
    (list 'x 'y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; STRESS TEST

;;; The hardest part of this is building a sequence of FredExps of
;;; increasing size.

;; tester : (Number -> X) Number -> Number
;; builds an example of size proportional to n, and times free-vars on it.
;; and returns n
(define (tester n)
  (local
    ((define example (build-example n)))
    (time (free-vars example))))


;; build-example : Number -> FredExp 
;; RETURNS: an example of size proportional to the given number
(define (build-example n)
  (local
    ((define type (remainder n 2)))
    (cond
      [(<= n 10) (make-var (choose-name n))]
      [(= type 0) (make-lam 
                    (choose-name n)
                    (build-example (- n 1)))]
      [(= type 1) (make-app
                    (build-example (- n 1))
                    (build-example (- n 2)))])))


;; Number -> Symbol
(define (choose-name n)
  (cond
    [(<= n 1) 'x]
    [(= n 2) 'y]
    [(= n 3) 'z]
    [(= n 4) 'u]
    [(= n 5) 'v]
    [else (choose-name (remainder n 5))]))

#;(check-equal? 
 (build-example 11)
 (make-app
  (build-example 5)
  (build-example 4)))

;; Any -> Boolean
;; check to see that a recursive structure is really a foomble
;; this is needed to test build-example
(define (really-a-fredexp? f)
  (cond
    [(var? f) (symbol? (var-name f))]
    [(lam? f) (and
                (symbol? (lam-var f))
                (really-a-fredexp? (lam-body f)))]
    [(app? f) (and
                (really-a-fredexp? (app-fn f))
                (really-a-fredexp? (app-arg f)))]
    [else false]))

;; n=10 should be a sufficient test for build-example
(check-true (really-a-fredexp? (build-example 20)))
 
(define (fredexp-size f)
  (cond
    [(var? f) 1]
    [(lam? f) (+ 1 (fredexp-size (lam-body f)))]
    [(app? f) (+ 1 
                (fredexp-size (app-fn f))
                (fredexp-size (app-arg f)))]))


#;(build-list 40 (lambda (n) (list n (fredexp-size (build-example n)))))

;; (fredexp-size (build-example 30))
;; (fredexp-size (build-example 40))
;; (fredexp-size (build-example 45))
;; (fredexp-size (build-example 50))

(define (stress-tests dummy)
  (list
  (tester 30)
  (tester 40)
  (tester 45)
  (tester 50))
  )
 
; (stress-tests 1)


