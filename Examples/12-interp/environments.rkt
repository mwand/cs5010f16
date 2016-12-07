#lang racket

;; Environments for looking up values and procedures

(require rackunit)
(require "extras.rkt")

(require "datatypes.rkt")

(provide empty-env) ;  : Env
(provide make-env)  ;  : ListOfName ListOfValue Environment -> Environment
(provide env-lookup) ; : Env Name -> Value

;; Information: an environment is a partial map from names to values.

;; We represent an environment by a list of bindings.

;; A Binding (Bnd) is a (list Name Value)
(define (make-binding n v) (list n v))
(define (binding-name bnd) (first bnd))
(define (binding-value bnd)  (second bnd))


;; An Environment (Env) is one of
;; -- empty
;;    Interp: the environment that is undefined for any name.
;; -- (cons (list Name Value) Environment)
;;    Interp: (cons (list n v) env) represents the environment that
;;    is just like env, but if you ask it about n, it returns v.

;; empty-env : Env
(define empty-env empty)

;; make-env : ListOfName ListOfValue Environment-> Environment
;; GIVEN: a list of names, a list of values, and an environment
;; WHERE: the two lists have the same length and there are no
;; duplications in the list of names.
;; RETURNS: an environment extending the given one, but in which each
;; name is bound to the corresponding value.
;; EXAMPLES: See tests below
;; STRATEGY: Use 2-argument map to map make-binding across names and
;; values, then append to prefix new bindings to env.
(define (make-env names values env)
  (append
    (map make-binding names values)
    env))

;; env-lookup : Env Name -> Value
;; GIVEN: a representation of an environment env and a name n
;; RETURNS: the value of env(n), if it defined
;; EFFECT: raises "no binding" error if env(n) is undefined
;; STRATEGY: SD on Env
(define (env-lookup env n)
  (cond
    [(empty? env) (error 'env-lookup "no binding for ~s" n)]
    [else 
      (if (equal? n (binding-name (first env)))
        (binding-value (first env))
        (env-lookup (rest env) n))]))



(begin-for-test

  ;; build the environment by hand.  Note that shadowing is allowed by
  ;; the data definition, even though make-env never builds an env
  ;; with shadowing
  (local
    ((define env1 (list
                    (make-binding 'x 3)
                    (make-binding 'y 4) ; we say this binding for y
                                        ; _shadows_ the one underneath.
                    (make-binding 'z 5)
                    (make-binding 'y 6)
                    (make-binding 'u 7))))
    (check-equal? (env-lookup env1 'x) 3)
    (check-equal? (env-lookup env1 'y) 4) ; not 6
    (check-equal? (env-lookup env1 'z) 5)
    (check-equal? (env-lookup env1 'u) 7)
    (check-error  (env-lookup env1 'v)))

  ;; now do it again using make-env
  (local
    ((define env1 
       (make-env
         (list 'x 'y 'z 'u)
         (list  3  4  5  7)
         empty-env)))
    (check-equal? (env-lookup env1 'x) 3)
    (check-equal? (env-lookup env1 'y) 4) 
    (check-equal? (env-lookup env1 'z) 5)
    (check-equal? (env-lookup env1 'u) 7)
    (check-error  (env-lookup env1 'v)))

  ;; now do it again with make-env doing an extension

(local
    ((define env1 
       (make-env
         (list 'x 'y) 
         (list  3  4)
         (make-env
           (list 'y 'z 'u)
           (list  8  5  7)
           empty-env))))
    (check-equal? (env-lookup env1 'x) 3)
    (check-equal? (env-lookup env1 'y) 4) ; not 8
    (check-equal? (env-lookup env1 'z) 5)
    (check-equal? (env-lookup env1 'u) 7)
    (check-error  (env-lookup env1 'v)))

)

            

