;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname binary-search-count) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; binary search

;; binary search is another example of general recursion.



;; binary-search-count : Nat (Nat -> Number) Number -> Nat
;; GIVEN: a number N, a function f : Nat -> Number and a number tgt
;; WHERE: f is monotonic (ie, i<=j implies f(i) <= f(j))
;; RETURNS: the number of i s.t. f(i) = x
;; EXAMPLES/TESTS: See below

;; STRATEGY: call a more general function
(define (binary-search-count N f tgt)
  (binary-search-count-loop 0 N f tgt))

;;  we generalize on 0 and N:

;; binary-search-count-loop : Nat Nat (Nat -> Number) Number -> Nat
;; GIVEN: two numbers lo and hi, a function f and a target tgt
;; WHERE: f is monotonic (ie, i<=j implies f(i) <= f(j))
;; AND: if i < lo then f(i) < x
;; AND: if hi < i, then x < f(i)
;; RETURNS: the number of i s.t. f(i) = x
;; STRATEGY: general recursion
;; HALTING MEASURE: (max (- hi lo) 0)
;; TERMINATION ARGUMENT: (max (- hi lo) 0) is guaranteed to be
;; non-negative,and it decreases at every recursive call, because p is
;; excluded. 
(define (binary-search-count-loop lo hi f tgt)
  (cond
    [(> lo hi) 
     ;; the search range is empty, return false
     0]    
    [(= lo hi) 
     ;; the search range has size 1
     (if (equal? (f lo) tgt) 1 0)] 
    [else (local
            ((define midpoint (floor (/ (+ lo hi) 2)))
             (define f-of-midpoint (f midpoint)))
            (cond
              [(< f-of-midpoint tgt)
               ;; the tgt is in the right half
               (binary-search-count-loop (+ midpoint 1) hi f tgt)]
              [(> f-of-midpoint tgt)
               ;; the tgt is in the left half
               (binary-search-count-loop lo (- midpoint 1) f tgt)]
              [else 
                ;; look on either side of the midpoint, and add 1
               (+ 1
                  (binary-search-count-loop lo (- midpoint 1) f tgt)
                  (binary-search-count-loop (+ midpoint 1) hi f tgt))]))]))

;; observe that we can say (+ midpoint 1) and (- midpoiont 1) because
;; in these cases we know that midpoint can't be the answer.  

;; Furthermore, without these, we wouldn't be guaranteed that 
;; (- hi lo) would decrease (think about a search range of size 2!) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require rackunit)
(require "extras.rkt")

(begin-for-test
  ;; successful search
  (check-equal?
    (binary-search-count 12 sqr 49)
    7)
  ;; unsuccessful search
  (check-equal?
    (binary-search-count 12 sqr 48)
    false)
  ;; make sure we don't miss the endpoints
  (check-equal?
    (binary-search-count 12 sqr 0)
    0)
  (check-equal?
    (binary-search-count 12 sqr 144)
    12)
  
  )

(begin-for-test
  (check-equal?
   (binary-search
