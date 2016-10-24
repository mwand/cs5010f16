;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 08-3-reachability) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require rackunit)
(require "extras.rkt")

(require "sets.rkt")

;; graph reachability 

;; data definitions using list of edges:

;; A Node is a Symbol
;; A Graph is a ListOfEdge with no repeats

(define-struct edge (from to))
;; An Edge is a (make-edge Node Node)

(define graph1
  (list
    (make-edge 'a 'c)
    (make-edge 'b 'a)
    (make-edge 'b 'c)))

;; this is the graph from the slides
(define graph2
  (list
    (make-edge 'a 'b)
    (make-edge 'a 'c)
    (make-edge 'a 'd)
    (make-edge 'b 'c)
    (make-edge 'd 'c)
    (make-edge 'd 'f)
    (make-edge 'c 'e)
    (make-edge 'f 'g)
    (make-edge 'e 'g)
    (make-edge 'g 'b)))

(define (node=? n1 n2) (symbol=? n1 n2))

;; Node Graph -> ListofNode
(define (successors n1 loe)
  (map 
   edge-to  
   (filter
    (lambda (e) (node=? (edge-from e) n1))
    loe)))

(begin-for-test
  (check set-equal?
    (successors 'a (list (make-edge 'a 'b) (make-edge 'a 'c)))
    (list 'b 'c))
  (check set-equal?
    (successors 'a graph2)
    '(b c d)))

;; SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes
;; RETURNS: the set of all their immediate successors
;; STRATEGY: use HOF foldr on nodes

(define (all-successors nodes graph)
  (foldr
    (lambda (node s)
      (set-union
        (successors node graph)
        s))
    empty
    nodes))

(begin-for-test
  (check set-equal?
    (all-successors empty graph2)
    empty)
  (check set-equal?
    (all-successors (list 'a 'c) graph2)
    (list 'b 'c 'd 'e))
  (check set-equal?
    (all-successors (list 'f 'g) graph2)
    (list 'g 'b)))

(begin-for-test
  (check-equal?
   (path? graph2 'a 'a) 
   true
   "there should be a path from a to a in graph2")

  (check-equal?
   (path? graph2 'a 'g) 
   true
   "there should be a path from a to g in graph2")

  (check-equal?
   (path? graph2 'b 'd)
   false
   "should find no path from b to d")
  
  (check-equal?
   (path? graph2 'd 'g)
   true
   "should find a path from d to g")
  
  (check-equal?
   (path? graph2 'e 'd)
   false
   "should find no path from e to d")
  
  (check-equal? (path? graph1 'a 'b) false)

  (check-equal? (path? graph1 'b 'c) true))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; reachables : SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes in a graph
;; RETURNS: the set of nodes reachable from 'nodes'

;; EXAMPLES/TESTS
(begin-for-test
  (check set-equal?
    (reachables (list 'b) graph2)
    (list 'b 'c 'e 'g))
  (check set-equal?
    (reachables (list 'a) graph2)
    (list 'a 'b 'c 'd 'e 'f 'g)))

;;; reachables: two versions

;; reachables.v1 : SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes in a finite graph
;; WHERE: nodes = the set of nodes reachable in graph g in at most
;; steps from a set of nodes S, for some n and some set of nodes S.
;; RETURNS: the set of nodes reachable from S.
;; STRATEGY: recur on nodes + their immediate successors
;; HALTING MEASURE: the number of graph nodes NOT in nodes

(define (reachables.v1 nodes g)
  (local
    ((define candidates (all-successors nodes g)))
    (cond
      [(subset? candidates nodes) nodes]
      [else (reachables.v1
              (set-union candidates nodes)
              g)])))

;; CORRECTNESS REASONING: If 'nodes' is the set of nodes reachable
;; from S in at most n steps, then 'candidates' is the set of nodes
;; reachable from S in at most n+1 steps.  If there are no more nodes
;; reachable in n+1 steps than there were in n steps, then we have
;; found all the nodes reachable from S.

;; TERMINATION REASONING: At the recursive call, 'candidates' contains at
;; least one element that is not in 'nodes' (otherwise the subset? test
;; would have returned true).  Hence the result of the set-union is at
;; least one element bigger than 'nodes'.  So the halting measure
;; decreases. 

;; This is called a CLOSURE ALGORITHM: we want to find the smallest
;; set containing nodes and which is closed under successors.

;; This version keeps looking at the successors of the original
;; nodes.  We only need to look at the successors of the most recently
;; added nodes. We'll do that in reachables.v2.

;;;;;;;;;;;;;;;; reachables.v2 ;;;;;;;;;;;;;;;;

;; reachables1: SetOfNode SetOfNode Graph -> SetOfNode
;; GIVEN: two sets of nodes and a finite graph g
;; WHERE:
;;  nodes is the set of nodes reachable in graph g in fewer than n steps
;;        from a set of nodes S, for some S and n
;;  recent is the set of nodes reachable from S in n steps but
;;         not in n-1 steps.
;; RETURNS: the set of nodes reachable from S in g.
(define (reachables1 nodes recent g)
  (local
      ((define next
         (set-diff (all-successors recent g)
                   nodes)))
    (cond
      [(empty? next) nodes]
      [else
       (reachables1
        (append next nodes)
        next
        g)])))

;; CORRECTNESS REASONING: If the invariant is true, then 'next' is the
;; set of the nodes reachable from S in fewer than n+1 steps but not
;; in fewer than n steps.  If there are no more nodes reachable in n+1
;; steps than in n steps, then we have found all the reachable nodes.

;; Otherwise, since next and nodes are disjoint, then (append next
;; nodes) is a set (that is, no duplications), and is the set of nodes
;; reachable from S in fewer than n+1 steps.  So the recursive call to
;; reachables1 satisfies the invariant.

;; TERMINATION REASONING: If the invariant is true, then 'next' is
;; non-empty, so at the recursive call the number of nodes _not_ in
;; 'nodes' is smaller.

;; reachables.v2 : SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes in a finite graph
;; RETURNS: the set of nodes reachable from S.
;; STRATEGY: Call a more general function

(define (reachables.v2 nodes g)
  (reachables1 empty nodes g))

;; CORRECTNESS REASONING: There are no nodes reachable from 'nodes' in
;; fewer than 0 steps.  The set of nodes reachable from 'nodes' in
;; at most 0 steps is just 'nodes'.  So the call to reachables1
;; satisfies reachable1's invariant.

;; TERMINATION REASONING: No termination reasoning necessary because
;; this function relies on the termination of reachables1, which we've
;; already established.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; reachable-from? : SetOfNode SetOfNode Graph
;; GIVEN: two sets of nodes, 'newest' and 'nodes'
;; WHERE: newest is a subset of nodes.
;; AND: newest is the most recently added set of nodes
;; RETURNS: the set of nodes reachable from 'nodes'.
;; STRATEGY: recur on successors of newest that are not already in
;; nodes; halt when no more successors 
;; HALTING MEASURE: the number of graph nodes _not_ in 'nodes'

(define (reachable-from? newest nodes graph)
  (local
    ((define candidates (set-diff 
                          (all-successors newest graph)
                          nodes)))
    (cond
      [(empty? candidates) nodes]
      [else (reachable-from?
              candidates
              (append candidates nodes)
              graph)])))


;; Since candidates is disjoint from nodes, we've replaced the
;; set-union by append.

;; TERMINATION ARGUMENT:
;; At the recursive call, 'candidates' is disjoint from 'nodes', and
;; it is non-empty.  So the new value of 'nodes' is at least one
;; element larger than the old one.  Therefore the halting measure
;; decreases. 

;; What does it mean for 'newest' to be the "last set" added to
;; 'nodes'?  It means that we've already added the successors of the
;; all the previously added nodes, that is:

;; INVARIANT: (all-successors (set-diff nodes newest)) is a subset of
;; nodes. 

;; Since candidates is disjoint from nodes, we've replaced the
;; set-union by append.

;; This is called the "worklist algorithm".  
;; What we've called 'newest' is usually called the worklist.
;; we have a set ('nodes').  We apply some function to the worklist and
;; see if it discovers any new nodes,

;; we initialize newest to nodes since initially all the nodes are new.

(define (reachables.v2 nodes graph)
  (reachable-from? nodes nodes graph))

;; uncomment all but one of these lines
; (define reachables reachables.v1) "using reachables.v1"
(define reachables reachables.v2) "using reachables.v2"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; paths, 2 versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; path? : Graph Node Node -> Boolean
;; GIVEN: a graph and a source and a target node in the graph
;; RETURNS: true iff there is a path in g from src to tgt
;; EXAMPLES: See tests above

;;;;;;;;;;;;;;;; path?.v1 ;;;;;;;;;;;;;;;;

;; STRATEGY: call simpler function
;; [or more general function -- either would be OK
(define (path?.v1 graph src tgt)
  (member tgt (reachables.v2 (list src) graph)))
 
;; does this code depend on the representation of the graph?

;;;;;;;;;;;;;;;; path?.v2 ;;;;;;;;;;;;;;;;

;; better: instead of building the whole reachability set, 
;; just watch for tgt to show up:

(define (path?.v2 graph src tgt)
  (local
    ((define (reachable-from? newest nodes)
       ;; RETURNS: true iff there is a path from src to tgt in graph
       ;; INVARIANT: newest is a subset of nodes
       ;; AND:
       ;;   (there is a path from src to tgt in graph)
       ;;   iff (there is a path from newest to tgt)
       ;; STRATEGY: recur on successors of newest; halt when tgt is
       ;; found. 
       ;; HALTING MEASURE: the number of graph nodes _not_ in 'nodes'
       (cond
         [(member tgt newest) true]
         [else (local
                 ((define candidates (set-diff 
                                       (all-successors newest graph)
                                       nodes)))
                 (cond
                   [(empty? candidates) false]
                   [else (reachable-from?
                           candidates
                           (append candidates nodes))]))])))
    (reachable-from? (list src) (list src))))

;; We use conds here because the pattern for general recursion uses a
;; cond.  You could use an 'if' if you preferred.


;; uncomment one of these lines:
;; (define path? path?.v1) "using path?.v1"
(define path? path?.v2) "using path?.v2"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not provided by sets.rkt
(define (set-diff set1 set2)
  (filter
    (lambda (x) (not (my-member? x set2)))
    set1))
