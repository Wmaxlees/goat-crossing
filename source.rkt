#lang racket

(require "graph-search.rkt")

(struct goat-state (open-location goats)
  #:transparent)

(define team-size 9)

(define-ruleset goat-crossing-ruleset)

(define (generate-start-state black white)
  (define result '())
  (for ([i (in-range 0 black)])
    (set! result (append result '(B))))
  (set! result (append result '(_)))
  (for ([i (in-range 0 white)])
    (set! result (append result '(W))))
  (goat-state black (list->vector result)))

(define (generate-goal-state black white)
  (define result '())
  (for ([i (in-range 0 white)])
    (set! result (append result '(W))))
  (set! result (append result '(_)))
  (for ([i (in-range 0 black)])
    (set! result (append result '(B))))
  (goat-state white (list->vector result)))

(define-rule goat-crossing-ruleset (B-move state)
  (and (B-next-to-free? state) (> (- (free-slot state) 1) -1))
  =>
  (move state (- (free-slot state) 1) (free-slot state)))
(define-rule goat-crossing-ruleset (B-jump state)
  (and (B-one-from-free? state) (> (- (free-slot state) 2) -1))
  =>
  (move state (- (free-slot state) 2) (free-slot state)))
(define-rule goat-crossing-ruleset (B-double-jump state)
  (and (B-two-from-free? state) (> (- (free-slot state) 3) -1))
  =>
  (move state (- (free-slot state) 3) (free-slot state)))


(define-rule goat-crossing-ruleset (W-move state)
  (W-next-to-free? state)
  =>
  (move state (+ (free-slot state) 1) (free-slot state)))
(define-rule goat-crossing-ruleset (W-jump state)
  (W-one-from-free? state)
  =>
  (move state (+ (free-slot state) 2) (free-slot state)))
(define-rule goat-crossing-ruleset (W-double-jump state)
  (W-two-from-free? state)
  =>
  (move state (+ (free-slot state) 3) (free-slot state)))



(define (B-next-to-free? state)
  (and (> (- (free-slot state) 1) -1)
       (B? state (- (free-slot state) 1))))
(define (B-one-from-free? state)
  (and (> (- (free-slot state) 2) -1)
       (B? state (- (free-slot state) 2))))
(define (B-two-from-free? state)
  (and (> (- (free-slot state) 3) -1)
       (B? state (- (free-slot state) 3))))




(define (W-next-to-free? state)
  (and (< (+ (free-slot state) 1) (vector-length (locations state)))
       (W? state (+ (free-slot state) 1))))
(define (W-one-from-free? state)
  (and (< (+ (free-slot state) 2) (- (vector-length (locations state)) 1))
       (W? state (+ (free-slot state) 2))))
(define (W-two-from-free? state)
  (and (< (+ (free-slot state) 3) (- (vector-length (locations state)) 1))
       (W? state (+ (free-slot state) 3))))



(define (free-slot state)
  (goat-state-open-location state))

(define (locations state)
  (goat-state-goats state))

(define (move state source-index target-index)
  (define result (goat-state source-index (vector-copy (locations state))))
  (vector-set! (locations result)
               target-index
               (vector-ref (locations state) source-index))
  (vector-set! (locations result)
               source-index
               '_)
  result)

(define (W? state index)
  (eq? (vector-ref (locations state) index) 'W))
(define (B? state index)
  (eq? (vector-ref (locations state) index) 'B))

(define (goat-problem black-goats white-goats)
  (define start-state (generate-start-state black-goats white-goats))
  (define goal-state (generate-goal-state black-goats white-goats))

  (define (complete? current-state)
    (equal? current-state goal-state))

  (graph-search
   goat-crossing-ruleset
   start-state
   complete?))
