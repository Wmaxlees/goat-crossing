#lang racket

(require "graph-search.rkt")

(struct goat-state (size goats)
  #:transparent)

(define-ruleset goat-crossing-ruleset)

; 1 for black
; 0 for white
; Index starts from the right
(define (generate-start-state black white)
  (define result white)
  (for ([i (in-range 0 black)])
    (set! result (bitwise-ior result (arithmetic-shift 1 (+ i 1 white 6)))))
  (goat-state (+ black white 1) result))

(define (generate-goal-state black white)
  (define result black)
  (for ([i (in-range 0 black)])
    (set! result (bitwise-ior result (arithmetic-shift 1 (+ i 6)))))
  (goat-state (+ black white 1) result))

(define-rule goat-crossing-ruleset (B-move state)
  (B-next-to-free? state)
  =>
  (move state (+ (free-slot state) 1) (free-slot state)))
(define-rule goat-crossing-ruleset (B-jump state)
  (B-one-from-free? state)
  =>
  (move state (+ (free-slot state) 2) (free-slot state)))
(define-rule goat-crossing-ruleset (B-double-jump state)
  (B-two-from-free? state)
  =>
  (move state (+ (free-slot state) 3) (free-slot state)))


(define-rule goat-crossing-ruleset (W-move state)
  (W-next-to-free? state)
  =>
  (move state (- (free-slot state) 1) (free-slot state)))
(define-rule goat-crossing-ruleset (W-jump state)
  (W-one-from-free? state)
  =>
  (move state (- (free-slot state) 2) (free-slot state)))
(define-rule goat-crossing-ruleset (W-double-jump state)
  (W-two-from-free? state)
  =>
  (move state (- (free-slot state) 3) (free-slot state)))

(define (W-next-to-free? state)
  (and (> (- (free-slot state) 1) -1)
       (W? state (- (free-slot state) 1))))
(define (W-one-from-free? state)
  (and (> (- (free-slot state) 2) -1)
       (W? state (- (free-slot state) 2))))
(define (W-two-from-free? state)
  (and (> (- (free-slot state) 3) -1)
       (W? state (- (free-slot state) 3))))

(define (B-next-to-free? state)
  (and (< (+ (free-slot state) 1) (goat-state-size state))
       (B? state (+ (free-slot state) 1))))
(define (B-one-from-free? state)
  (and (< (+ (free-slot state) 2) (- (goat-state-size state) 1))
       (B? state (+ (free-slot state) 2))))
(define (B-two-from-free? state)
  (and (< (+ (free-slot state) 3) (- (goat-state-size state) 1))
       (B? state (+ (free-slot state) 3))))

(define (free-slot state)
  (bitwise-and (goat-state-goats state) 63))

(define (locations state)
  (arithmetic-shift (goat-state-goats state) -6))

(define (move state source-index target-index)
  (goat-state (goat-state-size state)
              (bitwise-ior (arithmetic-shift
                            ((if (W? state source-index) move-W move-B)
                             (locations state) source-index target-index) 6)
                            source-index)))

(define (set-index number index)
  (bitwise-ior number (arithmetic-shift 1 index)))
(define (reset-index number index)
  (bitwise-and number (bitwise-not (arithmetic-shift 1 index))))

(define (move-W number source target)
  (reset-index (reset-index number target) source))
(define (move-B number source target)
  (reset-index (set-index number target) source))

(define (W? state index)
  (= 0 (bitwise-and (locations state)
                    (arithmetic-shift 1 index))))
(define (B? state index)
  (define mask (arithmetic-shift 1 index))
  (= mask (bitwise-and (locations state) mask)))

(define (goat-problem black-goats white-goats)
  (define start-state (generate-start-state black-goats white-goats))
  (define goal-state (generate-goal-state black-goats white-goats))

  (define size (+ black-goats white-goats 1))
  (define high-mask (arithmetic-shift 15 (- size 4)))
  (define high-compare (arithmetic-shift 8 (- size 4)))
  (define mid-mask (arithmetic-shift 31 (- (+ size 1) 5)))
  (define mid-compare (arithmetic-shift 24 (- size 5)))
  (define full-mask 127)
  (define full-compare 56)
  (define (deadend? state)
    (define num (locations state))
    (define free (free-slot state))
    (or (= (bitwise-and 15 num) 14)
        (= (bitwise-and high-mask num) high-compare)
        (= (bitwise-and 31 num) 30)
        (= (bitwise-and mid-mask num) mid-compare)
        (for/or ([i (in-range 0 size)])
                               (and (or (< free i) (> free (+ i 6)))
                                    (= (bitwise-and num (arithmetic-shift full-mask i)) (arithmetic-shift full-compare i))))))

  (define (complete? current-state)
    (equal? current-state goal-state))

  (graph-search
   goat-crossing-ruleset
   start-state
   complete?
   #:deadend? deadend?))
