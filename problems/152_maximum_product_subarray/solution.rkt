#lang racket

;; Leetcode 152. Maximum Product Subarray

(struct state (curr-max curr-min global-max) #:transparent)

;; update-state : number state -> state
(define (update-state num st)
  ;; let  is parallel binding
  ;; let* is sequential binding
  (let* ([prev-max (state-curr-max st)]
         [prev-min (state-curr-min st)]
         ; Consider all three ways to form the new product at this step
         [c1 num]
         [c2 (* num prev-max)]
         [c3 (* num prev-min)]
         [new-max (max c1 c2 c3)]
         [new-min (min c1 c2 c3)]
         [new-global (max (state-global-max st) new-max)])
    (state new-max new-min new-global)))

;; max-product : (listof number) -> number
;; Returns the maximum product of a contiguous subarray.
(define (max-product nums)
  (cond
    [(null? nums)
     (error 'max-product "empty list has no maximum product")]
    [else
     (define first (car nums))
     (define init (state first first first))
     (state-global-max
      (foldl update-state init (cdr nums)))]))


(define (show nums)
  (printf "nums = ~a  =>  max-product = ~a\n" nums (max-product nums)))

(module+ main
  (show '(2 3 -2 4))
  (show '(-2 0 -1))
  (show '(-2 -3 -4))
  (show '(1 0 -1 2 3 -5 -2)))
