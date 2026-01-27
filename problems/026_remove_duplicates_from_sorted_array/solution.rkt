#lang racket

(define (remove-duplicates nums)
  (define (helper lst prev)
    (cond
      [(empty? lst) '()]
      [(equal? (first lst) prev)
       ;; Skip this element (duplicate of previous)
       (helper (rest lst) prev)]
      [else
       ;; Keep this element (different from previous)
       (cons (first lst) (helper (rest lst) (first lst)))]))

  (if (empty? nums)
      '()
      ;; Start with first element, use a sentinel that won't match
      (cons (first nums) (helper (rest nums) (first nums)))))

;; Test cases
(remove-duplicates '(1 1 2))        ; '(1 2)
(remove-duplicates '(1 1 1 1 2 2 2 3))    ; '(1 2 3)
(remove-duplicates '(1 1 1 2 2 3 3 3 5))  ; '(1 2 3 5)