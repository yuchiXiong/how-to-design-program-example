#lang racket
(+ 1 2)
(sqrt 4)
(sqrt 2)
(max 1 3)

(define (isOdd n)
  (cond
    [(= n 0) #f]
    [else (isEven (- n 1))]))

(define (isEven n)
  (cond
    [(= n 0) #t]
    [else (isOdd (- n 1))]))

(isEven 999999999)
(isEven 2)
