#lang racket
(= 3 4)
(= 5 5)
(and (> 1 3) (< 4 3))
(or true false)
(not false)
(cond
  [(= 1 1) 1]
  [(> 3 1) 2]
  [else 3])

(define n 20)
(cond
  [(< n 10) 20]
  [(> n 20) 0]
  [else 1])

(cond
  [(< n 10) 20]
  [(and (> n 20) (<= n 30))]
  [else 1])
