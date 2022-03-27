#lang racket
;; 习题3.3.4 设计程序area-pipe,计算一个管道(管道 是- -中空的圆柱体)的表面积。程序的输入为管道的内半径、长度和厚度。请设计两种版本，- - 个版本的程序仅包含单-一函数，另一个版本的程序包含若千个函数定义。考虑一下哪一种版本的程序更有用。
(define PI 3.14)
(define (area r)
  (* PI (* r r)))
(define (circumference r)
  (* 2 PI r))
(define (area_pipe r l h)
  (+ (* (circumference r) h)
     (* (- (area r) (area l)) 2)))
(area_pipe 10 3 10) ;; 381 * PI
