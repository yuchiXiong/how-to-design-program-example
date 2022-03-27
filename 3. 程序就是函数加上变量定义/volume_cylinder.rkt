#lang racket
;; 习题3.3.2 设计程序volume-cylinder。给定圆柱体半径和高度,该程序计算圆柱体的体积。
(define PI 3.14)
(define (volume_cylinder r h)
  (* PI r r h))
(volume_cylinder 1 10)
