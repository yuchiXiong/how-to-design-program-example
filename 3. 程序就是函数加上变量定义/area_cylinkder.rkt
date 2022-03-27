#lang racket
;; 习题3.3.3 设计程序area-cylinder，给定圆柱体半径和高度，该程序计算圆柱体的表面积。
(define PI 3.14)
(define (area_cylinder r h)
  (+ (* 2 (* PI r r)) (* 2 PI r h)))
(area_cylinder 1 3)
