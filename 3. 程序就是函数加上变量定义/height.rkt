#lang racket
;; 习题3.3.5设计程序height以计算一枚火箭升空后在给定时刻所到达的高度。假定火箭的加速度g为常量,在时间t时速度为g.t,高度为1/2*v*t, 其中v是在时刻t时火箭的速度。
(define g 9.8)
(define (get_v t)
  (* g t))
(define (height t)
  (* 0.5 (get_v t) t))
