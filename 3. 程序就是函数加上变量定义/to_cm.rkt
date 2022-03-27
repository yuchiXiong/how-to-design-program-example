#lang racket
;; 习题3.3.1 由于美国使用的是英制单位，而世界上其它国家使用的是公制单位，因此在世界各地旅游的人士以及和外商进行商业往来的公司常常需要在这两种度量衡之间进行转换。这是6种主要英制长度单位和公制单位的换算表:
;; 请设计函数inches->cm, feet->inches, yards->feet, rods->yards, furlongs-> rods和and miles->furlongs。
;; 请进一步设计函数feet->cm, yards->cm, rods ->inches和miles-> feet。
(define INCH_TO_CM 2.54)
(define FOOT_TO_IN 12)
(define YARD_TO_FT 3)
(define (inches_to_cm n)
  (* n INCH_TO_CM))
(define (feet_to_inches n)
  (* n 12))
;;; ... 懒得写了 - -||
