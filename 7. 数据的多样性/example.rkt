#lang racket
(define-struct Student (name age))

(define xiaoming
  (make-Student 'xiaoming 18))
(define teacher_name 'teacher_zhang)

(Student? xiaoming)
(Student? teacher_name)

;; 习题 7.1.3 开发函数 area，该函数读入一个圆形或者一个正方形，计算它的面积。考虑能不能使用 perimeter模板？
(define PI 3.14)
(define-struct circle (r))
(define-struct square (r))
(define (area p)
  (cond
    [(circle? p)
     (* PI (circle-r p) (circle-r p))]
    [(square? p) (* 4 (square-r p))]))

(define p1 (make-circle 2))
(define p2 (make-square 2))

(area p1)
(area p2)

;; 习题 7.2.1 给出动物圆中动物的结构体和数据定义，涉及的动物包括
;;  1. 蜘蛛，属性包括所剩的腿的数目（假设蜘蛛可能会在意外事故中失去一些腿）和运输它们时所需的空间大小；
;;  2. 大象，属性只包括其在运输时所需的空间大小；
;;  3. 猴子，属性包括智力和其在运输时所需的空间大小。
;; 再开发一个读入动物的函数模板。
;; 开发函数 fits?，该函数读入一个动物和一个笼子的容积，判断笼子能否容下动物。
(define-struct spider (legs space))
(define-struct elephant (space))
(define-struct monkey
               (intelligence space))
(define (fits animal cage)
  (cond
    [(spider? animal)
     (<= (spider-space animal) cage)]
    [(elephant? animal)
     (<= (elephant-space animal) cage)]
    [(monkey? animal)
     (<= (monkey-space animal) cage)]))

(define spider1 (make-spider 8 10))
(define elephant1 (make-elephant 10))
(define monkey1 (make-monkey 2 10))

(fits spider1 10)
(fits elephant1 9)
(fits monkey1 21)

;; 习题 7.5.1 area-of-disk 的自检查版本还可以要求函数的参数是一个正数，而不仅仅只是一个数。按照这个要求修改 checked-area-of-disk。
(define (checed-area-of-disk r)
  (cond
    [(and (number? r) (> r 0))
     (* PI r r)]
    [else
     (error
      "r is not a number or r is not positive")]))

(checed-area-of-disk 40)
(checed-area-of-disk 'aa)
(checed-area-of-disk -40)
