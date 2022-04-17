#lang racket

(add1 99)
(sub1 99)
(define n 0)
(zero? n)

;; 习题 11.2.1 把 hellos 一般化为 repeat，该函数读入自然数 n 和一个符号，返回包含 n 个符号的表。
(define (repeat count symbol)
  (cond
    [(zero? count) empty]
    [else
     (cons symbol
           (repeat (sub1 count)
                   symbol))]))

(repeat 2 'hello)
(repeat 0 'hello)
(repeat 10 'a)

;;  习题 11.2.2 设计函数 tabulate-f，把函数 f 应用于一些由自然数值组成的表，其中 f 是
;; f : number -> number
(define (f x)
  (+ (* 3 (* x x)) (+ (* -6 x) -1)))
;;  具体地说，函数读入自然数 n，返回由 n 个 posn 结构体组成的表，表的第一个元素是点(n (f n))，第二个元素是点(n-1 (f n-1))，以此类推。
(define-struct pson (x y))
(define (tabulate-f n)
  (cond
    [(zero? n) empty]
    [else
     (cons (make-pson n (f n))
           (tabulate-f (sub1 n)))]))

(tabulate-f 10)

;; 习题 11.2.4 表的成员也可以是表，即数据可以是表的表，甚至可以嵌套多层。下面就是这种思想下的一个极端的数据定义：
;;  deep-list（深层表）是下列两者之一
;;  1． s，其中 s 是符号
;;  2． (cons dl empty)，其中 dl 是深层表。
;;设计函数 depth，该函数读入一个深层表，测定这个表用了多少次 cons 构成。设计函数 make-deep，该函数读入符号 s 和自然数 n，返回包含 s，使用 n 次 cons 构成的表。
(define (depth dl)
  (cond
    [(symbol? (first dl)) 1]
    [else (add1 (depth (first dl)))]))

(depth (cons 'a empty))
(depth
 (cons (cons (cons (cons (cons 'b empty)
                         empty)
                   empty)
             empty)
       empty))

(define (make-deep count sym)
  (cond
    [(zero? count) sym]
    [else
     (cons (make-deep (sub1 count) sym)
           empty)]))
(make-deep 10 'a)

;; 习题 11.3.2 设计函数 tie-dyed，该函数读入一个自然数，返回一个数表，表中每个数在 20 和 120 之间。使用 tie-dyed 来测试习题 9.5.8 中的 draw-circles。
(define (tie-dyed n)
  (cond
    [(zero? n) empty]
    [else
     (cons (+ (random 100) 20)
           (tie-dyed (sub1 n)))]))

(tie-dyed 20)

;; 习题 11.3.3 设计函数 create-temps，该函数读入自然数 n 以及两个整数，分别称作 low 和 high，返回由 n个 low 和 high 之间的整数构成的表。使用 create-temps 测试习题 9.5.4 中的 check-range。
;; 最后，讨论如下问题：是否可以直接把 create-temps 的返回值传给 check-range，还是必须先知道create-temps 生成的表是什么？有没有这样的 low 和 high 的值，我们无需知道 create-temps 的返回值，就可以预言测试的结果？用自动生成的数据进行函数测试，告诉了我们什么？
(define (create-temps n low high)
  (if (< high low)
      (error
       "high must be greater than low")
      (cond
        [(zero? n) empty]
        [else
         (cons
          (+ (+ (random (- high low))
                low))
          (create-temps (sub1 n)
                        low
                        high))])))

(create-temps 10 35 100)

;; 习题 11.3.4 开发函数 create-prices，该函数读入一个自然数，返回相应的价格表，表中的价格都在.10 元和10.00 元之间，其中最小的计价单位是角。用 create-prices 来测试习题 9.5.3 中的 dollar-store?。
;; 提示：在.10 元和 10.00 元之间，总共有多少金额是角的整数倍？
(define (create-prices n)
  (cond
    [(zero? n) empty]
    [else
     (cons (/ (+ (random 990) 10) 10.0)
           (create-prices (sub1 n)))]))

(create-prices 20)